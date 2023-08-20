from __future__ import annotations

import asyncio
import logging
from asyncio import gather
from contextlib import AsyncExitStack
from dataclasses import asdict
from pathlib import Path
from typing import Dict

from aiohttp import ClientSession
from joblib import Parallel, delayed
from tqdm.auto import tqdm
from tqdm.std import tqdm as tqdm_instance

from components.common.logging import get_logger
from components.common.nitta_node import NittaNodeInTree
from components.common.saving import save_dicts_list_to_csv_with_timestamp
from components.data_crawling.leaf_metrics_collector import LeafMetricsCollector
from components.data_crawling.nitta_running import NittaRunResult, run_nitta_server
from components.data_crawling.sampling_processing import SamplingResult, process_and_save_sampling_results
from components.data_crawling.tree_processing import assemble_training_data_via_backpropagation_from_leaf
from components.data_crawling.tree_retrieving import retrieve_random_descending_thread, retrieve_tree_root
from components.utils.tqdm_joblib import tqdm_joblib
from consts import DATA_DIR, EXAMPLES_DIR

logger = get_logger(__name__)

DEFAULT_N_SAMPLES = 5000
DEFAULT_N_SAMPLES_PER_BATCH = 150  # was empirically found to yield maximum samples/s
DEFAULT_N_WORKERS = 1
DEFAULT_N_NITTAS = 1
DEFAULT_NITTA_RUN_COMMAND = "stack exec nitta --"


async def produce_data_for_example(
    example: Path,
    data_dir: Path = DATA_DIR,
    **runner_kwargs,
) -> SamplingResult | None:
    """
    This function shouldn't raise any exceptions, so the burden of handling errors generated by the circus inside
    run_example_and_sample_tree does not leak outside this function. It can be safely called straight from main().
    """
    # TODO: rename "example" to "target algorithm" everywhere?
    logger.info(f"Starting producing model input data from a target algorithm at {example}")

    if not example.exists():
        logger.error(f"Couldn't find a target algorithm description at '{example.absolute()}'.")
        return None

    results: list[dict] = []

    try:
        await run_example_and_sample_tree(
            example,
            results_accum=results,
            **runner_kwargs,
        )
    except KeyboardInterrupt:
        logger.info("Interrupted by user, processing nodes gathered so far.")
    except Exception:
        logger.exception("Unexpected error, processing nodes gathered so far.")

    return process_and_save_sampling_results(example, results, data_dir)


CrawlConfig = Dict[Path, dict]  # example_filename -> runner_kwargs
MANUAL_FULL_CRAWL_CONFIG: CrawlConfig = {
    EXAMPLES_DIR / "spi3.lua": dict(n_samples_per_batch=149, n_samples=39757),
    EXAMPLES_DIR / "sum.lua": dict(n_samples_per_batch=131, n_samples=28014),
    EXAMPLES_DIR / "constantFolding.lua": dict(n_samples_per_batch=128, n_samples=26607),
    EXAMPLES_DIR / "pid.lua": dict(n_samples_per_batch=72, n_samples=14356),
    EXAMPLES_DIR / "teacup.lua": dict(n_samples_per_batch=88, n_samples=16559),
    EXAMPLES_DIR / "generated/matrix-mult-1x3.lua": dict(n_samples_per_batch=98, n_samples=18219),
    EXAMPLES_DIR / "generated/cyclic3.lua": dict(n_samples_per_batch=95, n_samples=17677),
    EXAMPLES_DIR / "generated/cyclic4.lua": dict(n_samples_per_batch=63, n_samples=13345),
    EXAMPLES_DIR / "generated/cyclic5.lua": dict(n_samples_per_batch=59, n_samples=12933),
    EXAMPLES_DIR / "generated/vars.lua": dict(n_samples_per_batch=27, n_samples=10452),
}
"""
Those examples are manually chosen to have:
1) ~400k training data rows per example
2) ~50% grand total negative label share

How it was done:
- all examples were evaluated with a fixed test number of samples
- their stats (see csv crawling summary) were examined (negative label share and average result rows yield per sample)
- only examples with 0 < neg_label_share < 1 were taken (presented here) <- COULD BE IMPROVED, lots of examples with =1
- n_samples was chosen based on avg_result_rows_per_sample so that the number of result rows is ~400k per example
- batch sizes are adjusted so the frequency of progress bar updates (once per batch) looks consistent

TODO: automation script for all this optimal crawl config selection procedure?
"""


async def produce_data_for_many_examples(crawl_config: CrawlConfig | None = None):
    if crawl_config is None:
        logger.info("Using the default hardcoded state of the art crawl config.")
        crawl_config = MANUAL_FULL_CRAWL_CONFIG

    examples_str = "\n\t".join(str(e) for e in crawl_config.keys())
    logger.info(f"Producing the data for {len(crawl_config)} examples: \n\t{examples_str}")

    collected_stats: list[dict] = []

    for i, (example, kwargs) in enumerate(crawl_config.items()):
        # intentionally using root logger here, otherwise module name is too long and this bar is shifted to the right
        logging.info(f"===================== Processing {example} ({i+1} / {len(crawl_config)}) =====================")
        sampling_result = await produce_data_for_example(example, **kwargs)
        if sampling_result is not None:
            collected_stats.append({example: example.name, **asdict(sampling_result.stats)})

    logger.info(f"Done! Produced stats for {len(collected_stats)} examples.")
    save_dicts_list_to_csv_with_timestamp(collected_stats, DATA_DIR, "stats", what="crawling summary")


# TODO: split modules in two (produce + run)


async def run_example_and_sample_tree(
    example: Path,
    n_samples: int = DEFAULT_N_SAMPLES,
    n_samples_per_batch=DEFAULT_N_SAMPLES_PER_BATCH,
    n_workers: int = DEFAULT_N_WORKERS,
    n_nittas: int = DEFAULT_N_NITTAS,
    nitta_run_command: str = DEFAULT_NITTA_RUN_COMMAND,
    results_accum: list[dict] | None = None,
) -> list[dict]:
    """
    Instead of evaluating the whole tree and only then processing it, we sample the tree (randomly descend many times)
    and process samples on the fly. This way we:
    - can parallelize processing of a single example tree, which was troublesome before
    - can handle huge trees (astronomical number of nodes)  <--- this is the main reason

    This way we also don't need to store the whole tree in RAM, but caching tree node info in RAM speeds things up
    significantly, so trees should be kept in RAM when possible.Major part of RAM is used by NITTA, not Python, so
    for now it's enough to restart the NITTAs (by restarting the sampling process).

    We sacrifice some accuracy, but it seems to be a reasonable trade-off.

    About n_nittas: for Intel Core i5-12400F (6 cores, 12 threads)
    - 1 NITTA is enough to give 80%-100% load on all cores, seems optimal
    - 2+ NITTAs lead to huge performance drop (context switching, perhaps)
    So, n_nittas=1 seems the best choice for now.

    About n_workers: we can parallelize on IO waits thanks to asyncio. It seems enough, because multiprocessing doesn't
    give any performance boost (perhaps, even the opposite due to the overhead). So, n_workers=1 fits best too.
    """
    # eariler we used deque here, but that was needed only for O(1) appendleft.
    # now we don't use appendleft anymore, so list should be fine.
    if results_accum is None:
        results_accum = []

    async with AsyncExitStack() as stack:
        nittas: list[NittaRunResult] = await gather(
            *[stack.enter_async_context(run_nitta_server(example, nitta_run_command)) for _ in range(n_nittas)],
        )
        nitta_baseurls = list(await gather(*[nitta.get_base_url() for nitta in nittas]))
        session = await stack.enter_async_context(ClientSession())

        logger.info("Retrieving tree root...")
        root = await retrieve_tree_root(nitta_baseurls[0], session)

        n_batches = n_samples // n_samples_per_batch + 1

        logger.info(
            f"\n\t=== Sampling tree of {example.name}: ==="
            + f"\n\t{n_batches} batches"
            + f"\n\t{n_samples_per_batch} samples per batch"
            + f"\n\t=> {n_samples} total samples"
            + f"\n\tdistributed over {n_workers} worker process(es)"
            + f"\n\twith {n_nittas} NITTA instance(s) running",
        )

        tqdm_args: dict = dict(total=n_samples, desc=f"Sampling {example.name}", unit="samples")

        if n_workers > 1:
            with tqdm_joblib(n_samples_per_batch, **tqdm_args):
                results_accum.extend(
                    sum(
                        Parallel(n_jobs=n_workers)(
                            delayed(_retrieve_and_process_tree_with_sampling_remote_job)(
                                nitta_baseurl=nitta_baseurls[i % len(nitta_baseurls)],
                                root=root,
                                n_samples=n_samples_per_batch,
                                n_samples_per_batch=n_samples_per_batch,
                                example_name=example.name,
                            )
                            for i in range(n_batches)
                        ),
                        [],
                    ),
                )
        else:
            # set logging level to INFO for tqdm (otherwise DEBUG messages will break tqdm's progress bar)
            logging.getLogger().setLevel(logging.INFO)

            with tqdm(**tqdm_args) as pbar:
                await _retrieve_and_process_tree_with_sampling(
                    results_accum=results_accum,
                    session=session,
                    nitta_baseurl=nitta_baseurls[0],
                    root=root,
                    metrics_collector=LeafMetricsCollector(),
                    n_samples=n_samples,
                    n_samples_per_batch=n_samples_per_batch,
                    pbar=pbar,
                    example_name=example.name,
                )

            logging.getLogger().setLevel(logging.DEBUG)

    return results_accum


def _retrieve_and_process_tree_with_sampling_remote_job(**kwargs):
    """
    A remote worker process job that does necessary initialization before calling
    `_retrieve_and_process_tree_with_sampling`.
    """

    async def _async_job():
        async with ClientSession() as session:
            return await _retrieve_and_process_tree_with_sampling(
                **kwargs,
                session=session,
                metrics_collector=LeafMetricsCollector(),
                results_accum=[],
            )

    return asyncio.run(_async_job())


async def _retrieve_and_process_tree_with_sampling(
    results_accum: list[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    n_samples: int,
    n_samples_per_batch: int,
    pbar: tqdm_instance | None = None,
    example_name: str | None = None,
):
    """
    Implements sampling-based tree gathering and processing into training data.

    Forks the sampling into N coroutines-batches and uses `asyncio.gather` to parallelize their IO waits.
    """
    for batch_n in range(n_samples // n_samples_per_batch + 1):
        samples_left = (
            n_samples_per_batch if batch_n < n_samples // n_samples_per_batch else n_samples % n_samples_per_batch
        )
        await asyncio.gather(
            *[
                _retrieve_and_process_single_tree_sample(
                    results_accum,
                    session,
                    nitta_baseurl,
                    root,
                    metrics_collector,
                    example_name,
                )
                for _ in range(samples_left)
            ],
        )
        if pbar is not None:
            pbar.update(samples_left)

    return results_accum


async def _retrieve_and_process_single_tree_sample(
    results_accum: list[dict],
    session: ClientSession,
    nitta_baseurl: str,
    root: NittaNodeInTree,
    metrics_collector: LeafMetricsCollector,
    example_name: str | None = None,
):
    leaf = await retrieve_random_descending_thread(root, nitta_baseurl, session, ignore_dirty_tree=True)
    metrics_collector.collect_leaf_node(leaf)
    return assemble_training_data_via_backpropagation_from_leaf(
        results_accum,
        leaf,
        metrics_collector,
        example_name=example_name,
    )
