import asyncio
import logging
from pathlib import Path
from typing import Dict, List

import pandas as pd

from components.common.logging import configure_logging, get_logger
from components.common.saving import save_df_with_timestamp
from components.data_crawling.example_running import run_example_and_sample_tree
from components.data_crawling.sampling_processing import (
    process_and_save_sampling_results,
)
from consts import DATA_DIR, EXAMPLES_DIR

logger = get_logger(__name__)

# fmt: off
_EXAMPLES_TO_CRAWL: Dict[Path, dict] = {
    Path("examples/spi3.lua"): {"n_samples_per_batch": 149, "n_samples": 39757},
    Path("examples/sum.lua"): {"n_samples_per_batch": 131, "n_samples": 28014},
    Path("examples/constantFolding.lua"): {"n_samples_per_batch": 128, "n_samples": 26607},
    Path("examples/generated/matrix-mult-1x3.lua"): {"n_samples_per_batch": 98, "n_samples": 18219},
    Path("examples/generated/cyclic3.lua"): {"n_samples_per_batch": 95, "n_samples": 17677},
    Path("examples/teacup.lua"): {"n_samples_per_batch": 88, "n_samples": 16559},
    Path("examples/pid.lua"): {"n_samples_per_batch": 72, "n_samples": 14356},
    Path("examples/generated/cyclic4.lua"): {"n_samples_per_batch": 63, "n_samples": 13345},
    Path("examples/generated/cyclic5.lua"): {"n_samples_per_batch": 59, "n_samples": 12933},
    Path("examples/generated/vars.lua"): {"n_samples_per_batch": 27, "n_samples": 10452},
} # { Path: kwargs }
# fmt: on


def _run_safe(example: Path):
    results: List[dict] = []

    kwargs: Dict = dict(
        n_samples=5000,
        n_samples_per_batch=20,
    )
    kwargs.update(_EXAMPLES_TO_CRAWL.get(example, {}))

    logger.info(f"Processing {example}")
    try:
        asyncio.run(
            # TODO: replace with produce_data_for_example?
            run_example_and_sample_tree(
                example=example,
                results_accum=results,
                **kwargs,
            )
        )
    except Exception:
        logger.exception(f"Encountered exception processing {example}, finishing with {len(results)} results")

    processed_results = process_and_save_sampling_results(example, results)
    if processed_results:
        _, stats = processed_results
        return stats


if __name__ == "__main__":
    configure_logging(base_level=logging.INFO)

    examples = list(_EXAMPLES_TO_CRAWL.keys()) or list(sorted(EXAMPLES_DIR.glob("**/*.lua")))

    examples_str = "\n\t".join(map(str, examples))
    logger.info(f"Processing {len(examples)} examples: \n\t{examples_str}")

    stats = []

    for example in examples:
        logger.info(f"======================= Processing {example} =======================")
        example_stats = _run_safe(example)
        if example_stats is not None:
            stats.append(example_stats)

    save_df_with_timestamp(pd.DataFrame(stats), DATA_DIR, "stats", what="crawling summary")
