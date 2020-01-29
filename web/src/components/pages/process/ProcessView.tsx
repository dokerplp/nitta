import React, { useEffect, useState } from "react";
import { haskellApiService } from "../../../services/HaskellApiService";
import { ProcessTimelines, TimelinePoint, TimelineWithViewPoint, ViewPointID } from "../../../gen/types";

import "./ProcessView.scss";
import { TimelineView } from "./TimelineView";
import { AppContext, IAppContext } from "../../app/AppContext";
import { useContext } from "react";
import { AxiosError } from "axios";

export interface Highlight {
  up: number[];
  current: number[];
  down: number[];
}

// TODO: diff from previous synthesis process step
// TODO: highlight point by click on info part
export const ProcessView: React.FC = () => {
  const appContext = useContext(AppContext) as IAppContext;

  const [pIdIndex, setPIdIndex] = useState<Record<number, TimelinePoint<number>> | null>(null);
  const [highlight, setHighlight] = useState<Highlight>({ up: [], current: [], down: [] } as Highlight);
  const [detail, setDetail] = useState<TimelinePoint<number>[]>([] as TimelinePoint<number>[]);
  const [data, setData] = useState<ProcessTimelines<number> | null>(null);

  useEffect(() => {
    setDetail([]);
    setHighlight({ up: [], current: [], down: [] });
    haskellApiService
      .getTimelines(appContext.selectedNodeId)
      .then((response: { data: ProcessTimelines<number> }) => {
        console.log("> ProcessView.requestTimelines - done");
        let pIdIndex: Record<number, TimelinePoint<number>> = {};
        response.data.timelines.forEach(vt => {
          vt.timelinePoints.forEach(point => {
            point.forEach(e => {
              const x: number = e.pID;
              pIdIndex[x] = e;
            });
          });
        });
        let resort = resortTimeline(response.data);
        setData(resort);
        setPIdIndex(pIdIndex);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId]);

  return (
    <div className="p-3 d-flex flex-nowrap">
      {!data ? (
        <pre>LOADING</pre>
      ) : data.timelines.length === 0 ? (
        <pre>EMPTY PROCESS TIMELINE</pre>
      ) : (
        <>
          <TimelineView
            timelines={data.timelines}
            highlight={highlight}
            data={data}
            onHighlightChange={h => setHighlight(h)}
            onDetailChange={d => setDetail(d)}
          />
          <div className="ml-2 flex-grow-1" style={{ minWidth: "30%" }}>
            <hr />
            <div className="squeeze upRelation">upper related:</div>
            <div className="x-scrollable">
              {highlight.up.map(e =>
                pIdIndex !== null && pIdIndex[e] !== undefined ? <div className="squeeze">- {pIdIndex[e].pInfo}</div> : ""
              )}
            </div>
            <hr />
            <div className="squeeze current">current:</div>
            <div className="x-scrollable">
              {detail.map(e => (
                <div className="squeeze">- {e.pInfo}</div>
              ))}
            </div>
            <hr />
            <div className="squeeze downRelation">bottom related:</div>
            <div className="x-scrollable">
              {highlight.down.map(e =>
                pIdIndex != null && pIdIndex[e] != null ? <div className="squeeze">-- {pIdIndex[e].pInfo}</div> : ""
              )}
            </div>
          </div>
        </>
      )}
    </div>
  );
};

function resortTimeline(data: ProcessTimelines<number>) {
  let result: ProcessTimelines<number> = {
    timelines: [],
    verticalRelations: data.verticalRelations,
  };
  function cmp(a: TimelineWithViewPoint<number>, b: TimelineWithViewPoint<number>) {
    if (a.timelineViewpoint.component < b.timelineViewpoint.component) return -1;
    if (a.timelineViewpoint.component > b.timelineViewpoint.component) return 1;
    return 0;
  }
  let tmp: TimelineWithViewPoint<number>[] = data.timelines.sort(cmp);
  function extract(p: (id: ViewPointID) => boolean) {
    let newTmp: TimelineWithViewPoint<number>[] = [];
    tmp.forEach(e => {
      if (p(e.timelineViewpoint)) {
        result.timelines.push(e);
      } else {
        newTmp.push(e);
      }
    });
    tmp = newTmp;
  }
  extract(e => e.component.length === 0);
  extract(e => e.level === "CAD");
  extract(e => e.level === "Fun");
  extract(e => e.level === "EndPoint");
  extract(e => true);
  return result;
}