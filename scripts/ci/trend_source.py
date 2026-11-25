"""The runs a selection names (scripts/ci/ci_trends.py's header lists the forms): records for a branch's newest runs, a
commit range, run ids or a time window -- VictoriaLogs' events first (with a URL), then the API's for what it lacks.

    from trend_source import Source
"""
from __future__ import annotations

import concurrent.futures
import re
import subprocess
import sys
from typing import Any, Dict, Iterable, List, Optional, Sequence, Set

from trend_events import records_from_events, vl_query
from trend_github import GitHub, latest_attempts, record_of
from trend_measures import run_window
from trend_records import Record, iso, parse_when

# Runs fetched side by side (Source.fetch): GitHub's secondary rate limit allows far more concurrent reads than this.
FETCH_THREADS = 8
_COMMIT_RANGE = re.compile(r"[0-9a-fA-F]{6,40}\.\.[0-9a-fA-F]{6,40}")


def workflow_matches(record: Record, workflow: Optional[str]) -> bool:
    if not workflow:
        return True
    return (record.get("path") or "").endswith("/" + workflow) or record.get("wf") == workflow


def commits_in(spec: str) -> Set[str]:
    out = subprocess.run(["git", "rev-list", spec], check=True, capture_output=True, text=True, timeout=60).stdout
    return set(out.split())


def _started(record: Record) -> float:
    return run_window(record)[0] or 0


class Source:
    """Records for a selection: VictoriaLogs' events first (with a URL), then the API's for what it lacks."""

    def __init__(self, github: GitHub, vl_url: Optional[str] = None, include_red: bool = False,
                 with_metrics: bool = True) -> None:
        self.github = github
        self.vl_url = vl_url
        self.include_red = include_red
        self.with_metrics = with_metrics

    def _keep(self, run: Record) -> bool:
        if run.get("status") != "completed" or run.get("conclusion") in ("cancelled", "skipped"):
            return False
        return self.include_red or run.get("conclusion") == "success"

    def _keep_record(self, record: Record) -> bool:
        if record.get("concl") in ("cancelled", "skipped"):
            return False
        return self.include_red or record.get("concl") == "success"

    def from_vl(self, since: Optional[float], until: Optional[float], workflow: Optional[str] = None,
                run_ids: Optional[Sequence[int]] = None) -> List[Record]:
        if not self.vl_url:
            return []
        query = 'source:ci repo:"%s" kind:in(run,job,step,cache)' % self.github.repo
        if workflow:
            query += ' workflow_file:"%s"' % workflow
        if run_ids:
            query += " run_id:in(%s)" % ",".join(str(i) for i in run_ids)
        try:
            return [r for r in records_from_events(vl_query(self.vl_url, query, since, until)) if r["jobs"]]
        except Exception as error:  # noqa: BLE001 -- the API still answers
            print("ci-timings: VictoriaLogs unreadable (%s); reading the API" % error, file=sys.stderr)
            return []

    def _record(self, run: Record) -> Record:
        return record_of(self.github, run, self.with_metrics)

    def fetch(self, runs: Iterable[Record]) -> List[Record]:
        """Every run's record, FETCH_THREADS at a time: one run is 1-3 sequential API calls (jobs, artifacts, their
        zips), so a cold selection of 40 runs took minutes one by one. Completed runs are cached on disk after."""
        runs = list(runs)
        if len(runs) < 2:
            return [self._record(run) for run in runs]
        with concurrent.futures.ThreadPoolExecutor(FETCH_THREADS) as pool:
            return list(pool.map(self._record, runs))

    def _by_ids(self, spec: str) -> List[Record]:
        ids = [int(x) for x in spec[5:].split(",") if x.strip()]
        held = {r["id"]: r for r in self.from_vl(None, None, None, ids)}
        missing = [i for i in ids if i not in held]
        with concurrent.futures.ThreadPoolExecutor(FETCH_THREADS) as pool:
            fetched = dict(zip(missing, self.fetch(list(pool.map(self.github.run, missing)))))
        return [held.get(i) or fetched[i] for i in ids]

    def _by_commits(self, spec: str, workflow: Optional[str]) -> List[Record]:
        shas = commits_in(spec)
        return self.fetch([r for r in self.github.runs(workflow=workflow) if r.get("head_sha") in shas and self._keep(r)])

    def _kept_runs(self, workflow: Optional[str], **query: Any) -> List[Record]:
        return [r for r in self.github.runs(workflow=workflow, **query) if self._keep(r)]

    def _newest_on(self, spec: str, workflow: Optional[str]) -> List[Record]:
        branch, _, count_text = spec.partition(":")
        count = int(count_text or 10)
        runs = self._kept_runs(workflow, branch=branch, limit=count * 3)[:count]
        held = {r["id"]: r for r in self.from_vl(None, None, workflow, [run["id"] for run in runs])}
        fetched = {r["id"]: r for r in self.fetch([run for run in runs if run["id"] not in held])}
        return [held.get(run["id"]) or fetched[run["id"]] for run in runs]

    def select(self, spec: str, workflow: Optional[str]) -> List[Record]:
        """A selection (the header's forms) as records, oldest first."""
        spec = spec.strip()
        records: List[Any]
        if spec.startswith("runs:"):
            records = self._by_ids(spec)
        elif _COMMIT_RANGE.fullmatch(spec):
            records = self._by_commits(spec, workflow)
        elif ".." in spec:
            since, until = (parse_when(part) for part in spec.split("..", 1))
            records = self.window(workflow, since, until)
        else:
            records = self._newest_on(spec, workflow)
        records = [r for r in records if r and r["jobs"] and workflow_matches(r, workflow)]
        return sorted(latest_attempts(records), key=_started)

    def _runs_between(self, workflow: Optional[str], since: float, until: float) -> List[Record]:
        """The API's kept runs created a day either side of the window; none when the API cannot say."""
        created = "%s..%s" % (iso(since - 86400)[:10], iso(until + 86400)[:10])
        try:
            return self._kept_runs(workflow, created=created)
        except Exception:  # noqa: BLE001 -- VictoriaLogs' runs still stand
            return []

    def window(self, workflow: Optional[str], since: float, until: float) -> List[Record]:
        """Every kept run of the workflow whose first job started in [since, until]: VictoriaLogs', then the API's."""
        held: Dict[Any, Record] = {r["id"]: r for r in latest_attempts(self.from_vl(since, until, workflow))
                                   if self._keep_record(r)}
        for record in self.fetch([run for run in self._runs_between(workflow, since, until) if run["id"] not in held]):
            held[record["id"]] = record
        return [r for r in held.values() if r["jobs"] and since <= _started(r) <= until]
