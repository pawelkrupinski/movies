"""Prometheus's HTTP API for the alert backtest (infra/bin/backtest-alerts): `curl` for the transport, because only
curl speaks SOCKS without a dependency, and the range fetch that halves a query that timed out."""

from __future__ import annotations

import json
import subprocess
import urllib.parse
from backtest_clock import SYSTEM, Clock
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, Set, Tuple


class QueryTimeout(RuntimeError):
    """Prometheus could not evaluate a query inside its query timeout (or curl gave up waiting)."""


_TIMEOUT_MARKERS = ("query timed out", "context deadline exceeded", "curl: (28)")
_PERMANENT_MARKERS = ("exceeded maximum resolution", "parse error")


def is_query_timeout(message: str) -> bool:
    """Whether an error says the query ran out of time -- which a SMALLER range can fix -- rather
    than being wrong (a parse error) or the path to Prometheus failing (a proxy drop), which it cannot."""
    return any(m in message for m in _TIMEOUT_MARKERS)


def fetch_range(get_chunk: Callable[[float, float], Sequence[Mapping[str, Any]]], start: float, end: float, step: float,
                chunk: float = 86400, min_chunk: float = 3600) -> Dict[str, Set[float]]:
    """{series key: set(timestamps)} over [start, end] on a `step` grid, asking `get_chunk(t0, t1)`
    for `chunk`-second ranges. A range that times out is HALVED (on the step grid) and both halves
    asked, down to `min_chunk`: a heavy rule over a day can exceed Prometheus's query timeout where
    the same rule over six hours does not, and retrying the day whole only times out again. A range
    still timing out at the floor raises QueryTimeout."""
    series: Dict[str, Set[float]] = {}

    def fetch(t0: float, t1: float) -> None:
        try:
            results = get_chunk(t0, t1)
        except QueryTimeout:
            if t1 - t0 < 2 * min_chunk:
                raise
            mid = t0 + ((t1 - t0) // 2 // step) * step
            fetch(t0, mid)
            fetch(mid + step, t1)
            return
        for result in results:
            key = json.dumps(result["metric"], sort_keys=True)
            series.setdefault(key, set()).update(float(v[0]) for v in result["values"])

    t = start
    while t <= end:
        chunk_end = min(t + chunk - step, end)
        fetch(t, chunk_end)
        t = chunk_end + step
    return series


def _json_body(stdout: str) -> Mapping[str, Any]:
    body: Any
    try:
        body = json.loads(stdout)
    except ValueError:
        body = None
    return body if isinstance(body, dict) else {"status": "error", "error": stdout[:200]}


def reply_of(returncode: int, stdout: str, stderr: str) -> Tuple[Any, Optional[str]]:
    """(data, None) for a successful reply, (None, the error) for one worth asking again; a reply that fails the
    same way every time (a parse error, a range past the maximum resolution) raises RuntimeError."""
    if returncode != 0:
        return None, stderr.strip()[:200]
    body = _json_body(stdout)
    if body.get("status") == "success":
        return body["data"], None
    error = str(body.get("error", "unknown error"))
    if any(marker in error for marker in _PERMANENT_MARKERS):
        raise RuntimeError(error)
    return None, error


def get_command(url: str, socks: Optional[str], params: Mapping[str, Any], timeout: float) -> List[str]:
    command = ["curl", "-sS", "-G", "--max-time", str(timeout), "--data", urllib.parse.urlencode(params, doseq=True)]
    if socks:
        command += ["--socks5-hostname", socks]
    return command + [url]


def ready_command(url: str, socks: Optional[str]) -> List[str]:
    command = ["curl", "-s", "--max-time", "5", url + "/-/ready"]
    if socks:
        command[1:1] = ["--socks5-hostname", socks]
    return command


def _curl(command: Sequence[str]) -> "subprocess.CompletedProcess[str]":
    return subprocess.run(command, capture_output=True, text=True)


class Prometheus(object):
    """Prometheus's HTTP API over curl."""

    def __init__(self, url: str, socks: Optional[str], clock: Clock = SYSTEM) -> None:
        self.url, self.socks, self.clock = url.rstrip("/"), socks, clock

    def get(self, path: str, params: Mapping[str, Any], timeout: float = 180) -> Any:
        command = get_command(self.url + path, self.socks, params, timeout)
        error = "no attempt made"
        for attempt in range(3):
            out = _curl(command)
            data, failure = reply_of(out.returncode, out.stdout, out.stderr)
            if failure is None:
                return data
            error = failure
            # A timeout fails the same way again at the same size: the caller splits the range instead.
            if is_query_timeout(error):
                raise QueryTimeout(error)
            self.clock.pause(2 * (attempt + 1))
        raise RuntimeError(error)

    def ready(self) -> bool:
        return _curl(ready_command(self.url, self.socks)).returncode == 0

    def metric_names(self, start: float, end: float) -> Set[str]:
        return set(self.get("/api/v1/label/__name__/values", {"start": start, "end": end}))

    def loaded_alerts(self) -> Set[str]:
        data = self.get("/api/v1/rules", {"type": "alert"})
        return {r["name"] for g in data["groups"] for r in g["rules"]}

    def loaded_recordings(self) -> Dict[str, str]:
        data = self.get("/api/v1/rules", {"type": "record"})
        return {r["name"]: r["query"] for g in data["groups"] for r in g["rules"]}

    def format_query(self, expr: str) -> str:
        return str(self.get("/api/v1/format_query", {"query": expr}))

    def lowest_timestamp(self) -> Optional[float]:
        data = self.get("/api/v1/query", {"query": "min(prometheus_tsdb_lowest_timestamp_seconds)"})
        return float(data["result"][0]["value"][1]) if data["result"] else None

    def presence(self, expr: str, start: float, end: float, step: float, chunk: float = 86400) -> Dict[str, Set[float]]:
        """{series key: set(timestamps)} for `expr` over [start, end] on a `step` grid."""
        def get_chunk(t0: float, t1: float) -> Sequence[Mapping[str, Any]]:
            return self.get("/api/v1/query_range", {"query": expr, "start": t0, "end": t1, "step": step})["result"]
        return fetch_range(get_chunk, start, end, step, chunk=chunk, min_chunk=min(3600, chunk))
