"""WHERE the alert backtest's history comes from (infra/bin/backtest-alerts): production's own Prometheus (`LiveSource`),
or a pulled copy of its blocks (`Snapshot`) that a throwaway Prometheus of the version production runs (`Replay`) is
started over, queried and torn down."""

from __future__ import annotations

import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
from types import TracebackType
from typing import Any, Callable, Dict, List, Literal, Mapping, Optional, Set, Type

from backtest_clock import SYSTEM, Clock
from backtest_prometheus import Prometheus


def loaded_rules(document: Mapping[str, Any], kind: str) -> Dict[str, str]:
    """{name: query} of the `kind` ("alerting" / "recording") rules in a /api/v1/rules document."""
    return {r["name"]: r["query"] for g in document["data"]["groups"] for r in g["rules"]
            if r.get("type") == kind}


class LiveSource(object):
    """Production's Prometheus itself: what it has loaded now, its retention floor, the window to now."""

    def __init__(self, prom: Prometheus, clock: Clock = SYSTEM) -> None:
        self.prom, self.clock = prom, clock

    def loaded_alerts(self) -> Set[str]:
        return self.prom.loaded_alerts()

    def loaded_recordings(self) -> Dict[str, str]:
        return self.prom.loaded_recordings()

    def floor(self) -> Optional[float]:
        return self.prom.lowest_timestamp()

    def latest(self) -> float:
        return self.clock.now()


def read_json(path: str) -> Any:
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


class Snapshot(object):
    """A pulled copy of production's blocks (DIR/tsdb/<ulid>/), with what production had loaded
    (rules.json, buildinfo.json) and the range its blocks cover (pull.json), all captured with it."""

    def __init__(self, directory: str) -> None:
        self.directory = os.path.realpath(directory)
        self.tsdb = os.path.join(self.directory, "tsdb")
        self.pull = read_json(os.path.join(self.directory, "pull.json"))
        self.rules = read_json(os.path.join(self.directory, "rules.json"))
        self.buildinfo = read_json(os.path.join(self.directory, "buildinfo.json"))
        self.version: str = self.buildinfo["data"]["version"]

    def loaded_alerts(self) -> Set[str]:
        return set(loaded_rules(self.rules, "alerting"))

    def loaded_recordings(self) -> Dict[str, str]:
        return loaded_rules(self.rules, "recording")

    def floor(self) -> Optional[float]:
        return float(self.pull["min_time"])

    def latest(self) -> float:
        return float(self.pull["max_time"])


def replay_arguments(binary: str, config: str, data: str, port: int, jobs: int) -> List[str]:
    """The throwaway Prometheus's command line. No scrape targets and no rules (`config` is empty), so
    nothing is written but its own lock and WAL; compaction off (min = max block duration), so the
    copied blocks are never rewritten; retention far past any block, so none is deleted; a query
    timeout long enough that a heavy rule over a day finishes rather than being split."""
    return [binary,
            "--config.file=" + config,
            "--storage.tsdb.path=" + data,
            "--storage.tsdb.retention.time=3650d",
            "--storage.tsdb.min-block-duration=2h",
            "--storage.tsdb.max-block-duration=2h",
            "--web.listen-address=127.0.0.1:%d" % port,
            "--query.timeout=10m",
            "--query.max-concurrency=%d" % max(jobs, 4),
            "--query.max-samples=500000000",
            "--log.level=warn"]


def _clone(source: str, target: str) -> bool:
    """A copy-on-write clone (`cp -c`, APFS clonefile); a failed one leaves nothing at `target`."""
    cloned = subprocess.run(["cp", "-c", "-R", "-L", source, target], capture_output=True).returncode == 0
    return cloned or _discard(target)


def _discard(target: str) -> bool:
    shutil.rmtree(target, ignore_errors=True)
    return False


def _no_clone(source: str, target: str) -> bool:
    return False


_CLONERS: Dict[str, Callable[[str, str], bool]] = {"darwin": _clone}


def private_copy(source: str, target: str, platform: str = sys.platform) -> None:
    """`source` copied to `target` for the replay's Prometheus to own, following symlinks (infra/bin/backtest-alerts-local
    links its cached blocks in). On macOS a copy-on-write clone (`cp -c`, APFS clonefile): two weeks of blocks cost
    no time and no space, and the replay's writes land in the clone, never in the cache. Elsewhere, or when the clone
    fails (another volume), a plain copy, as CI's run has always taken."""
    if not _CLONERS.get(platform, _no_clone)(source, target):
        shutil.copytree(source, target)


def _free_port() -> int:
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        return int(probe.getsockname()[1])


def _write_empty_config(path: str) -> None:
    with open(path, "w", encoding="utf-8") as handle:
        handle.write("global: {}\n")


def _tail(path: str) -> str:
    with open(path, encoding="utf-8") as handle:
        return handle.read()[-2000:]


def _stop(process: "subprocess.Popen[bytes]") -> None:
    process.terminate()
    try:
        process.wait(60)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()


def _stop_if_running(process: Optional["subprocess.Popen[bytes]"]) -> None:
    if process is not None and process.poll() is None:
        _stop(process)


def _remove(directory: Optional[str]) -> None:
    if directory:
        shutil.rmtree(directory, ignore_errors=True)


class Replay(object):
    """A throwaway Prometheus over a PRIVATE copy of a snapshot, for one run: the copy is taken so the
    snapshot itself (read-only on ci-runner) is never touched, and is removed with the process."""

    def __init__(self, snapshot: Snapshot, binary: str, jobs: int, ready_timeout: float = 300,
                 clock: Clock = SYSTEM) -> None:
        self.snapshot, self.binary, self.jobs, self.ready_timeout = snapshot, binary, jobs, ready_timeout
        self.clock = clock
        self.work: Optional[str] = None
        self.process: Optional["subprocess.Popen[bytes]"] = None

    def __enter__(self) -> Prometheus:
        started = self.clock.now()
        self.work = tempfile.mkdtemp(prefix="backtest-replay-")
        log = os.path.join(self.work, "prometheus.log")
        try:
            prom = self._launch(self.work, log)
            self._await_ready(prom, log)
            self._check_version(prom)
        except RuntimeError:
            self.__exit__(None, None, None)
            raise
        print("replay: Prometheus %s over %s, ready in %.0f s"
              % (self.snapshot.version, self.snapshot.directory, self.clock.now() - started))
        return prom

    def _launch(self, work: str, log: str) -> Prometheus:
        data, config = os.path.join(work, "data"), os.path.join(work, "prometheus.yml")
        private_copy(self.snapshot.tsdb, data)
        _write_empty_config(config)
        port = _free_port()
        with open(log, "w", encoding="utf-8") as handle:
            self.process = subprocess.Popen(replay_arguments(self.binary, config, data, port, self.jobs),
                                            stdout=handle, stderr=subprocess.STDOUT)
        return Prometheus("http://127.0.0.1:%d" % port, None)

    def _await_ready(self, prom: Prometheus, log: str) -> None:
        deadline = self.clock.now() + self.ready_timeout
        while not prom.ready():
            if (self.process is not None and self.process.poll() is not None) or self.clock.now() > deadline:
                raise RuntimeError("the replay Prometheus did not become ready:\n" + _tail(log))
            self.clock.pause(1)

    def _check_version(self, prom: Prometheus) -> None:
        version = prom.get("/api/v1/status/buildinfo", {})["version"]
        if version != self.snapshot.version:
            raise RuntimeError("the replay Prometheus is %s, production ran %s: install the same version"
                               % (version, self.snapshot.version))

    def __exit__(self, exc_type: Optional[Type[BaseException]], exc: Optional[BaseException],
                 traceback: Optional[TracebackType]) -> Literal[False]:
        _stop_if_running(self.process)
        _remove(self.work)
        return False
