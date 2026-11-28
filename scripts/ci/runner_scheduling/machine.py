"""THE SCHEDULER'S ONE ADAPTER (test/resources/design-rules.tsv's `adapter` row): ci-runner's files (the cgroup tree,
/proc, the state directory), its processes (systemctl, docker, the token helper), the clock and GitHub's API. Every
other module of the scheduler receives a Machine and decides; this one only touches. Stdlib alone: ci-runner fetches the
scheduler's files and nothing else of movies (bitcashier infra, movies-runner-scheduler-pin.nix)."""
from __future__ import annotations

import glob
import os
import subprocess
import time
import urllib.request
from email.message import Message
from typing import Callable, Dict, List, Optional, Tuple

Runner = Callable[..., "subprocess.CompletedProcess[str]"]  # (cmd, timeout=...) -> its completed process


def run_process(cmd: List[str], timeout: int = 30) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


class Machine:
    """The host's effects. `run` runs a command (`run_process`; a test's fake answers as systemctl and docker would)."""

    def __init__(self, run: Runner = run_process):
        self._run = run

    def run(self, cmd: List[str], timeout: int = 30) -> subprocess.CompletedProcess[str]:
        return self._run(cmd, timeout=timeout)

    def read(self, path: str) -> Optional[str]:
        """The file's text; None when it cannot be read (a process or cgroup gone meanwhile)."""
        try:
            with open(path) as f:
                return f.read()
        except OSError:
            return None

    def read_text(self, path: str) -> str:
        with open(path) as f:
            return f.read()

    def write(self, path: str, text: str) -> None:
        os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
        with open(path, "w") as f:
            f.write(text)

    def remove(self, path: str) -> None:
        os.remove(path)

    def modified(self, path: str) -> float:
        return os.path.getmtime(path)

    def glob(self, pattern: str, recursive: bool = False) -> List[str]:
        return glob.glob(pattern, recursive=recursive)

    def cpu_count(self) -> int:
        return os.cpu_count() or 1

    def now(self) -> float:
        return time.time()

    def sleep(self, seconds: float) -> None:
        time.sleep(seconds)

    def http(self, method: str, url: str, body: Optional[bytes], headers: Dict[str, str],
             timeout: int = 30) -> Tuple[Message, bytes]:
        """(headers, body) of the response; an HTTP error status raises urllib's HTTPError, as urlopen does."""
        request = urllib.request.Request(url, data=body, method=method, headers=headers)
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return response.headers, response.read()
