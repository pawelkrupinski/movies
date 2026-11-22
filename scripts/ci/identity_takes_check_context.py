"""What every part of the identity takes check reads (identity_takes_check.py): the row's country, out dir, head and
checkout, its environment and settings, its collaborators (runner, gh, git, clock, sleep), and the one way it speaks --
a line to stdout, to stderr, or to stdout and the verdict (and the step summary) at once, as `| tee -a` does."""
from __future__ import annotations

import os
import sys
import time
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Optional

import tool_git
import tool_github
import tool_process

IDENTITY_SBT = ("sbt", "-J-Duser.language=en", "-J-Duser.country=US")  # identity-tools.sh's IDENTITY_SBT


class Stop(Exception):
    """The check ends here with `code` (2: no verdict)."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


@dataclass
class Context:
    cc: str
    out: str                  # absolute
    here: str                 # the checkout the row replays in (the working directory)
    head: str
    env: Mapping[str, str]
    temp: str                 # RUNNER_TEMP, or a directory of this run's own
    runner: tool_process.Runner
    gh: tool_github.GitHub
    git: tool_git.Git
    clock: Callable[[], float] = time.time
    sleep: Callable[[float], None] = time.sleep
    mark: float = 0.0         # where the phase being timed began (the shell's SECONDS=0)

    def setting(self, name: str, default: str) -> str:
        """`${NAME:-default}`."""
        return self.env.get(name) or default

    def seconds(self, name: str, default: str) -> float:
        return float(self.setting(name, default))

    @property
    def label(self) -> str:
        return f"identity takes ({self.cc})"

    def say(self, line: str) -> None:
        sys.stdout.write(line + "\n")
        sys.stdout.flush()

    def warn(self, line: str) -> None:
        sys.stderr.write(line + "\n")
        sys.stderr.flush()

    def tee(self, line: str, *files: str) -> None:
        """The line to stdout and appended to each file (the verdict, the step summary)."""
        self.say(line)
        for path in files:
            with open(path, "a") as f:
                f.write(line + "\n")

    def verdict(self, line: str, summary: bool = False) -> None:
        """`| tee -a verdict.txt [$GITHUB_STEP_SUMMARY]`."""
        extra = [self.env["GITHUB_STEP_SUMMARY"]] if summary and self.env.get("GITHUB_STEP_SUMMARY") else []
        self.tee(line, self.path("verdict.txt"), *extra)

    def fail(self, line: str) -> Stop:
        """`echo line | tee verdict.txt; exit 2`: the verdict replaced by why there is none."""
        self.say(line)
        write(self.path("verdict.txt"), line + "\n")
        return Stop(2)

    def path(self, *parts: str) -> str:
        return os.path.join(self.out, *parts)

    def fixtures(self, *parts: str) -> str:
        return os.path.join(self.here, "test/resources/fixtures", *parts)

    def restart(self) -> None:
        self.mark = self.clock()

    def elapsed(self) -> int:
        return int(self.clock() - self.mark)

    def timing(self, phase: str, how: str, seconds: Optional[int] = None) -> None:
        """A row of <out>/timings.tsv: the phase, its seconds (since `restart`, unless given), how it was had."""
        with open(self.path("timings.tsv"), "a") as f:
            f.write(f"{phase}\t{self.elapsed() if seconds is None else seconds}\t{how}\n")

    def sbt(self, *words: str) -> List[str]:
        return [*IDENTITY_SBT, *words]

    def with_env(self, extra: Dict[str, str]) -> Dict[str, str]:
        return {**self.env, **extra}

def write(path: str, text: str) -> None:
    with open(path, "w") as f:
        f.write(text)


def read(path: str) -> str:
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def nonempty(path: str) -> bool:
    return os.path.isfile(path) and os.path.getsize(path) > 0
