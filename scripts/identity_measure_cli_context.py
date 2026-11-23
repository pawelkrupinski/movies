"""What every part of identity-measure (scripts/identity_measure_cli.py) shares: the options, the measurement's state
(the commit, base and countries it measures, the branch and run it made), gh and git bounded through the injected
runner, the sleeper and the clocks, the shared cache's paths, and the classification by scripts/identity_measure.py."""
from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from typing import Callable, List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_github  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402
from identity_measure_cli_options import Options  # noqa: E402

TOOL = "identity-measure"
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"
IDENTITY_TOOLS = "scripts/identity-tools.sh"
# A takes row's job is named `leg (<country>) / takes (<country>)` (the reusable leg's job inside the leg's matrix
# job; `<country> / takes` before it): takes_country is the country of a takes row's name, nothing for any other job.
TAKES_ROW_JQ = 'def takes_country: capture("^(leg \\\\()?(?<country>[a-z-]+)\\\\)? / takes( \\\\([a-z-]+\\\\))?$").country;'


class Die(Exception):
    """The measurement could not be made: said, exit 2."""


class Exit(BaseException):
    """The tool ends here with this status, its clean-up run."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def say(line: str) -> None:
    print(f"{TOOL}: {line}", file=sys.stderr, flush=True)


def csv_and(a: str, b: str) -> str:
    """a ∩ b, in a's order."""
    theirs = b.split(",")
    return ",".join(cc for cc in a.split(",") if cc and cc in theirs)


@dataclass
class State:
    commit: str = ""
    base: str = ""
    recording: str = ""
    countries: str = ""
    named: str = ""  # the countries named (or narrowed to), as the workflow's takes-countries; empty: every leg
    branch: str = ""
    push: str = ""
    measured_run: str = ""
    finished: bool = False
    keep: bool = False
    convergence_run: str = ""
    convergence_done: bool = False
    collected_table: str = ""
    collected_judgements: str = ""


class Context:
    def __init__(self, o: Options, env: Mapping[str, str], runner: tool_process.Runner, scratch: str,
                 clock: Callable[[], float], now: Callable[[], float]) -> None:
        self.o, self.env, self.runner, self.scratch, self.clock, self.now = o, env, runner, scratch, clock, now
        self.gh_timeout = float(env.get("IDENTITY_MEASURE_GH_TIMEOUT") or 90)
        self.poll = int(env.get("IDENTITY_MEASURE_POLL") or 30)
        self.github = tool_github.GitHub(runner, env, env.get("KINOWO_GH") or "gh")
        self.git = tool_git.Git(runner, env, os.getcwd())
        self.remote = env.get("IDENTITY_MEASURE_REMOTE") or "origin"
        self.cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(env.get("HOME", ""), ".cache", "kinowo-identity")
        self.state = State(recording=o.recording)
        self.scratches = 0

    def gh(self, args: Sequence[str], timeout: Optional[float] = None, quiet: bool = False) -> Optional[str]:
        return self.github.read(args, timeout or self.gh_timeout, quiet)

    def sleep(self, seconds: int) -> None:
        """The `sleep` command, as the shell tool slept (a test stands one in on PATH)."""
        self.runner.run(tool_process.Command(["sleep", str(seconds)], self.env))

    def measured(self, run: str, *parts: str) -> str:
        return os.path.join(self.cache, "measure", run, *parts)

    def scratch_dir(self) -> str:
        self.scratches += 1
        path = os.path.join(self.scratch, str(self.scratches))
        os.makedirs(path)
        return path

    def oneline(self, commit: str) -> str:
        return (self.git.out("log", "--oneline", "-1", commit) or "").rstrip("\n")

    def say_commit_and_base(self) -> None:
        say(f"commit {self.oneline(self.state.commit)}")
        say(f"base   {self.oneline(self.state.base)}")

    def shell(self, library: str, function: str, args: Sequence[str]) -> tool_process.Captured:
        return tool_shell.shell_function(self.runner, self.env, library, function, args)

    def judgements_into(self, commit: str, path: str) -> bool:
        """The commit's judgements into `path`, else the remote main's (said)."""
        if self.show_into(f"{commit}:{JUDGEMENTS}", path):
            return True
        say(f"{commit} holds no identity-judgements.tsv: classifying with {self.remote}/main's")
        return self.show_into(f"{self.remote}/main:{JUDGEMENTS}", path)

    def show_into(self, spec: str, path: str) -> bool:
        text = self.git.out("show", spec, quiet=True)
        if text is None:
            return False
        with open(path, "w") as f:
            f.write(text)
        return True

    def classify(self, directory: str, judgements: str, rows: str, table: str, more: List[str]) -> int:
        """scripts/identity_measure.py over the takes in `directory`: its table on stdout and in `table`; its verdict.
        `-B` beside `-I`: -I drops PYTHONDONTWRITEBYTECODE, and the modules it imports would leave scripts/ci/__pycache__
        in the checkout."""
        argv = ["python3", "-I", "-B", os.path.join(HERE, "identity_measure.py"), directory, "--judgements", judgements,
                "--countries", self.state.countries, "--rows", rows, *more, *(["--json"] if self.o.json else [])]
        done = self.runner.capture(tool_process.Command(argv, self.env))
        sys.stdout.write(done.stdout)
        sys.stdout.flush()
        sys.stderr.write(done.stderr)
        with open(table, "w") as f:
            f.write(done.stdout)
        return done.code
