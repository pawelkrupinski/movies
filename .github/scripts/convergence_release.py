"""What the convergence scripts (convergence_setup.py and its siblings) ask GitHub, over an injected runner: a download
that may legitimately find nothing (scripts/ci/gh-optional.sh: present, absent, or a FAILED read) and a run's
recording time. gh's own messages go to stderr as they came; a candidate for scripts/lib's GitHub module once it lands."""
from __future__ import annotations

import datetime
import os
import sys
from typing import Callable, List, Mapping, Optional, Sequence, TextIO

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "scripts/lib"))
import tool_actions  # noqa: E402
import tool_process  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
GH_OPTIONAL = os.path.join(ROOT, "scripts/ci/gh-optional.sh")


class Exit(Exception):
    """The step ends here with this status (a shell `exit` from inside a function)."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


class ReadFailed(Exception):
    """A read GitHub did not answer (an auth refusal, a 5xx past its retries): never an absent asset."""


class GitHub:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.runner = runner
        self.env = env
        self.gh = env.get("KINOWO_GH") or "gh"

    def captured(self, argv: Sequence[str], env: Optional[Mapping[str, str]] = None,
                 quiet: bool = False) -> tool_process.Captured:
        """`$(argv)`: its stdout captured, its stderr passed on (dropped when quiet)."""
        done = self.runner.capture(tool_process.Command(list(argv), env or self.env))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def optional(self, *args: str, release_exists: bool = False) -> bool:
        """Whether gh-optional.sh downloaded it (False: absent); ReadFailed on a failed read."""
        env = dict(self.env, GH_DOWNLOAD_RELEASE_EXISTS="1") if release_exists else self.env
        done = self.captured([GH_OPTIONAL, *args], env)
        if done.code != 0:
            raise ReadFailed(" ".join(args))
        return done.stdout.strip() == "present"

    def created_at(self, run: str) -> str:
        """The run's createdAt, "" when it cannot be read (it only labels a log line and the provenance)."""
        done = self.captured([self.gh, "run", "view", run, "--json", "createdAt", "--jq", ".createdAt"], quiet=True)
        return done.stdout.rstrip("\n") if done.code == 0 else ""

    def green_recordings(self) -> List[List[str]]:
        """`[run, recordedAt]` of the last 10 green `Record scrape fixtures` runs, newest first."""
        done = self.captured([self.gh, "run", "list", "--workflow", "Record scrape fixtures", "--status", "success",
                              "--limit", "10", "--json", "databaseId,createdAt", "--jq",
                              '.[] | "\\(.databaseId) \\(.createdAt)"'])
        return [line.split(None, 1) + [""] * (2 - len(line.split(None, 1))) for line in done.stdout.splitlines()
                if line.strip()]


class Step:
    """One step's collaborators, composed once by its tool: the environment, the runner, its stdout, GitHub, the
    step commands and the clock."""

    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner, out: TextIO,
                 clock: Callable[[], datetime.datetime]) -> None:
        self.env = env
        self.runner = runner
        self.out = out
        self.clock = clock
        self.github = GitHub(runner, env)
        self.actions = tool_actions.Actions(env, out)

    def say(self, line: str) -> None:
        self.out.write(line + "\n")
        self.out.flush()

    def run_or_exit(self, argv: Sequence[str]) -> None:
        """`set -e`'s command: its failure ends the step with its status."""
        status = self.runner.run(tool_process.Command(list(argv), self.env))
        if status != 0:
            raise Exit(status)
