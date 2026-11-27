"""What the convergence scripts (convergence_setup.py and its siblings) ask GitHub, over an injected runner: a download
that may legitimately find nothing (scripts/ci/gh-optional.sh: present, absent, or a FAILED read) and a run's
recording time. gh's own messages go to stderr as they came; a candidate for scripts/lib's GitHub module once it lands."""
from __future__ import annotations

import datetime
import os
import sys
from typing import Callable, Dict, List, Mapping, NamedTuple, Optional, Sequence, TextIO, Type

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "scripts/lib"))
import machine  # noqa: E402
import tool_actions  # noqa: E402
import tool_cli  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
GH_OPTIONAL = os.path.join(ROOT, "scripts/ci/gh-optional.sh")
GH_RELEASE = os.path.join(ROOT, "scripts/ci/gh-release.sh")
TOOL_CONVENTIONS = os.path.join(ROOT, "scripts/tool-conventions.sh")  # a release write, retried on GitHub's transient answers


class Exit(Exception):
    """The step ends here with this status (a shell `exit` from inside a function)."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


class ReadFailed(Exception):
    """A read GitHub did not answer (an auth refusal, a 5xx past its retries): never an absent asset."""


class GitHub(tool_github.GitHub):
    """tool_github's gh (KINOWO_GH, or gh), and the family's own reads: gh-optional.sh, gh_download, the runs."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        super().__init__(runner, env, env.get("KINOWO_GH") or "gh")

    def captured(self, argv: Sequence[str], env: Optional[Mapping[str, str]] = None, quiet: bool = False,
                 bound: Optional[float] = None) -> tool_process.Captured:
        """`$(argv)`: its stdout captured, its stderr passed on (dropped when quiet); ended past `bound` seconds."""
        done = self.runner.capture(tool_process.Command(list(argv), env or self.env, timeout=bound))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def optional(self, *args: str, release_exists: bool = False, timeout: Optional[str] = None,
                 bound: Optional[float] = None) -> bool:
        """Whether gh-optional.sh downloaded it (False: absent); ReadFailed on a failed read, or one past `bound`
        seconds (each attempt's own bound: `timeout`, its GH_OPTIONAL_TIMEOUT)."""
        env = dict(self.env)
        if release_exists:
            env["GH_DOWNLOAD_RELEASE_EXISTS"] = "1"
        if timeout:
            env["GH_OPTIONAL_TIMEOUT"] = timeout
        done = self.captured([GH_OPTIONAL, *args], env, bound=bound)
        if done.code != 0:
            raise ReadFailed(" ".join(args))
        return done.stdout.strip() == "present"

    def gh_download(self, seconds: int, args: Sequence[str], env: Mapping[str, str], quiet: bool = False) -> int:
        """tool-conventions.sh's gh_download (its one classifier of absent against failed, its retries): 0 downloaded,
        3 absent, else failed. gh's messages pass to stderr (dropped when quiet), its stdout too."""
        done = self.runner.capture(tool_process.Command(
            ["bash", "-c", '. "$1"; shift; gh_download "$@" >&2', "_", TOOL_CONVENTIONS, str(seconds), *args], env))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done.code

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


class Usage(NamedTuple):
    """A subcommand's arguments: `least` to `most` of them (with `more`, any after `most` ignored, as a shell
    function's unread "$@" is), and the usage line naming them."""
    least: int
    most: int
    line: str
    more: bool = False


def dispatch(step: Step, tool: str, table: Dict[str, Usage], overall: str, argv: List[str]) -> int:
    """`<tool> <subcommand> <args...>`: the step's method of that name (`-` as `_`), its `Exit` its status."""
    command, args = (argv[0], argv[1:]) if argv else ("", [])
    if command not in table:
        raise tool_cli.UsageError(f"usage: {tool} {overall}")
    usage = table[command]
    if len(args) < usage.least or (len(args) > usage.most and not usage.more):
        raise tool_cli.UsageError(f"usage: {tool} {usage.line}")
    handler: Callable[..., int] = getattr(step, command.replace("-", "_"))
    try:
        return handler(*args[:usage.most])
    except Exit as e:
        return e.code


def main(step: Type[Step], tool: str, table: Dict[str, Usage], overall: str, argv: List[str]) -> int:
    """A convergence script's composition point: the environment, the real runner, stdout and the clock."""
    composed = step(machine.environment(), tool_process.SubprocessRunner(), sys.stdout,
                    lambda: datetime.datetime.now(datetime.timezone.utc))
    return tool_cli.run_main(lambda: dispatch(composed, tool, table, overall, argv))


def regular_files(*roots: str) -> int:
    """`find <roots> -type f | wc -l`, a missing root none."""
    return sum(1 for root in roots for top, _, files in os.walk(root) for f in files
               if os.path.isfile(os.path.join(top, f)) and not os.path.islink(os.path.join(top, f)))
