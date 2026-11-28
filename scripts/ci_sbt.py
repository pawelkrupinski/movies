#!/usr/bin/env python3
"""One sbt command on ci-runner's measure lane, with its verdict: the laptop's heavy test runs off the laptop.

ONE sbt COMMAND ON ci-runner's MEASURE LANE, with its verdict: the laptop's heavy test runs (a whole-module testUnit,
scripts/fail-before's two runs) off the laptop when its governor would hold them (the owner, 2026-11-18: "stop the
laptop being the bottleneck"). It runs through scripts/identity-ci-run (.github/workflows/identity-run.yml: `sbt -batch
<command>` on movies-measure, a throwaway MongoDB beside it, the GitOps manifests checked out), at a commit.

  scripts/ci-sbt [--commit <rev> | --tree] [--log <file>] [--quiet] -- '<sbt command>'

  --tree     (default) this checkout as it is: HEAD when clean, else a commit of HEAD plus its tracked changes and its
             untracked files under */src/ (a new spec), never any other untracked file (a stray .env is never pushed)
             -- the files left out are named
  --commit   that commit instead
  --log      where the command's sbt output goes (default: printed)
  --quiet    print only the verdict line

THE VERDICT is the command's own, as a local run's exit status is: the conclusion of the run job's "Run the command"
step (`sbt -batch <command>`), never the run's as a whole --
  0  the command passed (the step succeeded)
  1  the command failed (the step failed: a test, a compile error -- what a local nonzero exit is)
  2  no verdict: the run could not be made, a step before the command failed (checkout, setup, MongoDB), it was
     cancelled or timed out (KINOWO_CI_SBT_TIMEOUT seconds, default 10800), or it could not be read. Never a failure:
     scripts/fail-before must not read an infrastructure red as "fails before".
Polls every KINOWO_CI_SBT_POLL seconds (30). KINOWO_IDENTITY_CI_RUN names the dispatcher (the tests' seam)."""
from __future__ import annotations

import json
import os
import re
import shutil
import sys
import tempfile
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Mapping, Optional, cast

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
from cli import tool_cli  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
from processes import tool_shell  # noqa: E402

TOOL = "ci-sbt"
HERE = os.path.dirname(os.path.abspath(__file__))
GH_TIMEOUT = 90


class NoVerdict(Exception):
    """The run could not be made or read: said, exit 2."""


def say(text: str) -> None:
    sys.stdout.flush()
    print(f"{TOOL}: {text}", file=sys.stderr, flush=True)


@dataclass
class Options:
    commit: Optional[str] = None  # --commit's revision; None for the working tree
    log: str = ""
    quiet: bool = False
    command: str = ""


def options(argv: List[str]) -> Optional[Options]:
    """The options; None when --help asked for the header (printed)."""
    o = Options()
    if tool_cli.read_options(o, argv, (__doc__ or "").split("\n\n", 1)[1] + "\n", option) is None:
        return None
    if not o.command:
        raise tool_cli.UsageError("no command: put the sbt command after --")
    return o


def option(o: Options, argv: List[str], i: int) -> int:
    """The word at argv[i] read into `o`: the index of the last word it took (every word after `--`, the command)."""
    word = argv[i]
    if word == "--":
        o.command = " ".join(argv[i + 1:])
        return len(argv)
    if word == "--commit":
        o.commit = tool_cli.value_at(argv, i)
    elif word == "--log":
        o.log = tool_cli.value_at(argv, i)
    elif word == "--tree":
        o.commit = None
        return i
    elif word == "--quiet":
        o.quiet = True
        return i
    else:
        raise tool_cli.UsageError(f"unknown argument: {word} (the sbt command goes after --)")
    return i + 1


def command_steps(run_view: Mapping[str, Any]) -> List[str]:
    """The conclusion of the "Run the command" step of every run job (one shard: the command names no {cc})."""
    jobs = cast(List[Dict[str, Any]], run_view.get("jobs") or [])
    steps = [cast(Dict[str, Any], s) for j in jobs if str(j.get("name") or "").startswith("run ")
             for s in cast(List[Any], j.get("steps") or [])]
    return [str(s.get("conclusion")) for s in steps if s.get("name") == "Run the command"]


def verdict(run_view: Mapping[str, Any]) -> str:
    """pass, fail, or `none <the command steps' conclusions> (run <its conclusion>)`: no command step, or one not
    success / failure (skipped: an earlier step failed; cancelled), is no verdict."""
    steps = command_steps(run_view)
    if steps and all(s == "success" for s in steps):
        return "pass"
    if "failure" in steps and all(s in ("success", "failure") for s in steps):
        return "fail"
    return f"none {','.join(steps) or 'no command step'} (run {run_view.get('conclusion')})"


class CiSbt:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner) -> None:
        self.o = o
        self.env = env
        self.runner = runner
        self.git = tool_git.Git(runner, env, ".")
        self.gh = env.get("KINOWO_GH") or "gh"

    def read(self, *args: str) -> str:
        """git's stdout stripped, its stderr kept quiet; "" when it failed."""
        return (self.git.out(*args, quiet=True) or "").strip()

    def snapshot(self) -> str:
        """The commit of this checkout as it is (HEAD when nothing tracked changed and no new source)."""
        head = self.read("rev-parse", "--verify", "-q", "HEAD")
        if not head:
            raise NoVerdict("could not snapshot this checkout")
        with tempfile.TemporaryDirectory(prefix=f"{TOOL}.", dir=self.env.get("TMPDIR") or None) as scratch:
            tree = self.snapshot_tree(os.path.join(scratch, "index"))
        untracked = self.read("ls-files", "--others", "--exclude-standard").splitlines()
        left = [path for path in untracked if not re.match(r"[^/]*/src/", path)][:5]
        if left:
            say(f"untracked files outside */src/ are not run: {' '.join(left)}")
        if tree == self.read("rev-parse", f"{head}^{{tree}}"):
            return head
        # a throwaway commit: an identity where none is configured (a CI container), the configured one otherwise
        name = self.read("config", "user.name") or TOOL
        email = self.read("config", "user.email") or "ci-sbt@localhost"
        short = self.read("rev-parse", "--short", head)
        done = self.runner.capture(tool_process.Command(
            ["git", "-c", f"user.name={name}", "-c", f"user.email={email}", "commit-tree", tree, "-p", head, "-m",
             f"ci-sbt: the working tree of {short}"], self.env))
        if done.code != 0:
            raise NoVerdict("could not snapshot this checkout")
        return done.stdout.strip()

    def snapshot_tree(self, index: str) -> str:
        """The tree of HEAD's index plus every tracked change and every untracked file under */src/, written through
        a private index (the checkout's own never staged)."""
        own = self.read("rev-parse", "--git-path", "index")
        if os.path.exists(own):
            # copy2, keeping the index's mtime: git re-reads an entry changed in the tick the index was written only by
            # comparing the two, and a fresh mtime would read it as clean (Tool self-tests #533)
            shutil.copy2(own, index)
        env = {**self.env, "GIT_INDEX_FILE": os.path.abspath(index)}
        private = tool_git.Git(self.runner, env, ".")
        private.call("add", "-u", "--", ".", quiet=True)
        sources = self.read("ls-files", "--others", "--exclude-standard", "--", "*/src/*").splitlines()
        if sources:
            private.call("add", "--", *sources, quiet=True)
        tree = (private.out("write-tree") or "").strip()
        if not tree:
            raise NoVerdict("could not snapshot this checkout")
        return tree

    def commit(self) -> str:
        if self.o.commit is None:
            return self.snapshot()
        commit = self.read("rev-parse", "--verify", "-q", f"{self.o.commit}^{{commit}}")
        if not commit:
            raise NoVerdict(f"no commit {self.o.commit}")
        return commit

    def dispatch(self, commit: str) -> str:
        dispatcher = self.env.get("KINOWO_IDENTITY_CI_RUN") or os.path.join(HERE, "identity-ci-run")
        done = tool_process.capture_loudly(self.runner, tool_process.Command(
            [dispatcher, "--head", commit, "--no-wait", "--countries", "pl", "--command", self.o.command], self.env))
        if done.code != 0:
            raise NoVerdict(f"could not dispatch `{self.o.command}` at {commit[:9]} (identity-ci-run)")
        lines = done.stdout.splitlines()
        run = lines[-1] if lines else ""
        if not re.fullmatch(r"[0-9]+", run):
            raise NoVerdict(f"identity-ci-run gave no run id (got: {run})")
        say(f"run {run}: `sbt {self.o.command}` at {commit[:9]} on ci-runner's measure lane")
        return run

    def view(self, run: str) -> Dict[str, Any]:
        """The run's status, conclusion and jobs; {} when gh could not read it."""
        done = self.runner.capture(tool_process.Command(
            [self.gh, "run", "view", run, "--json", "status,conclusion,jobs"], self.env, timeout=GH_TIMEOUT))
        try:
            return cast(Dict[str, Any], json.loads(done.stdout)) if done.code == 0 else {}
        except ValueError:
            return {}

    def completed(self, run: str) -> Dict[str, Any]:
        """The run's view once it completed; NoVerdict past the deadline."""
        timeout = int(self.env.get("KINOWO_CI_SBT_TIMEOUT") or "10800")
        deadline = int(machine.monotonic()) + timeout
        while True:
            run_view = self.view(run)
            if run_view.get("status") == "completed":
                return run_view
            if int(machine.monotonic()) >= deadline:
                raise NoVerdict(f"run {run} did not complete within {timeout}s")
            time.sleep(int(self.env.get("KINOWO_CI_SBT_POLL") or "30"))

    def sbt_log(self, run: str) -> None:
        """The run's sbt log: printed, into --log, or (--quiet) neither."""
        with tempfile.TemporaryDirectory(prefix=f"{TOOL}.", dir=self.env.get("TMPDIR") or None) as jobs:
            if not tool_shell.download_run(self.runner, self.env, GH_TIMEOUT, run, "identity-run*", jobs):
                say(f"could not download run {run}'s sbt log")
                return
            logs = sorted(os.path.join(d, f) for d, _, files in os.walk(jobs) for f in files
                          if f.startswith("sbt") and f.endswith(".log"))
            if not logs:
                return
            if self.o.log:
                shutil.copyfile(logs[0], self.o.log)
            elif not self.o.quiet:
                with open(logs[0], errors="replace") as f:
                    sys.stdout.write(f.read())
                sys.stdout.flush()

    def main(self) -> int:
        run = self.dispatch(self.commit())
        found = verdict(self.completed(run))
        self.sbt_log(run)
        if found == "pass":
            say(f"run {run}: PASSED `{self.o.command}`")
            return 0
        if found == "fail":
            say(f"run {run}: FAILED `{self.o.command}`")
            return 1
        say(f"run {run}: NO VERDICT: the command step is {found[len('none '):]}")
        return 2


def main() -> int:
    code, o = tool_cli.parse_call(TOOL, "", lambda: options(sys.argv[1:]))
    if o is None:
        return code or 0
    try:
        return CiSbt(o, dict(machine.environment()), tool_process.SubprocessRunner()).main()
    except NoVerdict as e:
        say(str(e))
        return 2


if __name__ == "__main__":
    sys.exit(main())
