#!/usr/bin/env python3
"""ANY sbt COMMAND OVER A CACHED RECORDING, ON CI: .github/workflows/identity-run.yml at a commit of yours, off the laptop
(the user's rule, 2026-10-21: measurements, ablations, simulations and replay sweeps run on CI). The CI twin of
a local `scripts/it-local.sh 'worker/IntegrationTest/runMain …'` over a laid recording.

  scripts/identity-ci-run [--recording RUN|prod-latest] [--countries pl,de] [--head REF] [--heap 8g] [--out DIR] [--no-wait]
                          [--command '<sbt command>' ...] [-- '<sbt command>']

  SIDE BY SIDE, a movies-measure job each (scripts/ci/identity_run_shards.py): a command naming {cc} runs once per
  country, and each command (every --command, and the one after --) in a job of its own -- so two countries' commands
  that name their own files (no {cc}) go as two --command, never one `a; b` sbt line, which runs them one after the
  other (run 42370669779: 28 min in a row). Every job's output is laid into the one --out directory.

  the command's placeholders: {cc} the country (the command runs once per country when it names it, else once),
  {fixtures} the laid pair's test/resources/fixtures, {corpus} its corpus/, {out} what comes back (per country with {cc})
  e.g. scripts/identity-ci-run --recording 528 --countries pl,de -- \\
         'worker/IntegrationTest/runMain integration.IdentityResolveDump --out {out} --full {cc} --corpus-dir {corpus} --fixtures {fixtures}'
  no --recording: any sbt target at the commit, nothing laid (e.g. -- 'worker/testOnly deploy.*' or -- 'Test/compile')
  --head     the commit run (default HEAD; uncommitted changes are NOT run). Pushed as measure/<user>/<sha>-<rand>, deleted
             when the run ends (as scripts/identity-measure)
  --out      where the `identity-run` artifact lands (default target/identity-ci-run/<run>)
  --no-wait  dispatch, print the run, exit; fetch later with  gh run download <run> -p 'identity-run*' -D DIR (a
             directory a job; laid as one by this tool's collect)

Typical wall time: ~3 min of setup (recording download, sbt) plus the longest job's command. Exit: the run's own (0 success, 1 not),
2 when it could not be made."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(HERE, "lib"))
import identity_dispatch  # noqa: E402
import tool_ci_wait  # noqa: E402
import tool_dispatch  # noqa: E402
import tool_git  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402

TOOL = "identity-ci-run"
WORKFLOW = "identity-run.yml"


class Die(Exception):
    """The tool could not do its work: said, exit 2."""


class UsageError(Exception):
    pass


@dataclass
class Options:
    recording: str = ""
    countries: str = "pl,de,uk,us,es"
    head: str = "HEAD"
    heap: str = "8g"
    out: str = ""
    no_wait: bool = False
    commands: List[str] = field(default_factory=lambda: [])


VALUED = {"--recording": "recording", "--countries": "countries", "--country": "countries", "--head": "head",
          "--heap": "heap", "--out": "out"}


def parse(argv: List[str]) -> Optional[Options]:
    """The options, or None for --help (printed)."""
    o, rest = Options(), list(argv)
    while rest:
        arg = rest.pop(0)
        if arg == "--":
            o.commands += [" ".join(rest)] if rest else []
            break
        if not take(o, arg, rest):
            return None
    if not o.commands:
        raise UsageError("no command: put the sbt command after -- (or --command, one a job)")
    return o


def take(o: Options, arg: str, rest: List[str]) -> bool:
    """One flag (and its value, off `rest`) into `o`; False for --help, printed."""
    if arg in ("-h", "--help"):
        print(__doc__)
        return False
    if arg == "--no-wait":
        o.no_wait = True
        return True
    if arg not in VALUED and arg != "--command":
        raise UsageError(f"unknown argument: {arg} (the command goes after --)")
    if not rest:
        raise UsageError(f"{arg} needs a value")
    value = rest.pop(0)
    if arg == "--command":
        o.commands.append(value)
    else:
        setattr(o, VALUED[arg], value)
    return True


def say(line: str) -> None:
    print(f"{TOOL}: {line}", file=sys.stderr, flush=True)


def collect(jobs: str, out: str) -> bool:
    """identity-run.yml's jobs each upload an identity-run-<shard> artifact (gh run download --pattern puts each in a
    directory of its own): laid into one `out`, the one-job run's layout again. Whether there was any."""
    os.makedirs(out, exist_ok=True)
    found = False
    for name in sorted(os.listdir(jobs)):
        artifact = os.path.join(jobs, name)
        if name.startswith("identity-run") and os.path.isdir(artifact):
            shutil.copytree(artifact, out, dirs_exist_ok=True)
            found = True
    return found


class CiRun:
    def __init__(self, o: Options, env: Mapping[str, str], runner: tool_process.Runner) -> None:
        self.o, self.env, self.runner = o, env, runner
        self.gh_timeout = float(env.get("IDENTITY_CI_RUN_GH_TIMEOUT") or 90)
        self.github = tool_github.GitHub(runner, env, env.get("KINOWO_GH") or "gh")
        self.git = tool_git.Git(runner, env, os.getcwd())
        dispatcher = tool_dispatch.Dispatcher(self.github, tool_dispatch.Settings.of(env, self.gh_timeout))
        self.ci = identity_dispatch.MeasureCi(runner, env, self.git, dispatcher)
        self.remote = env.get("IDENTITY_MEASURE_REMOTE") or "origin"
        self.branch = ""

    def main(self) -> int:
        self.resolve_recording()
        commit = self.commit()
        run = self.dispatched(commit, self.pushed(commit))
        if self.o.no_wait:
            print(run)
            return 0
        minutes = int(self.env.get("IDENTITY_CI_RUN_TIMEOUT_MINUTES") or 660)
        conclusion = tool_ci_wait.wait_for_run(self.runner, self.env, run, minutes * 60,
                                               int(self.env.get("IDENTITY_CI_RUN_POLL") or 30))
        if conclusion is None:
            raise Die(f"run {run} gave no conclusion in time")
        say(f"run {run}: {conclusion}")
        self.lay(run)
        return 0 if conclusion == "success" else 1

    def resolve_recording(self) -> None:
        """prod-latest: the freshest complete nightly recording, resolved once, by the first country's marker."""
        if self.o.recording == "prod-latest":
            done = tool_shell.shell_function(self.runner, self.env, "scripts/tool-conventions.sh", "prod_latest_run",
                                             [self.o.countries.split(",")[0]])
            if done.code != 0:
                raise Die("")
            self.o.recording = done.stdout.strip()
            say(f"--recording prod-latest is run {self.o.recording}")
        if self.o.recording and not re.fullmatch(r"[0-9]+", self.o.recording):
            raise UsageError(f"--recording takes a run id or prod-latest, not {self.o.recording}")

    def commit(self) -> str:
        commit = (self.git.out("rev-parse", "--verify", "-q", f"{self.o.head}^{{commit}}", quiet=True) or "").strip()
        if not commit:
            raise Die(f"no commit {self.o.head}")
        if self.o.head == "HEAD" and (self.git.out("status", "--porcelain", "--untracked-files=no", quiet=True) or ""):
            say(f"WARNING: uncommitted changes are not run -- only {commit}")
        return commit

    def pushed(self, commit: str) -> str:
        """The commit, or -- its workflows lacking identity-run.yml -- main's .github/ and scripts/ci/ laid over it."""
        if self.git.ok("cat-file", "-e", f"{commit}:.github/workflows/{WORKFLOW}", quiet=True):
            return commit
        try:
            push = self.ci.main_ci_fetched_over(self.remote, commit, "measure")
        except identity_dispatch.DispatchFailed as e:
            raise Die(str(e)) from None
        say(f"{commit} predates {WORKFLOW}: running it under {self.remote}/main's .github/ and scripts/ci/ ({push})")
        return push

    def dispatched(self, commit: str, push: str) -> str:
        """Before the dispatch the branch is ours to delete; after it, the run's."""
        self.branch = self.ci.branch(commit)
        error = self.ci.push(self.remote, push, self.branch)  # one measure/ push at a time on this machine
        if error is not None:
            self.delete_branch()
            raise Die(f"could not push {self.branch}: {error}")
        say(f"pushed {self.branch}")
        inputs = ["-f", f"recording={self.o.recording}", "-f", f"countries={self.o.countries}",
                  "-f", f"command={chr(10).join(self.o.commands)}", "-f", f"heap={self.o.heap}"]
        try:
            run = self.ci.dispatch(WORKFLOW, self.branch, push, f"{TOOL} {commit[:9]}", inputs)
        except identity_dispatch.DispatchFailed as e:
            if not str(e).startswith("dispatched"):
                self.delete_branch()
            raise Die(str(e)) from None
        url = (self.github.read(["run", "view", run, "--json", "url", "--jq", ".url"], self.gh_timeout, quiet=True)
               or "").strip()
        say(f"run {url or run} (it deletes {self.branch} when it ends)")
        return run

    def delete_branch(self) -> None:
        self.git.ok("push", "-q", self.remote, "--delete", f"refs/heads/{self.branch}", timeout=120, quiet=True)

    def lay(self, run: str) -> None:
        top = (self.git.out("rev-parse", "--show-toplevel", quiet=True) or os.getcwd()).strip()
        out = self.o.out or os.path.join(top, "target", TOOL, run)
        with tempfile.TemporaryDirectory(prefix=f"{TOOL}.", dir=self.env.get("TMPDIR") or None) as jobs:
            done = tool_shell.shell_function(self.runner, self.env, "scripts/tool-conventions.sh", "gh_download", [
                str(int(self.gh_timeout)), "run", "download", run, "--pattern", "identity-run*", "--dir", jobs])
            if done.code != 0:
                raise Die(f"could not download run {run}'s identity-run artifacts")
            if not collect(jobs, out):
                raise Die(f"run {run} left no identity-run artifact")
        say(f"output in {out}")


def main(argv: List[str], env: Dict[str, str]) -> int:
    try:
        o = parse(argv)
        if o is None:
            return 0
        return CiRun(o, env, tool_process.SubprocessRunner()).main()
    except UsageError as e:
        say(str(e))
        say(f"see {TOOL} --help")
        return 2
    except Die as e:
        if str(e):
            say(str(e))
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
