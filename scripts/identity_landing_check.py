#!/usr/bin/env python3
"""WILL THIS LAND GREEN? One command for an identity change before it is pushed to main: the measure AND the
identity-relevant specs Main runs, one verdict.

Why: on 2026-10-22 at least six Main reds came from changes `identity-measure` had called LANDABLE. The measure
answers "did a take move", over the recording; Main's rows also hold the change to specs the measure never runs:
MatchedTakesLabelSpec (edc0614db "Patrick Monahan", the 0089ce67e..2034c8a25 Cineamo/Jailbreakers judgements),
IdentitySliceCasesSpec (3eee813cb's relay slice cases), UnmatchedClustersRatchetSpec, SearchQueryMarkersSpec, the
title-rule golden/examples/mutation specs, the hard-cluster convergence (its outage recovery), FilmScheduleEndToEnd,
IdentityCutoverEndToEnd and DocCommentPlacementSpec.

usage: scripts/identity-landing-check [<commit> | --head REF] [--base SHA] [--full | --footprint-only] [--local]
                                      [--no-measure | --no-specs] [--no-cache] [--no-what-if WHY] [--drift | --no-drift]
  <commit>         default HEAD (committed only: uncommitted changes are neither measured nor tested)
  --base           default the commit's merge-base with <remote>/main (identity-measure's)
  (no mode flag)   the measure: `identity-measure --footprint` -- the footprint measure when the change is title rules
                   alone and provably moves no take, otherwise the full measure (identity-measure falls back itself)
  --full           the full measure, never the footprint (what a change LANDS on: the footprint is for iterating),
                   over the whole convergence suite (identity-measure --full): every row Main's run has, so Main's run
                   of the same tree reuses its verdict instead of running it again (scripts/ci/convergence_reuse.py)
  --footprint-only the footprint measure or nothing (no dispatch): a measure it cannot answer is BLOCKED, said so
  --local          the specs on the laptop (scripts/sbt-warm; the hard-cluster convergence through scripts/it-local.sh),
                   one group after another -- each spec group's sbt-warm / it-local.sh takes a JVM slot (scripts/jvm-gate.sh). Default: on CI,
                   each group its own `scripts/identity-ci-run` (identity-run.yml) at the commit, all at once
  --no-measure     the specs only;  --no-specs  the measure only
  --no-cache       passed to identity-measure: measure even when its result cache holds the verdict
  --no-what-if WHY passed to identity-measure: dispatch without a scripts/identity-what-if verdict for the head and
                   base (identity-measure's what-if gate refuses otherwise); name it in your report
  --drift          the `drift` group whatever the change touches;  --no-drift  never (see THE DRIFT GROUP)

THE SPECS: exactly the identity-relevant suites of Main's rows (ci.yml), each group as its row runs it:
  unit        `test (worker-identity)` + `test (heavy1)` + `test (heavy2)` + the identity/common parts of `test (rest)` + the
              worker lint and ratchet specs: every services.identity.* (worker and identity modules), services.titles.rules.*
              (the golden, examples and mutation specs), scripts.* (the capture re-resolves), DocCommentPlacementSpec,
              HardClusterRatchetSpec
  it          `integration-test (identity)`: HardClusterConvergenceIntegrationSpec (the hard clusters, outage recovery)
  e2e-corpus  `e2e (corpus)`: FilmScheduleEndToEndSpec        } only when Main's scope would run the e2e rows for
  e2e-rest    `e2e (rest)`'s identity specs: SearchQueryMarkers,  } this change (scripts/ci/ci-row-scope.sh e2e):
              IdentityCutoverEndToEnd                          } judgements and identity tests alone do not

THE DRIFT GROUP: the incremental = whole replay -- every `takes-drift-1|2 (<country>)` row of main's convergence run,
the step behind 28 of 36 red convergence runs in the week to 2026-10-29 -- as scripts/identity-drift-ci's drift-only
run (~25-40 min for all ten rows), beside the measure and the specs. On by default when
the change touches a path in .github/identity-drift-paths.txt (the resolver, worker/src/main, the title rules, the
replay harness, the flip predicate, the allow-lists); --full runs it anyway inside the whole suite, so not again.
Each row is held against main's newest decided run (identity-drift-ci --against main): a row worse than main's -- more
open findings, or red where main's is green -- BLOCKS; red only where main is, and no worse, does not (main's red has
its own fixer).

THE VERDICT, last on stdout: `LANDABLE`, or `BLOCKED` with each failing spec, the test, and a hint for its usual cause
(e.g. MatchedTakesLabelSpec's "known-wrong in scope rows": judgement rows stale -> known-wrong scope rows). Every
group's sbt log and the measure's output stay under target/identity-landing-check/<sha>/.
Exit: 0 LANDABLE, 1 BLOCKED, 2 could not check (a group or the measure could not run: no verdict).
"""
# The --help is the header above, as the shell tool's usage_from_header printed it. How it runs: the change resolved
# (commit, base, out directory); the spec groups each a command -- identity-ci-run per group at once, or sbt-warm /
# it-local.sh one after another with --local -- beside identity-measure and, when wanted, identity-drift-ci, every one
# a command by path (IDENTITY_LANDING_CI_RUN / _MEASURE / _DRIFT name stand-ins); the logs read and the verdict
# printed by identity_landing_check_verdict.py (the sbt logs by identity_landing_check_sbt.py).
from __future__ import annotations

import os
import shutil
import sys
from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import identity_landing_check_sbt as sbt  # noqa: E402
import identity_landing_check_verdict as verdict  # noqa: E402
import tool_cli  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

TOOL = "identity-landing-check"
# name, sbt command (a root testOnly runs in every aggregated module, so one command covers worker, identity and common)
GROUPS = [("unit", "testOnly services.identity.* services.titles.rules.* scripts.* tools.DocCommentPlacementSpec "
                   "tools.HardClusterRatchetSpec"),
          ("it", "worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec")]
E2E_GROUPS = [("e2e-corpus", "e2e/testOnly services.movies.FilmScheduleEndToEndSpec"),
              ("e2e-rest", "e2e/testOnly services.movies.SearchQueryMarkersSpec "
                           "services.movies.IdentityCutoverEndToEndSpec")]
Group = Tuple[str, str]


class Die(Exception):
    """The tool could not check: said, exit 2 (no verdict)."""


class Usage(Exception):
    """Asked wrongly: said with a pointer to --help, exit 2."""


@dataclass
class Options:
    ref: str = "HEAD"
    base: str = ""
    mode: str = "footprint"
    local: bool = False
    measure: bool = True
    specs: bool = True
    no_cache: bool = False
    no_what_if: str = ""
    drift: Optional[bool] = None  # None: when the change touches a drift path


SWITCHES: Dict[str, Tuple[str, object]] = {
    "--full": ("mode", "full"), "--footprint-only": ("mode", "footprint-only"), "--local": ("local", True),
    "--no-measure": ("measure", False), "--no-specs": ("specs", False), "--no-cache": ("no_cache", True),
    "--drift": ("drift", True), "--no-drift": ("drift", False)}
VALUED = {"--head": "ref", "--base": "base", "--no-what-if": "no_what_if"}


def parse(argv: List[str]) -> Optional[Options]:
    """The options, or None for --help (printed)."""
    o, rest = Options(), list(argv)
    while rest:
        arg = rest.pop(0)
        if arg in ("-h", "--help"):
            sys.stdout.write(__doc__ or "")
            return None
        take(o, arg, rest)
    if not (o.measure or o.specs):
        raise Usage("--no-measure and --no-specs leave nothing to check")
    return o


def take(o: Options, arg: str, rest: List[str]) -> None:
    """One argument (and its value, off `rest`) into `o`."""
    if arg in SWITCHES:
        setattr(o, *SWITCHES[arg])
    elif arg in VALUED:
        if not rest:
            raise Usage(f"{arg} needs a value")
        setattr(o, VALUED[arg], rest.pop(0))
    elif arg.startswith("-"):
        raise Usage(f"unknown option {arg}")
    else:
        o.ref = arg


say = tool_cli.say_as(TOOL)


@dataclass(frozen=True)
class Change:
    commit: str
    base: str
    top: str  # the checkout's top level
    out: str  # where every log lands
    paths: tool_process.Captured  # `git diff --name-only --no-renames base commit`


def resolve(o: Options, git: tool_git.Git, env: Mapping[str, str]) -> Change:
    """The commit and its base, the out directory made afresh, both said."""
    commit = git.out("rev-parse", "--verify", "-q", f"{o.ref}^{{commit}}", quiet=True)
    if commit is None:
        raise Die(f"no commit {o.ref}")
    commit = commit.rstrip("\n")
    if o.ref == "HEAD" and git.out("status", "--porcelain", "--untracked-files=no"):
        say(f"WARNING: uncommitted changes are not checked -- only {commit[:9]}")
    base = o.base or merge_base(commit, git, env.get("IDENTITY_MEASURE_REMOTE") or "origin")
    resolved = git.out("rev-parse", "--verify", "-q", f"{base}^{{commit}}", quiet=True)
    if resolved is None:
        raise Die(f"no base commit {base}")
    base = resolved.rstrip("\n")
    top = (git.out("rev-parse", "--show-toplevel") or "").rstrip("\n")
    out = env.get("IDENTITY_LANDING_OUT") or f"{top}/target/identity-landing-check/{commit[:12]}"
    shutil.rmtree(out, ignore_errors=True)
    os.makedirs(out, exist_ok=True)
    say_commits(git, commit, base)
    return Change(commit, base, top, out, git.call("diff", "--name-only", "--no-renames", base, commit))


def say_commits(git: tool_git.Git, commit: str, base: str) -> None:
    for name, ref in (("commit", commit), ("base  ", base)):
        line = (git.out("log", "--oneline", "-1", ref) or "").split("\n")[0]
        say(f"{name} {line[:100]}")


def merge_base(commit: str, git: tool_git.Git, remote: str) -> str:
    """The commit's merge-base with <remote>/main, fetched first (its parent when the commit is on main)."""
    if not git.ok("fetch", "-q", remote, "main", timeout=120):
        say(f"could not fetch {remote} main: the base is its last fetched state")
    base = git.out("merge-base", commit, f"{remote}/main")
    if base is None:
        raise Die(f"no merge-base of {commit} and {remote}/main")
    base = base.rstrip("\n")
    return git.call("rev-parse", f"{commit}^").stdout.rstrip("\n") if base == commit else base


def changed_paths_say(change: Change, runner: tool_process.Runner, env: Mapping[str, str], argv: List[str],
                      quiet: bool = False) -> Optional[str]:
    """What a script reading the change's paths on stdin prints; None when it or the diff failed (`set -o pipefail`).
    Its stderr is passed on unless `quiet`."""
    done = runner.capture(tool_process.Command(argv, env, input=change.paths.stdout))
    if not quiet:
        sys.stderr.write(done.stderr)
        sys.stderr.flush()
    return done.stdout if change.paths.code == 0 and done.code == 0 else None


def groups_of(change: Change, runner: tool_process.Runner, env: Mapping[str, str]) -> List[Group]:
    """The spec groups: the e2e rows only when Main's scope would run them for this change."""
    e2e = changed_paths_say(change, runner, env, [os.path.join(HERE, "ci", "ci-row-scope.sh"), "e2e"], quiet=True)
    if e2e is None or e2e.rstrip("\n") == "true":
        return GROUPS + E2E_GROUPS
    say("e2e rows: Main's scope skips them for this change (scripts/ci/ci-row-scope.sh e2e) -- not run")
    return list(GROUPS)


def drift_wanted(o: Options, change: Change, runner: tool_process.Runner, env: Mapping[str, str]) -> bool:
    """THE DRIFT GROUP (the header): on when the change touches a drift path; a --full measure runs the drift rows
    itself."""
    if o.drift is not None or o.mode == "full":
        return bool(o.drift)
    github = os.path.join(HERE, "..", ".github")
    matched = changed_paths_say(change, runner, env, [os.path.join(github, "actions", "changed-paths", "matches.sh"),
                                                      os.path.join(github, "identity-drift-paths.txt")])
    return matched is not None and "true" in matched.split("\n")


class Checks:
    """The spec groups, the measure and the drift group, run side by side, each a command by path whose output lands
    under the change's out directory."""

    def __init__(self, o: Options, change: Change, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.o, self.change, self.runner, self.env = o, change, runner, env

    def tool(self, variable: str, name: str) -> str:
        return self.env.get(variable) or os.path.join(HERE, name)

    def logged(self, argv: List[str], log: str, errors: Optional[str] = None, cwd: Optional[str] = None) -> int:
        """`argv > log 2> errors` (2>&1 without `errors`), in the background as the shell's `&` ran it: stdin closed."""
        return self.runner.run(tool_process.Command(argv, self.env, log=log, stderr_log=errors, closed_stdin=True,
                                                    cwd=cwd))

    def group(self, name: str, command: str) -> None:
        """Its sbt output in <out>/<name>/sbt.log, its exit in <out>/<name>/exit (0 passed, 1 failed, 2 not run)."""
        folder = os.path.join(self.change.out, name)
        os.makedirs(folder, exist_ok=True)
        log = os.path.join(folder, "sbt.log")
        if self.o.local and name == "it":
            self.logged([os.path.join(HERE, "it-local.sh"), command], log, cwd=self.change.top)
        elif self.o.local:
            self.logged([os.path.join(HERE, "sbt-warm"), command], log)
        else:
            run = os.path.join(folder, "run")
            self.logged([self.tool("IDENTITY_LANDING_CI_RUN", "identity-ci-run"), "--head", self.change.commit,
                         "--out", run, "--", command], os.path.join(folder, "ci-run.log"))
            # the artifact's sbt.log (identity-run.yml tees it); a run that never made one is no answer
            if os.path.isfile(os.path.join(run, "sbt.log")):
                shutil.copyfile(os.path.join(run, "sbt.log"), log)
        # THE SBT LOG DECIDES, not the run's conclusion: the run's tail (its timings job) fails now and then and turns
        # a run whose every spec passed red (runs 1635, 1641, 1642 on 2026-10-23)
        with open(os.path.join(folder, "exit"), "w") as f:
            f.write(f"{sbt.verdict(log) if os.path.isfile(log) else 2}\n")

    def measure_flags(self) -> List[str]:
        flags = ["--head", self.change.commit, "--base", self.change.base, f"--{self.o.mode}"]
        flags += ["--no-cache"] if self.o.no_cache else []
        return flags + (["--no-what-if", self.o.no_what_if] if self.o.no_what_if else [])

    def measure(self) -> int:
        out = self.change.out
        return self.logged([self.tool("IDENTITY_LANDING_MEASURE", "identity-measure"), *self.measure_flags()],
                           os.path.join(out, "measure.txt"), os.path.join(out, "measure.log"))

    def drift(self) -> int:
        out = self.change.out
        return self.logged([self.tool("IDENTITY_LANDING_DRIFT", "identity-drift-ci"), "--head", self.change.commit,
                            "--against", "main", "--out", os.path.join(out, "drift", "run")],
                           os.path.join(out, "drift.txt"), os.path.join(out, "drift.log"))

    def specs(self, pool: ThreadPoolExecutor, groups: List[Group]) -> List["Future[None]"]:
        names = "".join(f"{name} " for name, _ in groups)
        if self.o.local:
            say(f"specs: on the laptop, one group after another ({names})")
            return [pool.submit(self.in_turn, groups)]
        say(f"specs: on CI, one identity-run per group, all at once ({names})")
        return [pool.submit(self.group, *g) for g in groups]

    def in_turn(self, groups: List[Group]) -> None:
        for name, command in groups:
            self.group(name, command)

    def run(self, groups: List[Group], drift: bool) -> Tuple[Optional[int], Optional[int]]:
        """Everything asked, at once; the measure's exit and the drift group's (None: not run)."""
        with ThreadPoolExecutor(max_workers=len(groups) + 2) as pool:
            drift_run = self.started(pool, drift, "drift: identity-drift-ci (every takes-drift row, drift-only)",
                                     self.drift)
            spec_runs = self.specs(pool, groups) if self.o.specs else []
            measure_run = self.started(pool, self.o.measure,
                                       f"measure: identity-measure {' '.join(self.measure_flags()[4:])}", self.measure)
            for done in spec_runs:
                done.result()
            return (None if measure_run is None else measure_run.result(),
                    None if drift_run is None else drift_run.result())

    @staticmethod
    def started(pool: ThreadPoolExecutor, wanted: bool, line: str,
                job: Callable[[], int]) -> "Optional[Future[int]]":
        if not wanted:
            return None
        say(line)
        return pool.submit(job)


def check(argv: List[str], runner: tool_process.Runner, env: Mapping[str, str]) -> int:
    o = parse(argv)
    if o is None:
        return 0
    git = tool_git.Git(runner, env, ".")
    change = resolve(o, git, env)
    groups = groups_of(change, runner, env)
    if o.local and o.specs and (git.out("rev-parse", "HEAD") or "").rstrip("\n") != change.commit:
        raise Die(f"--local tests the working tree: check out {change.commit[:9]} first (or run on CI, without --local)")
    drift = drift_wanted(o, change, runner, env)
    measure_rc, drift_rc = Checks(o, change, runner, env).run(groups, drift)
    return verdict.Verdict(change.commit, change.base, change.out).of(
        measure_rc, drift_rc, [name for name, _ in groups] if o.specs else None)


def main() -> int:
    # tool-conventions.sh's export, which the shell tool's commands inherited
    env = {**os.environ, "KINOWO_IDENTITY_CACHE": os.environ.get("KINOWO_IDENTITY_CACHE")
           or os.path.join(os.path.expanduser("~"), ".cache", "kinowo-identity")}
    try:
        return check(sys.argv[1:], tool_process.SubprocessRunner(), env)
    except Usage as e:
        print(f"{TOOL}: {e}\n{TOOL}: see {TOOL} --help", file=sys.stderr)
        return 2
    except Die as e:
        print(f"{TOOL}: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
