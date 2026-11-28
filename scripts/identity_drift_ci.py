#!/usr/bin/env python3
"""THE PRE-LAND DRIFT CHECK: a commit's `takes-drift-1|2|... (<country>)` rows -- "Check that the incremental state ends
where a whole resolve does" (scripts/ci/identity-drift-check.sh), the step behind 28 of 36 red convergence runs in the
week to 2026-10-29 -- run on CI BEFORE the commit lands, and nothing else of the convergence suite.

Why (docs: convergence-failures-2026-10-29, §5 Tier 2 and §7.2): main's full suite gives a drift verdict ~48 minutes
after a push (red) or ~80 (green), and only once the commit is on main; fixers' "fixes" were refuted there, one after
another. This dispatches Identity model convergence with `drift-only` (10 rows, not 37), so one
red row is judged in ~12-16 minutes and all ten in ~25-40, before the push.

usage: scripts/identity-drift-ci [<commit> | --head REF] [--countries pl,de,uk,us,es] [--seeds S1,S2|S1|,S2|both]
                                 [--for-red <run> | --against <run>|main] [--timeout <minutes>]
                                 [--out DIR] [--no-wait] [--profile] [--lane red-fix|landing|measure]
       scripts/identity-drift-ci --run <run id> [--out DIR]
  <commit>     default HEAD (committed only; a commit whose workflows predate drift-only gets main's .github/ and
               scripts/ci/ laid over it, as identity-measure does)
  --countries  the countries whose drift rows run (the workflow's takes-countries); default all five
  --seeds      the seeds the rows replay (pins, scripts/ci/drift_seeds.py): `S1,S2` both rows, `S1` (= `S1,`) row 1
               alone, `,S2` row 2 alone; `both` (the default) both rows on their FIXED seeds (drift-gate-seeds.tsv). --for-red and --against pin their baseline run's seeds
               themselves, so a row is only ever held against the same seed
  --for-red    only the rows a red convergence run failed: its failed `takes-drift-1|2 (<country>)` jobs name the
               countries and rows, the red run's own seeds the seeds they replay (its rows' `seed` artifact files; a run
               from before the rotation replayed seeds 1 and 2; a run with none left, the rows' fixed seeds) (a run id, or the run's number as its URL shows it). A red with no failed drift row
               has nothing to judge here: exit 0, said so. A FIX MAY BE PARTIAL (5df3295bd cleared PL's flips while DE
               seed 2's stayed): each row still red is held against the red run's own verdict.txt for it -- open
               divergences plus flips beyond the allow-lists. Fewer in some row and more in none is PROGRESS (exit 0,
               `DRIFT PROGRESS: ...`); no row better, or one worse, is a refuted fix (exit 1); the red's verdict unreadable (its
               artifacts kept 7 days) is no verdict (exit 2)
  --against    every requested row held against a run's own verdict for it -- `main`: main's newest decided convergence
               run -- in place of "any red row is red": a row worse than there (more open findings, or red where it was
               green) is DRIFT RED (exit 1); none worse is `DRIFT NOT WORSE` (exit 0) even while main itself is red.
               What a NEW OR TIGHTENED CHECK must show to land (scripts/ci/commit_rules.py's rule): it finds
               nothing main's run does not, every finding at birth on its allow-list as TEMPORARY. And what
               identity-landing-check asks of a change while main is red on drift
  --timeout    minutes to WAIT for the rows (default max(75, 1.5 x the ETA of recent measure runs)). It only stops the
               waiting: the run is never cancelled; it says `--run <id>` and exits 3 (STILL RUNNING, no verdict)
  --no-wait    dispatch, print the run id, exit 0; collect with --run
  --run        collect an existing drift-only run
  --profile    a JFR CPU profile of every JVM each row starts (the workflow's `profile`, scripts/ci/jfr-every-jvm.sh),
               uploaded as the row's convergence-profile-* artifact; the head is replayed, never read from the store
  --out        where each row's artifact goes (default target/identity-drift-ci/<run>)
  --lane       the ci-runner lane the rows queue in (scripts/ci/measure-lane.sh): default `red-fix` with --for-red, else
               `landing` -- this is a landing check, and its rows run ahead of every optional measurement (2026-11-16:
               a red fix's ten rows ran 1-3 at a time for 80+ min behind 14-27 queued measurement jobs); `measure` for
               a check nothing waits on (identity-landing-check's). A commit whose identity-takes.yml has no `lane`
               input is dispatched without it (movies-measure)

It pushes the commit as `measure/<user>/<short sha>-drift-<random>` (the pre-push hook checks nothing on a measure/
ref; the run deletes its branch as its last job), dispatches through scripts/lib/vcs/tool_dispatch.py, waits with backoff (60 s,
x1.5, at most 5 min; a line per poll, and $KINOWO_PROGRESS_FILE touched, so the pre-push hook's no-progress watchdog
sees it alive), stops as soon as every requested drift row has ended (the run's tail goes on without it), then prints
each row's verdict.txt and its report's divergence and flip lines.

Called by scripts/identity-landing-check (its `drift` group) and by the pre-push hook's `identitydrift` check (a push carrying
`Fixes-Red: <run>` for a convergence drift red: `--for-red <run>`). Never runs anything heavy on the laptop.

The last line on stdout is the verdict: `DRIFT CLEAN`, `DRIFT RED: <rows>`, or `NO VERDICT: <why>`.
Exit: 0 clean (or nothing to judge), 1 a divergence or flip beyond the allow-lists, 2 could not run, 3 still running at --timeout.
"""
from __future__ import annotations

import os
import sys
import tempfile
import time
from typing import Callable, Dict, List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from processes import machine  # noqa: E402
from identity_tooling.ci import identity_dispatch  # noqa: E402
import identity_drift_ci_baseline as baseline_  # noqa: E402
import identity_drift_ci_options as options_  # noqa: E402
import identity_drift_ci_runs as runs_  # noqa: E402
import identity_drift_ci_verdict as verdict_  # noqa: E402
import tool_process  # noqa: E402
from identity_drift_ci_runs import say  # noqa: E402

TOOL = "identity-drift-ci"
WORKFLOW = "identity-takes.yml"  # the takes rows are Identity takes' (the owner, 2026-11-08): no convergence row


class Die(Exception):
    """The tool could not do its work: said, exit 2 (the shell tool's DIE_STATUS)."""


class DriftCi:
    def __init__(self, o: options_.Options, env: Mapping[str, str], runner: tool_process.Runner, base_root: str,
                 clock: Callable[[], float] = time.monotonic) -> None:
        self.o, self.env, self.runner, self.clock = o, env, runner, clock
        self.timeout = float(env.get("IDENTITY_DRIFT_GH_TIMEOUT") or 90)
        wiring = runs_.Wiring(runner, env, self.timeout, base_root, WORKFLOW)
        self.reads, self.dispatcher, self.git, self.ci = wiring.reads, wiring.dispatcher, wiring.git, wiring.ci
        self.remote = env.get("IDENTITY_MEASURE_REMOTE") or "origin"
        self.branch = ""
        self.started = clock()

    def main(self) -> int:
        base = baseline_.Baseline(self.reads, self.o)
        if not base.resolve():
            return 0
        countries = self.o.countries or list(baseline_.ROW_ORDER)
        expected = baseline_.expected_rows(countries, self.o.pins)
        if not self.o.run:
            self.o.run = self.dispatched(expected)
            if self.o.no_wait:
                say(f"not waiting: collect with  scripts/identity-drift-ci --run {self.o.run}")
                print(self.o.run)
                return 0
        else:
            expected = []  # --run: the rows the run itself has (its countries and seeds were its dispatch's)
        rows = self.wait(expected)
        if rows is None:
            return 3
        self.branch = ""  # the run's tail (timings, the branch drop) goes on; the branch is the run's to delete
        top = self.git.out("rev-parse", "--show-toplevel", quiet=True)
        out = self.o.out or os.path.join(top.strip() if top else os.getcwd(), "target", TOOL, self.o.run)
        request = verdict_.Request(self.o.run, out, expected, base.ids, base.label, self.o.against, self.o.for_red,
                                   int(self.clock() - self.started) // 60)
        judged = verdict_.Verdict(self.reads, request)
        judged.collect()
        return judged.judge(rows)

    def dispatched(self, expected: List[str]) -> str:
        commit = self.commit()
        push = self.pushed(commit)
        self.branch = self.ci.branch(commit, "drift")
        oneline = self.git.out("log", "--oneline", "-1", commit) or ""
        say(f"commit {oneline.rstrip(chr(10))[:100]}")
        error = self.ci.push(self.remote, push, self.branch)
        if error is not None:
            self.delete_branch()
            raise Die(f"could not push {self.branch}: {error}")
        self.started = self.clock()
        purpose = f"{TOOL} {'--for-red ' + self.o.for_red + ' ' if self.o.for_red else ''}" \
                  f"{'--against ' + self.o.against + ' ' if self.o.against else ''}{commit[:9]}"
        try:
            run = self.ci.dispatch(WORKFLOW, self.branch, push, purpose, self.inputs(push))
        except identity_dispatch.DispatchFailed as e:
            if not str(e).startswith("dispatched"):
                self.delete_branch()
            raise Die(str(e)) from None
        say(f"run {run} on {self.branch}: drift-only, {len(expected)} row(s) ({','.join(expected)})")
        return run

    def commit(self) -> str:
        commit = self.git.out("rev-parse", "--verify", "-q", f"{self.o.ref}^{{commit}}", quiet=True)
        if not commit:
            raise Die(f"no commit {self.o.ref}")
        commit = commit.strip()
        if self.o.ref == "HEAD" and (self.git.out("status", "--porcelain", "--untracked-files=no", quiet=True) or ""):
            say(f"WARNING: uncommitted changes are not checked -- only {commit[:9]}")
        return commit

    def pushed(self, commit: str) -> str:
        """The commit, or -- its workflows predating drift-only or identity-takes.yml (2026-11-08) -- main's CI laid
        over it (identity-measure's way)."""
        leg = self.git.out("show", f"{commit}:.github/workflows/country-convergence-leg.yml", quiet=True) or ""
        if "drift-only" in leg and self.git.ok("cat-file", "-e", f"{commit}:.github/workflows/{WORKFLOW}", quiet=True):
            return commit
        try:
            push = self.ci.main_ci_fetched_over(self.remote, commit, "drift")
        except identity_dispatch.DispatchFailed as e:
            raise Die(str(e)) from None
        say(f"{commit[:9]} predates the drift-only dispatch: checking it under {self.remote}/main's .github/ and "
            f"scripts/ci/ ({push[:9]})")
        return push

    def inputs(self, push: str) -> List[str]:
        only = ["-f", "drift-only=true"]
        if self.o.countries:
            only += ["-f", f"takes-countries={','.join(self.o.countries)}"]
        if self.o.pins:
            only += ["-f", f"drift-seeds={self.o.pins}"]
        if self.o.profile:
            only += ["-f", "profile=true"]
        # the lane input, where the pushed workflow knows it (an older one refuses an unknown input)
        workflow = self.git.out("show", f"{push}:.github/workflows/{WORKFLOW}", quiet=True) or ""
        if self.o.lane != "measure" and "            lane:" in workflow.split("\n"):
            only += ["-f", f"lane={self.o.lane}"]
        return only

    def delete_branch(self) -> None:
        if self.branch and self.git.ok("push", "-q", self.remote, "--delete", f"refs/heads/{self.branch}",
                                       timeout=120, quiet=True):
            say(f"deleted branch {self.branch}")

    def wait(self, expected: List[str]) -> Optional[List[runs_.Row]]:
        """Until every expected drift row has ended (or the run has): its rows; None past --timeout (said)."""
        poll = baseline_.Poll(self.reads, self.o.run, expected, self.clock, self.started, self.env)
        return poll.until_ended(self.timeout_minutes())

    def timeout_minutes(self) -> int:
        minutes = self.o.timeout_min
        if not self.o.timeout_set:
            eta = self.dispatcher.eta(WORKFLOW, "measure/", quiet=True) or ""
            figure = eta[1:].split(" ", 1)[0]
            if figure.isdigit() and int(figure) * 3 // 2 > minutes:
                minutes = int(figure) * 3 // 2
        return minutes


def main(argv: List[str], env: Dict[str, str]) -> int:
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[attr-defined] # in order with stderr, as the shell tool wrote
    try:
        o = options_.parse(argv, __doc__ or "")
        if o is None:
            return 0
        with tempfile.TemporaryDirectory(prefix="identity-drift-ci.", dir=env.get("TMPDIR") or None) as base_root:
            tool = DriftCi(o, env, tool_process.SubprocessRunner(), base_root)
            try:
                return tool.main()
            except (Die, baseline_.Unread) as e:
                print(f"{TOOL}: {e}", file=sys.stderr)
                return 2
    except options_.UsageError as e:
        return options_.usage_failed(TOOL, e)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
