"""A FRESH MEASUREMENT of identity-measure (scripts/identity_measure_cli.py, its header): THE WHAT-IF GATE, the commit
pushed to a measure/ branch (under main's CI when its own workflows predate the takes-only dispatch), Identity takes
dispatched on it (--full: Identity model convergence, whose preflight dispatches the takes run beside it), a run
cancelled by hand dispatched again until the deadline, then collected, stored for REUSE and in THE RESULT CACHE."""
from __future__ import annotations

from typing import List, Optional, Tuple

import identity_dispatch
import tool_ci_wait
import tool_dispatch
from identity_measure_cli_collect import Collector
from identity_measure_cli_context import IDENTITY_TOOLS, Context, Die, Exit, say
from identity_measure_cli_paths import TakePaths
from identity_measure_cli_recordings import Recordings
from identity_measure_cli_results import Results
from identity_measure_cli_wait import Waiter

# THE TAKES ROWS ARE IDENTITY TAKES' (the owner, 2026-11-08): a takes-only measurement dispatches it alone; --full
# dispatches Identity model convergence, whose preflight dispatches the takes run beside it (dispatch-takes.sh): the
# takes run is measured, and the convergence run's conclusion joins the verdict.
WORKFLOW = "identity-takes.yml"
CONVERGENCE_WORKFLOW = "identity-model-convergence.yml"
Parts = Tuple[Waiter, Collector, TakePaths, Results, Recordings]


def what_if_gate(c: Context) -> None:
    """THE WHAT-IF GATE: a lookup of scripts/identity-what-if's verdict for the head and base, before anything is
    pushed or dispatched; none refuses, exit 2, unless --no-what-if says why."""
    s, why = c.state, c.o.no_what_if
    if why:
        say(f"WHAT-IF SKIPPED (--no-what-if): {why} -- name it in your report")
        return
    done = c.shell(IDENTITY_TOOLS, "what_if_verdicts", [s.commit, s.base])
    if done.code == 0:
        say(f"what-if done for {s.commit[:9]} against {s.base[:9]}: {done.stdout.strip()}")
        return
    say(f"REFUSED: no scripts/identity-what-if verdict for {s.commit[:9]} against {s.base[:9]} (what-if first: minutes "
        "on the laptop, not a CI measure). Run:")
    say(f"  scripts/identity-what-if --base {s.base} --head {s.commit} {s.countries.replace(',', ' ')}")
    say('then measure again; or dispatch anyway with --no-what-if "<why what-if cannot see it>", named in your report')
    raise Exit(2)


def parts_of(c: Context) -> Tuple[tool_dispatch.Dispatcher, Parts]:
    """The collaborators a measurement is made of, over one dispatcher."""
    dispatcher = tool_dispatch.Dispatcher(c.github, tool_dispatch.Settings.of(c.env, c.gh_timeout))
    return dispatcher, (Waiter(c, dispatcher, WORKFLOW), Collector(c), TakePaths(c), Results(c), Recordings(c))


class Dispatch:
    def __init__(self, c: Context, dispatcher: tool_dispatch.Dispatcher, parts: Parts) -> None:
        self.c, self.dispatcher = c, dispatcher
        self.waiter, self.collector, self.paths, self.results, self.recordings = parts
        self.ci = identity_dispatch.MeasureCi(c.runner, c.env, c.git, dispatcher)
        self.attempts = 0
        self.dispatched_at = 0

    def main(self, mode: str) -> int:
        c, s = self.c, self.c.state
        what_if_gate(c)
        s.push = self.pushed()
        s.branch = self.ci.branch(s.commit)
        c.say_commit_and_base()
        # one measure/ push at a time on this machine; a push that fails -- or times out -- may still have made the
        # branch: the clean-up deletes it either way
        error = self.ci.push(c.remote, s.push, s.branch)
        if error is not None:
            raise Die(f"could not push the measure branch {s.branch}: {error}")
        say(f"pushed {s.branch}")
        self.dispatch()
        self.waiter.default_timeout()
        deadline = c.clock() + c.o.timeout_min * 60  # one deadline for every (re-)dispatch and the run
        if c.o.no_wait:
            s.keep = s.finished = True
            self.results.leave_key(s.measured_run, mode)
            say(f"not waiting: collect with  scripts/identity-measure --run {s.measured_run}   (it deletes {s.branch})")
            return 0
        conclusion = self.until_run(deadline)
        s.finished = True
        if conclusion == "takes-done":  # its tail still reads the branch, and its last job deletes it
            say(f"run {s.measured_run}: every takes row has ended -- collecting now, the run's tail goes on without us")
        else:
            say(f"run {s.measured_run}: {conclusion}")
        verdict = self.full_verdict(self.collector.collect(s.measured_run, s.commit), deadline)
        if verdict <= 1 and not (c.o.full or s.recording or c.o.no_store) and conclusion != "cancelled":
            self.paths.store(s.base, s.commit, s.measured_run, self.dispatched_at)
        if conclusion != "cancelled":
            self.results.store(mode, verdict, s.measured_run, s.collected_table, c.measured(s.measured_run, "moved"))
        return self.recordings.then_cross_country(verdict, s.measured_run)

    def pushed(self) -> str:
        """The commit, or -- its workflows predating the takes-only dispatch (takes-countries, 2026-10-21; takes-no-store
        for --no-store, 2026-10-22; identity-takes.yml, 2026-11-08) -- main's .github/ and scripts/ci/ laid over it."""
        c, s = self.c, self.c.state
        leg = c.git.out("show", f"{s.commit}:.github/workflows/country-convergence-leg.yml", quiet=True) or ""
        if "takes-countries" in leg and not (c.o.no_store and "takes-no-store" not in leg) \
                and c.git.ok("cat-file", "-e", f"{s.commit}:.github/workflows/{WORKFLOW}", quiet=True):
            return s.commit
        push = self.ci.main_ci_over(c.remote, s.commit, "measure")
        if push is None:
            raise Die(f"could not lay main's CI over {s.commit}")
        say(f"{s.commit} predates the takes-only dispatch: measuring it under {c.remote}/main's .github/ and "
            f"scripts/ci/ ({push})")
        return push

    def inputs(self) -> List[str]:
        c, s = self.c, self.c.state
        only = [] if c.o.full else ["-f", "takes-only=true"]
        only += ["-f", f"recording={s.recording}"] if s.recording else []
        only += ["-f", f"takes-countries={s.named}"] if s.named else []
        only += ["-f", "takes-no-store=true"] if c.o.no_store else []
        return ["-f", f"takes-base={s.base}", *only]

    def dispatch(self) -> None:
        c, s = self.c, self.c.state
        self.attempts += 1
        say(f"dispatching (attempt {self.attempts})")
        self.dispatched_at = int(c.now())
        against = f"{s.commit[:9]} against {s.base[:9]}"
        try:
            if c.o.full:
                s.measured_run = self.full_run(f"identity-measure --full {against}")
            else:
                s.measured_run = self.ci.dispatch(WORKFLOW, s.branch, s.push, f"identity-measure {against}",
                                                  self.inputs())
        except identity_dispatch.DispatchFailed as e:
            raise Die(str(e)) from None
        url = c.gh(["run", "view", s.measured_run, "--json", "url", "--jq", ".url"])
        say(f"run {url.strip() if url else s.measured_run}")
        self.waiter.say_eta(s.measured_run)

    def full_run(self, purpose: str) -> str:
        """--full: the convergence run dispatched, and the Identity takes run its preflight dispatches, recorded."""
        c, s = self.c, self.c.state
        s.convergence_run = self.ci.dispatch(CONVERGENCE_WORKFLOW, s.branch, s.push, purpose, self.inputs())
        say(f"convergence run {s.convergence_run}: waiting for the Identity takes run its preflight dispatches")
        takes = self.dispatcher.find_titled(WORKFLOW, f"Identity takes (convergence run {s.convergence_run})",
                                            float(c.env.get("DISPATCH_TAKES_FIND_SECONDS") or 1800), c.poll)
        if takes is None:
            raise Die(f"convergence run {s.convergence_run} started no Identity takes run")
        self.ci.record(WORKFLOW, s.branch, takes, f"identity-measure --full {s.commit[:9]} (takes)")
        return takes

    def until_run(self, deadline: float) -> str:
        """The run's conclusion; a run cancelled by hand (or by a lane shared after all) dispatched again until the
        deadline, not a fixed few times."""
        c, s = self.c, self.c.state
        while True:
            conclusion = self.waiter.for_takes(s.measured_run, deadline - c.clock())
            if conclusion is None:
                self.waiter.timed_out(s.measured_run)
            if conclusion != "cancelled" or c.clock() >= deadline:
                return conclusion
            say(f"run {s.measured_run} was cancelled: dispatching again")
            self.dispatch()

    def full_verdict(self, verdict: int, deadline: float) -> int:
        """--full: the whole suite is both runs -- the convergence run's rows decide too (its tail reads the branch)."""
        c, s = self.c, self.c.state
        if not s.convergence_run or verdict > 1:
            return verdict
        conclusion: Optional[str] = tool_ci_wait.wait_for_run(c.runner, c.env, s.convergence_run,
                                                              int(max(deadline - c.clock(), 0)), c.poll)
        if conclusion is None:
            self.waiter.timed_out(s.convergence_run)
        s.convergence_done = True
        say(f"convergence run {s.convergence_run}: {conclusion}")
        if conclusion != "success":
            say(f"--full: convergence run {s.convergence_run} is {conclusion} -- NOT LANDABLE")
            return 1
        return verdict

