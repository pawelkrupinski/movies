#!/usr/bin/env python3
"""Dispatch each convergence suite whose verdict this push could change — and ONLY those.

WHY. Each suite runs in one lane that finishes the run in flight and keeps a single run
pending behind it; every newer dispatch replaces the pending one. Main used to dispatch
on EVERY green push, so a push that touched only the iOS app or a Grafana panel evicted
the pending run of the pipeline commit before it — measured 2026-08-31..09-24: 239 of 398
`Country convergence` runs and 253 of 388 `US convergence` runs ended cancelled, ~78% of
them without ever reaching a runner, while only 44% of main's commits (525 of 1,201)
touched a path that can change a verdict. A push that cannot change the answer now leaves
the pending run alone, and costs nothing.

"Since when?" is asked of the SUITE, not of the push: the base is the head of the suite's
newest run. Main's own lane cancels superseded pushes, so a push's `before` can skip over
commits whose Main run never reached this job — diffing from the suite's last run cannot.
No base, or a base this checkout does not contain, dispatches: an extra verification run
is cheap next to a pipeline change that never gets one.

A refused dispatch fails the job (after the other suites are tried): a green job that dispatched nothing reads as
"nothing to verify" when a pipeline change went unverified. A 5xx is GitHub's own blip (2026-10-12: one HTTP 500 left a
pipeline push unverified), and a spent API rate limit (2026-10-15: run 38818736115) resets within the hour, so the
dispatcher (scripts/lib/tool_dispatch.py) retries it, twice, after a pause (DISPATCH_RETRY_SECONDS), and waits a near
rate-limit reset out; any other refusal (403, 404, 422) fails at once. Every gh read is bounded by this process, not
timeout(1): stock macOS has none, and there every read of a suite's last run failed, so a local run dispatched every
suite whatever the push touched.

Usage: kick_convergence.py <head-sha> <ref> <workflow name>...  (.github/scripts/kick-convergence.sh, the name main.yml runs)
KICK_CONVERGENCE_GH_TIMEOUT (60 s) bounds the read of each suite's last run, KICK_CONVERGENCE_REUSE_TIMEOUT (180 s) the
search for a run to reuse.
Tested by ConvergenceDispatchGateSpec and .github/scripts/kick_convergence_test.py against a scratch repository and a
fake `gh`."""
from __future__ import annotations

import os
import sys
from typing import List, Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
import machine  # noqa: E402
import tool_dispatch  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

IDENTITY = "Identity model convergence"


class Kick:
    def __init__(self, head: str, ref: str, env: Mapping[str, str], runner: tool_process.Runner) -> None:
        self.head, self.ref, self.env, self.runner = head, ref, env, runner
        self.paths = env.get("CONVERGENCE_PATHS_FILE") or os.path.join(HERE, "..", "convergence-paths.txt")
        self.matcher = env.get("CHANGED_PATHS_MATCHER") or os.path.join(HERE, "..", "actions", "changed-paths",
                                                                          "matches.sh")
        self.gh = tool_github.GitHub(runner, env, "gh")
        self.dispatcher = tool_dispatch.Dispatcher(tool_github.GitHub(runner, env, env.get("DISPATCH_GH") or "gh"),
                                                   tool_dispatch.Settings.of(env))
        self.failed = False

    def suite(self, workflow: str) -> None:
        """THE SUITE'S NEWEST RUN ON THIS BRANCH: never a measurement's (measure/*) or another branch's run, whose head
        can sit past every pipeline change since main's last run (2026-10-28: a measure run's head made six hours of
        main's pushes look unchanged, and main got no convergence verdict at all). An unreadable last run leaves no
        base, which DISPATCHES -- the safe direction."""
        base = (self.gh.read(["run", "list", "--workflow", workflow, "--branch", self.ref, "--limit", "1",
                              "--json", "headSha", "--jq", '.[0].headSha // ""'],
                             float(self.env.get("KICK_CONVERGENCE_GH_TIMEOUT") or 60), quiet=True) or "").strip()
        if not base or not self.git_ok(["git", "cat-file", "-e", f"{base}^{{commit}}"]):
            print(f"{workflow}: no earlier run to diff against (base '{base or 'none'}') — dispatching")
            self.dispatch(workflow)
            return
        # A failed diff is not "nothing changed": dispatch, as for a missing base.
        diff = self.runner.capture(tool_process.Command(
            ["bash", "-c", 'set -o pipefail; git diff --name-only "$1" "$2" | "$3" "$4"', "diff", base, self.head,
             self.matcher, self.paths], self.env))
        if diff.code != 0:
            print(f"{workflow}: could not diff {base[:9]}..{self.head[:9]} — dispatching")
            self.dispatch(workflow)
        elif diff.stdout.strip() == "true":
            print(f"{workflow}: pipeline paths changed since {base[:9]} — dispatching for {self.head[:9]}")
            self.dispatch(workflow)
        else:
            print(f"{workflow}: nothing since {base[:9]} can change its verdict — not dispatched, so the run already "
                  "pending keeps its place")

    def git_ok(self, argv: List[str]) -> bool:
        return self.runner.capture(tool_process.Command(argv, self.env)).code == 0

    def dispatch(self, workflow: str) -> None:
        """A RUN OF THIS COMMIT ALREADY QUEUED OR RUNNING judges it: a second one only waits behind it in the lane,
        then runs in full (run 3656: 54 min queued behind 3653, the same commit, which the daily run had dispatched
        39 s earlier). An unreadable list dispatches, the safe direction."""
        running = self.dispatcher.in_flight(workflow, self.ref, self.head)
        if running:
            print(f"{workflow}: run {running} for {self.head[:9]} is already queued or running -- not dispatched again",
                  flush=True)
            return
        reuse = self.reuse_hint(workflow)
        if reuse:
            print(f"{workflow}: run {reuse} ran this tree's whole suite -- dispatching it as reuse-run={reuse} (the "
                  "run's preflight checks it)", flush=True)
        if not self.dispatcher.dispatch(workflow, self.ref, ["-f", f"reuse-run={reuse}"] if reuse else []):
            print(f"::error::could not dispatch {workflow}", flush=True)
            self.failed = True

    def reuse_hint(self, workflow: str) -> str:
        """A VERDICT ALREADY PROVEN (scripts/ci/convergence_reuse.py): the identity model's suite is dispatched naming
        the newest green run whose facts carry this commit's tree key -- a landing check's, usually of this very
        commit -- and that run's preflight decides whether it holds; this is only the hint. No hint, a failed or slow
        search, an answer that is no run id: the plain dispatch (the full suite, the safe direction)."""
        if workflow != IDENTITY:
            return ""
        finder = self.env.get("CONVERGENCE_REUSE_FINDER") or os.path.join(HERE, "..", "..", "scripts", "ci",
                                                                            "convergence_reuse.py")
        done = self.runner.capture(tool_process.Command(
            [finder, "find", "--sha", self.head], self.env,
            timeout=float(self.env.get("KICK_CONVERGENCE_REUSE_TIMEOUT") or 180)))
        found = done.stdout.strip() if done.code == 0 else ""
        return found if found.isdigit() else ""


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if len(argv) < 2:
        print("usage: kick-convergence.sh <head-sha> <ref> <workflow name>...", file=sys.stderr)
        return 2
    kick = Kick(argv[0], argv[1], env, tool_process.SubprocessRunner())
    for workflow in argv[2:]:
        kick.suite(workflow)
        sys.stdout.flush()
    return 1 if kick.failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
