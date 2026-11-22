#!/usr/bin/env python3
"""Find the commit that turned a country's HERMETIC convergence leg red. convergence-bisect.sh, beside it, is the name
the workflow copies and runs.

WHY. A convergence leg is 60 minutes (a country) to 200 (the US), the lane keeps one run in flight and one pending, and a
day of pushes collapses into a handful of verdicts -- so a red leg arrives naming a SHA with dozens of untested commits
under it. A hermetic leg replays the same recorded corpus and enrichment tree whatever the code, so once it goes red the
question "which commit?" has a mechanical answer: replay the fast SAMPLE leg over the untested range, halving it.

HOW. `git bisect run` over (last green, first red], limited to commits that touch the pipeline
(`.github/convergence-paths.txt`) and to first-parent history -- once the last green commit replays green under the red
leg's recording (it may have been green under an older one, and then no commit is to blame). Cost is CAPPED: at most
`MAX_STEPS` bisection replays, and none started once the budget cannot fit another. What it could not narrow further it
reports as a range, never as a guess.

STANDALONE, the standard library alone: the workflow copies it OUT of the checkout (`git bisect` rewrites the working
tree under a running tool), so it imports nothing of the tree.

Usage:
  convergence-bisect.sh last-green              print the newest SHA whose leg was green (exit 1: none; 2: the runs
                                                could not be read)
  convergence-bisect.sh bisect <good> <bad>     bisect and write the verdict

Environment: COUNTRY (`poland`, `united-states`), WORKFLOW (`Identity model convergence`), RUN_ID (excluded from the
green search), STEP_COMMAND (run once per candidate at that commit: 0 good, 1 bad, 125 untestable), STEP_MINUTES (one
STEP_COMMAND's length), BUDGET_MINUTES (the halvings' ceiling, default 90, counted once <good> is confirmed), MAX_STEPS
(default 3), SAMPLE_FAILED ('true': the SAMPLE was red at <bad>, not replayed there), VERDICT_FILE (default
$GITHUB_STEP_SUMMARY), FIRST_BAD_FILE (the first bad SHA, when pinned exactly). The clock is `date +%s` (the
ConvergenceBisectSpec's stand-in). (convergence_bisect_test.py, ConvergenceBisectSpec)"""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from typing import Callable, List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
JOBS = ('.jobs[] | select((.name == "{c} / convergence" or (.name | endswith("convergence ({c})"))) and .conclusion =='
        ' "success") | .name')
WRAPPER = """#!/usr/bin/env bash
steps=$(cat "{counter}")
if [ "$steps" -ge "{max_steps}" ]; then exit 255; fi
now=$(date +%s)
if [ $(( {budget} * 60 - (now - {started}) )) -lt $(( {step} * 60 )) ]; then exit 255; fi
echo $((steps + 1)) > "{counter}"
"{command}"
"""


def run(argv: Sequence[str], timeout: Optional[float] = None) -> "tuple[int, str]":
    """A command's status and stdout, its stderr passed on; 124 past `timeout` (`timeout 60 gh ...`)."""
    try:
        done = subprocess.run(list(argv), stdout=subprocess.PIPE, text=True, timeout=timeout)
        return done.returncode, done.stdout
    except subprocess.TimeoutExpired:
        return 124, ""
    except OSError:
        return 127, ""


def date_now() -> int:
    return int(run(["date", "+%s"])[1].strip() or 0)


class Bisect:
    def __init__(self, env: Mapping[str, str], runner: Callable[..., "tuple[int, str]"] = run,
                 now: Callable[[], int] = date_now) -> None:
        self.env = env
        self.run = runner
        self.now = now

    def git(self, *args: str) -> "tuple[int, str]":
        return self.run(["git", *args])

    def pathspecs(self) -> List[str]:
        """The pipeline paths, as git pathspecs (`dir/**` becomes `dir/`)."""
        path = self.env.get("CONVERGENCE_PATHS_FILE") or os.path.join(HERE, "../convergence-paths.txt")
        with open(path) as f:
            lines = [line.rstrip("\n") for line in f if line.strip() and not line.lstrip().startswith("#")]
        return [line[:-2] if line.endswith("/**") else line for line in lines]

    def last_green(self) -> int:
        """1: no run's full leg passed. 2: the runs could not be read -- an outage, not "nothing to bisect from"."""
        code, runs = self.run(["gh", "run", "list", "--workflow", self.env.get("WORKFLOW", ""), "--branch", "main",
                               "--limit", "60", "--json", "databaseId,headSha", "--jq",
                               '.[] | "\\(.databaseId) \\(.headSha)"'], timeout=60)
        if code != 0:
            return 2
        rows = (line.split() for line in runs.splitlines())
        earlier = [w + [""] for w in rows if w and w[0] != self.env.get("RUN_ID", "")]
        for run_id, sha in ((w[0], w[1]) for w in earlier):
            green = self.full_leg_green(run_id)
            if green == 0:
                print(sha)
            if green != 1:
                return green
        return 1

    def full_leg_green(self, run: str) -> int:
        """0: the run's FULL leg passed (a sample alone says nothing of it); 1: it did not; 2: the run could not be read
        -- no red run: stop rather than bisect from an older base."""
        code, green = self.run(["gh", "run", "view", run, "--json", "jobs", "--jq",
                                JOBS.format(c=self.env.get("COUNTRY", ""))], timeout=60)
        if code != 0:
            return 2
        return 0 if green.strip() else 1

    def verdict(self, *lines: str) -> None:
        path = self.env.get("VERDICT_FILE") or self.env.get("GITHUB_STEP_SUMMARY") or ""
        if not path:
            print("\n".join(lines))
            return
        with open(path, "a") as f:
            f.write("".join(line + "\n" for line in lines))

    def step(self) -> int:
        return subprocess.run([self.env.get("STEP_COMMAND", "")]).returncode

    def checkout_and_step(self, sha: str) -> int:
        self.git("checkout", "-q", "--force", sha)
        return self.step()

    def bisect(self, good: str, bad: str) -> int:
        self.verdict(f"### Convergence bisect — {self.env.get('COUNTRY', '')}", "")
        reason = self.unbisectable(good, bad)
        if reason:
            self.verdict(reason)
            return 0
        specs = self.pathspecs()
        candidates = self.git("rev-list", "--first-parent", f"{good}..{bad}", "--", *specs)[1].split()
        if not candidates:
            self.verdict(f"No commit in `{good[:9]}..{bad[:9]}` touches the pipeline — the red comes from data or the"
                         " recording, not code. Not bisected.")
            return 0
        self.verdict(f"Range: `{good[:9]}..{bad[:9]}`, {len(candidates)} pipeline commit(s).")
        reason = self.unconfirmed(good, bad)
        if reason:
            self.verdict(reason)
            return 0
        if len(candidates) == 1:
            self.finish_exact(candidates[0], "the only pipeline commit in the range")
            return 0
        self.halve(good, bad, specs)
        return 0

    def unbisectable(self, good: str, bad: str) -> str:
        if good == bad:
            return (f"The same commit `{bad[:9]}` was GREEN in an earlier run — this failure is not a commit's doing (a"
                    " flake or a recording change). Not bisected.")
        if self.git("merge-base", "--is-ancestor", good, bad)[0] != 0:
            return f"Last green `{good[:9]}` is not an ancestor of `{bad[:9]}` — no range to bisect."
        return ""

    def unconfirmed(self, good: str, bad: str) -> str:
        """Why the ends do not make the range's answer a commit's: a full leg's failure the sample does not reproduce,
        or a last green commit that is not green under THIS recorded pair (re-pinned nightly)."""
        if self.env.get("SAMPLE_FAILED", "false") != "true":
            code = self.checkout_and_step(bad)
            if code != 1:
                return (f"The sample leg does not reproduce the failure at `{bad[:9]}` (exit {code}) — only the full leg"
                        " sees it, and bisecting with the full leg is not worth its cost. Not bisected.")
        code = self.checkout_and_step(good)
        if code == 1:
            return (f"The sample is red at the last green commit `{good[:9]}` too, under this recording — the red comes"
                    " from the recorded pair, not from a commit. Not bisected.")
        if code != 0:
            return (f"The sample could not confirm `{good[:9]}` green under this recording (exit {code}), so no commit in"
                    " the range can be blamed. Not bisected.")
        return ""

    def halve(self, good: str, bad: str, specs: List[str]) -> None:
        """`git bisect run` drives the search; a wrapper enforces the caps (exit 255 aborts, the remaining candidates
        left in refs/bisect/*). The budget is the HALVINGS': the two confirming replays are the price of any answer."""
        started = self.now()
        with tempfile.TemporaryDirectory() as directory:
            counter, wrapper = os.path.join(directory, "counter"), os.path.join(directory, "wrapper")
            with open(counter, "w") as f:
                f.write("0\n")
            with open(wrapper, "w") as f:
                f.write(WRAPPER.format(counter=counter, max_steps=self.env.get("MAX_STEPS") or 3, started=started,
                                       budget=self.env.get("BUDGET_MINUTES") or 90, step=self.env.get("STEP_MINUTES") or 20,
                                       command=self.env.get("STEP_COMMAND", "")))
            os.chmod(wrapper, 0o755)
            self.git("bisect", "start", "--first-parent", bad, good, "--", *specs)
            subprocess.run(["git", "bisect", "run", wrapper], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            with open(counter) as f:
                replays = f.read().strip()
            self.report(specs, replays)
        subprocess.run(["git", "bisect", "reset"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def report(self, specs: List[str], replays: str) -> None:
        """Decided from bisect's own refs, never its prose (git 2.55 reworded "is the first bad commit"): what is left
        under the newest bad and above every good; nothing but that bad left, it is the first bad commit."""
        goods = ["^" + s for s in self.git("for-each-ref", "--format=%(objectname)", "refs/bisect/good-*")[1].split()]
        skips = self.git("for-each-ref", "--format=%(objectname)", "refs/bisect/skip-*")[1].split()
        newest_bad = self.git("rev-parse", "refs/bisect/bad")[1].strip()
        left = self.git("rev-list", "--first-parent", newest_bad, *goods, "--", *specs)[1].split()
        if not [sha for sha in left if sha != newest_bad]:
            self.finish_exact(newest_bad, f"after {replays} bisection replay(s)")
            return
        self.verdict(f"Stopped after {replays} replay(s) (cap: {self.env.get('MAX_STEPS') or 3} replays,"
                     f" {self.env.get('BUDGET_MINUTES') or 90} min). The first bad commit is one of:", "")
        # A skipped commit stays listed: the step could not test it, so it may well be the first bad one.
        for sha in left:
            note = " — untested (the sample could not run here)" if sha in skips else ""
            self.verdict(f"- {self.describe(sha)}{note}")

    def describe(self, sha: str) -> str:
        return self.git("log", "-1", "--format=`%h` %s (%an)", sha)[1].rstrip("\n")

    def finish_exact(self, sha: str, why: str) -> None:
        self.verdict(f"**First bad commit: {self.describe(sha)}** — {why}.")
        if self.env.get("FIRST_BAD_FILE"):
            with open(self.env["FIRST_BAD_FILE"], "w") as f:
                f.write(sha + "\n")


def main(argv: List[str]) -> int:
    """The composition point: the environment, the real commands and the `date` clock."""
    bisect = Bisect(os.environ)
    if argv[:1] == ["last-green"]:
        return bisect.last_green()
    if argv[:1] == ["bisect"] and len(argv) >= 3:
        return bisect.bisect(argv[1], argv[2])
    print("usage: convergence-bisect.sh last-green | bisect <good> <bad>", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
