#!/usr/bin/env python3
"""convergence-bisect (which commit turned a hermetic leg red), run for real, black-box: `last-green` against a fake
`gh` (the newest FULL leg passed, this run and other phases aside; an unreadable run or listing exit 2; none exit 1),
and `bisect` over a scratch repository whose pipeline commits a fake step judges by their content -- every verdict it
writes, the caps, untestable commits. ConvergenceBisectSpec holds the same verdict prose from the Scala side. The same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(ROOT, "scripts/lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/convergence_bisect.py", ".github/scripts/convergence-bisect.sh")
# `run list` prints <stub>/runs (fails when absent); `run view <id>` the jobs of <stub>/green-<id> through the caller's
# own --jq, failing when <stub>/view-<id>.fail exists.
GH = r"""
import json, subprocess
stub = env["STUB"]
with open(os.path.join(stub, "calls"), "a") as f:
    f.write("gh " + " ".join(argv) + "\n")
verb = " ".join(argv[:2])
if verb == "run list":
    if not os.path.exists(os.path.join(stub, "runs")):
        err("HTTP 502"); exit(1)
    sys.stdout.write(open(os.path.join(stub, "runs")).read()); exit(0)
if verb == "run view":
    run = argv[2]
    if os.path.exists(os.path.join(stub, f"view-{run}.fail")):
        err("HTTP 500"); exit(1)
    green = os.path.join(stub, f"green-{run}")
    if not os.path.exists(green):
        exit(0)
    jobs = [{"name": n, "conclusion": "success"} for n in open(green).read().splitlines()]
    done = subprocess.run(["jq", "-r", argv[argv.index("--jq") + 1]], input=json.dumps({"jobs": jobs}),
                          capture_output=True, text=True)
    sys.stdout.write(done.stdout); exit(done.returncode)
"""
STEP = """#!/bin/sh
sha=$(cat .git/HEAD)
echo "$sha" >> "{work}/replays"
grep -qx "$sha" "{work}/untestable" 2>/dev/null && exit 125
grep -qx broken pipeline/state && exit 1
exit 0
"""


class LastGreenTest(cli_harness.ToolCase):
    TOOL = TOOL

    def last_green(self) -> cli_harness.Done:
        return self.run_tool("last-green", env={"STUB": self.work, "COUNTRY": "poland", "RUN_ID": "900",
                                                "WORKFLOW": "Identity model convergence"})

    def test_last_green(self) -> None:
        self.fakes.program("gh", GH)
        self.write("runs", "900 sha-this-run\n901 sha-red\n902 sha-sample-only\n903 sha-green\n904 sha-older-green\n")
        self.write("green-900", "poland / convergence\n")
        self.write("green-902", "poland / sample\n")
        self.write("green-903", "germany / convergence\npoland / convergence\n")
        self.write("green-904", "poland / convergence\n")
        done = self.last_green()
        self.assertEqual(("sha-green\n", 0), (done.stdout, done.code), "the newest FULL leg green, this run aside")
        calls = self.read("calls")
        self.assertNotIn("run view 904", calls, "read no further than it")
        self.assertNotIn("run view 900", calls, "never this run itself")
        for n in ("903", "904"):
            os.remove(self.path(f"green-{n}"))
        self.assertEqual(("", 1), (self.last_green().stdout, self.last_green().code))
        self.write("green-904", "convergence (poland)\n")
        self.assertEqual(("sha-older-green\n", 0), (self.last_green().stdout, self.last_green().code))
        self.write("green-904", "poland / order-independence (poland)\n")
        self.assertEqual(("", 1), (self.last_green().stdout, self.last_green().code), "never another phase's row")
        self.write("green-904", "poland / convergence\n")
        self.write("view-903.fail", "")
        self.assertEqual(("", 2), (self.last_green().stdout, self.last_green().code), "an unreadable run: exit 2")
        os.remove(self.path("view-903.fail"))
        os.remove(self.path("runs"))
        self.assertEqual(2, self.last_green().code, "the runs cannot be listed")
        self.write("runs", "")
        self.assertEqual(1, self.last_green().code, "no runs at all")
        self.assertEqual(2, self.run_tool("nonsense").code, "a word that is no command")


class BisectTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.write("paths.txt", "pipeline/**\n# a comment\n\n")
        self.repo_dir = self.repo("repo")
        self.good = self.commit("pipeline/state", "ok", "base")
        self.docs = self.commit("docs/readme", "words", "docs only")
        self.commit("pipeline/a", "1", "pipeline change 1")
        self.commit("pipeline/a", "2", "pipeline change 2")
        self.p3 = self.commit("pipeline/a", "3", "pipeline change 3")
        self.p4 = self.commit("pipeline/state", "broken", "pipeline change 4")
        self.commit("pipeline/a", "5", "pipeline change 5")
        self.bad = self.commit("pipeline/a", "6", "pipeline change 6")
        step = self.write("step", STEP.format(work=self.work))
        os.chmod(step, 0o755)

    def commit(self, path: str, text: str, message: str) -> str:
        self.write(f"repo/{path}", text + "\n")
        self.git(self.repo_dir, "add", "-A")
        self.git(self.repo_dir, "commit", "-qm", message)
        return self.git(self.repo_dir, "rev-parse", "HEAD")

    def bisect(self, good: str, bad: str, **extra: Optional[str]) -> str:
        for name in ("verdict", "replays", "first-bad"):
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))
        env = {"CONVERGENCE_PATHS_FILE": self.path("paths.txt"), "COUNTRY": "poland", "STEP_COMMAND": self.path("step"),
               "STEP_MINUTES": "1", "VERDICT_FILE": self.path("verdict"), "FIRST_BAD_FILE": self.path("first-bad"),
               "MAX_STEPS": "10", **extra}
        done = self.run_tool("bisect", good, bad, env=env, cwd=self.repo_dir)
        return f"status {done.code}\n" + (self.read("verdict") if os.path.exists(self.path("verdict")) else "")

    def replays(self) -> List[str]:
        return self.read("replays").split() if os.path.exists(self.path("replays")) else []

    def first_bad(self) -> str:
        return self.read("first-bad").strip() if os.path.exists(self.path("first-bad")) else ""

    def test_the_first_bad_commit_is_pinned_exactly(self) -> None:
        v = self.bisect(self.good, self.bad)
        self.assertIn("6 pipeline commit(s)", v)
        self.assertIn(f"First bad commit: `{self.p4[:7]}", v)
        self.assertEqual(self.p4, self.first_bad())
        self.assertIn("status 0", v)
        self.assertEqual([self.bad, self.good], self.replays()[:2], "<bad> and <good> replayed first")
        self.assertFalse(os.path.exists(self.path("repo/.git/BISECT_START")), "the repository left off bisect")
        v = self.bisect(self.good, self.bad, SAMPLE_FAILED="true")
        self.assertEqual(self.good, self.replays()[0], "a sample already red at <bad> is not replayed there")
        self.assertIn(f"First bad commit: `{self.p4[:7]}", v)

    def test_the_caps_give_a_range_never_a_guess(self) -> None:
        v = self.bisect(self.good, self.bad, MAX_STEPS="1")
        self.assertIn("Stopped after 1 replay(s)", v)
        self.assertIn(self.p4[:7], v)
        self.assertEqual(("", 3), (self.first_bad(), len(self.replays())))
        v = self.bisect(self.good, self.bad, MAX_STEPS="0")
        self.assertIn("Stopped after 0 replay(s)", v)
        self.assertEqual(6, sum(1 for line in v.splitlines() if line.startswith("- ")))
        self.assertNotIn("docs only", v)
        self.assertIn("Stopped after 0 replay(s)", self.bisect(self.good, self.bad, BUDGET_MINUTES="0"))

    def test_an_untestable_commit_stays_a_candidate(self) -> None:
        self.write("untestable", f"{self.p3}\n{self.p4}\n")
        v = self.bisect(self.good, self.bad)
        self.assertIn("untested (the sample could not run here)", v)
        self.assertIn("The first bad commit is one of:", v)

    def test_the_ranges_bisected_not_at_all(self) -> None:
        self.assertIn("was GREEN in an earlier run", self.bisect(self.bad, self.bad))
        self.assertEqual([], self.replays())
        self.assertIn("is not an ancestor of", self.bisect(self.bad, self.good))
        self.assertIn("touches the pipeline", self.bisect(self.good, self.docs))
        self.assertEqual([], self.replays())
        self.assertIn("does not reproduce the failure", self.bisect(self.good, self.p3))
        self.assertIn("red at the last green commit", self.bisect(self.p4, self.bad, SAMPLE_FAILED="true"))
        self.assertEqual("", self.first_bad())
        self.write("untestable", f"{self.good}\n")
        self.assertIn("could not confirm", self.bisect(self.good, self.bad, SAMPLE_FAILED="true"))

    def test_one_pipeline_commit_is_it_after_both_ends(self) -> None:
        v = self.bisect(self.p3, self.p4)
        self.assertIn("the only pipeline commit in the range", v)
        self.assertEqual((self.p4, 2), (self.first_bad(), len(self.replays())))


if __name__ == "__main__":
    unittest.main()
