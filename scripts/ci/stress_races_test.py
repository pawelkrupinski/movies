#!/usr/bin/env python3
"""scripts/ci/stress-races.sh, black-box, in a scratch checkout holding the harness classes it names (found by asking
the tool which ones are missing, so the list stays the tool's own), against a stub `sbt --client` that fails the calls
it is told to and writes their JUnit reports. The promise: every iteration runs even after a failure, one testOnly per
module and config; each failed test is counted by iteration into failures.tsv and the summary; a listed class that is
gone fails the run before any sbt. It had no test."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/stress_races.py", "scripts/ci/stress-races.sh")
MISSING = re.compile(r"^::error::stress-races\.sh lists ([\w.]+) under (\S+), but no such class is in (\S+)$")

# sbt --client <command>: each testOnly call counted; the calls STUB_FAIL names (1-based, comma-separated) fail, each
# leaving a JUnit report whose failed test is its first class's `races` (the same report again on every failure).
SBT = r"""
import os
if argv[:2] == ["--client", "shutdown"]:
    exit(0)
counter = env["STUB_COUNTER"]
calls = (int(open(counter).read()) if os.path.exists(counter) else 0) + 1
open(counter, "w").write(str(calls))
print("sbt ran " + argv[-1])
if str(calls) in env.get("STUB_FAIL", "").split(","):
    first = argv[-1].split()[1]
    reports = "target/test-reports/it" if "IntegrationTest" in argv[-1] else "target/test-reports/unit"
    os.makedirs(reports, exist_ok=True)
    with open(os.path.join(reports, "TEST-%s.xml" % first), "w") as f:
        f.write('<testsuite><testcase classname="%s" name="races"><failure/></testcase></testsuite>' % first)
    print("the failing call's own log")
    exit(1)
"""


class StressRacesTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("sbt", SBT)
        self.checkout = self.repo("checkout", "-b", "main")
        self.write("checkout/README", "a checkout\n")
        self.git(self.checkout, "add", "-A")
        self.listed: List[str] = []
        for line in self.stress("1", "all").stdout.splitlines():
            found = MISSING.match(line)
            if found:
                cls, target, directory = found.groups()
                self.listed.append(f"{target} {cls}")
                simple = cls.rsplit(".", 1)[-1]
                self.write(f"checkout/{directory}/{simple}.scala", f"final class {simple} extends AnyFlatSpec\n")
        self.git(self.checkout, "add", "-A")
        self.fakes.clear()
        self.out = ""

    def stress(self, *args: str, **env: str) -> cli_harness.Done:
        return self.run_tool(*args, cwd=self.checkout, env={"STUB_COUNTER": self.path("counter"),
                                                           "GITHUB_STEP_SUMMARY": self.path("summary"), **env})

    def runs(self, *args: str, **env: str) -> int:
        if os.path.exists(self.path("counter")):
            os.remove(self.path("counter"))
        self.fakes.clear()
        done = self.stress(*args, **env)
        self.out = done.output
        return done.code

    def sbt_commands(self) -> List[str]:
        return [" ".join(call[1:]) for call in self.fakes.calls("sbt")]

    def failures(self) -> List[str]:
        return self.read("checkout/target/stress-races/failures.tsv").splitlines()

    def test_the_tool_names_both_unit_and_integration_harnesses(self) -> None:
        targets = {line.split()[0] for line in self.listed}
        self.assertIn("worker/Test", targets)
        self.assertIn("worker/IntegrationTest", targets)

    def test_one_testonly_per_module_and_config_each_iteration_then_shutdown(self) -> None:
        self.assertEqual(0, self.runs("2", "all"), self.out)
        targets = sorted({line.split()[0] for line in self.listed})
        expected = [f"{t}/testOnly " + " ".join(line.split()[1] for line in self.listed if line.split()[0] == t)
                    for t in targets]
        self.assertEqual(expected * 2 + ["shutdown"], self.sbt_commands())

    def test_unit_runs_only_the_in_jvm_harnesses_and_it_only_the_mongo_ones(self) -> None:
        self.runs("1", "unit")
        self.assertTrue(all("/Test/" in c for c in self.sbt_commands()[:-1]), self.sbt_commands())
        self.runs("1", "it")
        self.assertTrue(all("/IntegrationTest/" in c for c in self.sbt_commands()[:-1]), self.sbt_commands())
        self.assertEqual(3, len(self.sbt_commands()))

    def test_all_passing_passes_counting_every_iteration_in_the_summary(self) -> None:
        self.assertEqual(0, self.runs("3", "unit"), self.out)
        self.assertEqual(["iteration 1/3: ok", "iteration 2/3: ok", "iteration 3/3: ok"],
                         [line for line in self.out.splitlines() if line.startswith("iteration ")])
        self.assertEqual("### Race harnesses × 3 (unit)\n\n3/3 iterations passed.\n", self.read("summary"))
        self.assertEqual([], self.failures())

    def test_a_failed_iteration_fails_the_run_yet_every_iteration_runs(self) -> None:
        unit_targets = len({line.split()[0] for line in self.listed if line.split()[0].endswith("/Test")})
        self.assertEqual(1, self.runs("3", "unit", STUB_FAIL="1"), self.out)
        self.assertEqual(3 * unit_targets + 1, len(self.sbt_commands()))
        self.assertEqual(["iteration 1/3: FAILED", "iteration 2/3: ok", "iteration 3/3: ok"],
                         [line for line in self.out.splitlines() if line.startswith("iteration ")])
        self.assertIn("the failing call's own log", self.out)

    def test_each_failed_test_is_recorded_by_iteration_and_counted_in_the_summary(self) -> None:
        unit_targets = len({line.split()[0] for line in self.listed if line.split()[0].endswith("/Test")})
        fail = f"1,{unit_targets * 2 + 1}"  # the first call of iterations 1 and 3
        self.assertEqual(1, self.runs("3", "unit", STUB_FAIL=fail), self.out)
        first = self.sbt_commands()[0].split()[1]
        self.assertEqual([f"1\t{first}\traces", f"3\t{first}\traces"], self.failures())
        summary = self.read("summary")
        self.assertIn("1/3 iterations passed.", summary)
        self.assertIn(f"| 2/3 | `{first}` | races |", summary)
        self.assertTrue(os.path.isfile(self.path("checkout/target/stress-races/iteration-1/unit", f"TEST-{first}.xml")))
        self.assertFalse(os.path.exists(self.path("checkout/target/stress-races/iteration-2")))

    def test_the_summary_goes_to_stdout_too(self) -> None:
        self.runs("1", "unit")
        self.assertIn("1/1 iterations passed.", self.out)

    def test_a_listed_class_that_is_gone_fails_before_any_sbt(self) -> None:
        gone = self.listed[0].split()[1].rsplit(".", 1)[-1]
        target = self.listed[0].split()[0]
        module, config = target.split("/")
        directory = f"{module}/src/{'it' if config == 'IntegrationTest' else 'test'}"
        self.git(self.checkout, "rm", "-qf", f"{directory}/{gone}.scala")
        self.assertEqual(1, self.runs("1", "all"))
        self.assertIn(f"::error::stress-races.sh lists {self.listed[0].split()[1]} under {target}, but no such class",
                      self.out)
        self.assertEqual([], self.sbt_commands())

    def test_iterations_must_be_a_number_from_1_to_9999_and_the_scope_known(self) -> None:
        for bad in ("0", "10000", "x", "-1"):
            self.assertEqual(2, self.runs(bad, "unit"), bad)
        self.assertEqual(2, self.runs("1", "everything"))
        self.assertNotEqual(0, self.runs())
        self.assertEqual([], self.sbt_commands())

    def test_a_previous_runs_reports_are_cleared_first(self) -> None:
        self.write("checkout/target/test-reports/unit/TEST-old.xml",
                   '<testsuite><testcase classname="Old" name="stale"><failure/></testcase></testsuite>')
        self.write("checkout/target/stress-races/iteration-9/x", "old")
        unit_targets = len({line.split()[0] for line in self.listed if line.split()[0].endswith("/Test")})
        self.assertEqual(1, self.runs("1", "unit", STUB_FAIL=str(unit_targets)), self.out)
        self.assertNotIn("Old", self.read("checkout/target/stress-races/failures.tsv"))
        self.assertFalse(os.path.exists(self.path("checkout/target/stress-races/iteration-9")))


if __name__ == "__main__":
    unittest.main()
