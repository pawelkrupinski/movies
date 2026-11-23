#!/usr/bin/env python3
"""scripts/identity-boot-cost, black-box, against a stand-in sbt and java (KINOWO_BOOT_COST_SBT, KINOWO_BOOT_COST_JAVA):
it compiles worker's tests and reads their classpath from sbt, runs the measure in a JVM of its own without escape
analysis (the runs asked passed on), and holds the rows to the baseline (KINOWO_BOOT_COST_BASELINE) -- within it exit 0,
a rise past the tolerance exit 1, --update rewrites the baseline, a failed compile or measure exit 2 with its log's
tail, a usage error 2. Ported from identity-boot-cost-test.sh case for case; the same cases hold the shell tool and its
Python rewrite (the extensionless name, whichever it execs). identity_boot_cost_test.py tests the ratchet it ends in."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
CLASSPATH = "/cp/worker-test-classes:/cp/scala-library.jar"
# FAKE_SBT_FAIL fails the compile; FAKE_SBT_NO_CLASSPATH exports none
SBT = f'''echo "[info] compiling"
[ -z "${{FAKE_SBT_FAIL:-}}" ] || {{ echo "[error] IdentityBootCost.scala:1: not found"; exit 1; }}
[ -n "${{FAKE_SBT_NO_CLASSPATH:-}}" ] || echo "{CLASSPATH}"'''
# rows.tsv (its next-to-last argument) as scripts.IdentityBootCost writes it; FAKE_BOOT_BYTES the boot phase's total
JAVA = '''if env.get("FAKE_JAVA_FAIL"):
    out("Exception in thread main")
    exit(1)
boot = env.get("FAKE_BOOT_BYTES") or "20000000"
with open(argv[-2], "w") as rows:
    rows.write(f"resolve\\t*\\t10\\t10000000\\t5000000\\nboot\\t*\\t10\\t{boot}\\t6000000\\n"
               "boot-steady\\t*\\t10\\t1000000\\t100000\\n")
out("IdentityBootCost: 90 slices, 3 runs a phase")'''
BASELINE = ("tolerance\t2\nphase\tresolve\t10\t1000000\t500000\nphase\tboot\t10\t2000000\t600000\n"
            "phase\tboot-steady\t10\t100000\t10000\n")
JVM_FLAGS = ["-XX:-DoEscapeAnalysis", "-XX:AutoBoxCacheMax=16384", "-Xmx2g", "-Djava.awt.headless=true",
             "-Duser.language=en", "-Duser.country=US", "-cp", CLASSPATH, "scripts.IdentityBootCost"]


class IdentityBootCostCliTest(cli_harness.ToolCase):
    TOOL = os.path.join(ROOT, "scripts", "identity-boot-cost")

    def setUp(self) -> None:
        super().setUp()
        self.sbt = self.fakes.script("sbt", SBT)
        self.java = self.fakes.program("java", JAVA)
        self.baseline = self.write("baseline.tsv", BASELINE)

    def measure(self, *args: str, cwd: str = "", **modes: str) -> cli_harness.Done:
        env = {"KINOWO_BOOT_COST_SBT": self.sbt, "KINOWO_BOOT_COST_JAVA": self.java,
               "KINOWO_BOOT_COST_BASELINE": self.baseline, **modes}
        return self.run_tool(*args, env=env, cwd=cwd or None)

    def lines(self, done: cli_harness.Done, part: str) -> List[str]:
        return [line for line in done.output.splitlines() if part in line]

    def test_the_measure_as_recorded_passes_on_its_own_jvm_the_runs_passed_on(self) -> None:
        done = self.measure("--runs", "5")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["worker/Test/compile", "export worker/Test/fullClasspath"]], self.fakes.calls("sbt"))
        [java] = self.fakes.calls("java")
        self.assertEqual(JVM_FLAGS, java[:-2], java)
        self.assertEqual(("rows.tsv", "5"), (os.path.basename(java[-2]), java[-1]))
        self.assertEqual(3, len([line for line in done.stdout.splitlines() if line.endswith("| ok |")]), done.output)
        self.assertIn("IdentityBootCost: 90 slices, 3 runs a phase", done.stdout.splitlines())
        self.assertNotIn("[info] compiling", done.output)

    def test_the_runs_default_to_three(self) -> None:
        self.assertEqual(0, self.measure().code)
        self.assertEqual("3", self.fakes.calls("java")[0][-1])

    def test_a_rise_past_the_tolerance_fails_naming_the_phase(self) -> None:
        done = self.measure(FAKE_BOOT_BYTES="20500000")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, len([line for line in self.lines(done, "| boot | ") if "ROSE past the tolerance" in line]),
                         done.output)

    def test_update_rewrites_the_baseline_keeping_its_tolerance(self) -> None:
        done = self.measure("--update", FAKE_BOOT_BYTES="20500000")
        self.assertEqual(0, done.code, done.output)
        rows = self.read("baseline.tsv").splitlines()
        self.assertIn("phase\tboot\t10\t2050000\t600000", rows)
        self.assertIn("tolerance\t2", rows)

    def test_a_measure_that_failed_is_no_verdict_its_log_tail_shown(self) -> None:
        done = self.measure(FAKE_JAVA_FAIL="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, len(self.lines(done, "Exception in thread main")), done.output)
        self.assertEqual(1, len(self.lines(done, "identity-boot-cost: the measure failed (log: ")), done.output)

    def test_a_compile_that_failed_is_no_verdict_its_log_tail_shown(self) -> None:
        done = self.measure(FAKE_SBT_FAIL="1")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("IdentityBootCost.scala:1: not found", done.stderr)
        self.assertEqual(1, len(self.lines(done, "identity-boot-cost: sbt worker/Test/compile failed (log: ")))
        self.assertEqual([], self.fakes.calls("java"))

    def test_no_classpath_exported_is_no_verdict(self) -> None:
        done = self.measure(FAKE_SBT_NO_CLASSPATH="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, len(self.lines(done, "identity-boot-cost: no classpath in sbt's export (log: ")))

    def test_out_keeps_the_rows_and_both_logs(self) -> None:
        done = self.measure("--out", "kept")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["jvm.log", "rows.tsv", "sbt.log"], sorted(os.listdir(self.path("kept"))))
        self.assertEqual(self.path("kept", "rows.tsv"), self.fakes.calls("java")[0][-2])
        self.assertIn(CLASSPATH, self.read("kept/sbt.log"))

    def test_runs_of_zero_or_not_a_number_are_usage_errors(self) -> None:
        for runs in ("0", "x", ""):
            done = self.measure("--runs", runs)
            self.assertEqual(2, done.code, (runs, done.output))
            self.assertIn(f"identity-boot-cost: --runs wants a whole number above 0, not '{runs}'", done.stderr)
        self.assertEqual([], self.fakes.calls("sbt"))

    def test_a_flag_without_its_value_or_an_unknown_argument_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.measure("--out").code)
        done = self.measure("--bogus")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("identity-boot-cost: unknown argument --bogus\nidentity-boot-cost: see identity-boot-cost "
                         "--help\n", done.stderr)

    def test_inside_another_checkout_it_refuses(self) -> None:
        other = self.repo("other")
        done = self.measure(cwd=other)
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"this copy measures {ROOT}, its own checkout, but this is {other}: run {other}/scripts/",
                      done.stderr)
        self.assertEqual([], self.fakes.calls("sbt"))

    def test_help_prints_the_header(self) -> None:
        done = self.measure("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, len([line for line in done.stdout.splitlines()
                                 if line.startswith("THE IDENTITY BOOT-COST CHECK")]), done.stdout)
        self.assertIn("Exit 0 within the baseline (or updated); 1 a phase's bytes a family rose past the tolerance; "
                      "2 the measure failed.", done.stdout.splitlines())


if __name__ == "__main__":
    unittest.main()
