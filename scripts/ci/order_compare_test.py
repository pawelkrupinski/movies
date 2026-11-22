#!/usr/bin/env python3
"""order-compare: the Order independence verdict, black-box. Each leg runs through `leg`; on a failure its failing tests
run again in default order (the control pass) and the leg is labelled REGRESSION (they fail there too) or ORDER (they
pass there), never turned green; a leg failing naming no test has no verdict. Plus the sbt / JUnit / XCTest readers,
scala's reverse vs shuffled comparison, the run's one-word verdict and its JSON, and the workflow's wiring of it.
The same cases hold the shell tool and its Python rewrite at one command line (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, List, Optional

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/order_compare.py", "scripts/ci/order-compare.sh")
WORKFLOW = os.path.join(cli_harness.ROOT, ".github/workflows/order-independence.yml")
ESC = "\x1b"
JUNIT = """<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="pl.kinowo.order.ShuffledOrderSuite" tests="3" failures="1" errors="1">
  <testcase name="passes" classname="pl.kinowo.PassTest" time="0.1"/>
  <testcase name="nullUntilSet" classname="pl.kinowo.prefs.PrefsTest" time="0.2">
    <failure message="expected null" type="java.lang.AssertionError">java.lang.AssertionError: expected null
\tat pl.kinowo.prefs.PrefsTest.nullUntilSet(PrefsTest.kt:12)</failure>
  </testcase>
  <testcase name="boom" classname="pl.kinowo.sync.SyncTest$Nested" time="0.0">
    <error message="boom" type="java.lang.IllegalStateException">boom</error>
  </testcase>
  <testcase name="passesToo" classname="pl.kinowo.PassTest" time="0.1"></testcase>
</testsuite>
"""
# A leg command writing the tests it failed to $ORDER_FAILED_FILE (as the each-alone scripts do), and a control printing
# XCTest's failure lines for those still failing in default order
FAKE_LEG = """#!/bin/sh
echo "running the leg"
[ -z "${FAKE_FAILED:-}" ] || printf '%s\\n' $FAKE_FAILED > "$ORDER_FAILED_FILE"
exit "${FAKE_EXIT:-0}"
"""
FAKE_CONTROL = """#!/bin/sh
shift
echo "control ran: $*" >> "$FAKE_CONTROL_CALLS"
for t in $FAKE_STILL_FAILING; do echo "Test Case '-[${t%%/*} ${t#*/}]' failed (0.1 seconds)."; done
[ -z "$FAKE_STILL_FAILING" ]
"""


class OrderCompareTest(cli_harness.ToolCase):
    TOOL = TOOL

    def executable(self, name: str, text: str) -> str:
        path = self.write(name, text)
        os.chmod(path, 0o755)
        return path

    def out(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> str:
        return self.run_tool(*args, env=env).stdout

    def lines(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> List[str]:
        return self.out(*args, env=env).splitlines()

    # ── the readers ────────────────────────────────────────────────────────────────────────────

    def test_failed_suites_reads_both_headings_once_each_sorted(self) -> None:
        self.write("sbt.log", "\n".join([
            "[info] Passed: Total 10, Failed 1", f"{ESC}[31m[error]{ESC}[0m Failed tests:",
            "[error] \tservices.movies.MovieServiceSpec", "[error] \tclients.TmdbClientSpec",
            "[error] Error during tests:", "[error] \tservices.identity.ResolverSpec",
            "[error] (web / Test / testOnly) sbt.TestsFailedException: Tests unsuccessful",
            "[error] Failed tests:", "[error] \tservices.movies.MovieServiceSpec", "[error] Total time: 12 s"]) + "\n")
        self.assertEqual(["clients.TmdbClientSpec", "services.identity.ResolverSpec", "services.movies.MovieServiceSpec"],
                         self.lines("failed-suites", self.path("sbt.log")))

    def test_failed_suites_reads_cis_coloured_log_alike(self) -> None:
        e = ESC
        self.write("ci.log", "\n".join([
            f"{e}[0m[{e}[0m{e}[31merror{e}[0m] {e}[0m{e}[0mFailed tests:{e}[0m",
            f"{e}[0m[{e}[0m{e}[31merror{e}[0m] {e}[0m{e}[0m\tservices.identity.PopulationMonotonicityPart2Spec{e}[0m",
            f"{e}[0m[{e}[0m{e}[31merror{e}[0m] {e}[0m{e}[0m(worker / Test / {e}[31mtest{e}[0m) sbt.TestsFailedException: x{e}[0m",
            f"{e}[0J"]) + "\n")
        self.assertEqual(["services.identity.PopulationMonotonicityPart2Spec"], self.lines("failed-suites", self.path("ci.log")))

    def test_failed_suites_of_a_green_log_or_none_is_nothing(self) -> None:
        self.write("green.log", "[info] all passed\n[error] Total time: 3 s\n")
        self.assertEqual("", self.out("failed-suites", self.path("green.log")))
        done = self.run_tool("failed-suites", self.path("none.log"))
        self.assertEqual((0, ""), (done.code, done.stdout))

    def test_failed_junit_names_each_failed_or_errored_class_once(self) -> None:
        self.write("junit/TEST-a.xml", JUNIT)
        self.assertEqual(["pl.kinowo.prefs.PrefsTest", "pl.kinowo.sync.SyncTest$Nested"],
                         self.lines("failed-junit", self.path("junit")))
        self.assertEqual("", self.out("failed-junit", self.path("no-such-dir")))

    def test_failed_xctest_names_each_failed_case(self) -> None:
        self.write("xctest.log", "\n".join([
            "Test Case '-[KinowoCoreTests.CityTests testA]' started.",
            "Test Case '-[KinowoCoreTests.CityTests testA]' failed (0.001 seconds).",
            "Test Case '-[KinowoUITests.CardShareMenuUITests testLongPress]' failed (55.224 seconds).",
            "Test Case '-[KinowoCoreTests.CityTests testB]' passed (0.001 seconds)."]) + "\n")
        self.assertEqual(["KinowoCoreTests.CityTests/testA", "KinowoUITests.CardShareMenuUITests/testLongPress"],
                         self.lines("failed-xctest", self.path("xctest.log")))

    # ── the default-order command each leg's control pass runs ─────────────────────────────────

    def test_control_command_scala_runs_those_suites_in_default_order(self) -> None:
        self.assertEqual(["env", "-u", "TEST_ORDER_SEED", "sbt", "testOnly a.ASpec b.BSpec"],
                         self.lines("control-command", "scala-shuffled", "a.ASpec", "b.BSpec"))

    def test_control_command_android_offline_by_tests_with_the_legs_own_settings(self) -> None:
        words = self.lines("control-command", "android", "p.ATest", "p.BTest")
        self.assertEqual(["--offline", "testDebugUnitTest", "--tests", "p.ATest", "--tests", "p.BTest"],
                         words[words.index("--no-daemon") + 1:])
        self.assertFalse(any("testOrderSeed" in w for w in words))
        extra = self.lines("control-command", "android", "p.ATest", env={"ORDER_CONTROL_EXTRA": "-PsyncModelSeeds=20000"})
        self.assertIn("-PsyncModelSeeds=20000", extra)

    def test_control_command_swift_one_anchored_alternation(self) -> None:
        words = [w for w in self.lines("control-command", "swift", "M.A/testX", "M.B/testY")
                 if w != "--package-path" and not w.endswith("/ios")]
        self.assertEqual(["swift", "test", "--skip-build", "--filter", r"^(M\.A/testX|M\.B/testY)$"], words)

    def test_control_command_ios_only_testing_each_class(self) -> None:
        words = self.lines("control-command", "ios", "AUITests", "BUITests")
        self.assertEqual(["-only-testing:KinowoUITests/AUITests", "-only-testing:KinowoUITests/BUITests"],
                         [w for w in words if w.startswith("-only-testing")])

    def test_usage_errors(self) -> None:
        self.assertEqual(64, self.run_tool("control-command", "nope", "x").code)
        self.assertEqual(64, self.run_tool("leg", "nope", self.path("x"), "--", "true").code)
        self.assertEqual(64, self.run_tool("nope").code)
        self.assertEqual(64, self.run_tool().code)

    # ── leg: the leg's command, then on a failure the control pass ─────────────────────────────

    def run_leg(self, record: str, failed: str, code: int, still_failing: str) -> int:
        for name in ("calls", "github-output", "summary"):
            self.write(name, "")
        env: Dict[str, Optional[str]] = {
            "FAKE_FAILED": failed, "FAKE_EXIT": str(code), "FAKE_STILL_FAILING": still_failing,
            "FAKE_CONTROL_CALLS": self.path("calls"), "ORDER_CONTROL_CMD": self.executable("fake-control", FAKE_CONTROL),
            "GITHUB_OUTPUT": self.path("github-output"), "GITHUB_STEP_SUMMARY": self.path("summary"),
            "ORDER_REPLAY": "ios/scripts/swift-test-shuffled.sh 42"}
        done = self.run_tool("leg", "swift", self.path(record), "--", self.executable("fake-leg", FAKE_LEG), env=env)
        self.write(f"{record}.out", done.stdout + done.stderr)
        return done.code

    def test_leg_regression_keeps_the_legs_own_exit(self) -> None:
        self.assertEqual(3, self.run_leg("r1", "M.A/testX M.B/testY", 3, "M.A/testX"))
        self.assertEqual("regression\n", self.read("r1/swift.verdict"))
        self.assertEqual("M.A/testX\n", self.read("r1/swift.control.txt"))
        self.assertEqual("order-verdict=regression\n", self.read("github-output"))
        self.assertEqual("control ran: M.A/testX M.B/testY\n", self.read("calls"))
        self.assertIn("::error title=Order verdict (swift)::REGRESSION, not an order dependence: `M.A/testX` fail in "
                      "default order too", self.read("r1.out"))
        self.assertTrue(self.read("summary").startswith("### Order verdict (swift): REGRESSION\n"))

    def test_leg_order_dependence_stays_red_with_the_replay(self) -> None:
        self.assertEqual(1, self.run_leg("r2", "M.B/testY", 1, ""))
        self.assertEqual("order\n", self.read("r2/swift.verdict"))
        self.assertEqual("", self.read("r2/swift.control.txt"))
        self.assertIn("::error title=Order verdict (swift)::ORDER DEPENDENCE (or a flake): `M.B/testY` fail in the swift "
                      "order but pass in default order -- replay: ios/scripts/swift-test-shuffled.sh 42\n",
                      self.read("r2.out"))

    def test_leg_failing_naming_no_test_has_no_verdict_and_no_control_pass(self) -> None:
        self.assertEqual(2, self.run_leg("r3", "", 2, ""))
        self.assertEqual("none\n", self.read("r3/swift.verdict"))
        self.assertEqual("", self.read("calls"))
        self.assertIn("::warning title=Order verdict (swift)::NO ORDER VERDICT: the swift leg failed (exit 2) naming no "
                      "test", self.read("r3.out"))

    def test_leg_pass_keeps_its_output_in_the_log(self) -> None:
        self.assertEqual(0, self.run_leg("r4", "", 0, ""))
        self.assertEqual(("pass\n", "success\n", ""), (self.read("r4/swift.verdict"), self.read("r4/swift.result"),
                                                       self.read("calls")))
        self.assertIn("running the leg", self.read("r4/swift.log"))
        self.assertIn("running the leg", self.read("r4.out"))
        self.assertEqual("order-verdict=pass\n", self.read("github-output"))

    def test_leg_scala_reads_sbts_log_and_ignores_a_suite_it_was_not_asked(self) -> None:
        leg = self.executable("fake-sbt-leg", "#!/bin/sh\nprintf '%s\\n' '[error] Failed tests:' '[error] \ta.ASpec' "
                                              "'[error] \tb.BSpec' '[error] Total time: 1 s'\nexit 1\n")
        control = self.executable("fake-sbt-control", "#!/bin/sh\nprintf '%s\\n' '[error] Failed tests:' "
                                                      "'[error] \tb.BSpec' '[error] \tz.NotAskedSpec'\nexit 1\n")
        done = self.run_tool("leg", "scala-reverse", self.path("s"), "--", leg,
                             env={"ORDER_CONTROL_CMD": control, "GITHUB_OUTPUT": os.devnull, "GITHUB_STEP_SUMMARY": os.devnull})
        self.assertEqual(1, done.code)
        self.assertEqual(("a.ASpec\nb.BSpec\n", "b.BSpec\n", "regression\n"),
                         (self.read("s/scala-reverse.txt"), self.read("s/scala-reverse.control.txt"),
                          self.read("s/scala-reverse.verdict")))

    def test_leg_android_reads_the_junit_reports(self) -> None:
        self.write("junit/TEST-a.xml", JUNIT)
        control = self.executable("fake-gradle-control", "#!/bin/sh\nmkdir -p \"$ORDER_ANDROID_RESULTS\"\necho "
                                  "'<testsuite><testcase name=\"t\" classname=\"pl.kinowo.prefs.PrefsTest\"></testcase>"
                                  "</testsuite>' > \"$ORDER_ANDROID_RESULTS/TEST-x.xml\"\n")
        self.run_tool("leg", "android", self.path("a"), "--", "false",
                      env={"ORDER_ANDROID_RESULTS": self.path("junit"), "ORDER_CONTROL_CMD": control,
                           "GITHUB_OUTPUT": os.devnull, "GITHUB_STEP_SUMMARY": os.devnull})
        self.assertEqual(("pl.kinowo.prefs.PrefsTest\npl.kinowo.sync.SyncTest$Nested\n", "order\n"),
                         (self.read("a/android.txt"), self.read("a/android.verdict")))

    # ── the run's verdict over every leg, and scala's two orders compared ─────────────────────

    def record(self, directory: str, leg: str, verdict: str, failed: str, control: str, result: str) -> None:
        self.write(f"{directory}/{leg}.verdict", verdict)
        self.write(f"{directory}/{leg}.txt", "".join(t + "\n" for t in failed.split()))
        self.write(f"{directory}/{leg}.control.txt", "".join(t + "\n" for t in control.split()))
        self.write(f"{directory}/{leg}.result", result + "\n")

    def test_verdict_and_summary_over_every_leg(self) -> None:
        self.record("v", "ios", "order", "AUITests", "", "failure")
        self.record("v", "swift", "pass", "", "", "success")
        self.assertEqual("order\n", self.out("verdict", self.path("v")))
        self.record("v", "android", "regression", "p.A p.B", "p.A", "failure")
        self.assertEqual("regression\n", self.out("verdict", self.path("v")))
        self.assertEqual(
            '{"verdict":"regression","legs":{"android":{"verdict":"regression","failed":["p.A","p.B"],'
            '"default_order_failed":["p.A"]},"ios":{"verdict":"order","failed":["AUITests"],"default_order_failed":[]},'
            '"swift":{"verdict":"pass","failed":[],"default_order_failed":[]}}}\n', self.read("v/order-verdict.json"))
        summary = self.out("summary", self.path("v"))
        every = summary[summary.index("every leg"):summary.index("reverse vs shuffled")]
        self.assertEqual(["- `android`: **REGRESSION, not an order dependence**: `p.A` fail in default order too",
                          "- `ios`: **order dependence (or a flake)**: `AUITests` pass in default order",
                          "- `swift`: passed"], [line for line in every.splitlines() if line.startswith("- ")])

    def test_verdict_of_no_leg_is_none(self) -> None:
        os.makedirs(self.path("empty"))
        self.assertEqual("none\n", self.out("verdict", self.path("empty")))

    def scala(self, reverse: str, reverse_result: str, shuffled: str, shuffled_result: str) -> List[str]:
        directory = f"d-{len(os.listdir(self.work))}"
        os.makedirs(self.path(directory))
        for order, suites, result in (("reverse", reverse, reverse_result), ("shuffled", shuffled, shuffled_result)):
            self.write(f"{directory}/scala-{order}.txt", "".join(s + "\n" for s in suites.split()))
            if result:
                self.write(f"{directory}/scala-{order}.result", result + "\n")
        summary = self.out("summary", self.path(directory), "4242")
        return [line for line in summary[summary.index("reverse vs shuffled"):].splitlines() if line.startswith("- ")]

    def test_scala_the_same_suite_in_both_orders_is_a_regression(self) -> None:
        self.assertEqual(["- **regression, not an order dependence: the same suite fails in both orders**: `a.ASpec`, "
                          "`b.BSpec`"], self.scala("a.ASpec b.BSpec", "failure", "a.ASpec b.BSpec", "failure"))

    def test_scala_one_order_only_is_an_order_dependence_with_its_replay(self) -> None:
        self.assertEqual(["- **order dependence**: `c.CSpec` fails only in the shuffled order -- replay: "
                          "`TEST_ORDER_SEED=4242 sbt testUnitNoE2e`"], self.scala("", "success", "c.CSpec", "failure"))
        both = self.scala("a.ASpec r.RSpec", "failure", "a.ASpec", "failure")
        self.assertTrue(both[0].startswith("- **regression"), both)
        self.assertTrue(both[1].startswith("- **order dependence**: `r.RSpec` fails only in the reverse"), both)

    def test_scala_a_leg_with_no_verdict_compares_nothing(self) -> None:
        self.assertEqual(["- `a.ASpec` failed in the shuffled order; the reverse leg reached no verdict to compare it with",
                          "- **no order verdict** from the reverse leg: it ended failure naming no failing suite (a "
                          "compile error, a timeout or the runner)"], self.scala("", "failure", "a.ASpec", "failure"))
        self.assertIn("- **no order verdict** from the reverse leg: it ended no record (the leg did not finish) naming no "
                      "failing suite (a compile error, a timeout or the runner)", self.scala("", "", "", "success"))
        self.assertEqual(["- both orders passed"], self.scala("", "success", "", "success"))

    # ── the workflow: every leg through `leg`, one verdict job after them all ─────────────────

    def test_the_workflow_runs_every_leg_through_it(self) -> None:
        with open(WORKFLOW) as f:
            text = f.read()
        name = os.path.basename(TOOL)
        jobs = job_blocks(text)
        for leg in ("scala", "android", "android-instrumented", "swift", "ios"):
            self.assertIn(f"{name} leg ", jobs[leg], leg)
            keep = jobs[leg].split("name: Keep the order record", 1)[1].splitlines()[1]
            self.assertIn("if: always()", keep, leg)
        verdict = jobs["order-verdict"]
        self.assertIn("needs: [seed, scala, android, android-instrumented, swift, ios]", verdict)
        self.assertIn(f'{name} summary order-records "$SEED" >> "$GITHUB_STEP_SUMMARY"', verdict)
        self.assertIn("verdict: ${{ steps.verdict.outputs.verdict }}", verdict)

    def test_the_android_warm_step_fetches_the_unit_test_classpaths(self) -> None:
        # Order independence #113: assembleDebugUnitTest left the test runtime classpath's AARs unfetched, and the
        # offline test run failed "No cached version available for offline mode"
        with open(WORKFLOW) as f:
            warm = job_blocks(f.read())["android"].split("name: Warm the dependency caches", 1)[1].split("- name:", 1)[0]
        self.assertIn("--init-script scripts/warm-dependencies.init.gradle", warm)
        self.assertIn(":app:warmDependencies", warm)
        with open(os.path.join(cli_harness.ROOT, "android/scripts/warm-dependencies.init.gradle")) as f:
            self.assertIn("'debugUnitTestRuntimeClasspath'", f.read())

    def test_the_android_leg_checks_the_offline_caches_before_its_tests(self) -> None:
        # #116: a gap in the warm step met the tests; it now fails a preflight step (infra), resolved again offline
        with open(WORKFLOW) as f:
            android = job_blocks(f.read())["android"]
        check = android.split("name: Runner preflight, the offline caches hold every test classpath", 1)
        self.assertEqual(2, len(check))
        step = check[1].split("- name:", 1)[0]
        for word in ("no-network --", "--offline", "--init-script scripts/warm-dependencies.init.gradle", ":app:warmDependencies"):
            self.assertIn(word, step)
        self.assertLess(android.index("offline caches hold"), android.index("name: Robolectric tests"))


def job_blocks(workflow: str) -> Dict[str, str]:
    """Each top-level job's text (a `    <name>:` line to the next), as the shell test's sed range read them."""
    blocks: Dict[str, str] = {}
    current: Optional[str] = None
    for line in workflow.splitlines():
        stripped = line.rstrip()
        if stripped.startswith("    ") and not stripped.startswith("     ") and stripped.endswith(":") \
                and stripped[4:-1].replace("-", "").isalnum() and stripped[4].islower():
            current = stripped[4:-1]
            blocks[current] = ""
        if current is not None:
            blocks[current] += line + "\n"
    return blocks


if __name__ == "__main__":
    unittest.main()
