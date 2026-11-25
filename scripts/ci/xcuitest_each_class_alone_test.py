#!/usr/bin/env python3
"""scripts/ci/xcuitest-each-class-alone.sh, black-box, with a stub xcodebuild and a stub xcrun (the failed bundle's test
tree), so it runs anywhere. The promise: every XCUITest class runs in its own xcodebuild, even after one fails; the step
then fails naming the classes; and a failed test's first error is its own assertion, at its file and line -- the line
its red is known by (Order independence #28-#31 failed in different classes and read as one red).
Ported from xcuitest-each-class-alone-test.sh; the same cases hold the shell tool and its Python rewrite."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/xcuitest_each_class_alone.py", "scripts/ci/xcuitest-each-class-alone.sh")
APP = "build/dd/Build/Products/Debug-iphonesimulator/Kinowo.app"

# xcodebuild: records each run's class and arguments, prints a line to the class's log, fails $FAILING.
XCODEBUILD = r"""
for arg in "$@"; do case "$arg" in -only-testing:*) class=${arg##*/} ;; esac; done
case "$*" in *runner-warm-up.xcresult*)
  for arg in "$@"; do case "$arg" in -only-testing:*) echo "xcodebuild warm-up ${arg#-only-testing:KinowoUITests/}" >>"$CALLS.order" ;; esac; done
  [ -z "${RUNNER_WARMUP_FAILS:-}" ]; exit ;;
esac
echo "$class" >>"$CALLS"
echo "xcodebuild $class" >>"$CALLS.order"
echo "$*" >>"$CALLS.args"
echo "error: -[KinowoUITests.$class testLaunches] : xcodebuild's own report for $class"
[ "$class" != "${FAILING:-}" ]
"""
# xcrun simctl <...>: the warm-up, recorded in order with the classes; fails when $WARMUP_FAILS.
# xcrun xcresulttool get test-results tests --path <bundle>: the failed class's tree.
XCRUN = r"""
if [ "$1" = simctl ]; then
  echo "$*" >>"$CALLS.order"
  [ "$2" = launch ] && echo "fixture=${SIMCTL_CHILD_KINOWO_UITEST_FIXTURE:-} city=${SIMCTL_CHILD_KINOWO_FORCE_DETECTED_CITY:-}" >>"$CALLS.order"
  [ -z "${WARMUP_FAILS:-}" ]; exit
fi
cat "$TREE"
"""


class EachClassAloneTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.write("tests/AlphaUITests.swift", "final class AlphaUITests: XCTestCase {}\n")
        self.write("tests/Shared.swift", "@MainActor\nfinal class GammaUITests : XCTestCase {}\n"
                   "final class BetaUITests: XCTestCase {}\nenum FixtureLaunch {}\n")
        os.makedirs(self.path("none"))
        self.fakes.script("xcodebuild", XCODEBUILD)
        self.fakes.script("xcrun", XCRUN)
        self.write("beta.json", """{"testNodes": [{"name": "Kinowo", "nodeType": "Test Plan", "result": "Failed", "children": [
 {"name": "KinowoUITests", "nodeType": "UI test bundle", "result": "Failed", "children": [
  {"name": "BetaUITests", "nodeType": "Test Suite", "result": "Failed", "children": [
   {"name": "testLaunches()", "nodeType": "Test Case", "result": "Failed", "children": [
    {"name": "XCTAssertTrue failed - Expected at least one film card after launch", "nodeType": "Failure Message",
     "sourceLocation": {"filePath": "%s/ios/KinowoUITests/BetaUITests.swift", "lineNumber": 25}}]}]}]}]}]}
""" % self.work)

    def alone(self, failing: str = "", directory: str = "tests", extra: Optional[Mapping[str, str]] = None):
        for name in ("calls", "calls.args", "calls.order", "summary"):
            self.write(name, "")
        env = {"CALLS": self.path("calls"), "FAILING": failing, "TREE": self.path("beta.json"),
               "DESTINATION": "platform=iOS Simulator,id=SIM", "XCUITEST_DIR": self.path(directory),
               "GITHUB_STEP_SUMMARY": self.path("summary"), "ORDER_FAILED_FILE": self.path("failed")}
        env.update(extra or {})
        return self.run_tool(env=env)

    def lines(self, name: str) -> List[str]:
        return self.read(name).splitlines()

    def test_every_class_runs_alone_after_one_failed_and_the_step_fails_naming_it(self) -> None:
        done = self.alone("BetaUITests")
        self.assertEqual(["AlphaUITests", "BetaUITests", "GammaUITests"], self.lines("calls"))
        self.assertEqual(1, sum("-destination platform=iOS Simulator,id=SIM " in a and
                                "-only-testing:KinowoUITests/BetaUITests -resultBundlePath build/alone/BetaUITests.xcresult "
                                "-test-timeouts-enabled YES" in a for a in self.lines("calls.args")))
        self.assertEqual(1, done.code, done.output)
        out = done.stdout.splitlines()
        first = next(l for l in out if "::error" in l or "error:" in l)
        self.assertEqual("::error file=ios/KinowoUITests/BetaUITests.swift,line=25,title=BetaUITests.testLaunches::"
                         "BetaUITests.testLaunches: XCTAssertTrue failed - Expected at least one film card after launch", first)
        self.assertEqual(1, done.stdout.count("xcodebuild's own report for BetaUITests"))
        self.assertEqual(0, done.stdout.count("report for AlphaUITests"))
        self.assertEqual("::error::XCUITest classes that fail when run alone: BetaUITests", out[-1])
        self.assertEqual("XCUITest classes that fail when run alone: BetaUITests", self.read("summary").strip())
        self.assertEqual("BetaUITests", self.read("failed").strip())

    def test_all_passing_warms_the_simulator_then_the_runner_before_the_first_class(self) -> None:
        done = self.alone()
        self.assertEqual(0, done.code, done.output)
        order = self.lines("calls.order")
        self.assertEqual(f"simctl install SIM {APP}|"
                         "simctl launch SIM dev.kinowo.Kinowo -UITests 1 -selectedCountryCode pl -selectedLanguageCode pl "
                         "-AppleLanguages (pl) -AppleLocale pl -selectedCity warszawa|fixture=1 city=warszawa|"
                         "simctl terminate SIM dev.kinowo.Kinowo", "|".join(order[:4]))
        self.assertEqual("xcodebuild warm-up RepertoireLaunchUITests/testAppLaunches|xcodebuild AlphaUITests",
                         "|".join(order[4:6]))
        self.assertEqual(0, done.stdout.count("::error"))

    def test_a_failed_runner_warm_up_only_warns(self) -> None:
        done = self.alone(extra={"RUNNER_WARMUP_FAILS": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(3, sum(l.startswith("xcodebuild ") and l[11:12].isupper() for l in self.lines("calls.order")))
        self.assertEqual(1, done.stdout.count("::warning::the XCUITest runner warm-up"))

    def test_a_failed_simulator_warm_up_only_warns(self) -> None:
        done = self.alone(extra={"WARMUP_FAILS": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(3, sum(l.startswith("xcodebuild ") and l[11:12].isupper() for l in self.lines("calls.order")))
        self.assertEqual(1, done.stdout.count("::warning::the Simulator warm-up launch"))

    def test_no_class_found_fails_naming_where_it_looked(self) -> None:
        done = self.alone(directory="none")
        self.assertEqual(1, done.code)
        self.assertEqual(f"::error::no XCUITest classes found in {self.path('none')}", done.stdout.splitlines()[-1])


if __name__ == "__main__":
    unittest.main()
