#!/usr/bin/env python3
"""Every XCUITest class ALONE, each in its own `xcodebuild test-without-building` on the booted $DESTINATION, so none
can lean on state another class left behind (order-independence.yml's ios job, after a build-for-testing into
build/dd). Run from the checkout:

    DESTINATION=<an xcodebuild -destination> scripts/ci/xcuitest-each-class-alone.sh

Every class runs even after a failure; the step then fails naming the classes. A failed class's group opens with
an error per failed test carrying its own assertion at its file and line (xcresult_annotations.py), then the tail of
its xcodebuild log: the first error a failed step prints is the line its red is known by. That is also why this is
a script and not the workflow's `run:` block: GitHub echoes a `run:` block into the log, and its
`echo "::error::..."` lines were read as the failure -- Order independence #28-#31 failed in four different ways
and read as one red. Each test is bounded: a hung one fails at its allowance, naming itself.
Result bundles and logs: build/alone/<class>.xcresult, build/alone/<class>.log (the runner's warm-up:
build/alone/runner-warm-up.*). XCUITEST_WARMUP_TEST (default RepertoireLaunchUITests/testAppLaunches) is its test.
XCUITEST_DIR (default ios/KinowoUITests) stands in for the classes' directory in its test.

FIRST LAUNCH BEFORE THE FIRST CLASS: a fresh Simulator's first launch of the app is minutes of system work (Order
independence #77: 148 s, and XCUITest's launch timed out; every later launch ~16 s), and it fell on whichever class
sorts first, every run CardShareMenuUITests (#74 a press read as a tap in the same stall). The workflow boots the
Simulator first; this installs the built app and launches it once, outside any test. A warm-up that fails or hangs
only warns: the classes still say for themselves whether they pass.
It launches as FixtureLaunch.intoGrid does (the UI-test flags, a pinned country, language and city, the fixture
listing): a bare launch asked for the location (Order independence #82: "Allow “Showtimes” to use your location?" was
still up when CardShareMenuUITests began, handled by XCUITest's "Don't Allow", and its second test's launch then
failed to terminate the first's app). No test ever meets that prompt; the warm-up must not leave it either.

AND THE TEST RUNNER'S FIRST RUN BEFORE THE FIRST CLASS (Order independence #115): the app warm, XCUITest's own first
run on a fresh Simulator -- its runner app's install and launch, the automation session, the accessibility stack --
still cost ~70 s, and it fell inside whichever class sorts first: CardShareMenuUITests' long-press test took 81 s
(13.7 s warm) and its hold read as no press, labelled ORDER by the control pass though no class ran before it. One
throwaway run of a quick test pays it; it only warns, its result counts for nothing.

Tested by xcuitest_each_class_alone_test.py beside it; the name the workflow runs is the .sh, a thin exec of this."""
from __future__ import annotations

import os
import re
import sys
import time
from pathlib import Path
from typing import List, Mapping, Optional, Sequence

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "lib"))
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

CLASS_DECLARATION = re.compile(r"class\s+([A-Za-z0-9_]+)\s*:\s*XCTestCase")
PROJECT = ["-project", "ios/Kinowo.xcodeproj", "-scheme", "Kinowo"]
DEFAULT_APP = "build/dd/Build/Products/Debug-iphonesimulator/Kinowo.app"


def classes_in(directory: str) -> List[str]:
    found = set()
    for swift in sorted(Path(directory).glob("*.swift")):
        found.update(CLASS_DECLARATION.findall(swift.read_text(errors="replace")))
    return sorted(found)


class EachClassAlone:
    def __init__(self, runner: Runner, env: Mapping[str, str]) -> None:
        self.runner = runner
        self.env = env
        self.destination = env["DESTINATION"]
        self.udid = self.destination.split("id=")[-1]

    def xcode(self, only: str, bundle: str, *extra: str) -> List[str]:
        return ["xcodebuild", "test-without-building", *PROJECT, "-destination", self.destination,
                "-derivedDataPath", "build/dd", f"-only-testing:KinowoUITests/{only}", "-resultBundlePath", bundle,
                *extra, "CODE_SIGNING_ALLOWED=NO"]

    def simctl(self, seconds: float, *args: str, env: Optional[Mapping[str, str]] = None) -> bool:
        argv = ["xcrun", "simctl", *args]
        return self.runner.run(Command(argv, {**self.env, **(env or {})}, timeout=seconds)) == 0

    def warm_simulator(self) -> None:
        app = self.env.get("XCUITEST_APP", DEFAULT_APP)
        bundle = self.env.get("XCUITEST_BUNDLE_ID", "dev.kinowo.Kinowo")
        started = time.monotonic()
        launch_env = {"SIMCTL_CHILD_KINOWO_UITEST_FIXTURE": "1", "SIMCTL_CHILD_KINOWO_FORCE_DETECTED_CITY": "warszawa"}
        if (self.simctl(300, "install", self.udid, app)
                and self.simctl(300, "launch", self.udid, bundle, "-UITests", "1", "-selectedCountryCode", "pl",
                                "-selectedLanguageCode", "pl", "-AppleLanguages", "(pl)", "-AppleLocale", "pl",
                                "-selectedCity", "warszawa", env=launch_env)):
            print(f"warmed the Simulator: {bundle} installed and launched once in {since(started)}s", flush=True)
            if not self.simctl(60, "terminate", self.udid, bundle):
                print(f"::warning::could not end the warm-up launch of {bundle}", flush=True)
        else:
            print(f"::warning::the Simulator warm-up launch of {bundle} failed after {since(started)}s: "
                  "the first class pays the first launch", flush=True)

    def warm_runner(self) -> None:
        test = self.env.get("XCUITEST_WARMUP_TEST", "RepertoireLaunchUITests/testAppLaunches")
        started = time.monotonic()
        argv = self.xcode(test, "build/alone/runner-warm-up.xcresult")
        if self.runner.run(Command(argv, self.env, log="build/alone/runner-warm-up.log", timeout=600)) == 0:
            print(f"warmed the XCUITest runner: {test} once in {since(started)}s", flush=True)
        else:
            print(f"::warning::the XCUITest runner warm-up ({test}) failed after {since(started)}s: "
                  "the first class pays the runner's first run", flush=True)

    def run_class(self, name: str) -> bool:
        print(f"::group::{name}", flush=True)
        started = time.monotonic()
        argv = self.xcode(name, f"build/alone/{name}.xcresult", "-test-timeouts-enabled", "YES",
                          "-default-test-execution-time-allowance", "180", "-maximum-test-execution-time-allowance", "300")
        log = f"build/alone/{name}.log"
        passed = self.runner.run(Command(argv, self.env, log=log)) == 0
        if passed:
            print(f"{name} passed alone in {since(started)}s")
        else:
            print(f"{name} FAILED alone after {since(started)}s", flush=True)
            self.runner.run(Command([sys.executable, str(HERE / "xcresult_annotations.py"), "--level", "error",
                                     f"build/alone/{name}.xcresult"], self.env))
            print("".join(Path(log).read_text(errors="replace").splitlines(keepends=True)[-80:]), end="")
        print("::endgroup::", flush=True)
        return passed

    def report(self, failed: Sequence[str]) -> int:
        line = "XCUITest classes that fail when run alone:" + "".join(f" {c}" for c in failed)
        with open(self.env.get("GITHUB_STEP_SUMMARY", os.devnull), "a") as summary:
            summary.write(line + "\n")
        print(f"::error::{line}", flush=True)
        if self.env.get("ORDER_FAILED_FILE"):  # one a line, for Order independence's control pass (order_compare.py leg)
            Path(self.env["ORDER_FAILED_FILE"]).write_text("".join(f"{c}\n" for c in failed))
        return 1

    def run(self) -> int:
        directory = self.env.get("XCUITEST_DIR", "ios/KinowoUITests")
        self.warm_simulator()
        os.makedirs("build/alone", exist_ok=True)
        self.warm_runner()
        classes = classes_in(directory)
        if not classes:
            print(f"::error::no XCUITest classes found in {directory}")
            return 1
        failed = [c for c in classes if not self.run_class(c)]
        return self.report(failed) if failed else 0


def since(started: float) -> int:
    return int(time.monotonic() - started)


def main() -> int:
    if not os.environ.get("DESTINATION"):
        print("xcuitest-each-class-alone: DESTINATION: the simulator to run on, as an xcodebuild -destination",
              file=sys.stderr)
        return 1
    return EachClassAlone(SubprocessRunner(), os.environ).run()


if __name__ == "__main__":
    sys.exit(main())
