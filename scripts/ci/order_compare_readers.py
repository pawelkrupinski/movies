"""What order_compare.py reads a leg's failing tests from -- sbt's log, Gradle's JUnit reports, XCTest's log, the
each-alone scripts' $ORDER_FAILED_FILE -- and the default-order command its control pass runs for them, per leg."""
from __future__ import annotations

import os
import re
from typing import Iterable, List, Mapping, Sequence

ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
HEADING = re.compile(r"^\[error\]\s+(Failed tests|Error during tests):\s*$")
SUITE = re.compile(r"^\[error\]\s+([A-Za-z_][A-Za-z0-9_.$]*)\s*$")
FAILED_CASE = re.compile(r"^<testcase [^>]*[^/]>[^<]*<(failure|error)[ >/]")
CLASSNAME = re.compile(r'^<testcase [^>]*classname="([^"]+)"')
XCTEST_FAILED = re.compile(r"^.*Test Case '-\[([A-Za-z0-9_]+)\.([A-Za-z0-9_]+) ([A-Za-z0-9_]+)\]' failed")
REGEX_SPECIAL = re.compile(r"([.\[\\*^$()+?{}|])")


def unique(names: Iterable[str]) -> List[str]:
    return sorted(set(names))


def read_text(path: str) -> str:
    with open(path, encoding="utf-8", errors="surrogateescape") as f:
        return f.read()


def failed_suites(log: str) -> List[str]:
    """The suites sbt names under each `Failed tests:` / `Error during tests:` heading (colours off); no log, none."""
    if not os.path.isfile(log):
        return []
    found: List[str] = []
    inside = False
    for line in ANSI.sub("", read_text(log)).split("\n"):
        if HEADING.match(line):
            inside = True
            continue
        suite = SUITE.match(line) if inside else None
        if suite:
            found.append(suite.group(1))
        else:
            inside = False
    return unique(found)


def failed_junit(directory: str) -> List[str]:
    """The class of each <testcase> holding a <failure or <error, in every *.xml under <directory>."""
    if not os.path.isdir(directory):
        return []
    text = ""
    for top, _, files in os.walk(directory):
        for name in files:
            if name.endswith(".xml"):
                text += read_text(os.path.join(top, name))
    cases = ["<testcase " + piece for piece in text.replace("\n", " ").split("<testcase ")[1:]]
    return unique(m.group(1) for c in cases if FAILED_CASE.match(c) for m in [CLASSNAME.match(c)] if m)


def failed_xctest(log: str) -> List[str]:
    """Darwin's `Test Case '-[Mod.Class test]' failed`, as Mod.Class/test."""
    if not os.path.isfile(log):
        return []
    matches = (XCTEST_FAILED.match(line) for line in read_text(log).split("\n"))
    return unique(f"{m.group(1)}.{m.group(2)}/{m.group(3)}" for m in matches if m)


def named_by(leg: str, log: str, android_results: str) -> List[str]:
    """The tests a failed control run of <leg> names: Gradle's reports, XCTest's cases (ios: their classes), sbt's."""
    if leg == "android":
        return failed_junit(android_results)
    if leg == "swift":
        return failed_xctest(log)
    if leg == "ios":
        return unique(case.split(".", 1)[1].split("/", 1)[0] for case in failed_xctest(log))
    return failed_suites(log)


def failed_by(leg: str, log: str, failed_file: str, android_results: str) -> List[str]:
    """The tests a failed leg names: sbt's log, Gradle's reports, or what the leg's script wrote to $ORDER_FAILED_FILE."""
    if leg.startswith("scala-"):
        return failed_suites(log)
    if leg == "android":
        return failed_junit(android_results)
    return unique(line for line in read_text(failed_file).split("\n") if line) if os.path.isfile(failed_file) else []


def control_command(leg: str, tests: Sequence[str], root: str, env: Mapping[str, str]) -> List[str]:
    if leg.startswith("scala-"):
        return ["env", "-u", "TEST_ORDER_SEED", "sbt", "testOnly " + " ".join(tests)]
    if leg == "android":
        android = os.path.join(root, "android")
        # the leg's other settings (its sync-model depth): only the order may differ from the leg's run
        return [os.path.join(android, "gradlew"), "-p", android, "--no-daemon", "--offline", "testDebugUnitTest",
                *env.get("ORDER_CONTROL_EXTRA", "").split(), *[w for t in tests for w in ("--tests", t)]]
    if leg == "android-instrumented":
        return [f"{root}/android/scripts/devtest.sh --skip-live-youtube {t}" for t in tests]
    if leg == "swift":
        alternation = "|".join(REGEX_SPECIAL.sub(r"\\\1", t) for t in tests)
        return ["swift", "test", *env.get("SWIFTPM_FLAGS", "").split(), "--package-path", os.path.join(root, "ios"),
                "--skip-build", "--filter", f"^({alternation})$"]
    return ios_command(tests, root, env)


def ios_command(tests: Sequence[str], root: str, env: Mapping[str, str]) -> List[str]:
    return ["xcodebuild", "test-without-building", "-project", f"{root}/ios/Kinowo.xcodeproj", "-scheme", "Kinowo",
            "-destination", env.get("DESTINATION", ""), "-derivedDataPath", f"{root}/build/dd",
            "-resultBundlePath", f"{root}/build/alone/control.xcresult", "-test-timeouts-enabled", "YES",
            "-default-test-execution-time-allowance", "180", "-maximum-test-execution-time-allowance", "300",
            *[f"-only-testing:KinowoUITests/{t}" for t in tests], "CODE_SIGNING_ALLOWED=NO"]
