#!/usr/bin/env python3
"""Every instrumented (androidTest) class ALONE, in an order drawn from a seed -- the emulator lane of the nightly
order-independence workflow. The ordinary connectedDebugAndroidTest run always meets the classes in one order, on one
app install whose prefs and caches carry over from class to class, so a class that passes only because another ran
first looks healthy forever. Each class here gets its own connectedDebugAndroidTest run (devtest.sh -- installed fresh
and uninstalled after), in a seeded order. Reproduce a failure with the seed it prints:

    android/scripts/devtest-shuffled.sh <seed> [--skip-live-youtube]

Every class's run writes its report to the same Gradle paths, so a failing class's report would be overwritten by the
next class's: a copy is kept per failing class under app/build/order-independence/<class>/. ORDER_FAILED_FILE: where
the failing classes go, one a line, for Order independence's control pass (scripts/ci/order_compare.py leg)."""
from __future__ import annotations

import os
import shutil
import sys
from typing import List, Mapping, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
from processes import machine  # noqa: E402
from mobile.seeded_order import seeded_order  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

NAME = "devtest-shuffled"
# Where a connected run leaves its report and its results, and the name each is kept under
REPORTS = (("build/reports/androidTests/connected", "reports"),
           ("build/outputs/androidTest-results/connected", "results"))


def test_classes(sources: str) -> List[str]:
    """Every Kotlin file with a JUnit @Test under `sources`, as a class name (one top-level test class per file)."""
    found: List[str] = []
    for directory, _, files in os.walk(sources):
        for name in files:
            path = os.path.join(directory, name)
            if name.endswith(".kt") and _mentions_test(path):
                found.append(os.path.relpath(path, sources)[:-len(".kt")].replace(os.sep, "."))
    return sorted(found)


def _mentions_test(path: str) -> bool:
    with open(path, errors="replace") as f:
        return "@Test" in f.read()


class Shuffled:
    def __init__(self, runner: Runner, env: Mapping[str, str], android: str, args: Sequence[str]) -> None:
        self.runner = runner
        self.env = env
        self.android = android
        self.args = list(args)
        self.kept = os.path.join(android, "app", "build", "order-independence")

    def passes_alone(self, test_class: str) -> bool:
        # closed stdin: adb and Gradle's client both read theirs
        print(f"::group::{test_class}", flush=True)
        argv = [os.path.join(self.android, "scripts", "devtest.sh"), *self.args, test_class]
        passed = self.runner.run(Command(argv, self.env, closed_stdin=True)) == 0
        if not passed:
            self.keep_report(test_class)
        print("::endgroup::", flush=True)
        return passed

    def keep_report(self, test_class: str) -> None:
        for source, name in REPORTS:
            report = os.path.join(self.android, "app", source)
            if os.path.isdir(report):
                shutil.copytree(report, os.path.join(self.kept, test_class, name))
            os.makedirs(os.path.join(self.kept, test_class), exist_ok=True)


def report(seed: str, count: int, failed: Sequence[str], env: Mapping[str, str]) -> int:
    if not failed:
        print(f"{NAME}: all {count} classes pass alone under seed {seed}")
        return 0
    print(f"{NAME}: {len(failed)} of {count} classes fail alone under seed {seed}:")
    print("".join(f"  {test_class}\n" for test_class in failed), end="")
    if env.get("ORDER_FAILED_FILE"):
        with open(env["ORDER_FAILED_FILE"], "w") as f:
            f.write("".join(f"{test_class}\n" for test_class in failed))
    return 1


def run(argv: Sequence[str], env: Mapping[str, str], runner: Runner) -> int:
    if not argv or not argv[0]:
        print(f"usage: {NAME}.sh <seed> [--skip-live-youtube]", file=sys.stderr)
        return 1
    seed, args = argv[0], argv[1:]
    android = os.path.dirname(HERE)
    sources = os.path.join(android, "app", "src", "androidTest", "java")
    ordered = seeded_order(seed, test_classes(sources))
    if not ordered:
        print(f"{NAME}: no androidTest classes under {sources}", file=sys.stderr)
        return 1
    print(f"{NAME}: seed {seed} — {len(ordered)} classes, each alone — reproduce with "
          f"android/scripts/{NAME}.sh {seed} {' '.join(args)}", flush=True)
    shuffled = Shuffled(runner, env, android, args)
    shutil.rmtree(shuffled.kept, ignore_errors=True)
    failed = [test_class for test_class in ordered if not shuffled.passes_alone(test_class)]
    return report(seed, len(ordered), failed, env)


def main() -> int:
    return run(sys.argv[1:], machine.environment(), SubprocessRunner())


if __name__ == "__main__":
    sys.exit(main())
