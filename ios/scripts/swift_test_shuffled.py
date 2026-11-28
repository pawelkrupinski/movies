#!/usr/bin/env python3
"""Every `swift test` case ALONE, in an order drawn from a seed -- the iOS lane of the nightly order-independence
workflow. `swift test` has no shuffle and XCTest always runs a process's tests in name order, so a test that passes
only because another ran first (or never ran first) in that process, or because of what an earlier process left on
disk (UserDefaults suites, the caches directory), looks healthy forever. Running each case in its own process, in a
seeded order, meets both. Reproduce a failure with the seed it prints:

    ios/scripts/swift-test-shuffled.sh <seed>

SWIFTPM_FLAGS: words added to every swift invocation -- Order independence's `--disable-sandbox`: its tests already
run inside scripts/ci/no-network's sandbox-exec, and SwiftPM's own manifest sandbox cannot nest in it
("sandbox-exec: sandbox_apply: Operation not permitted").

ORDER_FAILED_FILE: where the failing cases go, one a line, for Order independence's control pass
(scripts/ci/order_compare.py leg)."""
from __future__ import annotations

import os
import re
import sys
import tempfile
from typing import List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
from processes import machine  # noqa: E402
from mobile.seeded_order import seeded_order  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

NAME = "swift-test-shuffled"
# The characters an extended regex reads as operators (the shell tool's `sed 's/[.[\*^$()+?{}|]/\\&/g'`)
REGEX_OPERATOR = re.compile(r"([.\[\\*^$()+?{}|])")
EXECUTED = re.compile(r"Executed ([0-9]+) tests?")


def exact_pattern(test: str) -> str:
    """The `--filter` regex matching `test` and nothing else."""
    return "^" + REGEX_OPERATOR.sub(r"\\\1", test) + "$"


def executed_count(log: str) -> Optional[str]:
    """XCTest's last "Executed N test(s)" count: the whole run's tally."""
    counts = EXECUTED.findall(log)
    return counts[-1] if counts else None


class Shuffled:
    def __init__(self, runner: Runner, env: Mapping[str, str], package: str, scratch: str) -> None:
        self.runner = runner
        self.env = env
        self.package = package
        self.log = os.path.join(scratch, "case.log")

    def swift(self, subcommand: Sequence[str], *args: str) -> List[str]:
        """`swift <subcommand> $SWIFTPM_FLAGS --package-path <package> <args>`, the shell tool's word order."""
        return ["swift", *subcommand, *self.env.get("SWIFTPM_FLAGS", "").split(), "--package-path", self.package,
                *args]

    def listed(self) -> Optional[List[str]]:
        if self.runner.run(Command(self.swift(["build"], "--build-tests"), self.env)) != 0:
            return None
        listing = self.runner.capture(Command(self.swift(["test", "list"], "--skip-build"), self.env))
        sys.stderr.write(listing.stderr)
        return listing.stdout.splitlines() if listing.code == 0 else None

    def passes_alone(self, test: str) -> bool:
        """`test` run alone: a pass is exit 0 with exactly ONE test executed -- `swift test` exits 0 when the filter
        matches nothing ("No matching test cases were run"), so a case the pattern misses would pass unrun."""
        argv = self.swift(["test"], "--skip-build", "--filter", exact_pattern(test))
        status = self.runner.run(Command(argv, self.env, log=self.log, closed_stdin=True))
        with open(self.log, errors="replace") as f:
            output = f.read()
        executed = executed_count(output)
        if status == 0 and executed == "1":
            return True
        if status == 0:
            print(f"{NAME}: expected exactly 1 test to run, saw {executed or 'none'}")
        print(f"FAILED alone: {test}")
        print("".join(output.splitlines(keepends=True)[-40:]), end="", flush=True)
        return False


def report(seed: str, count: int, failed: Sequence[str], env: Mapping[str, str]) -> int:
    if not failed:
        print(f"{NAME}: all {count} cases pass alone under seed {seed}")
        return 0
    print(f"{NAME}: {len(failed)} of {count} cases fail alone under seed {seed}:")
    print("".join(f"  {test}\n" for test in failed), end="")
    if env.get("ORDER_FAILED_FILE"):
        with open(env["ORDER_FAILED_FILE"], "w") as f:
            f.write("".join(f"{test}\n" for test in failed))
    return 1


def run(argv: Sequence[str], env: Mapping[str, str], runner: Runner, scratch: str) -> int:
    if not argv or not argv[0]:
        print(f"usage: {NAME}.sh <seed>", file=sys.stderr)
        return 1
    seed = argv[0]
    shuffled = Shuffled(runner, env, os.path.dirname(HERE), scratch)
    tests = shuffled.listed()
    if tests is None:
        return 1
    ordered = seeded_order(seed, tests)
    if not ordered:
        print(f"{NAME}: no tests listed", file=sys.stderr)
        return 1
    print(f"{NAME}: seed {seed} — {len(ordered)} cases, each alone — reproduce with ios/scripts/{NAME}.sh {seed}",
          flush=True)
    failed = [test for test in ordered if not shuffled.passes_alone(test)]
    return report(seed, len(ordered), failed, env)


def main() -> int:
    with tempfile.TemporaryDirectory(prefix=f"{NAME}-") as scratch:
        return run(sys.argv[1:], machine.environment(), SubprocessRunner(), scratch)


if __name__ == "__main__":
    sys.exit(main())
