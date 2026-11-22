#!/usr/bin/env python3
"""ios/scripts/swift-test-shuffled.sh, black-box, against a fake `swift`, so it runs in a second with no toolchain.
The promise under test: a case counts as passed only when its own `swift test --filter` run shows exactly ONE test
executed. `swift test` exits 0 when a filter matches nothing ("No matching test cases were run"), so exit status alone
would pass a case that was never run -- a renamed test, or a name the regex escaping mangles, would look green
forever. Ported from swift-test-shuffled-test.sh case for case; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("ios/scripts/swift_test_shuffled.py", "ios/scripts/swift-test-shuffled.sh")
# `swift build` and `swift test list` answer from SHIM_TESTS; `swift test --filter <pattern>` reports by the test the
# pattern names: *Ghost* matches nothing, *Twice* runs two, *Broken* fails, anything else runs once and passes.
SWIFT = r'''
[ -n "${SHIM_ARGS:-}" ] && echo "$*" >> "$SHIM_ARGS"
case "$1 ${2:-}" in
  "build "*) exit 0 ;;
  "test list") printf '%s\n' $SHIM_TESTS; exit 0 ;;
esac
pattern=""
while [ $# -gt 0 ]; do [ "$1" = --filter ] && pattern="$2"; shift; done
# A test process may read stdin; SHIM_READS_STDIN makes every run drain it.
[ -n "${SHIM_READS_STDIN:-}" ] && cat >/dev/null
[ -n "${SHIM_LOG:-}" ] && echo "$pattern" >> "$SHIM_LOG"
case "$pattern" in
  *Ghost*)  echo "warning: No matching test cases were run"; exit 0 ;;
  *Twice*)  echo "	 Executed 2 tests, with 0 failures (0 unexpected) in 0.001 (0.002) seconds"; exit 0 ;;
  *Broken*) echo "	 Executed 1 test, with 1 failure (0 unexpected) in 0.001 (0.002) seconds"; exit 1 ;;
  *)        echo "	 Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.002) seconds"; exit 0 ;;
esac'''


class SwiftTestShuffledTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("swift", SWIFT, shell="/usr/bin/env bash")

    def shuffled(self, *tests: str, **env: str) -> cli_harness.Done:
        return self.run_tool("7", env={"SHIM_TESTS": " ".join(tests), **env})

    def test_every_case_running_once_and_passing_is_a_pass(self) -> None:
        done = self.shuffled("A.T/testOne", "A.T/testTwo")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("all 2 cases pass alone", done.stdout)

    def test_a_filter_that_matched_nothing_fails_though_swift_exited_0(self) -> None:
        done = self.shuffled("A.T/testOne", "A.T/testGhost")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, done.stdout.splitlines().count("FAILED alone: A.T/testGhost"), done.output)
        self.assertIn("swift-test-shuffled: expected exactly 1 test to run, saw none", done.stdout)

    def test_a_filter_that_ran_more_than_the_one_case_fails(self) -> None:
        done = self.shuffled("A.T/testTwice")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("saw 2", done.stdout)

    def test_a_case_failing_alone_fails_and_the_failures_go_to_order_failed_file(self) -> None:
        self.assertEqual(1, self.shuffled("A.T/testBroken").code)
        failed = self.path("failed")
        done = self.shuffled("A.T/testOne", "A.T/testBroken", "A.T/testGhost", ORDER_FAILED_FILE=failed)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["A.T/testBroken", "A.T/testGhost"], sorted(self.read("failed").splitlines()))
        self.assertIn("swift-test-shuffled: 2 of 3 cases fail alone under seed 7:", done.stdout)

    def test_the_banner_names_the_seed_the_count_and_the_replay(self) -> None:
        done = self.shuffled("A.T/testOne", "A.T/testTwo", "A.T/testThree")
        self.assertEqual(
            "swift-test-shuffled: seed 7 — 3 cases, each alone — reproduce with ios/scripts/swift-test-shuffled.sh 7",
            done.stdout.splitlines()[0])

    def test_the_cases_run_in_the_seeded_order_each_by_its_escaped_exact_pattern(self) -> None:
        log = self.path("log")
        self.shuffled("A.T/testOne", "A.T/testTwo", "A.T/testThree", "B.T/test$x", SHIM_LOG=log)
        # the order seeded_order gives seed 7 (scripts/seeded-order.sh: the POSIX cksum of "<seed>\n<line>")
        self.assertEqual(["^A\\.T/testThree$", "^B\\.T/test\\$x$", "^A\\.T/testOne$", "^A\\.T/testTwo$"],
                         self.read("log").splitlines())

    def test_a_case_that_reads_stdin_cannot_swallow_the_cases_still_to_run(self) -> None:
        log = self.path("log")
        self.shuffled("A.T/testOne", "A.T/testTwo", "A.T/testThree", SHIM_READS_STDIN="1", SHIM_LOG=log)
        self.assertEqual(3, len(self.read("log").splitlines()))

    def test_swiftpm_flags_reach_every_swift_invocation(self) -> None:
        args = self.path("args")
        self.shuffled("A.T/testOne", SHIM_ARGS=args, SWIFTPM_FLAGS="--disable-sandbox")
        lines = self.read("args").splitlines()
        self.assertEqual(3, len(lines), lines)
        self.assertTrue(all("--disable-sandbox" in line for line in lines), lines)

    def test_no_tests_listed_is_an_error(self) -> None:
        done = self.shuffled()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("swift-test-shuffled: no tests listed", done.stderr)

    def test_no_seed_is_a_usage_error(self) -> None:
        done = self.run_tool()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("usage: swift-test-shuffled.sh <seed>", done.stderr)


if __name__ == "__main__":
    unittest.main()
