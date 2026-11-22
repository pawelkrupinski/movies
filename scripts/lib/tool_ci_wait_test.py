"""tool_ci_wait.wait_for_run against a fake runner: scripts/ci-wait is asked for the run, bounded and polled as given
(a deadline already past still reads it once, `0s`), its line's third field is the conclusion for each exit ci-wait
gives one (0 success, 1 failure, 3 cancelled), and any other exit is no conclusion, said."""
from __future__ import annotations

import contextlib
import io
import os
import sys
import unittest
from typing import List

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tool_ci_wait  # noqa: E402
import tool_process  # noqa: E402


class FakeRunner:
    """Answers every command with one Captured, noting each argv."""

    def __init__(self, code: int, stdout: str = "", stderr: str = "") -> None:
        self.answer = tool_process.Captured(code, stdout, stderr)
        self.argvs: List[List[str]] = []

    def run(self, command: tool_process.Command) -> int:
        return self.capture(command).code

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        self.argvs.append(list(command.argv))
        return self.answer


def wait(runner: FakeRunner, timeout: int = 600) -> "tuple[object, str]":
    said = io.StringIO()
    with contextlib.redirect_stderr(said):
        conclusion = tool_ci_wait.wait_for_run(runner, {}, "901", timeout, 30)
    return conclusion, said.getvalue()


class WaitForRunTest(unittest.TestCase):
    def test_ci_wait_is_asked_for_the_run_bounded_and_polled_as_given(self) -> None:
        runner = FakeRunner(0, "901\tcompleted\tsuccess\n")
        self.assertEqual("success", wait(runner)[0])
        self.assertEqual([[tool_ci_wait.CI_WAIT, "--run", "901", "--no-follow", "--timeout", "600s", "--interval",
                           "30s"]], runner.argvs)
        self.assertTrue(os.access(tool_ci_wait.CI_WAIT, os.X_OK), tool_ci_wait.CI_WAIT)

    def test_a_deadline_already_past_still_reads_the_run_once(self) -> None:
        runner = FakeRunner(0, "901\tcompleted\tsuccess\n")
        wait(runner, timeout=-5)
        self.assertIn("0s", runner.argvs[0])

    def test_failure_and_cancelled_are_conclusions_too(self) -> None:
        self.assertEqual("failure", wait(FakeRunner(1, "901\tcompleted\tfailure\n"))[0])
        self.assertEqual("cancelled", wait(FakeRunner(3, "901\tcompleted\tcancelled\n"))[0])

    def test_any_other_exit_is_no_conclusion_said(self) -> None:
        conclusion, said = wait(FakeRunner(2, "", "ci-wait: gh unreadable\n"))
        self.assertIsNone(conclusion)
        self.assertIn("ci-wait: gh unreadable", said)
        self.assertIn("dispatch: run 901 gave no conclusion within 600s (or could not be read)", said)


if __name__ == "__main__":
    unittest.main()
