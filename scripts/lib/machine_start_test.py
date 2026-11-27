#!/usr/bin/env python3
"""machine.py's starts: a command refused for want of a resource (fork's EAGAIN, a full file table's ENFILE / EMFILE,
ENOMEM) is started again, a few times, and its own status is the answer; any other refusal, or one that never clears,
is 127 at once / after the tries -- what agentctl read as a NONZERO EXIT from the hook's `--plan` under load."""
from __future__ import annotations

import errno
import os
import subprocess
import sys
import unittest
from typing import Any, List
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import machine  # noqa: E402

REAL_RUN = subprocess.run


def refusing(refusals: List[int]) -> Any:
    """subprocess.run raising OSError(errno) for each of `refusals` in turn, then running the command."""
    left = list(refusals)

    def run(*args: Any, **kwargs: Any) -> Any:
        if left:
            raise OSError(left.pop(0), "refused")
        return REAL_RUN(*args, **kwargs)
    return run


class StartTest(unittest.TestCase):
    def setUp(self) -> None:
        paused = mock.patch.object(machine.time, "sleep")
        self.sleeps = paused.start()
        self.addCleanup(paused.stop)

    def refused(self, refusals: List[int]) -> Any:
        patched = mock.patch.object(machine.subprocess, "run", side_effect=refusing(refusals))
        self.addCleanup(patched.stop)
        return patched.start()

    def test_each_transient_refusal_is_started_again(self) -> None:
        for refusal in (errno.EAGAIN, errno.ENFILE, errno.EMFILE, errno.ENOMEM):
            with self.subTest(errno.errorcode[refusal]):
                with mock.patch.object(machine.subprocess, "run", side_effect=refusing([refusal, refusal])):
                    self.assertEqual((0, "toplevel\n"), machine.output(["echo", "toplevel"]))
                with mock.patch.object(machine.subprocess, "run", side_effect=refusing([refusal])):
                    self.assertEqual(3, machine.status(["sh", "-c", "exit 3"]))
                with mock.patch.object(machine.subprocess, "run", side_effect=refusing([refusal])):
                    self.assertEqual((0, "out\n", "err\n"), machine.both(["sh", "-c", "echo out; echo err >&2"]))

    def test_a_refusal_that_never_clears_is_127_after_the_tries(self) -> None:
        run = self.refused([errno.EAGAIN] * 10)
        self.assertEqual((127, ""), machine.output(["true"]))
        self.assertEqual(machine.START_TRIES, run.call_count)
        self.assertEqual(machine.START_TRIES - 1, self.sleeps.call_count)

    def test_any_other_refusal_is_127_at_once(self) -> None:
        run = self.refused([errno.ENOENT])
        self.assertEqual((127, ""), machine.output(["no-such-command"]))
        self.assertEqual(1, run.call_count)
        self.sleeps.assert_not_called()

    def test_a_timeout_is_still_124(self) -> None:
        self.assertEqual(124, machine.status(["sleep", "5"], timeout=0.2))


if __name__ == "__main__":
    unittest.main()
