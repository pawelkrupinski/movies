#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror_test.py
"""mirror_test.py's TERM_IN_WAIT hook itself (the sitecustomize mirror_tunnel_test.py's TermInWaitTest arms), run in this
process: the daemon's, the keeper's and every supervisor's probe waits all race to claim its one TERM, and the claim runs
holding the wait's lock. An exception raised there kept the lock, the Popen's own cleanup wait then blocked on it
forever, and a keeper hung so kept its ssh past the TERMed daemon (1 stress run in ~1,100 in CI's container).
Run: python3 scripts/local-mirror/mirror_term_hook_test.py
"""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
import tempfile
import types
import unittest
from typing import Any, Dict, List
from unittest import mock

from mirror_test import TERM_IN_WAIT

PROBE = types.SimpleNamespace(args=["/repo/scripts/local-mirror/mongosh-at.sh", "mongodb://x", "--quiet"])


class TermInWaitHookTest(unittest.TestCase):
    def setUp(self) -> None:
        work = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, work)
        self.armed = os.path.join(work, "term-in-wait")
        hook: Dict[str, Any] = {}
        with mock.patch.dict(os.environ, {"FAKE_TERM_IN_WAIT": self.armed}), \
                mock.patch.object(subprocess.Popen, "__init__", subprocess.Popen.__init__):
            exec(TERM_IN_WAIT, hook)  # the hook's Popen patch is undone on leaving: only its Window is used here
        self.window = hook["Window"](PROBE)
        self.terms: List[int] = []
        for patch in (mock.patch.object(sys, "argv", ["/repo/scripts/local-mirror/mirror.py", "--supervise", "kinowo", "0"]),
                      mock.patch("os.kill", lambda pid, signum: self.terms.append(signum))):
            patch.start()
            self.addCleanup(patch.stop)

    def arm(self, role: str) -> None:
        with open(self.armed, "w") as f:
            f.write(role + "\n")

    def wait_once(self) -> None:
        """One step of a timed child-wait: the lock taken without blocking, then released."""
        self.assertTrue(self.window.acquire(False))
        self.window.release()
        self.assertFalse(self.window.locked())

    def test_the_process_the_file_names_is_termed_once_and_the_file_is_gone(self) -> None:
        self.arm("supervisor")
        self.wait_once()
        self.wait_once()
        self.assertEqual([signal.SIGTERM], self.terms)
        self.assertFalse(os.path.exists(self.armed))

    def test_a_process_the_file_does_not_name_is_left_alone(self) -> None:
        self.arm("keeper")
        self.wait_once()
        self.assertEqual([], self.terms)
        self.assertTrue(os.path.exists(self.armed))

    def test_a_file_removed_by_the_process_it_names_after_this_one_saw_it_raises_nothing(self) -> None:
        with mock.patch("os.path.exists", return_value=True):  # there when looked for, the daemon's claim since
            self.wait_once()
        self.assertEqual([], self.terms)

    def test_of_two_supervisors_reading_their_role_only_the_one_that_removed_it_is_termed(self) -> None:
        self.arm("supervisor")
        with mock.patch("os.remove", side_effect=FileNotFoundError(2, "the other supervisor removed it first")):
            self.wait_once()
        self.assertEqual([], self.terms)


if __name__ == "__main__":
    unittest.main()
