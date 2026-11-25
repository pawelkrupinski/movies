#!/usr/bin/env python3
"""gh_bounded.py: $KINOWO_GH's stdout; a failure or a gh past its time limit raises CalledProcessError (124 for the
limit), never a hang."""
from __future__ import annotations

import contextlib
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from typing import Iterator
from unittest import mock

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gh_bounded  # noqa: E402

FAKE_GH = """#!/bin/sh
case "$1" in
  slow) exec sleep 30 ;;
  fail) echo "HTTP 502" >&2; exit 1 ;;
  *) cat; echo "args: $*" ;;
esac
"""


@contextlib.contextmanager
def hung_gh() -> Iterator[None]:
    """A gh that never answers, as $KINOWO_GH and first on PATH, under a half-second limit: a tool calling gh through
    gh_bounded raises CalledProcessError at once; one calling it unbounded hangs its 30 seconds and answers."""
    with tempfile.TemporaryDirectory() as d:
        fake = os.path.join(d, "gh")
        with open(fake, "w") as f:
            f.write("#!/bin/sh\nexec sleep 30\n")
        os.chmod(fake, os.stat(fake).st_mode | stat.S_IEXEC)
        env = {"KINOWO_GH": fake, "PATH": d + os.pathsep + os.environ.get("PATH", "")}
        with mock.patch.dict(os.environ, env), mock.patch.object(gh_bounded, "LIMIT", 0.5):
            yield


class GhBoundedTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        fake = os.path.join(self.tmp.name, "gh")
        with open(fake, "w") as f:
            f.write(FAKE_GH)
        os.chmod(fake, os.stat(fake).st_mode | stat.S_IEXEC)
        self.saved = os.environ.get("KINOWO_GH")
        os.environ["KINOWO_GH"] = fake

    def tearDown(self) -> None:
        if self.saved is None:
            os.environ.pop("KINOWO_GH", None)
        else:
            os.environ["KINOWO_GH"] = self.saved
        self.tmp.cleanup()

    def test_its_stdout_with_its_input(self) -> None:
        self.assertEqual("body\nargs: issue edit 1\n", gh_bounded.gh(["issue", "edit", "1"], input="body\n"))

    def test_a_failure_raises_with_its_stderr(self) -> None:
        with self.assertRaises(subprocess.CalledProcessError) as caught:
            gh_bounded.gh(["fail"])
        self.assertEqual((1, "HTTP 502\n"), (caught.exception.returncode, caught.exception.stderr))

    def test_a_gh_past_its_limit_is_a_failure_not_a_hang(self) -> None:
        with self.assertRaises(subprocess.CalledProcessError) as caught:
            gh_bounded.gh(["slow"], timeout=0.5)
        self.assertEqual((124, "gh timed out after 0.5s"), (caught.exception.returncode, caught.exception.stderr))

    def test_its_limit_is_read_at_each_call(self) -> None:
        with hung_gh(), self.assertRaises(subprocess.CalledProcessError):
            gh_bounded.gh(["api", "x"])


if __name__ == "__main__":
    unittest.main()
