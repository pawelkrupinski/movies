#!/usr/bin/env python3
"""tool_process.py: a command's own exit status; its output into a log (appended when asked), or teed to stdout as
well; stdin closed when asked; 127 for a command not found and 126 for one not executable, said on stderr."""
from __future__ import annotations

import io
import os
import sys
import tempfile
import time
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tool_process  # noqa: E402

SH = ["/bin/sh", "-c"]


class SubprocessRunnerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.log = os.path.join(self.dir.name, "log")
        self.stdout = io.BytesIO()
        self.runner = tool_process.SubprocessRunner(stdout=self.stdout)  # type: ignore[arg-type]

    def tearDown(self) -> None:
        self.dir.cleanup()

    def run_sh(self, script: str, **how: bool) -> int:
        return self.runner.run(tool_process.Command([*SH, script], dict(os.environ), log=self.log, **how))

    def logged(self) -> str:
        with open(self.log) as f:
            return f.read()

    def test_the_commands_own_status_and_both_streams_in_the_log(self) -> None:
        self.assertEqual(3, self.run_sh("echo out; echo err >&2; exit 3"))
        self.assertEqual("out\nerr\n", self.logged())
        self.assertEqual(b"", self.stdout.getvalue())

    def test_append_and_tee(self) -> None:
        self.run_sh("echo one")
        self.assertEqual(0, self.run_sh("echo two", append=True, tee=True))
        self.assertEqual("one\ntwo\n", self.logged())
        self.assertEqual(b"two\n", self.stdout.getvalue())

    def test_closed_stdin_reads_nothing(self) -> None:
        self.assertEqual(0, self.run_sh("if read line; then exit 1; fi", closed_stdin=True))

    def test_not_found_and_not_executable_as_a_shell_says(self) -> None:
        missing = os.path.join(self.dir.name, "no-such-tool")
        self.assertEqual(127, self.runner.run(tool_process.Command([missing], dict(os.environ), log=self.log)))
        plain = os.path.join(self.dir.name, "plain")
        open(plain, "w").close()
        self.assertEqual(126, self.runner.run(tool_process.Command([plain], dict(os.environ), log=self.log)))

    def test_capture_returns_the_status_and_both_streams(self) -> None:
        done = self.runner.capture(tool_process.Command([*SH, "echo out; echo err >&2; exit 4"], dict(os.environ)))
        self.assertEqual((4, "out\n", "err\n"), (done.code, done.stdout, done.stderr))

    def test_a_command_past_its_bound_is_ended_with_its_group_124(self) -> None:
        pids = os.path.join(self.dir.name, "pids")
        hung = f"(echo $$ >> {pids}; exec sleep 86400) & echo $! >> {pids}; exec sleep 86400"
        self.assertEqual(124, self.runner.run(tool_process.Command([*SH, hung], dict(os.environ), log=self.log,
                                                                   timeout=0.5)))
        self.assertEqual(124, self.runner.capture(tool_process.Command([*SH, hung], dict(os.environ), timeout=0.5)).code)
        with open(pids) as f:
            for pid in [int(line) for line in f if line.strip()]:
                self.assertFalse(alive(pid), pid)


def alive(pid: int) -> bool:
    for _ in range(100):
        try:
            os.kill(pid, 0)
        except OSError:
            return False
        time.sleep(0.05)
    return True


if __name__ == "__main__":
    unittest.main()
