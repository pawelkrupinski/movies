#!/usr/bin/env python3
"""tool_process.py: a command's own exit status; its output into a log (appended when asked), or teed to stdout as
well; stdin closed when asked; 127 for a command not found and 126 for one not executable, said on stderr."""
from __future__ import annotations

import io
import os
import signal
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cli_harness  # noqa: E402
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

    def test_cwd(self) -> None:
        done = self.runner.capture(tool_process.Command(["pwd"], dict(os.environ), cwd=self.dir.name))
        self.assertEqual(os.path.realpath(self.dir.name), os.path.realpath(done.stdout.strip()))

    def test_capture_returns_the_status_and_both_streams(self) -> None:
        done = self.runner.capture(tool_process.Command([*SH, "echo out; echo err >&2; exit 4"], dict(os.environ)))
        self.assertEqual((4, "out\n", "err\n"), (done.code, done.stdout, done.stderr))

    def test_a_command_past_its_bound_is_ended_with_its_group_124(self) -> None:
        pids = os.path.join(self.dir.name, "pids")
        self.assertEqual(124, self.runner.run(tool_process.Command([*SH, hung(pids)], dict(os.environ), log=self.log,
                                                                   timeout=0.5)))
        self.assertEqual(124, self.runner.capture(tool_process.Command([*SH, hung(pids)], dict(os.environ),
                                                                       timeout=0.5)).code)
        self.assertEqual(0, cli_harness.still_running(pids, 5))

    def test_a_teed_command_past_its_bound_too(self) -> None:
        pids = os.path.join(self.dir.name, "pids")
        self.assertEqual(124, cli_harness.run_bounded(
            [sys.executable, "-c", RUN_TEED, os.path.dirname(os.path.abspath(__file__)), hung(pids), self.log],
            env=dict(os.environ), cwd=self.dir.name, stdin=None, bound=60).code)
        self.assertEqual(0, cli_harness.still_running(pids, 5))

    def test_a_bounded_command_ends_with_the_tool_that_ran_it(self) -> None:
        """Its own session is out of reach of whatever ends the tool -- a caller's bound, a cancelled check, Ctrl-C --
        so the INT or TERM the tool gets ends the command's group first, and is then the tool's own (`bounded`)."""
        for sig in (signal.SIGTERM, signal.SIGINT):
            with self.subTest(signal=sig.name):
                pids = os.path.join(self.dir.name, f"pids-{sig.name}")
                tool = subprocess.Popen([sys.executable, "-c", RUN_BOUNDED, os.path.dirname(os.path.abspath(__file__)),
                                         hung(pids), self.log], start_new_session=True,
                                        stderr=subprocess.DEVNULL)
                try:
                    self.assertTrue(cli_harness.wait_until(lambda: lines(pids) >= 2))
                    tool.send_signal(sig)
                    self.assertEqual(-sig, tool.wait(timeout=30))
                finally:
                    if tool.poll() is None:
                        tool.kill()
                self.assertEqual(0, cli_harness.still_running(pids, 5))


    def test_a_bounded_command_a_worker_thread_started_ends_with_the_tool_too(self) -> None:
        """The handlers are the main thread's, the groups the runner's: a TERM ends a worker's command as well, and so
        does the tool's exit while the worker still waits on it."""
        for how, script, code in (("TERM", RUN_IN_A_THREAD + "thread.join()\n", -signal.SIGTERM),
                                  ("exit", RUN_IN_A_THREAD + "wait_for_pids()\nsys.exit(3)\n", 3)):
            with self.subTest(how=how):
                pids = os.path.join(self.dir.name, f"pids-thread-{how}")
                tool = subprocess.Popen([sys.executable, "-c", script, os.path.dirname(os.path.abspath(__file__)),
                                         hung(pids), self.log, pids], start_new_session=True, stderr=subprocess.DEVNULL)
                try:
                    self.assertTrue(cli_harness.wait_until(lambda: lines(pids) >= 2))
                    if how == "TERM":
                        tool.send_signal(signal.SIGTERM)
                    self.assertEqual(code, tool.wait(timeout=30))
                finally:
                    if tool.poll() is None:
                        tool.kill()
                self.assertEqual(0, cli_harness.still_running(pids, 5))

def hung(pids: str) -> str:
    """A command that never ends by itself, and a child of it in its group: both pids written to `pids`."""
    return f"(echo $$ >> {pids}; exec sleep 86400) & echo $! >> {pids}; exec sleep 86400"


def lines(path: str) -> int:
    if not os.path.exists(path):
        return 0
    with open(path) as f:
        return len([line for line in f if line.strip()])


TOOL = """import os, sys
sys.path.insert(0, sys.argv[1])
import tool_process
"""
RUN_TEED = TOOL + """sys.exit(tool_process.SubprocessRunner().run(tool_process.Command(["/bin/sh", "-c", sys.argv[2]],
    dict(os.environ), log=sys.argv[3], tee=True, timeout=0.5)))"""
RUN_BOUNDED = TOOL + """import signal
signal.signal(signal.SIGINT, signal.default_int_handler)  # whatever the run around it ignores
tool_process.SubprocessRunner().run(tool_process.Command(["/bin/sh", "-c", sys.argv[2]],
    dict(os.environ), log=sys.argv[3], timeout=600))"""
RUN_IN_A_THREAD = TOOL + """import threading
import cli_harness
runner = tool_process.SubprocessRunner()
command = tool_process.Command(["/bin/sh", "-c", sys.argv[2]], dict(os.environ), log=sys.argv[3], timeout=600)
thread = threading.Thread(target=runner.run, args=(command,), daemon=True)
thread.start()
def wait_for_pids():
    cli_harness.wait_until(lambda: os.path.exists(sys.argv[4]) and len(open(sys.argv[4]).read().split()) >= 2)
"""

if __name__ == "__main__":
    unittest.main()
