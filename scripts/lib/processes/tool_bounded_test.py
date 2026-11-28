#!/usr/bin/env python3
"""tool_bounded.bounded, run in a process of its own (it ends itself with the signal it was interrupted by): a Ctrl-C
landing at any moment, the command's spawn included, ends the command's whole group and the caller sees the interrupt.
The shell-level cases (a bound, a status, a grandchild, a Ctrl-C mid-run) are identity-tools-test.sh's."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from harness import cli_harness  # noqa: E402

LIB = os.path.dirname(os.path.abspath(__file__))
# bounded over `sleep 30`, its spawn wrapped so the Ctrl-C lands once the command runs, before spawn_group returns
# (Tool self-tests #665: on a loaded runner the command ran and the signal came while Popen was still returning)
INTERRUPTED_IN_SPAWN = """
import os, signal, sys
sys.path.insert(0, sys.argv[1])
from processes import machine, tool_bounded
real = machine.spawn_group
def spawn_then_interrupted(*args, **kwargs):
    proc = real(*args, **kwargs)
    open(sys.argv[2], "w").write(str(proc.pid))
    os.kill(os.getpid(), signal.SIGINT)
    return proc
machine.spawn_group = spawn_then_interrupted
sys.exit(tool_bounded.bounded(30, ["sleep", "30"], dict(os.environ)))
"""


class BoundedInterruptTest(cli_harness.ToolCase):
    TOOL = os.path.join(LIB, "tool_bounded.py")

    def alive(self, pid: int) -> bool:
        state = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)], capture_output=True, text=True)
        return bool(state.stdout.strip()) and not state.stdout.strip().startswith("Z")

    def test_a_ctrl_c_during_the_spawn_ends_the_command_and_the_caller_sees_it(self) -> None:
        pid_file = self.path("command")
        done = subprocess.run([sys.executable, "-c", INTERRUPTED_IN_SPAWN, os.path.dirname(LIB), pid_file], capture_output=True,
                              text=True, timeout=60)
        pid = int(self.read("command"))
        self.addCleanup(lambda: self.alive(pid) and os.kill(pid, signal.SIGKILL))
        self.assertEqual(-signal.SIGINT, done.returncode, done.stderr)
        self.assertTrue(cli_harness.wait_until(lambda: not self.alive(pid), bound=5), "the command outlived bounded")

    def test_the_command_runs_with_the_callers_signals_unblocked(self) -> None:
        done = subprocess.run([sys.executable, self.TOOL, "10", "sh", "-c", "kill -INT $$; sleep 5; echo survived"],
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(128 + signal.SIGINT, done.returncode, done.stdout + done.stderr)
        self.assertNotIn("survived", done.stdout)


if __name__ == "__main__":
    unittest.main()
