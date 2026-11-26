#!/usr/bin/env python3
"""scripts/stall-watch.sh `watch`, black-box: the no-progress watchdog run-script-tests and the pre-push hook start beside
a step. A step that makes no progress (no CPU in its process tree, no runnable process, no progress mark touched, no file
written in its log directory) for the bound's seconds is called STALLED: its label appended to the stalled file, the line
on stderr, its descendants ended (TERM, KILL ten seconds later). It ends itself when the root does, and at once on TERM,
leaving no stall-mark.* behind and nothing on stderr (run-script-tests appends that stderr to the test's own log).
Ported from stall-watch-test.sh and extended to every flag and output line its callers read; the same cases hold the
shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import glob
import os
import signal
import subprocess
import sys
import threading
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/stall_watch.py", "scripts/stall-watch.sh")
FAST = {"KINOWO_STALL_POLL_SECONDS": "1"}
# a root that touches `$1` eight times, 0.75 s apart (six seconds of progress, a bound of three), says it finished,
# and then waits on nothing
TOUCHES = ('for i in 1 2 3 4 5 6 7 8; do %s; sleep 0.75; done; : > finished; exec sleep 300')


class StallWatchTest(cli_harness.ToolCase):
    TOOL = TOOL

    def root(self, *argv: str) -> "subprocess.Popen[bytes]":
        """A step the watchdog watches, in a session of its own, ended however the case ends."""
        proc = subprocess.Popen(list(argv), cwd=self.work, env=self.env(), start_new_session=True,
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.addCleanup(self.end_group, proc)
        return proc

    @staticmethod
    def end_group(proc: "subprocess.Popen[bytes]") -> None:
        try:
            os.killpg(proc.pid, signal.SIGKILL)
        except OSError:
            pass
        proc.wait()

    def sh(self, script: str) -> "subprocess.Popen[bytes]":
        return self.root("sh", "-c", script)

    def watch(self, *args: str, env: Optional[dict] = None) -> "subprocess.Popen[bytes]":
        """The watchdog running beside the case; its stderr in `err`."""
        err = open(self.path("err"), "wb")
        self.addCleanup(err.close)
        proc = subprocess.Popen([self.TOOL, "watch", *args], cwd=self.work, env=self.env({**FAST, "TMPDIR": self.work, **(env or {})}),
                                start_new_session=True, stdout=subprocess.DEVNULL, stderr=err)
        self.addCleanup(self.end_group, proc)
        return proc

    def stalled(self) -> bool:
        return os.path.exists(self.path("stalled")) and os.path.getsize(self.path("stalled")) > 0

    def await_stall(self, proc: "subprocess.Popen[bytes]") -> None:
        self.assertTrue(cli_harness.wait_until(self.stalled), self.read("err"))
        proc.wait(timeout=60)

    def marks(self) -> List[str]:
        return glob.glob(self.path("stall-mark.*"))

    def alive(self, pid_file: str) -> bool:
        """Whether the process the file names runs: one ended but not yet waited for by its parent (a zombie) is gone."""
        state = subprocess.run(["ps", "-o", "stat=", "-p", self.read(pid_file).strip()], capture_output=True, text=True)
        return bool(state.stdout.strip()) and not state.stdout.strip().startswith("Z")

    def test_it_calls_a_step_with_no_progress_stalled_and_ends_its_descendants(self) -> None:
        root = self.sh("sleep 300 & echo $! > child; wait; : > freed; exec sleep 300")
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("child"))))
        proc = self.watch(str(root.pid), "2", "the step x", self.path("stalled"))
        self.await_stall(proc)
        self.assertEqual(0, proc.returncode)
        self.assertEqual("the step x\n", self.read("stalled"))
        self.assertRegex(self.read("err"), r"^STALLED: the step x \(no progress for \d+s: no CPU in its processes, "
                                           r"no output, no progress mark\)\n$")
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("freed"))), "the child was not ended")
        self.assertIsNone(root.poll(), "the root itself is never ended")
        self.assertEqual([], self.marks())

    def test_a_root_without_descendants_is_called_stalled_too(self) -> None:
        root = self.root("sleep", "300")
        self.await_stall(self.watch(str(root.pid), "2", "lonely", self.path("stalled")))
        self.assertEqual("lonely\n", self.read("stalled"))
        self.assertIsNone(root.poll())

    def test_a_progress_mark_touched_keeps_the_step_alive(self) -> None:
        root = self.sh(TOUCHES % ": > progress")
        proc = self.watch(str(root.pid), "3", "marked", self.path("stalled"), self.path("progress"))
        self.await_stall(proc)
        self.assertTrue(os.path.exists(self.path("finished")), "stalled while its progress mark was still touched")

    def test_a_file_written_in_the_log_directory_keeps_the_step_alive(self) -> None:
        os.makedirs(self.path("logs"))
        root = self.sh(TOUCHES % "echo x >> logs/step.log")
        proc = self.watch(str(root.pid), "3", "logged", self.path("stalled"), self.path("progress"), self.path("logs"))
        self.await_stall(proc)
        self.assertTrue(os.path.exists(self.path("finished")), "stalled while its log was still written")

    def test_a_file_in_a_subdirectory_of_the_log_directory_is_no_progress(self) -> None:
        os.makedirs(self.path("logs", "sub"))
        root = self.sh(TOUCHES % "echo x >> logs/sub/step.log")
        proc = self.watch(str(root.pid), "3", "nested", self.path("stalled"), self.path("progress"), self.path("logs"))
        self.await_stall(proc)
        self.assertFalse(os.path.exists(self.path("finished")), "a nested file was counted as progress")

    def test_a_busy_process_is_progress(self) -> None:
        busy = "import time; end = time.time() + 6\nwhile time.time() < end: pass"
        root = self.root(sys.executable, "-c", f"import signal, subprocess, sys\n"
                         f"subprocess.run([sys.executable, '-c', {busy!r}])\n"
                         f"open('finished', 'w').close(); signal.pause()")
        self.await_stall(self.watch(str(root.pid), "3", "busy", self.path("stalled")))
        self.assertTrue(os.path.exists(self.path("finished")), "stalled while a process was computing")

    def test_it_ends_itself_when_the_root_does(self) -> None:
        root = self.sh("exit 0")
        root.wait()
        proc = self.watch(str(root.pid), "60", "gone", self.path("stalled"))
        proc.wait(timeout=60)
        self.assertEqual(0, proc.returncode)
        self.assertEqual("", self.read("err"))
        self.assertFalse(self.stalled())
        self.assertEqual([], self.marks())

    def test_a_term_ends_it_at_once_and_removes_its_mark(self) -> None:
        root = self.root("sleep", "300")
        proc = self.watch(str(root.pid), "600", "x", self.path("stalled"), env={"KINOWO_STALL_POLL_SECONDS": "30"})
        self.assertTrue(cli_harness.wait_until(lambda: bool(self.marks())), "no stall mark was made")
        proc.send_signal(signal.SIGTERM)
        proc.wait(timeout=30)
        self.assertEqual(0, proc.returncode)
        self.assertEqual("", self.read("err"))
        self.assertEqual([], self.marks())
        self.assertFalse(self.stalled())

    def test_a_watchdog_ended_as_it_starts_says_nothing_and_leaves_no_mark(self) -> None:
        # run-script-tests ends a quick test's watchdog the moment it starts, 0-60 ms in: no stderr, no stall-mark.*
        root = self.root("sleep", "300")
        errors = []
        for i in range(40):
            proc = subprocess.Popen([self.TOOL, "watch", str(root.pid), "60", "x", self.path("stalled")],
                                    cwd=self.work, env=self.env({"TMPDIR": self.work}), stdin=subprocess.DEVNULL,
                                    stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
            threading.Event().wait(i * 0.0015)
            proc.terminate()
            errors.append(proc.communicate()[1].decode())
        self.assertEqual([""] * 40, errors)
        self.assertEqual([], self.marks())
        self.assertFalse(self.stalled())

    def test_a_group_of_its_own_is_reported_and_ended_but_the_roots_own_group_is_not(self) -> None:
        child = ("import signal, subprocess\n"
                 "open('other', 'w').write(str(subprocess.Popen(['sleep', '300'], start_new_session=True).pid))\n"
                 "open('own', 'w').write(str(subprocess.Popen(['sleep', '300']).pid))\n"
                 "signal.pause()")
        root = self.root(sys.executable, "-c", child)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("own"))
                                               and os.path.getsize(self.path("own")) > 0))
        proc = self.watch(str(root.pid), "2", "grouped", self.path("stalled"), env={"KINOWO_STALL_GROUPS": "1"})
        self.await_stall(proc)
        self.assertIn("stall report: process group", self.read("err"))
        self.assertIn("STALLED: grouped (no progress for", self.read("err"))
        self.assertTrue(cli_harness.wait_until(lambda: not self.alive("other")), "its group's process was not ended")
        self.assertTrue(self.alive("own"), "the root's own group was ended")

    def test_without_the_groups_flag_every_descendant_is_ended(self) -> None:
        child = ("import signal, subprocess\n"
                 "open('own', 'w').write(str(subprocess.Popen(['sleep', '300']).pid))\n"
                 "signal.pause()")
        root = self.root(sys.executable, "-c", child)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("own"))
                                               and os.path.getsize(self.path("own")) > 0))
        self.await_stall(self.watch(str(root.pid), "2", "plain", self.path("stalled")))
        self.assertTrue(cli_harness.wait_until(lambda: not self.alive("own")), "the descendant was not ended")
        self.assertNotIn("stall report", self.read("err"))

    def test_a_descendant_that_ignores_term_is_killed_ten_seconds_later(self) -> None:
        stubborn = 'trap "" TERM; echo $$ > stubborn; while :; do sleep 1; done'
        root = self.sh(f"sh -c '{stubborn}' & wait; exec sleep 300")
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("stubborn"))
                                               and os.path.getsize(self.path("stubborn")) > 0))
        proc = self.watch(str(root.pid), "2", "stubborn", self.path("stalled"))
        self.await_stall(proc)
        self.assertEqual(0, proc.returncode)
        self.assertFalse(self.alive("stubborn"), "the watchdog ended before the descendant was killed")
        self.assertIsNone(root.poll(), "the root is never ended")

    def test_a_missing_argument_is_a_usage_error(self) -> None:
        done = self.run_tool("watch", "1", "2", "label")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("usage: stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] "
                      "[<dir>]", done.stderr)

    def test_no_subcommand_prints_the_header_as_help_and_fails(self) -> None:
        for args in ([], ["nonsense"]):
            done = self.run_tool(*args)
            self.assertEqual(2, done.code, done.output)
            self.assertIn("A NO-PROGRESS bound: a step is ended once it has made no progress for N seconds",
                          done.stdout)
            self.assertIn("scripts/stall-watch.sh watch <root pid> <seconds> <label> <stalled file>", done.stdout)


if __name__ == "__main__":
    unittest.main()
