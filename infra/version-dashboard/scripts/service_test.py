#!/usr/bin/env python3
"""infra/version-dashboard/scripts/service.sh, black-box: the launchd entry point of the version dashboard, over a fake
launchctl (LAUNCHCTL), npm, curl, sleep and tail first on the PATH and a scratch HOME. install copies both plists and
loads them (waiting, bounded, for the bootout to finish), uninstall unloads and removes them, restart lints and tests
before it signals the job, status shows launchd's state and /healthz, logs follow the two log files. The same cases hold
the shell tool and its Python rewrite (the copy is the tool plus whatever it execs, service.py once there is one)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", "scripts", "lib"))
import cli_harness  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
JOBS = ["com.kinowo.nixos-dashboard", "com.kinowo.nixos-dashboard-watchdog"]
HEALTH = "http://127.0.0.1:8788/healthz"
LAUNCHCTL = """
verb = argv[0]
busy = os.path.join(env["FAKE_STATE"], "busy")
if verb == "print" and env.get("LAUNCHCTL_PRINT"):
    out(env["LAUNCHCTL_PRINT"])
elif verb == "print":
    left = int(open(busy).read()) if os.path.exists(busy) else 0
    if left <= 0:
        exit(113)
    open(busy, "w").write(str(left - 1))
elif verb == "bootout":
    open(busy, "w").write(env.get("BOOTOUT_BUSY", "0"))
"""
CURL = """
if env.get("CURL_FAIL"):
    exit(22)
out(env.get("CURL_BODY", '{"running":[]}'), end="")
"""
NPM = """
if env.get("NPM_FAIL") == " ".join(argv):
    exit(1)
"""


class ServiceTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        dash = self.path("dash")
        os.makedirs(os.path.join(dash, "scripts"))
        for name in ("service.sh", "service.py"):
            source = os.path.join(HERE, name)
            if os.path.exists(source):
                shutil.copy2(source, os.path.join(dash, "scripts"))
        for job in JOBS:
            self.write(f"dash/{job}.plist", f"plist of {job}\n")
        os.makedirs(self.path("home", "Library", "LaunchAgents"))  # macOS always has it
        os.makedirs(self.path("state"))
        self.TOOL = os.path.join(dash, "scripts", "service.sh")
        self.fakes.program("launchctl", LAUNCHCTL)
        self.fakes.program("curl", CURL)
        self.fakes.program("npm", NPM)
        self.fakes.script("sleep", "exit 0")
        self.fakes.script("tail", "exit 0")
        self.domain = f"gui/{os.getuid()}"

    def service(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        merged: Dict[str, Optional[str]] = {"HOME": self.path("home"), "FAKE_STATE": self.path("state"),
                                            "LAUNCHCTL": self.fakes.bin + "/launchctl"}
        merged.update(env or {})
        return self.run_tool(*args, env=merged)

    def launchctl(self) -> List[str]:
        return [" ".join(argv) for argv in self.fakes.calls("launchctl")]

    def test_install_copies_both_plists_and_loads_each_after_its_bootout_is_done(self) -> None:
        done = self.service("install", env={"BOOTOUT_BUSY": "2"})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("installed; http://127.0.0.1:8788/healthz should answer within a few seconds", done.stdout)
        for job in JOBS:
            self.assertEqual(f"plist of {job}\n", self.read(f"home/Library/LaunchAgents/{job}.plist"))
        self.assertTrue(os.path.isdir(self.path("home", ".kinowo-dashboard-logs")))
        target = f"{self.domain}/{JOBS[0]}"
        # bootout, then print until it fails (two busy answers, the third fails), then bootstrap
        self.assertEqual([f"bootout {target}", f"print {target}", f"print {target}", f"print {target}"],
                         self.launchctl()[:4])
        self.assertEqual([f"bootstrap {self.domain} {self.path('home', 'Library', 'LaunchAgents', JOBS[0] + '.plist')}"],
                         [c for c in self.launchctl() if c.endswith(JOBS[0] + ".plist")])
        self.assertEqual(2, len([c for c in self.launchctl() if c.startswith("bootstrap")]))

    def test_install_gives_up_waiting_after_ten_prints_and_bootstraps_anyway(self) -> None:
        done = self.service("install", env={"BOOTOUT_BUSY": "99"})
        self.assertEqual(0, done.code, done.output)
        first = [c for c in self.launchctl() if c == f"print {self.domain}/{JOBS[0]}"]
        self.assertEqual(10, len(first))

    def test_uninstall_unloads_both_and_removes_their_plists(self) -> None:
        self.service("install")
        done = self.service("uninstall")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], os.listdir(self.path("home", "Library", "LaunchAgents")))
        self.assertEqual([f"bootout {self.domain}/{job}" for job in JOBS], self.launchctl()[-2:])

    def test_restart_lints_and_tests_in_the_dashboard_then_signals_the_job(self) -> None:
        done = self.service("restart")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("nothing in flight; restarting", done.stdout)
        self.assertEqual([["run", "--silent", "lint"], ["test", "--silent"]], self.fakes.calls("npm"))
        self.assertEqual([HEALTH], [a[-1] for a in self.fakes.calls("curl")])
        self.assertEqual([f"kill SIGTERM {self.domain}/{JOBS[0]}"], self.launchctl())

    def test_restart_with_work_in_flight_says_so_and_still_signals(self) -> None:
        done = self.service("restart", env={"CURL_BODY": '{"running":["nixos-check"]}'})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("work in flight; the server finishes it before restarting", done.stdout)
        self.assertEqual([f"kill SIGTERM {self.domain}/{JOBS[0]}"], self.launchctl())

    def test_restart_is_refused_by_a_failing_lint_or_test_and_signals_nothing(self) -> None:
        for failing in ("run --silent lint", "test --silent"):
            with self.subTest(failing=failing):
                self.fakes.clear()
                done = self.service("restart", env={"NPM_FAIL": failing})
                self.assertNotEqual(0, done.code)
                self.assertEqual([], self.launchctl())

    def test_status_shows_launchds_state_lines_and_healthz(self) -> None:
        listing = "com.kinowo.nixos-dashboard = {\n\tstate = running\n\tpid = 4242\n\tprogram = /x\n\tlast exit code = 0\n}"
        done = self.service("status", env={"LAUNCHCTL_PRINT": listing, "CURL_BODY": '{"ok":true,"running":[]}'})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["state = running", "pid = 4242", "last exit code = 0", '{"ok":true,"running":[]}'],
                         [line.strip() for line in done.stdout.splitlines()])

    def test_status_says_when_healthz_does_not_answer(self) -> None:
        done = self.service("status", env={"LAUNCHCTL_PRINT": "state = waiting", "CURL_FAIL": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("healthz not answering", done.stdout)

    def test_logs_follow_both_server_logs(self) -> None:
        self.assertEqual(0, self.service("logs").code)
        logs = self.path("home", ".kinowo-dashboard-logs")
        self.assertEqual([["-F", f"{logs}/server.out.log", f"{logs}/server.err.log"]], self.fakes.calls("tail"))

    def test_no_command_or_an_unknown_one_is_exit_2_with_the_usage(self) -> None:
        for args in ([], ["nope"]):
            with self.subTest(args=args):
                done = self.service(*args)
                self.assertEqual(2, done.code)
                self.assertIn("install|uninstall|restart|status|logs", done.stderr)


if __name__ == "__main__":
    unittest.main()
