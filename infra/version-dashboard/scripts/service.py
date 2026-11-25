#!/usr/bin/env python3
"""One entry point for the version dashboard's launchd service (the shape of bitcashier's version-dashboard)

  install    copy both plists to ~/Library/LaunchAgents and load them
  uninstall  unload and remove them
  restart    lint + test this checkout, then SIGTERM the job: the server refuses new checks and
             switches, finishes running ones, exits, and KeepAlive starts the new code
  status     launchd state + /healthz (including any work in flight)
  logs       follow the server logs

LAUNCHCTL (default /bin/launchctl) names another launchctl, for a test's fake."""
from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
from typing import List, Sequence

DASHBOARD = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
JOBS = ("com.kinowo.nixos-dashboard", "com.kinowo.nixos-dashboard-watchdog")
SERVER = JOBS[0]
HEALTH = "http://127.0.0.1:8788/healthz"
STATE_LINES = re.compile(r"state =|pid =|last exit")
UNLOAD_WAITS = 10
USAGE = "usage: service.sh install|uninstall|restart|status|logs"


class Service:
    def __init__(self) -> None:
        self.launchctl = os.environ.get("LAUNCHCTL", "/bin/launchctl")
        self.domain = f"gui/{os.getuid()}"
        self.agents = os.path.join(os.path.expanduser("~"), "Library", "LaunchAgents")
        self.logs = os.path.join(os.path.expanduser("~"), ".kinowo-dashboard-logs")

    def launchd(self, *args: str, quiet: bool = False) -> "subprocess.CompletedProcess[str]":
        return subprocess.run([self.launchctl, *args], text=True, capture_output=quiet or None)

    def unload(self, job: str) -> None:
        self.launchd("bootout", f"{self.domain}/{job}", quiet=True)

    def loaded(self, job: str) -> bool:
        return self.launchd("print", f"{self.domain}/{job}", quiet=True).returncode == 0

    def install(self) -> int:
        os.makedirs(self.logs, exist_ok=True)
        for job in JOBS:
            shutil.copy(os.path.join(DASHBOARD, f"{job}.plist"), self.agents)
            self.unload(job)
            # bootout returns before the job is gone, and a bootstrap that lands first fails with
            # "5: Input/output error" -- wait (bounded) for it to be really unloaded.
            for _ in range(UNLOAD_WAITS):
                if not self.loaded(job):
                    break
                subprocess.run(["sleep", "1"])
            done = self.launchd("bootstrap", self.domain, os.path.join(self.agents, f"{job}.plist"))
            if done.returncode != 0:
                return done.returncode
        print(f"installed; {HEALTH} should answer within a few seconds")
        return 0

    def uninstall(self) -> int:
        for job in JOBS:
            self.unload(job)
            try:
                os.remove(os.path.join(self.agents, f"{job}.plist"))
            except FileNotFoundError:
                pass
        return 0

    def restart(self) -> int:
        for step in (["run", "--silent", "lint"], ["test", "--silent"]):
            done = subprocess.run(["npm", *step], cwd=DASHBOARD)
            if done.returncode != 0:
                return done.returncode
        health = subprocess.run(["curl", "-fsS", "--max-time", "3", HEALTH], text=True, capture_output=True)
        if health.returncode == 0 and '"running":[]' in health.stdout:
            print("nothing in flight; restarting")
        else:
            print(f"work in flight; the server finishes it before restarting (watch: {os.path.join(DASHBOARD, 'scripts', 'service.sh')} status)")
        return self.launchd("kill", "SIGTERM", f"{self.domain}/{SERVER}").returncode

    def status(self) -> int:
        listing = subprocess.run([self.launchctl, "print", f"{self.domain}/{SERVER}"], text=True, stdout=subprocess.PIPE)
        for line in listing.stdout.splitlines():
            if STATE_LINES.search(line):
                print(line)
        health = subprocess.run(["curl", "-fsS", "--max-time", "3", HEALTH], text=True, stdout=subprocess.PIPE)
        print(health.stdout if health.returncode == 0 else "healthz not answering", flush=True)
        return 0

    def follow_logs(self) -> int:
        os.execvp("tail", ["tail", "-F", os.path.join(self.logs, "server.out.log"),
                           os.path.join(self.logs, "server.err.log")])


def main(argv: Sequence[str]) -> int:
    service = Service()
    commands = {"install": service.install, "uninstall": service.uninstall, "restart": service.restart,
                "status": service.status, "logs": service.follow_logs}
    command = commands.get(argv[0]) if argv else None
    if command is None:
        print(USAGE, file=sys.stderr)
        return 2
    try:
        return command()
    except OSError as e:
        print(f"service.sh: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
