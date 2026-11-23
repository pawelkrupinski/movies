#!/usr/bin/env python3
"""Run the local `/debug` mirror sync as a macOS launchd user agent, so it starts at login and restarts on failure.

No terminal to babysit. The agent runs `mirror.sh`, which self-manages the whole stack: it brings up its own ssh tunnel
to prod Mongo, (re)ensures the native brew-managed mirror Mongo, seeds/re-seeds, and tails prod's change stream. The
Mongo is itself a `brew services` agent (see start-local-mongo.sh), so it restarts at login too; this agent manages the
rest.

Usage (service.sh is the name every caller and the plist run):
  scripts/local-mirror/service.sh install     # install + start, runs at login
  scripts/local-mirror/service.sh uninstall   # stop + remove the agent
  scripts/local-mirror/service.sh status      # show state + pid
  scripts/local-mirror/service.sh logs        # tail the agent log
  scripts/local-mirror/service.sh agent       # what launchd runs (see below)

THE SECRET. mirror.sh needs MONGODB_URI, which lives only in the Touch ID vault (`secrets`, namespace `movies`) --
there is no .env.local any more. A LaunchAgent has no terminal, and mirror.sh deliberately EXITS to be restarted (a
database supervisor died, its own code changed); if launchd restarted mirror.sh directly, every restart would re-exec it
under `secrets run` and raise a fresh Touch ID dialog, at login and overnight alike. So launchd runs `service.sh
agent`, which reads MONGODB_URI from the vault ONCE (one Touch ID when the agent starts -- at login, or on `install`)
and then restarts mirror.sh itself, the variable inherited. The value is held only in that process's environment:
never written into the plist, the log, or any file.

Prereqs (one-time): key-based ssh to the prod Mongo host working NON-INTERACTIVELY -- `ssh -o BatchMode=yes
root@<host> true` must succeed, because launchd has no terminal to answer a passphrase prompt with (see prod-tunnel.sh).

A pause is a `sleep` child (the self-test's own `sleep` first on the PATH stands in for it). Exit: 0, or 1 on an
unknown command (usage) and when the vault said no; a failing `launchctl bootstrap` / `enable` fails install with its
status."""
from __future__ import annotations

import os
import re
import shutil
import signal
import subprocess
import sys
import time
from types import FrameType
from typing import Callable, Dict, List, NoReturn, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.normpath(os.path.join(HERE, "..", ".."))
NAME = os.path.join(HERE, "service.sh")
LABEL = "pl.kinowo.local-mirror"
DOMAIN = f"gui/{os.getuid()}"
SERVICE = f"{DOMAIN}/{LABEL}"
PLIST = os.path.join(os.path.expanduser("~"), "Library", "LaunchAgents", f"{LABEL}.plist")
# launchd starts agents with a bare PATH that wouldn't find these: their directories go into the plist
TOOLS = ("bash", "ssh", "brew", "mongosh", "node", "nc", "secrets")
STATUS_LINE = re.compile(r"^\s+(state|pid) =")
PLIST_TEXT = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>{label}</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/bash</string>
    <string>{name}</string>
    <string>agent</string>
  </array>
  <key>WorkingDirectory</key><string>{root}</string>
  <key>EnvironmentVariables</key>
  <dict><key>PATH</key><string>{path}</string></dict>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ThrottleInterval</key><integer>10</integer>
  <key>StandardOutPath</key><string>{log}</string>
  <key>StandardErrorPath</key><string>{log}</string>
</dict>
</plist>
"""


def say(text: str) -> None:
    print(f"[service] {text}", flush=True)


def warn(text: str) -> None:
    print(f"[service] {text}", file=sys.stderr, flush=True)


def mirror_log() -> str:
    """log-path.sh's MIRROR_LOG: the one file the agent logs to and mirror.sh rotates."""
    done = subprocess.run(["bash", "-c", '. "$1"; printf %s "$MIRROR_LOG"', "log-path",
                           os.path.join(HERE, "log-path.sh")], stdout=subprocess.PIPE, text=True, check=True)
    return done.stdout


def launchctl(*args: str, quiet: bool = False) -> int:
    return subprocess.run(["launchctl", *args], stderr=subprocess.DEVNULL if quiet else None).returncode


def agent_path() -> str:
    """Where the tools resolve in this process (which has the user's full PATH), their directories de-duplicated, then
    /usr/bin:/bin."""
    dirs: List[str] = []
    for tool in TOOLS:
        found = shutil.which(tool)
        if not found:
            warn(f"WARN: '{tool}' not found in PATH — the agent may fail to find it")
        elif os.path.abspath(os.path.dirname(found)) not in dirs:
            dirs.append(os.path.abspath(os.path.dirname(found)))
    return ":".join([*dirs, "/usr/bin", "/bin"])


def install() -> int:
    path, log = agent_path(), mirror_log()
    os.makedirs(os.path.dirname(PLIST), exist_ok=True)
    os.makedirs(os.path.dirname(log), exist_ok=True)
    with open(PLIST, "w") as f:
        f.write(PLIST_TEXT.format(label=LABEL, name=NAME, root=ROOT, path=path, log=log))
    # Reload cleanly: boot out any prior copy (ignore "not loaded"), then in.
    launchctl("bootout", SERVICE, quiet=True)
    code = launchctl("bootstrap", DOMAIN, PLIST) or launchctl("enable", SERVICE)
    if code:
        return code
    say(f"installed + started: {LABEL}")
    say(f"plist: {PLIST}")
    say(f"logs:  {log}   (tail with: {NAME} logs)")
    return 0


def uninstall() -> int:
    launchctl("bootout", SERVICE, quiet=True)
    if os.path.exists(PLIST):
        os.remove(PLIST)
    say(f"uninstalled: {LABEL}")
    say("the native mirror Mongo is left running; stop it with:")
    print("          brew services stop mongodb-community@7.0", flush=True)
    return 0


def status() -> int:
    done = subprocess.run(["launchctl", "print", SERVICE], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                          text=True)
    lines = [line for line in done.stdout.splitlines() if STATUS_LINE.match(line)]
    print("\n".join(lines) if lines else "[service] not loaded", flush=True)
    return 0


def logs() -> NoReturn:
    os.execvp("tail", ["tail", "-n", "40", "-f", mirror_log()])


def pause(seconds: int) -> None:
    subprocess.run(["sleep", str(seconds)])


def under_the_vault() -> int:
    """MONGODB_URI unset: this agent again under the vault, once. Not exec: when the vault says no (Touch ID declined, or
    timed out with nobody at the Mac) launchd's KeepAlive would otherwise re-prompt every ThrottleInterval. Wait a
    while instead, then let launchd try again."""
    try:
        code = subprocess.run(["secrets", "run", "movies", "MONGODB_URI", "--", sys.executable,
                               os.path.abspath(__file__), "agent"]).returncode
    except OSError:
        code = 127
    if code == 0:
        return 0
    warn("could not read MONGODB_URI from the Touch ID vault — asking again in 10 minutes")
    pause(600)
    return 1


class MirrorLoop:
    """mirror.sh, restarted 10 s after each exit; a TERM or INT ends the running one and the agent (exit 0)."""

    def __init__(self) -> None:
        self.child: Optional["subprocess.Popen[bytes]"] = None

    def end(self, _signum: int, _frame: Optional[FrameType]) -> NoReturn:
        if self.child is not None and self.child.poll() is None:
            self.child.terminate()
        raise SystemExit(0)

    def run(self) -> NoReturn:
        signal.signal(signal.SIGTERM, self.end)
        signal.signal(signal.SIGINT, self.end)
        while True:
            self.child = subprocess.Popen([os.path.join(HERE, "mirror.sh")])
            self.child.wait()
            self.child = None
            print(f"{time.strftime('%Y-%m-%dT%H:%M:%S%z')} [service] mirror.sh exited — restarting it in 10s",
                  flush=True)
            pause(10)


def agent() -> int:
    if not os.environ.get("MONGODB_URI"):
        return under_the_vault()
    MirrorLoop().run()


COMMANDS: Dict[str, Callable[[], int]] = {"install": install, "uninstall": uninstall, "status": status, "logs": logs,
                                          "agent": agent}


def main(argv: List[str]) -> int:
    command = COMMANDS.get(argv[0] if argv else "")
    if command is None:
        print(f"usage: {NAME} {{install|uninstall|status|logs|agent}}", file=sys.stderr)
        return 1
    return command()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
