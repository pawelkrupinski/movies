#!/usr/bin/env python3
"""No network for the tests: run a command that may reach loopback and nothing else. Order independence's test steps run
through it once their dependencies are fetched (each leg's warm step, the one that may use the network, retrying at
the fetch layer only): a test that still reads the network then fails at once, naming the address, instead of
passing or failing by the day's network (a Maven 429 mid-run, run #53).

  scripts/ci/no-network -- <command...>    run it cut off
  scripts/ci/no-network --probe            the mode this runner allows, one word and why

Modes, the strongest the runner allows (NO_NETWORK_MODE forces one; a mode the runner cannot give is an error):
  netns    Linux: a network namespace holding only loopback, as the same user (unshare --user --map-current-user
           --net; on a github-hosted VM, AppArmor's unprivileged-userns restriction lifted first). Nothing leaves: a
           connect is "Network is unreachable".
  sandbox  macOS: sandbox-exec denying every outbound IP connection but localhost's (EPERM at connect).
  proxy    neither: every HTTP(S) client -- curl, python, every JVM through JAVA_TOOL_OPTIONS -- sent to a closed
           loopback port, loopback exempt. It does not stop a raw socket: a warning says so.
Tested by no_network_test.py beside it; the name callers use is scripts/ci/no-network, a thin exec of this."""
from __future__ import annotations

import os
import platform
import subprocess
import sys
from typing import List, NoReturn, Optional
import ci_machine  # noqa: E402

SANDBOX_PROFILE = ('(version 1)(allow default)(deny network-outbound (remote ip "*:*"))'
                   '(allow network-outbound (remote ip "localhost:*"))')
CLOSED = "127.0.0.1:9"  # discard: nothing listens there
# THE SAME USER INSIDE (Order independence #113, run 45030284135): mapped to root, the JVM's user.home is passwd's
# /root, not $HOME -- every sbt launcher failed "AccessDeniedException: /root/.sbt/..." on ci-runner's job image, whose
# job user is no root. --map-current-user keeps the uid (and so its home and caches); --keep-caps keeps the namespace's
# own capabilities across the exec, which bringing loopback up needs.
NETNS = ["unshare", "--user", "--map-current-user", "--keep-caps", "--net", "--"]
LOOPBACK_UP = "ip link set lo up 2> /dev/null || ifconfig lo up 2> /dev/null"
USAGE = "usage: no-network -- <command...> | --probe"
PROBES = {
    "netns": "netns (a network namespace with loopback only)",
    "sandbox": "sandbox (sandbox-exec: no outbound IP but localhost)",
    "proxy": "proxy (no network namespace here: HTTP(S) clients sent to a closed port; raw sockets not stopped)",
}


class ModeError(Exception):
    def __init__(self, message: str, code: int) -> None:
        super().__init__(message)
        self.code = code


def quiet(argv: List[str]) -> bool:
    try:
        return subprocess.run(argv, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0
    except OSError:
        return False


def netns_works() -> bool:
    return quiet([*NETNS, "sh", "-c", LOOPBACK_UP])


def netns_allowed() -> bool:
    if netns_works():
        return True
    # GitHub's Ubuntu VMs restrict unprivileged user namespaces (AppArmor); the VM is the job's alone, so lift it there
    if ci_machine.environment().get("RUNNER_ENVIRONMENT") != "github-hosted" or platform.system() != "Linux":
        return False
    if not quiet(["sudo", "-n", "sysctl", "-qw", "kernel.apparmor_restrict_unprivileged_userns=0"]):
        return False
    return netns_works()


def sandbox_works() -> bool:
    return platform.system() == "Darwin" and quiet(["sandbox-exec", "-p", SANDBOX_PROFILE, "true"])


def mode() -> str:
    forced = ci_machine.environment().get("NO_NETWORK_MODE", "")
    if forced == "netns" and not netns_allowed():
        raise ModeError("no-network: NO_NETWORK_MODE=netns, but this runner allows no network namespace", 1)
    if forced == "sandbox" and not sandbox_works():
        raise ModeError("no-network: NO_NETWORK_MODE=sandbox, but this is no macOS with sandbox-exec", 1)
    if forced in ("netns", "sandbox", "proxy"):
        return forced
    if forced:
        raise ModeError(f"no-network: unknown NO_NETWORK_MODE {forced}", 64)
    return "netns" if netns_allowed() else "sandbox" if sandbox_works() else "proxy"


def exec_proxied(command: List[str]) -> NoReturn:
    print("::warning title=no-network::this runner allows no network namespace: HTTP(S) clients go to a closed port, "
          "raw sockets are not stopped", file=sys.stderr, flush=True)
    for name in ("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY"):
        ci_machine.environment()[name] = f"http://{CLOSED}"
    ci_machine.environment()["no_proxy"] = ci_machine.environment()["NO_PROXY"] = "localhost,127.0.0.1,::1"
    java = ci_machine.environment().get("JAVA_TOOL_OPTIONS", "")
    ci_machine.environment()["JAVA_TOOL_OPTIONS"] = (f"{java} " if java else "") + (
        "-Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=9 -Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9 "
        "-Dhttp.nonProxyHosts=localhost|127.*|[::1]")
    exec_command(command)


def exec_command(argv: List[str]) -> NoReturn:
    try:
        os.execvp(argv[0], argv)
    except FileNotFoundError:
        print(f"no-network: {argv[0]}: command not found", file=sys.stderr)
        sys.exit(127)
    except OSError as error:
        print(f"no-network: {argv[0]}: {error.strerror}", file=sys.stderr)
        sys.exit(126)


def run(command: List[str]) -> NoReturn:
    chosen = mode()
    if chosen == "netns":
        exec_command([*NETNS, "sh", "-c", LOOPBACK_UP + ' || exit 70; exec "$@"', "no-network", *command])
    if chosen == "sandbox":
        exec_command(["sandbox-exec", "-p", SANDBOX_PROFILE, *command])
    exec_proxied(command)


def main(argv: Optional[List[str]] = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    try:
        if args[:1] == ["--probe"]:
            print(PROBES[mode()])
            return 0
        if args[:1] == ["--"]:
            if len(args) < 2:
                print("usage: no-network -- <command...>", file=sys.stderr)
                return 64
            run(args[1:])
        print(USAGE, file=sys.stderr)
        return 64
    except ModeError as error:
        print(error, file=sys.stderr)
        return error.code


if __name__ == "__main__":
    sys.exit(main())
