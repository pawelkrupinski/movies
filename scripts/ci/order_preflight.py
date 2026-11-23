#!/usr/bin/env python3
"""RUNNER PREFLIGHT for an Order independence leg: before a minute of tests, check that this runner can run them -- the
toolchain versions the leg pins, the simulator or emulator it drives, and the tool flavours its scripts assume (GNU
sed/date/timeout on Linux; on macOS the BSD ones the scripts were written to cope with). A runner that cannot is
INFRASTRUCTURE, never a test red: each failure is a `::error title=Runner preflight::UNKNOWN: ...` line, which
agentctl's classifier reads as infra (its UNKNOWN row; the step's own name, `Runner preflight ...`, says so too), and
the leg stops there with nothing of the tests run. ~12 of the ~66 failing jobs of runs #1-#100 were the environment (a
missing simulator, a Maven 429, macOS's openrsync and BSD tools), each read as a code red.

  scripts/ci/order-preflight.sh <leg>      scala | android | android-instrumented | device | swift | ios

`device` is the booted emulator, checked from inside the emulator step (adb answers, boot completed). Every check runs
(a runner missing two things names both); exit 1 when any failed. Also writes `preflight=ok|failed` to $GITHUB_OUTPUT
and the network-cutting mode the runner allows (scripts/ci/no-network --probe) as a notice.

Stdlib only. order-independence.yml runs it by the name scripts/ci/order-preflight.sh; tested by order_preflight_test.py
beside it."""
from __future__ import annotations

import os
import platform
import re
import shutil
import subprocess
import sys
from typing import Callable, Dict, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
LEGS = "scala|android|android-instrumented|device|swift|ios"
# the version line wherever it is: ci-runner's job image sets JAVA_TOOL_OPTIONS, so the JVM's first line is its "Picked
# up JAVA_TOOL_OPTIONS: ..." banner (and JDK_JAVA_OPTIONS' "NOTE: Picked up ..." likewise), never the version (Order
# independence #111: "java is unreadable" on every self-hosted leg)
JAVA_VERSION = re.compile(r'[^ ]+ version "([0-9]+)[."+]')
NETWORKED = ("scala", "android", "android-instrumented")


def captured(argv: Sequence[str], merged: bool = False) -> str:
    """`argv`'s stdout (with its stderr when `merged`, else stderr dropped), as `$( )` gives it; "" if it cannot run."""
    try:
        stderr = subprocess.STDOUT if merged else subprocess.DEVNULL
        done = subprocess.run(list(argv), stdout=subprocess.PIPE, stderr=stderr, text=True)
    except OSError:
        return ""
    return done.stdout.rstrip("\n")


def succeeds(argv: Sequence[str]) -> bool:
    try:
        return subprocess.run(list(argv), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0
    except OSError:
        return False


def first_line(text: str) -> str:
    return text.split("\n", 1)[0]


class Preflight:
    def __init__(self, leg: str, env: Mapping[str, str]) -> None:
        self.leg = leg
        self.env = env
        self.bad = False

    def fail(self, why: str) -> None:
        print(f"::error title=Runner preflight::UNKNOWN: runner preflight ({self.leg}): {why} -- the runner, "
              "not a test red", flush=True)
        self.bad = True

    def ok(self, what: str) -> None:
        print(f"preflight ({self.leg}): {what}", flush=True)

    def have(self, tool: str) -> bool:
        if shutil.which(tool):
            return True
        self.fail(f"`{tool}` is not on PATH")
        return False

    def java_is(self, major: str) -> None:
        if not self.have("java"):
            return
        lines = captured(["java", "-Djava.awt.headless=true", "-version"], merged=True).split("\n")
        found = next((m.group(1) for m in map(JAVA_VERSION.match, lines) if m), "")
        if found == major:
            self.ok(f"java {found}")
        else:
            self.fail(f"java is {found or 'unreadable'}, the leg pins {major}")

    def gnu(self, tool: str) -> None:
        """GNU's <tool>, as Linux CI and the scripts expect."""
        if not self.have(tool):
            return
        version = first_line(captured([tool, "--version"]))
        if "GNU" in version or "coreutils" in version:
            self.ok(f"{tool} is GNU's")
        else:
            self.fail(f"`{tool}` is not GNU's ({shutil.which(tool)}): a BSD tool on a Linux leg")

    def linux_tools(self) -> None:
        for tool in ("sed", "date", "timeout"):
            self.gnu(tool)
        if self.have("git"):
            self.ok("git")

    def macos_tools(self) -> None:
        system = platform.system()
        if system != "Darwin":
            self.fail(f"a macOS leg on {system}")
            return
        if not self.have("xcodebuild"):
            return
        xcode = first_line(captured(["xcodebuild", "-version"]))
        if xcode:
            self.ok(xcode)
        else:
            self.fail("xcodebuild -version says nothing (no Xcode selected?)")
        if self.have("swift"):
            self.ok(first_line(captured(["swift", "--version"])))
        if succeeds(["xcrun", "simctl", "list", "devices", "available"]):
            self.ok("simctl lists the simulators")
        else:
            self.fail("`xcrun simctl list devices` failed: CoreSimulator is unusable")

    def android_sdk(self) -> None:
        sdk = self.env.get("ANDROID_SDK_ROOT") or self.env.get("ANDROID_HOME") or ""
        if sdk and os.access(os.path.join(sdk, "platform-tools", "adb"), os.X_OK):
            self.ok(f"adb under {sdk}")
        else:
            self.fail(f"no adb under the SDK ({sdk or 'ANDROID_HOME unset'})")
        if not os.access(os.path.join(HERE, "..", "..", "android", "gradlew"), os.X_OK):
            self.fail("android/gradlew is not executable")

    def scala(self) -> None:
        self.linux_tools()
        self.java_is(self.env.get("ORDER_PREFLIGHT_JAVA") or "27")
        if self.have("sbt"):
            self.ok("sbt")

    def android(self) -> None:
        self.linux_tools()
        self.java_is("17")
        self.android_sdk()

    def android_instrumented(self) -> None:
        self.android()
        if os.access("/dev/kvm", os.R_OK) and os.access("/dev/kvm", os.W_OK):
            self.ok("/dev/kvm")
        else:
            self.fail("/dev/kvm is not read-writable: no hardware acceleration for the emulator")

    def device(self) -> Optional[int]:
        if not self.have("adb"):
            return 1
        state = captured(["adb", "get-state"])
        if state != "device":
            self.fail(f"adb sees no device (state: {state or 'none'})")
        booted = captured(["adb", "shell", "getprop", "sys.boot_completed"]).replace("\r", "")
        if booted != "1":
            self.fail(f"the emulator has not finished booting (sys.boot_completed={booted or 'unset'})")
        if not self.bad:
            self.ok("the emulator is booted")
        return None

    def ios(self) -> None:
        self.macos_tools()
        destination = captured(["bash", os.path.join(HERE, "ios-sim-destination.sh")])
        if re.search(r"id=.", destination):
            self.ok(f"simulator {destination}")
        else:
            self.fail(f"no iPhone simulator on this image (ios-sim-destination.sh: {destination or 'nothing'})")

    def verdict(self) -> int:
        if self.leg in NETWORKED:
            probe = captured([os.path.join(HERE, "no-network"), "--probe"], merged=True)
            print(f"::notice title=Runner preflight::network for the tests: {probe}", flush=True)
        with open(self.env.get("GITHUB_OUTPUT") or os.devnull, "a") as output:
            output.write(f"preflight={'failed' if self.bad else 'ok'}\n")
        if not self.bad:
            return 0
        with open(self.env.get("GITHUB_STEP_SUMMARY") or os.devnull, "a") as summary:
            summary.write(f"### Runner preflight ({self.leg}) FAILED: the runner, not the tests -- nothing of the leg "
                          "ran\n")
        return 1


CHECKS: Dict[str, Callable[[Preflight], Optional[int]]] = {
    "scala": Preflight.scala, "android": Preflight.android, "android-instrumented": Preflight.android_instrumented,
    "device": Preflight.device, "swift": Preflight.macos_tools, "ios": Preflight.ios}


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if not argv or not argv[0]:
        print(f"order-preflight.sh: 1: usage: order-preflight.sh {LEGS}", file=sys.stderr)
        return 1
    check = CHECKS.get(argv[0])
    if check is None:
        print(f"order-preflight.sh: unknown leg {argv[0]}", file=sys.stderr)
        return 64
    preflight = Preflight(argv[0], env)
    stopped = check(preflight)
    return stopped if stopped is not None else preflight.verdict()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
