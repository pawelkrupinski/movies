#!/usr/bin/env python3
"""DevPanel actions: one entry point per panel button.

    devpanel.py <action>        e.g. devpanel.py deploy-ios

Stdlib only, and 3.9-compatible: the panel runs it with the system
/usr/bin/python3. Every side effect (running, capturing or exec'ing a command,
sleeping) goes through `Host`, so test_devpanel.py drives the real logic
against scripted device replies instead of real phones.
"""
from __future__ import annotations

import json
import os
import plistlib
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Mapping, Optional, Sequence

SCRIPTS_DIR = Path(__file__).resolve().parent
WEB_PORT = 9000
FIXTURE_WORKER_MAIN = "modules.LocalFixtureWorkerMain"


class Host:
    """The real machine: the only place commands are actually run."""

    def run(self, cmd: Sequence[str], cwd: Optional[Path] = None) -> int:
        """Run with output streamed straight to our stdout; return the exit code."""
        sys.stdout.flush()
        return subprocess.call(list(cmd), cwd=cwd)

    def capture(self, cmd: Sequence[str]) -> tuple[int, str]:
        """Run with stdout+stderr merged and captured; (127, "") if not found."""
        try:
            p = subprocess.run(list(cmd), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        except OSError:
            return 127, ""
        return p.returncode, p.stdout.decode("utf-8", "replace")

    def exec(self, cwd: Path, cmd: Sequence[str]) -> None:
        """Replace this process, so a long-running sbt/gradle owns the console."""
        sys.stdout.flush()
        os.chdir(cwd)
        os.execvp(cmd[0], list(cmd))

    def sleep(self, seconds: float) -> None:
        time.sleep(seconds)


@dataclass
class Context:
    host: Host
    env: Mapping[str, str]
    repo_root: Path


def say(text: str = "") -> None:
    print(text, flush=True)


def warn(text: str) -> None:
    print(text, file=sys.stderr, flush=True)


def with_dev_tool_path(env: Mapping[str, str]) -> dict[str, str]:
    """A GUI-launched .app inherits a minimal PATH (no Homebrew, no Android SDK),
    so sbt / adb / gradle aren't found the way they are in a terminal. Append
    the usual dev-tool locations that exist and aren't already on PATH."""
    extra = ["/opt/homebrew/bin", "/usr/local/bin",
             str(Path.home() / "Library/Android/sdk/platform-tools")]
    extra += [f"{env[k]}/platform-tools" for k in ("ANDROID_HOME", "ANDROID_SDK_ROOT") if env.get(k)]
    parts = env.get("PATH", "").split(":")
    parts += [p for p in extra if Path(p).is_dir() and p not in parts]
    return {**env, "PATH": ":".join(p for p in parts if p)}


def repo_root(env: Mapping[str, str]) -> Path:
    """A worktree the panel passes via DEVPANEL_REPO_ROOT (the long-press "run
    on worktree" menu), else the checkout this file lives in."""
    return Path(env["DEVPANEL_REPO_ROOT"]) if env.get("DEVPANEL_REPO_ROOT") else SCRIPTS_DIR.parents[2]


def step(ctx: Context, cmd: Sequence[str]) -> int:
    """Announce and run one command of a multi-step action."""
    say(f"\n\033[1m▶ {' '.join(cmd)}\033[0m")
    return ctx.host.run(cmd)


def dispatch(ctx: Context, workdir: Path, label: str, cmd: Sequence[str]) -> None:
    """Announce, then hand the process over to the action's final command."""
    say(f"\033[1m▶ {label}\033[0m")
    say(f"  dir: {workdir}\n  cmd: {' '.join(cmd)}\n")
    ctx.host.exec(workdir, cmd)


# ── processes ────────────────────────────────────────────────────────────────

def _pids(output: str) -> list[int]:
    return [int(t) for t in output.split() if t.isdigit()]


def _terminate(ctx: Context, find_pids: Callable[[], list[int]]) -> None:
    """SIGTERM, wait up to ~2s for the pids to go, then SIGKILL what's left."""
    pids = find_pids()
    for sig in (signal.SIGTERM, signal.SIGKILL):
        for pid in pids:
            try:
                os.kill(pid, sig)
            except (ProcessLookupError, PermissionError):
                pass
        if sig == signal.SIGKILL:
            return
        for _ in range(6):
            ctx.host.sleep(0.3)
            pids = find_pids()
            if not pids:
                return


def free_port(ctx: Context, port: int) -> None:
    """Kill whatever LISTENs on <port> so a fresh `sbt web/run` can bind it
    (Play refuses to start if :9000 is already taken)."""
    def listeners() -> list[int]:
        return _pids(ctx.host.capture(["lsof", "-ti", f"tcp:{port}", "-sTCP:LISTEN"])[1])
    pids = listeners()
    if pids:
        say(f"  freeing :{port} (killing {' '.join(map(str, pids))})")
        _terminate(ctx, listeners)


def kill_pattern(ctx: Context, pattern: str, label: str) -> None:
    """Kill every process whose FULL command line matches <pattern>, except
    this process and its parent. The companion to free_port for processes that
    bind no port: a stale `sbt localStack` worker is a JVM forked by sbt's
    bgRunMain, so no pid file ever reaches us; its main class on the command
    line is the one stable handle."""
    own = {os.getpid(), os.getppid()}

    def matches() -> list[int]:
        return [p for p in _pids(ctx.host.capture(["pgrep", "-f", pattern])[1]) if p not in own]
    pids = matches()
    if pids:
        say(f"  killing stale {label} ({' '.join(map(str, pids))})")
        _terminate(ctx, matches)


def kill_stale_worker(ctx: Context) -> None:
    """Reap a previous run's fixture worker so it can't keep projecting
    fixtures into the local Mongo under a fresh stack."""
    kill_pattern(ctx, FIXTURE_WORKER_MAIN, "local fixture worker")


def reset_local_stack(ctx: Context) -> None:
    free_port(ctx, WEB_PORT)
    kill_stale_worker(ctx)


# ── Android ──────────────────────────────────────────────────────────────────

def resolve_adb(ctx: Context) -> Optional[str]:
    """A usable adb: $DEVPANEL_ADB, adb on PATH, the SDK named by
    android/local.properties' sdk.dir, then the common SDK locations."""
    if ctx.env.get("DEVPANEL_ADB"):
        candidates = [ctx.env["DEVPANEL_ADB"]]
    else:
        on_path = shutil.which("adb", path=ctx.env.get("PATH"))
        if on_path:
            return on_path
        props = ctx.repo_root / "android/local.properties"
        sdk_dirs = re.findall(r"^sdk\.dir=(.*)$", props.read_text(), re.M) if props.is_file() else []
        sdk_dirs += [ctx.env.get("ANDROID_HOME", ""), ctx.env.get("ANDROID_SDK_ROOT", ""),
                     str(Path.home() / "Library/Android/sdk")]
        candidates = [f"{d}/platform-tools/adb" for d in sdk_dirs if d]
    return next((c for c in candidates if os.path.isfile(c) and os.access(c, os.X_OK)), None)


def adb_devices(ctx: Context, adb: str) -> list[tuple[str, str]]:
    """(serial, state) per attached device, from `adb devices`."""
    out = ctx.host.capture([adb, "devices"])[1]
    return [(f[0], f[1]) for f in (line.split() for line in out.splitlines()[1:]) if len(f) >= 2]


def android_serial(ctx: Context, adb: str) -> Optional[str]:
    """The device to target: $DEVPANEL_ANDROID_SERIAL, else the single ready
    device, else the first of several (with a note on stderr).

    A WiFi phone is listed TWICE — as `host:port` and as its mDNS service name
    (`adb-<serial>-<suffix>._adb-tls-connect._tcp`). One phone, two transports:
    prefer the addressable form so it doesn't look like two devices, and keep
    the alias only when it is the sole transport (mDNS auto-connect)."""
    if ctx.env.get("DEVPANEL_ANDROID_SERIAL"):
        return ctx.env["DEVPANEL_ANDROID_SERIAL"]
    ready = [s for s, state in adb_devices(ctx, adb) if state == "device"]
    ready = [s for s in ready if not s.endswith("._tcp")] or ready
    if len(ready) > 1:
        warn(f"  multiple devices attached: {' '.join(ready)} — using the first; "
             "set DEVPANEL_ANDROID_SERIAL to pick")
    return ready[0] if ready else None


def android_device_state(ctx: Context, adb: str, serial: Optional[str]) -> Optional[str]:
    """adb's state for the device: "device" (ready), "unauthorized" (USB
    debugging prompt not accepted), "offline" (reconnecting), or None when not
    attached. Without a serial, the first attached device."""
    for s, state in adb_devices(ctx, adb):
        if serial is None or s == serial:
            return state
    return None


def keyguard_showing(dumpsys_window: str) -> bool:
    """Phones expose one flag or the other (an S24 reports only
    mKeyguardShowing). Neither present means "assume unlocked" rather than
    blocking forever — e.g. a phone set to never lock."""
    for flag in ("mDreamingLockscreen", "mKeyguardShowing"):
        m = re.search(flag + r"=(true|false)", dumpsys_window)
        if m:
            return m.group(1) == "true"
    return False


ANDROID_STATE_HINTS = {
    "unauthorized": "🔒 Android device is unauthorized — accept the “Allow USB debugging” prompt on the device…",
    "offline": "⏳ Android device is offline — reconnecting…",
    None: "🔌 waiting for an Android device to be attached…",
}


def wait_for_android_unlock(ctx: Context, serial: Optional[str]) -> None:
    """Block until the (optionally pinned) device is authorized AND its
    keyguard is dismissed. `adb wait-for-device` blocks SILENTLY forever on an
    unauthorized or offline device, so poll the state and say what to do."""
    adb = resolve_adb(ctx)
    if adb is None:
        say("  (adb not found — skipping unlock wait; set DEVPANEL_ADB or ANDROID_HOME)")
        return
    announced: object = "nothing yet"
    while True:
        state = android_device_state(ctx, adb, serial)
        if state == "device":
            if announced != "nothing yet":
                say("  device ready.")
            break
        hint_key = state if state in ANDROID_STATE_HINTS else None
        if hint_key != announced:
            say(ANDROID_STATE_HINTS[hint_key])
            announced = hint_key
        ctx.host.sleep(2)

    # Pin ONE transport: a bare `adb shell` refuses to pick when a WiFi phone is
    # attached twice (host:port + mDNS alias) and prints "more than one device".
    serial = serial or android_serial(ctx, adb)
    shell = [adb] + (["-s", serial] if serial else []) + ["shell", "dumpsys", "window"]
    announced_lock = False
    while keyguard_showing(ctx.host.capture(shell)[1]):
        if not announced_lock:
            say("🔒 waiting for Android unlock…")
            announced_lock = True
        ctx.host.sleep(2)
    if announced_lock:
        say("  unlocked.")


# ── iOS ──────────────────────────────────────────────────────────────────────

def devicectl_json(ctx: Context, args: Sequence[str]) -> Optional[dict]:
    """`xcrun devicectl <args>`'s --json-output document, or None on failure."""
    fd, path = tempfile.mkstemp(suffix=".json")
    os.close(fd)
    try:
        rc, _ = ctx.host.capture(["xcrun", "devicectl", *args, "--json-output", path, "-q"])
        if rc != 0:
            return None
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError):
        return None
    finally:
        os.unlink(path)


def ios_pick_device(devices_doc: dict) -> Optional[str]:
    """UDID of the iPhone/iPad to deploy to: the one on a cable (transportType
    "wired"), else one on Wi-Fi ("localNetwork"), else None. Keyed on the live
    transport, never list order — every phone ever paired stays listed, and an
    unplugged one listed first used to be picked and then waited on forever.
    Simulators are "sameMachine"; the cabled phone may carry no `reality` key,
    so transport is the only reliable filter."""
    devices = devices_doc.get("result", {}).get("devices", [])
    for transport in ("wired", "localNetwork"):
        for d in devices:
            hw = d.get("hardwareProperties", {})
            if (d.get("connectionProperties", {}).get("transportType") == transport
                    and hw.get("deviceType") in ("iPhone", "iPad")):
                return hw["udid"]
    return None


def ios_unlocked_enough(lock_state_doc: dict) -> bool:
    """devicectl can write to the device: no passcode, or unlocked at least
    once since boot (data partition available). That is the one gate that
    blocks `devicectl install`; a screen lock after first unlock does not."""
    result = lock_state_doc.get("result", {})
    return not result.get("passcodeRequired", False) or result.get("unlockedSinceBoot", False)


# Verbatim from a real failed `devicectl device process launch` on a locked
# iPhone: "…because the device was not, or could not be, unlocked",
# "BSErrorCodeDescription = Locked", "Reason: Locked".
IOS_LOCK_ERROR = re.compile(
    r"could not be,? unlocked|was not,? or could not be|BSErrorCodeDescription = Locked"
    r"|reason: locked|device is locked|unlock the device", re.I)


def wait_for_ios_unlock(ctx: Context, udid: str) -> None:
    announced = False
    while True:
        doc = devicectl_json(ctx, ["device", "info", "lockState", "--device", udid])
        if doc is not None and ios_unlocked_enough(doc):
            if announced:
                say("  unlocked.")
            return
        if not announced:
            say("🔒 waiting for iPhone to be unlocked…")
            announced = True
        ctx.host.sleep(2)


def ios_run_unlocked(ctx: Context, cmd: Sequence[str]) -> int:
    """Run a devicectl command, retrying while it fails with the device-locked
    error. `install` succeeds on a screen-locked phone but `launch` doesn't, so
    this is what makes the app come up the moment you unlock. Any other failure
    returns its exit code at once."""
    announced = False
    while True:
        say(f"\n\033[1m▶ {' '.join(cmd)}\033[0m")
        rc, out = ctx.host.capture(cmd)
        say(out.rstrip("\n"))
        if rc == 0:
            if announced:
                say("  unlocked.")
            return 0
        if not IOS_LOCK_ERROR.search(out):
            return rc
        if not announced:
            say("🔒 iPhone is locked — the app will launch as soon as you unlock it…")
            announced = True
        ctx.host.sleep(2)


# ── actions ──────────────────────────────────────────────────────────────────

def deploy_android(ctx: Context) -> int:
    """Build the signed releaseFast APK and install+launch it on the attached
    device, USB or WiFi (see android/app/build.gradle.kts:runOnDevice). Waits
    for the device FIRST, then picks the serial: resolving before the wait
    reads adb while the phone may not be attached yet."""
    serial = ctx.env.get("DEVPANEL_ANDROID_SERIAL") or None
    wait_for_android_unlock(ctx, serial)
    adb = resolve_adb(ctx)
    serial = android_serial(ctx, adb) if adb else serial
    cmd = ["./gradlew", "runOnDevice"] + ([f"-Pserial={serial}"] if serial else [])
    dispatch(ctx, ctx.repo_root / "android", "Deploy to Android (USB or WiFi)", cmd)
    return 0


def deploy_ios(ctx: Context) -> int:
    """Build the Kinowo scheme for the cabled iPhone/iPad, install it, launch
    it. Automatic signing may provision (-allowProvisioningUpdates); the device
    must be paired/trusted and a team set on the Kinowo target once."""
    udid = ios_pick_device(devicectl_json(ctx, ["list", "devices"]) or {})
    if udid is None:
        warn("✗ No cabled iOS device found.\n  Plug in an iPhone/iPad, unlock it, and tap 'Trust'.")
        warn(ctx.host.capture(["xcrun", "devicectl", "list", "devices"])[1])
        return 1
    say(f"device: {udid}")
    wait_for_ios_unlock(ctx, udid)

    derived = ctx.repo_root / "ios/build/devpanel"
    rc = step(ctx, ["xcodebuild", "-project", str(ctx.repo_root / "ios/Kinowo.xcodeproj"),
                    "-scheme", "Kinowo", "-configuration", "Debug", "-destination", f"id={udid}",
                    "-derivedDataPath", str(derived), "-allowProvisioningUpdates", "build"])
    if rc != 0:
        return rc

    # Read the bundle id from the fresh build rather than hard-coding it, so a
    # team-namespace change to it doesn't break the launch step.
    apps = sorted((derived / "Build/Products/Debug-iphoneos").glob("*.app"))
    if not apps:
        warn(f"✗ build produced no .app under {derived / 'Build/Products/Debug-iphoneos'}")
        return 1
    with open(apps[0] / "Info.plist", "rb") as f:
        bundle_id = plistlib.load(f)["CFBundleIdentifier"]

    for cmd in (["xcrun", "devicectl", "device", "install", "app", "--device", udid, str(apps[0])],
                ["xcrun", "devicectl", "device", "process", "launch", "--device", udid, bundle_id]):
        rc = ios_run_unlocked(ctx, cmd)
        if rc != 0:
            return rc
    return 0


def run_web(ctx: Context) -> int:
    """The Play app on :9000 via scripts/dev-server.sh, not a bare `sbt
    web/run`: the wrapper narrows the heap so an OOM trips early and leaves
    target/oom-<pid>.hprof behind (the 2026-09-04 dev server died with nothing
    but one OutOfMemoryError line)."""
    reset_local_stack(ctx)
    dispatch(ctx, ctx.repo_root, "Local web server (:9000)", ["./scripts/dev-server.sh"])
    return 0


def run_local_stack(ctx: Context) -> int:
    """Web + a fixture-replaying worker (sbt localStack; see build.sbt)."""
    reset_local_stack(ctx)
    dispatch(ctx, ctx.repo_root, "Local web + worker (fixtures)", ["sbt", "localStack"])
    return 0


def kill_stack(ctx: Context) -> int:
    """The panel has already SIGTERM'd its runner's process group before this
    runs; this is the belt-and-braces reap that guarantees nothing keeps :9000
    and no orphaned worker keeps projecting into the local Mongo."""
    say(f"▶ free :{WEB_PORT}")
    free_port(ctx, WEB_PORT)
    say("▶ reap stale fixture worker")
    kill_stale_worker(ctx)
    return 0


def reset_local_corpus(ctx: Context) -> int:
    """Drop the kinowo_local corpus so a local worker re-scrapes from scratch.
    --yes because the panel console has no TTY for the confirm word."""
    dispatch(ctx, ctx.repo_root, "Reset local corpus (kinowo_local)",
             ["scripts/reset-corpus.sh", "--local", "--yes"])
    return 0


def reap_worker(ctx: Context) -> int:
    """Run by the web console's Stop button: the worker JVM lives outside the
    process group Stop signals."""
    kill_stale_worker(ctx)
    return 0


ACTIONS: dict[str, Callable[[Context], int]] = {
    "deploy-android": deploy_android,
    "deploy-ios": deploy_ios,
    "run-web": run_web,
    "run-local-stack": run_local_stack,
    "kill-stack": kill_stack,
    "reset-local-corpus": reset_local_corpus,
    "reap-worker": reap_worker,
}


def main(argv: Sequence[str]) -> int:
    if len(argv) != 1 or argv[0] not in ACTIONS:
        warn(f"usage: devpanel.py {{{','.join(ACTIONS)}}}")
        return 2
    os.environ.update(with_dev_tool_path(os.environ))
    ctx = Context(host=Host(), env=os.environ, repo_root=repo_root(os.environ))
    return ACTIONS[argv[0]](ctx)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
