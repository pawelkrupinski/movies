"""DevPanel commit gate:  python3 -m unittest tools/devpanel/test_devpanel.py
(or bash tools/devpanel/test.sh).

The action logic runs against `ScriptedHost`, which records every command and
answers `capture` from a scripted reply function — the device/adb/devicectl
side of the seam. free_port / kill_pattern run against real processes, and the
Swift panel is compiled and self-tested for real.
"""
from __future__ import annotations

import contextlib
import copy
import io
import json
import os
import plistlib
import re
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from typing import Callable, Optional, Sequence

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE / "scripts"))
import devpanel  # noqa: E402

REPO = HERE.parents[1]
FIXTURE = json.loads((HERE / "fixtures/devicectl-list-devices.json").read_text())
IPHONE_15, IPHONE_17, IPAD = ("00008130-001C08413A60001C", "00008150-001945100208401C",
                              "00008142-000220921E6B401C")
# resolve_adb only accepts an executable file; the scripted host never runs it.
STAND_IN_ADB = "/bin/sh"

Reply = Callable[[Sequence[str]], "tuple[int, str]"]


class Exec(Exception):
    """Raised by ScriptedHost.exec: a real exec never returns either."""


class ScriptedHost(devpanel.Host):
    def __init__(self, reply: Optional[Reply] = None):
        self.reply = reply or (lambda cmd: (0, ""))
        self.ran: list[list[str]] = []
        self.captured: list[list[str]] = []
        self.execed: Optional[tuple[Path, list[str]]] = None
        self.sleeps = 0

    def run(self, cmd, cwd=None):
        self.ran.append(list(cmd))
        return 0

    def capture(self, cmd):
        self.captured.append(list(cmd))
        return self.reply(cmd)

    def exec(self, cwd, cmd):
        self.execed = (cwd, list(cmd))
        raise Exec()

    def sleep(self, seconds):
        self.sleeps += 1
        if self.sleeps > 50:
            raise AssertionError("still polling after 50 sleeps — would hang forever")


def context(host: ScriptedHost, root: Path = Path("/repo"), **env: str) -> devpanel.Context:
    return devpanel.Context(host=host, env={"PATH": "", **env}, repo_root=root)


def run_action(action, ctx) -> tuple[Optional[int], str]:
    """(exit code, or None when it exec'd; everything printed)."""
    out = io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
        try:
            rc = action(ctx)
        except Exec:
            rc = None
    return rc, out.getvalue()


def devicectl(devices: Optional[dict] = None, lock_states: Sequence[Optional[dict]] = (),
              other: Optional[Reply] = None) -> Reply:
    """Replies like devicectl: `list devices` writes `devices`, each `lockState`
    call writes the next of `lock_states` (None = the call fails, as it does for
    an unreachable phone), and the last state repeats."""
    states = list(lock_states)

    def reply(cmd):
        if cmd[:2] == ["xcrun", "devicectl"] and "--json-output" in cmd:
            doc = devices if cmd[2:4] == ["list", "devices"] else (
                states.pop(0) if len(states) > 1 else states[0])
            if doc is None:
                return 1, "ERROR: The device is not able to fulfill the requested usage assertion"
            Path(cmd[cmd.index("--json-output") + 1]).write_text(json.dumps(doc))
            return 0, ""
        return other(cmd) if other else (0, "")
    return reply


def with_transports(**by_udid: Optional[str]) -> dict:
    """The real capture with some devices' transportType overridden (None drops it)."""
    doc = copy.deepcopy(FIXTURE)
    for d in doc["result"]["devices"]:
        udid = d["hardwareProperties"]["udid"]
        for name, value in by_udid.items():
            if udid == globals()[name]:
                d["connectionProperties"].pop("transportType", None)
                if value:
                    d["connectionProperties"]["transportType"] = value
    return doc


UNLOCKED = {"result": {"passcodeRequired": True, "unlockedSinceBoot": True}}
LOCKED_SINCE_BOOT = {"result": {"passcodeRequired": True, "unlockedSinceBoot": False}}
# Verbatim from a real `devicectl device process launch` against a locked iPhone.
LAUNCH_LOCKED = ('The request was denied by service delegate (SBMainWorkspace) for reason: Locked '
                 '("Unable to launch dev.kinowo.Kinowo because the device was not, or could not be, '
                 'unlocked"). BSErrorCodeDescription = Locked')


class IosDevicePickTest(unittest.TestCase):
    """Trimmed from a real `devicectl list devices`: an unreachable iPhone 15
    listed FIRST, the cabled iPhone 17 (no `reality` key at all), an iPad on
    Wi-Fi, simulators. The old xctrace resolver picked the dead iPhone 15."""

    def test_picks_the_cabled_iphone_not_the_first_listed(self):
        self.assertEqual(devpanel.ios_pick_device(FIXTURE), IPHONE_17)

    def test_swapping_iphones_follows_the_cable(self):
        doc = with_transports(IPHONE_15="wired", IPHONE_17=None)
        self.assertEqual(devpanel.ios_pick_device(doc), IPHONE_15)

    def test_no_cable_falls_back_to_wifi(self):
        self.assertEqual(devpanel.ios_pick_device(with_transports(IPHONE_17=None)), IPAD)

    def test_nothing_reachable_picks_nothing_and_never_a_simulator(self):
        self.assertIsNone(devpanel.ios_pick_device(with_transports(IPHONE_17=None, IPAD=None)))


class IosLockTest(unittest.TestCase):
    def test_unlocked_enough(self):
        self.assertTrue(devpanel.ios_unlocked_enough(UNLOCKED))
        self.assertTrue(devpanel.ios_unlocked_enough(
            {"result": {"passcodeRequired": False, "unlockedSinceBoot": False}}))
        self.assertFalse(devpanel.ios_unlocked_enough(LOCKED_SINCE_BOOT))

    def test_lock_error_classifier(self):
        self.assertTrue(devpanel.IOS_LOCK_ERROR.search(LAUNCH_LOCKED))
        self.assertFalse(devpanel.IOS_LOCK_ERROR.search(
            'error: Signing for "Kinowo" requires a development team.'))

    def test_wait_announces_then_proceeds_once_unlocked(self):
        host = ScriptedHost(devicectl(lock_states=[LOCKED_SINCE_BOOT, None, UNLOCKED]))
        _, out = run_action(lambda c: devpanel.wait_for_ios_unlock(c, IPHONE_17), context(host))
        self.assertEqual(out.count("waiting for iPhone to be unlocked"), 1)
        self.assertIn("unlocked.", out)
        self.assertEqual(host.sleeps, 2)

    def test_wait_is_silent_when_already_unlocked(self):
        host = ScriptedHost(devicectl(lock_states=[UNLOCKED]))
        _, out = run_action(lambda c: devpanel.wait_for_ios_unlock(c, IPHONE_17), context(host))
        self.assertEqual((out, host.sleeps), ("", 0))

    def test_locked_launch_is_retried_until_unlock(self):
        replies = [(1, LAUNCH_LOCKED), (0, "Launched application")]
        host = ScriptedHost(lambda cmd: replies.pop(0))
        rc, out = run_action(lambda c: devpanel.ios_run_unlocked(c, ["launch"]), context(host))
        self.assertEqual(rc, 0)
        self.assertIn("the app will launch as soon as you unlock it", out)
        self.assertEqual(len(host.captured), 2)

    def test_non_lock_failure_propagates_its_exit_code(self):
        host = ScriptedHost(lambda cmd: (70, 'error: Signing for "Kinowo" requires a development team.'))
        rc, _ = run_action(lambda c: devpanel.ios_run_unlocked(c, ["launch"]), context(host))
        self.assertEqual((rc, len(host.captured)), (70, 1))


class DeployIosTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root)

    def built_app(self, bundle_id="dev.kinowo.Kinowo") -> Path:
        app = self.root / "ios/build/devpanel/Build/Products/Debug-iphoneos/Kinowo.app"
        app.mkdir(parents=True)
        with open(app / "Info.plist", "wb") as f:
            plistlib.dump({"CFBundleIdentifier": bundle_id}, f, fmt=plistlib.FMT_BINARY)
        return app

    def test_builds_installs_and_launches_on_the_cabled_device(self):
        app = self.built_app("team.namespaced.Kinowo")
        host = ScriptedHost(devicectl(FIXTURE, [UNLOCKED]))
        rc, out = run_action(devpanel.deploy_ios, context(host, self.root))
        self.assertEqual(rc, 0, out)
        self.assertIn(f"device: {IPHONE_17}", out)
        [build] = host.ran
        self.assertEqual(build[0], "xcodebuild")
        self.assertIn(f"id={IPHONE_17}", build)
        self.assertEqual(build[build.index("-scheme") + 1], "Kinowo")
        self.assertEqual(build[build.index("-project") + 1], str(self.root / "ios/Kinowo.xcodeproj"))
        install, launch = host.captured[-2:]
        self.assertEqual(install, ["xcrun", "devicectl", "device", "install", "app",
                                   "--device", IPHONE_17, str(app)])
        self.assertEqual(launch, ["xcrun", "devicectl", "device", "process", "launch",
                                  "--device", IPHONE_17, "team.namespaced.Kinowo"])

    def test_no_reachable_device_fails_fast_instead_of_waiting(self):
        host = ScriptedHost(devicectl(with_transports(IPHONE_17=None, IPAD=None)))
        rc, out = run_action(devpanel.deploy_ios, context(host, self.root))
        self.assertEqual((rc, host.ran, host.sleeps), (1, [], 0))
        self.assertIn("No cabled iOS device found", out)

    def test_waits_for_first_unlock_before_building(self):
        self.built_app()
        host = ScriptedHost(devicectl(FIXTURE, [LOCKED_SINCE_BOOT, UNLOCKED]))
        rc, out = run_action(devpanel.deploy_ios, context(host, self.root))
        self.assertEqual(rc, 0)
        self.assertLess(out.index("waiting for iPhone to be unlocked"), out.index("xcodebuild"))

    def test_build_without_an_app_fails(self):
        host = ScriptedHost(devicectl(FIXTURE, [UNLOCKED]))
        rc, out = run_action(devpanel.deploy_ios, context(host, self.root))
        self.assertEqual(rc, 1)
        self.assertIn("build produced no .app", out)


def adb(devices: str, dumpsys: Sequence[str] = ("mKeyguardShowing=false",),
        bare_shell_error: bool = False) -> Reply:
    """Replies like adb: `devices` lists `devices` (one "serial state" per
    line; a list means successive calls), `shell dumpsys` answers the next of
    `dumpsys` (last repeats)."""
    listings = [devices] if isinstance(devices, str) else list(devices)
    screens = list(dumpsys)

    def reply(cmd):
        args = cmd[1:]
        if args == ["devices"]:
            listing = listings.pop(0) if len(listings) > 1 else listings[0]
            return 0, "List of devices attached\n" + "".join(
                f"{line.split()[0]}\t{line.split(None, 1)[1]}\n" for line in listing.splitlines() if line)
        if args[:1] == ["-s"]:
            args = args[2:]
        elif bare_shell_error and args[:1] == ["shell"]:
            return 1, "adb: more than one device/emulator"
        if args[:2] == ["shell", "dumpsys"]:
            return 0, screens.pop(0) if len(screens) > 1 else screens[0]
        return 1, ""
    return reply


WIFI_PHONE = "192.168.2.137:33045 device\nadb-RFCX10WHEPX-6hSyIx._adb-tls-connect._tcp device"


class AndroidTest(unittest.TestCase):
    def ctx(self, reply: Reply, **env) -> devpanel.Context:
        return context(ScriptedHost(reply), DEVPANEL_ADB=STAND_IN_ADB, **env)

    def serial(self, devices: str, **env) -> tuple[Optional[str], str]:
        return run_action(lambda c: devpanel.android_serial(c, STAND_IN_ADB), self.ctx(adb(devices), **env))

    def test_serial_single_device(self):
        self.assertEqual(self.serial("ONLY device")[0], "ONLY")

    def test_serial_first_of_several_with_a_warning(self):
        serial, out = self.serial("AAA device\nBBB device")
        self.assertEqual(serial, "AAA")
        self.assertIn("multiple devices", out)

    def test_serial_wifi_alias_is_not_a_second_device(self):
        serial, out = self.serial(WIFI_PHONE)
        self.assertEqual(serial, "192.168.2.137:33045")
        self.assertNotIn("multiple devices", out)

    def test_serial_falls_back_to_the_mdns_alias_when_it_is_the_only_transport(self):
        self.assertEqual(self.serial("adb-XYZ._adb-tls-connect._tcp device")[0],
                         "adb-XYZ._adb-tls-connect._tcp")

    def test_serial_honours_the_override(self):
        self.assertEqual(self.serial("AAA device", DEVPANEL_ANDROID_SERIAL="ZZZ")[0], "ZZZ")

    def test_serial_ignores_devices_that_are_not_ready(self):
        self.assertEqual(self.serial("AAA unauthorized usb:1-1\nBBB device")[0], "BBB")

    def test_device_state_reads_unauthorized(self):
        ctx = self.ctx(adb("SER unauthorized usb:1-1"))
        self.assertEqual(devpanel.android_device_state(ctx, STAND_IN_ADB, None), "unauthorized")
        self.assertIsNone(devpanel.android_device_state(ctx, STAND_IN_ADB, "OTHER"))

    def test_keyguard_flags(self):
        self.assertTrue(devpanel.keyguard_showing("mDreamingLockscreen=true mKeyguardShowing=false"))
        self.assertFalse(devpanel.keyguard_showing("mKeyguardShowing=false"))
        self.assertTrue(devpanel.keyguard_showing("mKeyguardShowing=true"))
        self.assertFalse(devpanel.keyguard_showing("no recognised flag"))

    def test_missing_adb_skips_the_wait(self):
        ctx = context(ScriptedHost(), DEVPANEL_ADB="/no/such/adb")
        _, out = run_action(lambda c: devpanel.wait_for_android_unlock(c, None), ctx)
        self.assertIn("skipping unlock wait", out)

    def test_unauthorized_device_is_surfaced_then_proceeds(self):
        ctx = self.ctx(adb(["SER unauthorized usb:1-1", "SER device"]))
        _, out = run_action(lambda c: devpanel.wait_for_android_unlock(c, None), ctx)
        self.assertIn("Allow USB debugging", out)
        self.assertIn("device ready.", out)

    def test_waits_for_the_keyguard(self):
        ctx = self.ctx(adb("SER device", ["mKeyguardShowing=true", "mKeyguardShowing=false"]))
        _, out = run_action(lambda c: devpanel.wait_for_android_unlock(c, None), ctx)
        self.assertEqual(out.count("waiting for Android unlock"), 1)
        self.assertIn("unlocked.", out)

    def test_wifi_phone_pins_one_transport_for_the_keyguard_check(self):
        # Real adb refuses a bare `adb shell` with two transports attached.
        host = ScriptedHost(adb(WIFI_PHONE, bare_shell_error=True))
        run_action(lambda c: devpanel.wait_for_android_unlock(c, None),
                   context(host, DEVPANEL_ADB=STAND_IN_ADB))
        self.assertIn([STAND_IN_ADB, "-s", "192.168.2.137:33045", "shell", "dumpsys", "window"],
                      host.captured)

    def test_deploy_passes_the_resolved_serial_to_gradle(self):
        host = ScriptedHost(adb("SER device"))
        run_action(devpanel.deploy_android, context(host, Path("/wt"), DEVPANEL_ADB=STAND_IN_ADB))
        self.assertEqual(host.execed, (Path("/wt/android"), ["./gradlew", "runOnDevice", "-Pserial=SER"]))

    def test_deploy_without_adb_leaves_the_pick_to_gradle(self):
        host = ScriptedHost()
        run_action(devpanel.deploy_android, context(host, DEVPANEL_ADB="/no/such/adb"))
        self.assertEqual(host.execed, (Path("/repo/android"), ["./gradlew", "runOnDevice"]))


class WebActionsTest(unittest.TestCase):
    def execed(self, action, root=Path("/repo")) -> tuple[Path, list[str]]:
        host = ScriptedHost(lambda cmd: (1, ""))
        run_action(action, context(host, root))
        return host.execed, host.captured

    def assert_resets_stack_first(self, captured):
        self.assertIn(["lsof", "-ti", "tcp:9000", "-sTCP:LISTEN"], captured)
        self.assertIn(["pgrep", "-f", "modules.LocalFixtureWorkerMain"], captured)

    def test_web_runs_through_the_heap_canary(self):
        execed, captured = self.execed(devpanel.run_web)
        self.assertEqual(execed, (Path("/repo"), ["./scripts/dev-server.sh"]))
        self.assert_resets_stack_first(captured)

    def test_local_stack(self):
        execed, captured = self.execed(devpanel.run_local_stack)
        self.assertEqual(execed, (Path("/repo"), ["sbt", "localStack"]))
        self.assert_resets_stack_first(captured)

    def test_kill_stack(self):
        host = ScriptedHost(lambda cmd: (1, ""))
        _, out = run_action(devpanel.kill_stack, context(host))
        self.assertIsNone(host.execed)
        self.assert_resets_stack_first(host.captured)
        self.assertIn("free :9000", out)

    def test_reset_local_corpus(self):
        self.assertEqual(self.execed(devpanel.reset_local_corpus)[0],
                         (Path("/repo"), ["scripts/reset-corpus.sh", "--local", "--yes"]))

    def test_worktree_override(self):
        self.assertEqual(devpanel.repo_root({"DEVPANEL_REPO_ROOT": "/tmp/wt"}), Path("/tmp/wt"))
        self.assertEqual(devpanel.repo_root({}), REPO)
        self.assertEqual(self.execed(devpanel.run_web, Path("/tmp/wt"))[0][0], Path("/tmp/wt"))


class CliTest(unittest.TestCase):
    def cli(self, *args) -> subprocess.CompletedProcess:
        return subprocess.run([sys.executable, str(HERE / "scripts/devpanel.py"), *args],
                              capture_output=True, text=True, timeout=30)

    def test_unknown_action_prints_usage(self):
        p = self.cli("bogus")
        self.assertEqual(p.returncode, 2)
        self.assertIn("deploy-ios", p.stderr)

    def test_every_panel_button_names_a_real_action(self):
        swift = (HERE / "DevPanel/main.swift").read_text()
        buttons = re.findall(r'actionName: "([a-z-]+)"', swift)
        self.assertGreaterEqual(len(buttons), 6)
        self.assertLessEqual(set(buttons), set(devpanel.ACTIONS))
        self.assertIn('"reap-worker"', swift)

    def test_dev_tool_path_appends_existing_dirs_once(self):
        path = devpanel.with_dev_tool_path({"PATH": "/usr/bin:/usr/local/bin"})["PATH"].split(":")
        self.assertEqual(path[:2], ["/usr/bin", "/usr/local/bin"])
        self.assertEqual(len(path), len(set(path)))


def wait_until(predicate, timeout=5.0) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(0.1)
    return False


class RealProcessTest(unittest.TestCase):
    def ctx(self):
        return context(devpanel.Host())

    def test_free_port_kills_a_listener(self):
        port = 9099
        server = subprocess.Popen([sys.executable, "-m", "http.server", str(port), "--bind", "127.0.0.1"],
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.addCleanup(server.kill)
        listening = lambda: devpanel.Host().capture(["lsof", "-ti", f"tcp:{port}", "-sTCP:LISTEN"])[1].strip()
        self.assertTrue(wait_until(listening), "test server never listened")
        run_action(lambda c: devpanel.free_port(c, port), self.ctx())
        self.assertEqual(listening(), "")

    def test_kill_pattern_reaps_a_match_but_not_itself(self):
        marker = f"DEVPANEL_KILLTEST_{os.getpid()}"
        sleeper = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(300)", marker])
        self.addCleanup(sleeper.kill)
        matching = lambda: devpanel.Host().capture(["pgrep", "-f", marker])[1].strip()
        self.assertTrue(wait_until(matching))
        run_action(lambda c: devpanel.kill_pattern(c, marker, "test marker"), self.ctx())
        self.assertTrue(wait_until(lambda: sleeper.poll() is not None))


@unittest.skipUnless(shutil.which("swiftc"), "swiftc not installed")
class SwiftPanelTest(unittest.TestCase):
    def test_compiles_and_self_tests(self):
        with tempfile.TemporaryDirectory() as tmp:
            binary = Path(tmp) / "DevPanel"
            build = subprocess.run(["swiftc", "-O", "-o", str(binary), str(HERE / "DevPanel/main.swift")],
                                   capture_output=True, text=True)
            self.assertEqual(build.returncode, 0, build.stderr)
            proc = subprocess.Popen([str(binary)], env={**os.environ, "DEVPANEL_SELFTEST": "1",
                                         "DEVPANEL_SELFTEST_SCRIPTS": str(HERE / "scripts")},
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            out, _ = proc.communicate(timeout=60)
            # The self-test's UserDefaults suite leaves a plist behind per run.
            (Path.home() / f"Library/Preferences/devpanel.selftest.{proc.pid}.plist").unlink(missing_ok=True)
            self.assertEqual(proc.returncode, 0, out)
            self.assertIn("SELFTEST_OK", out)


class ShellEntryPointsTest(unittest.TestCase):
    def test_run_dev_panel_calls_build(self):
        self.assertIn("tools/devpanel/build.sh", (REPO / "runDevPanel.sh").read_text())

    def test_remaining_shell_scripts_parse(self):
        for script in (REPO / "runDevPanel.sh", HERE / "build.sh", HERE / "test.sh"):
            with self.subTest(script=script.name):
                self.assertEqual(subprocess.run(["bash", "-n", str(script)]).returncode, 0)


if __name__ == "__main__":
    unittest.main()
