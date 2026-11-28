"""The Android store-screenshot driver's DEVICES (android/scripts/store_screenshots.py): adb pinned to one emulator by
serial, booting the AVD (alone, or as a pool of read-only clones on ports 5554, 5556, …), reviving one that was
closed mid-run, installing the app, and shutting down exactly the emulators this run booted."""
from __future__ import annotations

import dataclasses
import glob
import os
import subprocess
from dataclasses import dataclass, field
from typing import List, Mapping, Optional

from mobile.store_screenshots_common import Died, Output, naps, png_size, play_safe_width

PKG = "net.pawel.kinowo"
ACT = "pl.kinowo.MainActivity"


@dataclass
class Rig:
    """What every device call needs: the SDK's tools, the AVD, where adb / gradle / emulator chatter goes (`noise`),
    and the run's own record of the emulators it booted (shut down on exit; a reused one is never in it)."""
    env: Mapping[str, str]
    output: Output
    repo_root: str
    noise: str
    sdk: str = ""
    avd: str = "kinowo_xl"
    booted: List[str] = field(default_factory=lambda: [])
    pool_apk: str = ""
    pool: bool = False

    @property
    def adb_path(self) -> str:
        return os.path.join(self.sdk, "platform-tools", "adb")

    @property
    def emulator_path(self) -> str:
        return os.path.join(self.sdk, "emulator", "emulator")

    def to(self, output: Output, noise: str) -> "Rig":
        """This rig writing to another output and noise file (a pool worker's), its booted list shared."""
        return dataclasses.replace(self, output=output, noise=noise, booted=self.booted)

    def run(self, argv: List[str], cwd: Optional[str] = None) -> int:
        with open(self.noise, "ab") as noise:
            return subprocess.call(argv, stdout=noise, stderr=noise, stdin=subprocess.DEVNULL, cwd=cwd,
                                   env=dict(self.env))

    def avd_exists(self) -> bool:
        listed = subprocess.run([self.emulator_path, "-list-avds"], capture_output=True, text=True,
                                env=dict(self.env)).stdout
        return self.avd in listed.splitlines()

    def require_avd(self) -> None:
        if not self.avd_exists():
            raise self.output.die(f"AVD '{self.avd}' not found. Create it (Android Studio) or pass AVD=<name>.")

    def launch(self, *extra: str) -> None:
        """The emulator started in the background, detached, its chatter to the noise file."""
        with open(self.noise, "ab") as noise:
            subprocess.Popen([self.emulator_path, "-avd", self.avd, *extra], stdout=noise, stderr=noise,
                             stdin=subprocess.DEVNULL, start_new_session=True, env=dict(self.env))

    def boot_readonly(self, port: str) -> None:
        """ONE read-only clone on `port`: read-only lets several instances share one AVD image."""
        self.launch("-read-only", "-port", port, "-no-snapshot-load", "-no-boot-anim", "-netdelay", "none",
                    "-netspeed", "full")

    def gradle(self, task: str) -> int:
        android = os.path.join(self.repo_root, "android")
        if not os.path.isfile(os.path.join(android, "local.properties")):
            with open(os.path.join(android, "local.properties"), "w") as f:
                f.write(f"sdk.dir={self.sdk}\n")
        return self.run(["./gradlew", task], cwd=android)

    def stop_emulators(self) -> None:
        """Every emulator this run booted asked to exit; one the developer already had running survives."""
        for serial in self.booted:
            Device(self, serial).adb("emu", "kill")


def pool_port(w: int) -> str:
    return str(5554 + 2 * w)


def pool_serial(w: int) -> str:
    return f"emulator-{pool_port(w)}"


class Device:
    """adb aimed at one emulator: `serial`, or adb's sole device when None."""

    def __init__(self, rig: Rig, serial: Optional[str]) -> None:
        self.rig = rig
        self.serial = serial

    def argv(self, *args: str) -> List[str]:
        return [self.rig.adb_path, *(["-s", self.serial] if self.serial else []), *args]

    def adb(self, *args: str) -> int:
        return self.rig.run(self.argv(*args))

    def read(self, *args: str) -> str:
        """adb's stdout, its stderr to the noise file."""
        with open(self.rig.noise, "ab") as noise:
            done = subprocess.run(self.argv(*args), stdout=subprocess.PIPE, stderr=noise, stdin=subprocess.DEVNULL,
                                  env=dict(self.rig.env))
        return done.stdout.decode(errors="replace")

    def must(self, *args: str) -> None:
        """A device step whose failure ends the capture (the shell driver's `set -e`)."""
        if self.adb(*args) != 0:
            raise Died(" ".join(args))

    def tap(self, point: "tuple[int, int]") -> None:
        self.must("shell", "input", "tap", str(point[0]), str(point[1]))

    def type_(self, text: str) -> None:
        """`adb shell input text` types only the first word unless each space is sent as the escape `%s`."""
        self.must("shell", "input", "text", text.replace(" ", "%s"))

    def back(self) -> None:
        self.must("shell", "input", "keyevent", "4")

    def snap(self, path: str) -> bool:
        with open(path, "wb") as out, open(os.devnull, "wb") as quiet:
            return subprocess.call(self.argv("exec-out", "screencap", "-p"), stdout=out, stderr=quiet,
                                   stdin=subprocess.DEVNULL, env=dict(self.rig.env)) == 0

    def state(self) -> str:
        with open(os.devnull, "wb") as quiet:
            return subprocess.run(self.argv("get-state"), stdout=subprocess.PIPE, stderr=quiet,
                                  stdin=subprocess.DEVNULL, env=dict(self.rig.env)).stdout.decode().strip()

    def boot_completed(self) -> bool:
        with open(os.devnull, "wb") as quiet:
            done = subprocess.run(self.argv("shell", "getprop", "sys.boot_completed"), stdout=subprocess.PIPE,
                                  stderr=quiet, stdin=subprocess.DEVNULL, env=dict(self.rig.env))
        return done.stdout.decode().replace("\r", "").strip() == "1"

    def booted(self) -> bool:
        return self.state() == "device" and self.boot_completed()

    def disable_private_dns(self) -> None:
        """Android 14's opportunistic Private DNS hangs behind a host VPN, so every lookup fails and the capture
        shoots the app's error state. The emulators boot -read-only, so it is reapplied on every boot."""
        self.adb("shell", "settings", "put", "global", "private_dns_mode", "off")

    def screen_size(self) -> str:
        """`wm size`'s "Physical size: WIDTHxHEIGHT" as WIDTHxHEIGHT."""
        words = self.read("shell", "wm", "size").replace("\r", "").split()
        return words[-1] if words else ""

    def wait_booted(self, limit: int, step: int, why: str) -> None:
        waited = 0
        while not self.booted():
            naps(self.rig.env, step)
            waited += step
            if waited > limit:
                raise self.rig.output.die(why)

    def ensure_emulator(self) -> None:
        """The sole emulator up (booted if needed, and then shut down on exit), at the size the taps assume."""
        rig = self.rig
        if not self.booted():
            if self.state():
                self.adb("kill-server")
                naps(rig.env, 2)
            rig.require_avd()
            rig.output.step(f"booting {rig.avd}")
            rig.launch("-no-snapshot-load", "-no-boot-anim", "-netdelay", "none", "-netspeed", "full")
            self.wait_booted(240, 3, "emulator didn't finish booting")
            self.disable_private_dns()
            rig.booted[:] = [self.read("get-serialno").replace("\r", "").strip()]
            rig.output.done()
        self.adb("shell", "wm", "size", "reset")
        self.adb("shell", "wm", "density", "reset")
        size = self.screen_size()
        if size != "1344x2992":
            rig.output.warn(f"screen is {size}, not 1344×2992 ({rig.avd}) — taps may miss.")

    def ensure_app(self) -> None:
        if self.rig.env.get("BUILD") or PKG not in self.read("shell", "pm", "list", "packages"):
            self.rig.output.step("installing app")
            if self.rig.gradle(":app:installDebug") != 0:
                raise self.rig.output.die("gradle installDebug failed")
            self.rig.output.done()

    def revive(self) -> None:
        """This pool worker's emulator back on its own port if it was closed or killed, the app re-installed (a
        read-only instance boots from the pristine image). Its serial is already in the run's booted list."""
        if self.booted():
            return
        rig = self.rig
        port = (self.serial or "").rsplit("-", 1)[-1]
        rig.output.warn(f"{self.serial} is gone (closed or killed) — bringing it back")
        rig.output.step(f"re-booting {rig.avd} :{port} (read-only)")
        rig.boot_readonly(port)
        self.wait_booted(420, 5, f"{self.serial} never came back after being closed")
        self.adb("shell", "wm", "size", "reset")
        rig.output.done()
        if rig.pool_apk:
            rig.output.step(f"re-installing app on {self.serial}")
            if self.adb("install", "-r", rig.pool_apk) != 0:
                raise rig.output.die(f"re-install failed on {self.serial}")
            rig.output.done()

    def in_app(self) -> bool:
        top = [line for line in self.read("shell", "dumpsys", "activity", "activities").splitlines()
               if "topResumedActivity" in line]
        return bool(top) and PKG in top[0]

    def pad_playsafe(self, source: str, out: str) -> None:
        """`source` padded (black) to a width keeping long:short at 1.98:1, written to `out`."""
        size = png_size(source)
        height = size[1] if size else 0
        self.rig.run(["sips", "--padToHeightWidth", str(height), str(play_safe_width(height)), "--padColor",
                      "000000", source, "--out", out])


def boot_pool(rig: Rig, k: int) -> None:
    """K read-only clones of the AVD on ports 5554, 5556, …, after clearing every emulator and the AVD's stale locks
    (a read-write instance or a crashed one's lock makes the rest refuse), each launch staggered so two do not race
    the multi-instance lock. Each serial is recorded before its boot is waited on, so a die() still shuts it down."""
    rig.require_avd()
    rig.output.step("clearing emulators + locks")
    rig.run([rig.adb_path, "kill-server"])
    with open(os.devnull, "wb") as quiet:
        subprocess.call(["pkill", "-f", f"qemu-system.*{rig.avd}"], stdout=quiet, stderr=quiet, env=dict(rig.env))
    naps(rig.env, 3)
    for lock in glob.glob(os.path.join(rig.env.get("HOME", ""), ".android", "avd", f"{rig.avd}.avd", "*.lock")):
        os.remove(lock)
    rig.run([rig.adb_path, "start-server"])
    rig.output.done()
    for w in range(k):
        rig.booted.append(pool_serial(w))
        rig.output.step(f"booting {rig.avd} :{pool_port(w)} (read-only)")
        rig.boot_readonly(pool_port(w))
        naps(rig.env, 25)
        rig.output.done()
    for w in range(k):
        device = Device(rig, pool_serial(w))
        rig.output.step(f"waiting {device.serial}")
        waited = 0
        while not device.boot_completed():
            naps(rig.env, 5)
            waited += 5
            if waited > 420:
                raise rig.output.die(f"{device.serial} didn't finish booting")
        device.disable_private_dns()
        device.adb("shell", "wm", "size", "reset")
        rig.output.done()


def build_apk(rig: Rig) -> str:
    """The debug APK built ONCE (one Gradle daemon), for the pool to install on each emulator."""
    rig.output.step("building APK", to_err=True)
    if rig.gradle(":app:assembleDebug") != 0:
        raise rig.output.die("gradle assembleDebug failed")
    apk = os.path.join(rig.repo_root, "android/app/build/outputs/apk/debug/app-debug.apk")
    if not os.path.isfile(apk):
        raise rig.output.die(f"APK not found at {apk}")
    rig.output.done(to_err=True)
    return apk
