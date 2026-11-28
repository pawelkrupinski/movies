#!/usr/bin/env python3
"""scripts/ci/order-preflight.sh, black-box: a runner that cannot run a leg fails at its preflight as INFRASTRUCTURE --
each finding a `::error title=Runner preflight::UNKNOWN: ...` line (agentctl's infra row), every check reported, exit
1 -- and a runner that can passes. Stubbed tools (java, sed, date, timeout, adb, xcodebuild, xcrun, swift) on PATH.
Ported from order-preflight-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import platform
import shutil
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/order_preflight.py", "scripts/ci/order-preflight.sh")
# GNU or BSD by $FLAVOUR, otherwise the real tool
FLAVOURED = """#!/usr/bin/env bash
if [ "${{1:-}}" = --version ]; then [ "$FLAVOUR" = gnu ] && echo "{name} (GNU coreutils) 9.4" || exit 1; exit 0; fi
exec {real} "$@"
"""
# a JDK's own three lines, after the banner a JVM prints first when an options variable is set (JAVA_BANNER)
JAVA = r"""#!/usr/bin/env bash
[ -z "${JAVA_BANNER:-}" ] || echo "$JAVA_BANNER" >&2
echo "openjdk version \"$JAVA\" 2026-09-15" >&2
echo "OpenJDK Runtime Environment Temurin-$JAVA (build $JAVA+35)" >&2
echo "OpenJDK 64-Bit Server VM Temurin-$JAVA (build $JAVA+35, mixed mode, sharing)" >&2
"""
ADB = """#!/usr/bin/env bash
case "$1" in get-state) echo "$ADB_STATE";; shell) echo "$BOOTED";; esac
"""
UNKNOWN = "::error title=Runner preflight::UNKNOWN: runner preflight"


class OrderPreflightTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        for name in ("sed", "date", "timeout"):
            self.executable(os.path.join(self.fakes.bin, name),
                            FLAVOURED.format(name=name, real=shutil.which(name) or "/usr/bin/false"))
        self.executable(os.path.join(self.fakes.bin, "java"), JAVA)
        self.executable(os.path.join(self.fakes.bin, "sbt"), "#!/usr/bin/env bash\ntrue\n")
        self.executable(self.path("sdk", "platform-tools", "adb"), ADB)
        self.out = ""

    def executable(self, path: str, text: str) -> None:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
        os.chmod(path, 0o755)

    def pre(self, leg: str, sdk: Optional[str] = None, **env: str) -> int:
        """The preflight's exit; its output in self.out, what it wrote to $GITHUB_OUTPUT in gh-out."""
        self.write("gh-out", "")
        extra = {"ANDROID_SDK_ROOT": None, "ANDROID_HOME": sdk or self.path("sdk"), "GITHUB_OUTPUT": self.path("gh-out"),
                 "GITHUB_STEP_SUMMARY": os.devnull, **env}
        done = self.run_tool(leg, env=extra)
        self.out = done.output
        return done.code

    def said(self, text: str) -> int:
        return sum(1 for line in self.out.split("\n") if text in line)

    def output_lines(self) -> list[str]:
        return self.read("gh-out").splitlines()

    def with_device(self) -> str:
        return self.path("sdk", "platform-tools") + os.pathsep + os.environ.get("PATH", "")

    def test_scala_on_a_runner_with_gnu_tools_java_27_and_sbt_passes(self) -> None:
        self.assertEqual(0, self.pre("scala", FLAVOUR="gnu", JAVA="27"), self.out)
        self.assertEqual(["preflight=ok"], self.output_lines())
        self.assertEqual(1, self.said("::notice title=Runner preflight::network for the tests: "), self.out)

    def test_scala_with_java_17_and_bsd_tools_fails_naming_each_as_the_runner(self) -> None:
        self.assertEqual(1, self.pre("scala", FLAVOUR="bsd", JAVA="17"))
        self.assertEqual(4, sum(1 for line in self.out.split("\n") if line.startswith(f"{UNKNOWN} (scala): ")), self.out)
        self.assertEqual(3, self.said("not GNU"))
        self.assertEqual(1, self.said("java is 17, the leg pins 27"))
        self.assertEqual(["preflight=failed"], self.output_lines())

    def test_android_with_java_17_and_adb_under_the_sdk_passes(self) -> None:
        self.assertEqual(0, self.pre("android", FLAVOUR="gnu", JAVA="17"), self.out)

    # Order independence #111 (run 45025091272): ci-runner's job image sets JAVA_TOOL_OPTIONS, whose banner is the JVM's
    # first line, and every self-hosted leg failed "java is unreadable"
    def test_java_behind_the_job_images_banners_is_read(self) -> None:
        self.assertEqual(0, self.pre("android", FLAVOUR="gnu", JAVA="17.0.13", JAVA_BANNER=(
            "Picked up JAVA_TOOL_OPTIONS: -Dfile.encoding=UTF-8 -Djava.net.preferIPv4Stack=true")), self.out)
        self.assertEqual(0, self.said("unreadable"))
        self.assertEqual(0, self.pre("scala", FLAVOUR="gnu", JAVA="27", JAVA_BANNER=(
            "NOTE: Picked up JDK_JAVA_OPTIONS: -Xlog:gc:file=/home/runner/_work/_temp/ci-gc/gc-%p.log")), self.out)

    def test_a_banner_naming_a_version_is_no_version_line(self) -> None:
        self.assertEqual(1, self.pre("scala", FLAVOUR="gnu", JAVA="17",
                                     JAVA_BANNER='Picked up JAVA_TOOL_OPTIONS: -Dx="version \\"27\\""'))
        self.assertEqual(1, self.said("java is 17, the leg pins 27"), self.out)

    def test_a_pinned_java_other_than_the_default_is_held(self) -> None:
        self.assertEqual(0, self.pre("scala", FLAVOUR="gnu", JAVA="25", ORDER_PREFLIGHT_JAVA="25"), self.out)

    def test_android_with_no_adb_under_the_sdk_fails(self) -> None:
        self.assertEqual(1, self.pre("android", sdk=self.path("none"), FLAVOUR="gnu", JAVA="17"))
        self.assertEqual(1, self.said(f"no adb under the SDK ({self.path('none')})"), self.out)

    def test_a_device_the_emulator_booted_passes(self) -> None:
        self.assertEqual(0, self.pre("device", PATH=self.with_device(), ADB_STATE="device", BOOTED="1"), self.out)
        self.assertEqual(1, self.said("preflight (device): the emulator is booted"))
        self.assertEqual(0, self.said("::notice"))

    def test_a_device_still_booting_fails_as_the_runner(self) -> None:
        self.assertEqual(1, self.pre("device", PATH=self.with_device(), ADB_STATE="device", BOOTED="0"))
        self.assertEqual(1, self.said(f"{UNKNOWN} (device): the emulator has not finished booting "
                                      "(sys.boot_completed=0)"), self.out)

    def test_no_device_at_all_names_both_findings(self) -> None:
        self.assertEqual(1, self.pre("device", PATH=self.with_device(), ADB_STATE="", BOOTED=""))
        self.assertEqual(1, self.said("adb sees no device (state: none)"), self.out)
        self.assertEqual(1, self.said("(sys.boot_completed=unset)"), self.out)

    @unittest.skipUnless(platform.system() == "Darwin", "the macOS legs' tools")
    def test_swift_with_xcode_swift_and_a_working_simctl_passes(self) -> None:
        self.mac_tools()
        self.assertEqual(0, self.pre("swift", SIMCTL="ok"), self.out)
        self.assertEqual(1, self.said("preflight (swift): Xcode 26.6"))
        self.assertEqual(1, self.said("preflight (swift): Swift version 6.2"))

    @unittest.skipUnless(platform.system() == "Darwin", "the macOS legs' tools")
    def test_swift_with_coresimulator_unusable_fails_as_the_runner(self) -> None:
        self.mac_tools()
        self.assertEqual(1, self.pre("swift", SIMCTL="no"))
        self.assertEqual(1, self.said(f"{UNKNOWN} (swift): `xcrun simctl list devices` failed"), self.out)

    @unittest.skipIf(platform.system() == "Darwin", "a Linux runner")
    def test_a_macos_leg_on_linux_is_the_runners_fault(self) -> None:
        self.assertEqual(1, self.pre("swift"))
        self.assertEqual(1, self.said("a macOS leg on Linux"), self.out)

    def mac_tools(self) -> None:
        self.executable(os.path.join(self.fakes.bin, "xcodebuild"), '#!/usr/bin/env bash\necho "Xcode 26.6"\n')
        self.executable(os.path.join(self.fakes.bin, "swift"), '#!/usr/bin/env bash\necho "Swift version 6.2"\n')
        self.executable(os.path.join(self.fakes.bin, "xcrun"), '#!/usr/bin/env bash\n[ "$SIMCTL" = ok ]\n')

    def test_an_unknown_leg_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.pre("nope"))
        self.assertIn("order-preflight.sh: unknown leg nope", self.out)

    def test_no_leg_is_refused(self) -> None:
        done = self.run_tool()
        self.assertEqual(1, done.code)
        self.assertIn("usage: order-preflight.sh scala|android|android-instrumented|device|swift|ios", done.stderr)


if __name__ == "__main__":
    unittest.main()
