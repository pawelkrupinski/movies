#!/usr/bin/env python3
"""scripts/ios-compile-check.sh, black-box, with fake xcodebuild, swift and docker on the PATH: each part's build runs
with its output kept aside -- removed when it compiles, kept and its errors shown when it does not -- a concurrency
diagnostic (shown once, colours stripped) failing every build, Linux's too; a part whose toolchain is missing is
skipped with a warning; the Linux build takes the sources on its stdin, never a mount; the project's and the package's
iOS floors must agree. Run from this checkout (the tool works in its git top level; the real ios/ tree is read, never
written). Ported from ios-compile-check-test.sh case for case, plus each build's exact argv and the lines said; the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ios_compile_check.py", "scripts/ios-compile-check.sh")
# each fake: `-version` / `--version` / `info` answer unless $STUB_MISSING names it; a build prints a line, records its
# stdin (docker -i), fails when $STUB_FAIL names it, and prints $STUB_WARNING when $STUB_WARN names it
FAKE = '''
name=$(basename "$0")
case "$1" in -version|--version|info) [ "${STUB_MISSING:-}" != "$name" ]; exit ;; esac
echo "$name: compiling"
case " $* " in *" -i "*) cat > "$STUB_STDIN" ;; esac
[ "${STUB_WARN:-}" = "$name" ] && printf '%b\\n' "$STUB_WARNING"
[ "${STUB_FAIL:-}" = "$name" ] && { echo "Foo.swift:1: error: no such thing"; exit 65; }
exit 0
'''
RACE = "/w/ios/Kinowo/Foo.swift:12:5: warning: capture of 'self' with non-Sendable type 'Foo' in a '@Sendable' closure"
LOG = "\033[36m▸\033[0m "
WARN = "\033[33m!\033[0m "
APP = ["build-for-testing", "-project", "ios/Kinowo.xcodeproj", "-scheme", "Kinowo", "-destination",
       "generic/platform=iOS Simulator", "-derivedDataPath", "ios/.build/compile-check-derived-data",
       "CODE_SIGNING_ALLOWED=NO"]
LINUX = ["run", "--rm", "-i", "-v", "kinowo-ios-linux6-build:/scratch", "swift:6.2", "sh", "-c",
         "mkdir -p /src && tar -xf - -C /src --strip-components=1 && exec swift build --build-tests --package-path /src "
         "--scratch-path /scratch"]


class IosCompileCheckTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        for name in ("xcodebuild", "swift", "docker"):
            self.fakes.script(name, FAKE, shell="/usr/bin/env bash")
        os.makedirs(self.path("tmp"))

    def compile(self, *args: str, **extra: str) -> cli_harness.Done:
        env = {"TMPDIR": self.path("tmp"), "STUB_STDIN": self.path("stdin.tar"), **extra}
        return self.run_tool(*args, env=env, cwd=cli_harness.ROOT)

    def left(self) -> List[str]:
        return sorted(os.listdir(self.path("tmp")))

    def kept_log(self, done: cli_harness.Done) -> str:
        found = re.search(r"full log: (\S+)", done.stderr)
        self.assertIsNotNone(found, done.output)
        assert found is not None
        return found.group(1)

    def floor(self, manifest: str, *project: str) -> str:
        self.write("ios/Package.swift", f"        .macOS(.v15),\n        .iOS(.v{manifest}),\n")
        self.write("ios/Kinowo.xcodeproj/project.pbxproj",
                   "".join(f"\t\t\t\tIPHONEOS_DEPLOYMENT_TARGET = {v};\n" for v in project))
        return self.path("ios")

    def test_all_three_compile_each_said_nothing_left(self) -> None:
        done = self.compile()
        self.assertEqual(0, done.code, done.output)
        for said in ("app target compiles", "the package compiles on the Mac", "the package compiles on Linux"):
            self.assertIn(f"{LOG}{said}\n", done.stdout)
        self.assertEqual([], self.left(), "no build output left behind")
        self.assertEqual(["-version", APP, "--version", ["build", "--build-tests", "--package-path", "ios"], "info",
                          LINUX], [c[0] if c[:1] in (["-version"], ["--version"], ["info"]) else c
                                   for c in self.fakes.calls()])

    def test_the_lines_said_in_order(self) -> None:
        done = self.compile("--spm-only")
        self.assertRegex(done.stdout, r"^" + re.escape(LOG) + r"iOS deployment target \d+\.0 in the project and the "
                         r"package\n" + re.escape(f"{LOG}swift build --build-tests: the SwiftPM package on this Mac, "
                                                  f"test targets included\n{LOG}the package compiles on the Mac\n") + "$")
        self.assertEqual("", done.stderr)

    def test_the_linux_build_takes_the_sources_on_its_stdin(self) -> None:
        done = self.compile("--linux-only")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([LINUX], [c for c in self.fakes.calls("docker") if c != ["info"]], "no host directory mounted")
        listing = subprocess.run(["tar", "-tf", self.path("stdin.tar")], capture_output=True, text=True, check=True)
        names = listing.stdout.splitlines()
        self.assertIn("ios/Package.swift", names)
        self.assertEqual([], [n for n in names if not n.startswith("ios/") or n.startswith("ios/.build/")])

    def test_a_failing_linux_build_shows_its_errors_and_keeps_its_log(self) -> None:
        done = self.compile(STUB_FAIL="docker")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("Foo.swift:1: error: no such thing\n", done.stderr)
        log = self.kept_log(done)
        self.assertEqual([os.path.basename(log)], self.left())
        self.assertRegex(os.path.basename(log), r"^ios-compile-check-linux\.")
        with open(log) as f:
            self.assertEqual("docker: compiling\nFoo.swift:1: error: no such thing\n", f.read())
        self.assertIn(f"{WARN}full log: {log}\n", done.stderr)

    def test_a_failing_app_build_shows_its_errors(self) -> None:
        done = self.compile("--app-only", STUB_FAIL="xcodebuild")
        self.assertEqual(1, done.code, done.output)
        self.assertTrue(done.stderr.startswith("Foo.swift:1: error: no such thing\n"), done.stderr)

    def test_a_concurrency_warning_in_the_app_build_fails_shown_once(self) -> None:
        done = self.compile("--app-only", STUB_WARN="xcodebuild", STUB_WARNING=f"{RACE}\\n{RACE}")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, done.output.count("non-Sendable type 'Foo'"), done.output)
        log = self.kept_log(done)
        self.assertEqual([os.path.basename(log)], self.left())
        self.assertIn(f"{WARN}1 concurrency diagnostic(s) — fix them (ios/Package.swift says how the checking is set "
                      f"up); full log: {log}\n", done.stderr)

    def test_a_coloured_one_in_the_mac_build_fails_shown_plain(self) -> None:
        line = ("/w/ios/Kinowo/Bar.swift:3:9: warning: main actor-isolated property 'x' can not be referenced from a "
                "nonisolated context")
        done = self.compile("--spm-only", STUB_WARN="swift", STUB_WARNING=f"\\033[1;33m{line}\\033[0m")
        self.assertEqual(1, done.code, done.output)
        self.assertTrue(done.stderr.startswith(line + "\n"), done.stderr)

    def test_any_other_warning_passes(self) -> None:
        done = self.compile("--app-only", STUB_WARN="xcodebuild",
                            STUB_WARNING="/w/ios/Kinowo/Foo.swift:3:9: warning: 'foo()' is deprecated")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.left())

    def test_a_concurrency_warning_in_the_linux_build_fails(self) -> None:
        self.assertEqual(1, self.compile("--linux-only", STUB_WARN="docker", STUB_WARNING=RACE).code)

    def test_a_missing_toolchain_is_skipped_with_a_warning(self) -> None:
        for mode, tool, said in (
                ("--app-only", "xcodebuild", "xcodebuild unavailable (no full Xcode selected) — SKIPPED the app-target "
                                             "build."),
                ("--spm-only", "swift", "no swift toolchain — SKIPPED the Mac SwiftPM build."),
                ("--linux-only", "docker", "no Docker daemon — SKIPPED the Linux swift build (CI's unit-integration "
                                           "job still runs it).")):
            done = self.compile(mode, STUB_MISSING=tool)
            self.assertEqual((0, f"{WARN}{said}\n"), (done.code, done.stderr), mode)

    def test_the_project_and_the_package_on_one_floor(self) -> None:
        done = self.compile("--spm-only", IOS_DIR=self.floor("18", "18.0", "18.0", "18.0"))
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"{LOG}iOS deployment target 18.0 in the project and the package\n", done.stdout)

    def test_a_build_configuration_left_behind_fails_naming_both(self) -> None:
        done = self.compile("--spm-only", IOS_DIR=self.floor("18", "18.0", "16.0", "18.0"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"{WARN}iOS deployment targets disagree: project.pbxproj says 16.0 18.0 , Package.swift 18.0 — "
                      "raise both together\n", done.stderr)
        self.assertIn("the package compiles on the Mac", done.stdout, "the build still runs")

    def test_the_package_left_behind_fails(self) -> None:
        done = self.compile("--app-only", IOS_DIR=self.floor("16", "18.0", "18.0"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("Package.swift 16.0 —", done.stderr)

    def test_no_floor_at_all_fails(self) -> None:
        self.write("ios/Package.swift", "// nothing\n")
        self.write("ios/Kinowo.xcodeproj/project.pbxproj", "\n")
        done = self.compile("--spm-only", IOS_DIR=self.path("ios"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("project.pbxproj says nothing, Package.swift nothing —", done.stderr)

    def test_the_real_tree_agrees(self) -> None:
        self.assertEqual(0, self.compile("--spm-only").code)

    def test_an_unknown_mode_is_a_usage_error(self) -> None:
        done = self.compile("--wat")
        self.assertEqual(2, done.code, done.output)
        self.assertRegex(done.stderr, r"^usage: \S*ios-compile-check\S* \[--app-only \| --spm-only \| --linux-only\]\n$")
        self.assertEqual([], [c for c in self.fakes.calls() if c not in (["-version"], ["--version"], ["info"])])


if __name__ == "__main__":
    unittest.main()
