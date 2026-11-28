#!/usr/bin/env python3
"""scripts/ci/preflight.sh, black-box: secrets present or not (never printed), and which unaccepted Android SDK licences
fail the build's preflight. sdkmanager is a stub replaying the shapes a real one prints. Ported from preflight-test.sh;
the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/preflight.py", "scripts/ci/preflight.sh")

# What run 35509874855 printed on a healthy runner: six unaccepted, none of them needed. Every one after the first
# follows the previous prompt on the same line.
HEALTHY = """6 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/6: License android-googletv-license:
Accept? (y/N): 2/6: License android-googlexr-license:
Accept? (y/N): 3/6: License android-sdk-arm-dbt-license:
Accept? (y/N): 4/6: License android-sdk-preview-license:
Accept? (y/N): 5/6: License google-gdk-license:
Accept? (y/N): 6/6: License mips-android-sysimage-license:
Accept? (y/N): """
MID_LINE = """2 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/2: License android-googletv-license:
Accept? (y/N): 2/2: License android-sdk-license:
Accept? (y/N): """
UNNEEDED = """5 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/5: License android-googletv-license:
---------------------------------------
Accept? (y/N): 2/5: License google-gdk-license:"""
SDK_LICENSE = """1 of 7 SDK package licenses not accepted.
Review licenses that have not been accepted (y/N)?
1/1: License android-sdk-license:
Accept? (y/N): """


class PreflightTest(cli_harness.ToolCase):
    TOOL = TOOL

    def sdkmanager_says(self, output: str) -> None:
        """The stub sdkmanager prints `output` for `--licenses` (after reading every answer it is given)."""
        self.write("output", output + "\n")
        self.fakes.script("sdkmanager", f"cat >/dev/null\ncat {self.path('output')}")

    def run_preflight(self, env: Dict[str, str], *args: str) -> cli_harness.Done:
        """The tool in an environment of `env` alone, beside a PATH of the stubs and the system's (`env -i`)."""
        full = {"PATH": f"{self.fakes.bin}:/usr/bin:/bin", **env}
        return cli_harness.run_bounded([TOOL, *args], env=full, cwd=self.work, stdin=None)

    def code(self, env: Dict[str, str], *args: str) -> int:
        return self.run_preflight(env, *args).code

    def test_every_named_secret_present_passes(self) -> None:
        self.assertEqual(0, self.code({"A": "1", "B": "x"}, "A", "B"))

    def test_an_unset_secret_fails_naming_it(self) -> None:
        done = self.run_preflight({"A": "1"}, "A", "B")
        self.assertEqual(1, done.code)
        self.assertIn("::error::secret B is unset or empty", done.output)

    def test_an_empty_secret_fails_not_just_an_unset_one(self) -> None:
        self.assertEqual(1, self.code({"A": "1", "B": ""}, "A", "B"))

    def test_a_present_secrets_value_is_never_printed(self) -> None:
        done = self.run_preflight({"SIGNING_KEY": "hunter2-secret-value"}, "SIGNING_KEY")
        self.assertEqual(0, done.code)
        self.assertNotIn("hunter2", done.output)

    def test_a_name_that_is_not_a_variable_name_is_refused(self) -> None:
        self.assertEqual(1, self.code({}, "A;id"))

    def test_an_unknown_option_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.code({}, "--nope"))

    def test_all_licences_accepted_passes(self) -> None:
        self.sdkmanager_says("All SDK package licenses accepted.")
        self.assertEqual(0, self.code({}, "--android-sdk-licence"))

    def test_a_healthy_runners_six_unneeded_licences_pass(self) -> None:
        self.sdkmanager_says(HEALTHY)
        done = self.run_preflight({}, "--android-sdk-licence")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("unaccepted licences this build does not need: android-googletv-license", done.output)

    def test_a_needed_licence_named_mid_line_after_a_prompt_is_caught(self) -> None:
        self.sdkmanager_says(MID_LINE)
        self.assertEqual(1, self.code({}, "--android-sdk-licence"))

    def test_licences_the_build_never_installs_do_not_fail_it(self) -> None:
        self.sdkmanager_says(UNNEEDED)
        self.assertEqual(0, self.code({}, "--android-sdk-licence"))

    def test_an_unaccepted_android_sdk_license_fails_saying_which(self) -> None:
        self.sdkmanager_says(SDK_LICENSE)
        done = self.run_preflight({}, "--android-sdk-licence")
        self.assertEqual(1, done.code)
        self.assertIn("licence android-sdk-license is not accepted", done.output)

    def test_output_it_cannot_read_fails_rather_than_passing_blind(self) -> None:
        self.sdkmanager_says("Warning: Could not create settings\njava.lang.IllegalArgumentException")
        done = self.run_preflight({}, "--android-sdk-licence")
        self.assertEqual(1, done.code)
        self.assertIn("java.lang.IllegalArgumentException", done.output)

    def test_an_sdkmanager_under_android_home_is_found(self) -> None:
        self.sdkmanager_says("All SDK package licenses accepted.")
        home = self.path("sdk")
        os.makedirs(os.path.join(home, "cmdline-tools", "latest", "bin"))
        os.rename(os.path.join(self.fakes.bin, "sdkmanager"),
                  os.path.join(home, "cmdline-tools", "latest", "bin", "sdkmanager"))
        self.assertEqual(0, self.code({"ANDROID_HOME": home}, "--android-sdk-licence"))

    def test_no_sdkmanager_at_all_fails(self) -> None:
        self.assertEqual(1, self.code({}, "--android-sdk-licence"))


if __name__ == "__main__":
    unittest.main()
