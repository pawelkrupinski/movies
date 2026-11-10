#!/usr/bin/env python3
"""Pins the lockstep groups in /renovate.json: artifacts that must move in ONE Renovate PR share a packageRule's
groupName, and each is declared in android/app/build.gradle.kts as a literal "group:artifact:version" coordinate,
the shape Renovate's gradle manager reads (a version held in a variable or a script it would not see).

Robolectric is the case this was written for: org.robolectric:robolectric and the android-all-instrumented runtime jar
it loads (the `robolectricAndroidAll` configuration) are versioned to each other's instrumentation level, and a bump of
one without the other fails every Robolectric test.

    python3 android/scripts/renovate_config_test.py
"""
import fnmatch
import json
import os
import re
import unittest

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
RENOVATE = os.path.join(ROOT, "renovate.json")
BUILD = os.path.join(ROOT, "android", "app", "build.gradle.kts")

LOCKSTEP = {
    "Robolectric": ["org.robolectric:robolectric", "org.robolectric:android-all-instrumented"],
}


def group_of(rules, package):
    """The groupName of the last packageRule naming `package` (Renovate applies rules in order, later wins)."""
    group = None
    for rule in rules:
        if any(fnmatch.fnmatchcase(package, pattern.replace("**", "*")) for pattern in rule.get("matchPackageNames", [])):
            group = rule.get("groupName", group)
    return group


class RenovateLockstepGroups(unittest.TestCase):
    def setUp(self):
        with open(RENOVATE) as f:
            self.rules = json.load(f).get("packageRules", [])
        with open(BUILD) as f:
            self.build = f.read()

    def test_each_lockstep_set_shares_one_group(self):
        for group, packages in LOCKSTEP.items():
            for package in packages:
                self.assertEqual(group_of(self.rules, package), group,
                                 f"{package} is not in Renovate's {group!r} group: a bump could move it alone")

    def test_each_lockstep_package_is_a_literal_gradle_coordinate(self):
        for packages in LOCKSTEP.values():
            for package in packages:
                self.assertRegex(self.build, r'\w+\("' + re.escape(package) + r':[^"$]+"\)',
                                 f"{package} has no literal coordinate in build.gradle.kts for Renovate to read")


TESTS = os.path.join(ROOT, "android", "app", "src", "test")
ANDROID_ALL = "org.robolectric:android-all-instrumented"


def configured_sdks():
    """Every API level a Robolectric test pins with @Config(sdk = [N])."""
    found = set()
    for folder, _, files in os.walk(TESTS):
        for name in files:
            if name.endswith((".kt", ".java")):
                with open(os.path.join(folder, name)) as f:
                    found |= {int(n) for n in re.findall(r"@Config\(sdk = \[(\d+)\]", f.read())}
    return found


class RobolectricRuntimeIsTheTestsSdk(unittest.TestCase):
    """android-all-instrumented's version names an Android SDK (`14-robolectric-...` is API 34) as well as an
    instrumentation level, and Robolectric -- offline, over the jar the build resolves -- loads the one the tests'
    @Config(sdk = [N]) asks for. Renovate moved it to `17-robolectric-...` (82ddc4baf1, API 37) while every test still
    pins API 34: Order independence #59 failed 210 tests "Unable to locate dependency ...-14-robolectric-...". The pin
    and Renovate's allowed versions keep the SDK the tests configure; only the instrumentation suffix moves."""

    def setUp(self):
        with open(BUILD) as f:
            self.build = f.read()
        with open(RENOVATE) as f:
            self.rules = json.load(f).get("packageRules", [])
        self.sdks = configured_sdks()

    def prefix(self):
        self.assertEqual(len(self.sdks), 1, f"the Robolectric tests pin {sorted(self.sdks)}: one runtime is resolved")
        return f"{next(iter(self.sdks)) - 20}-robolectric-"   # API 34 is Android 14, 35 is 15, ...

    def test_the_pinned_runtime_is_the_sdk_the_tests_configure(self):
        version = re.search(r'\w+\("' + re.escape(ANDROID_ALL) + r':([^"]+)"\)', self.build).group(1)
        self.assertTrue(version.startswith(self.prefix()),
                        f"{ANDROID_ALL}:{version} is not the API the tests pin ({sorted(self.sdks)}): {self.prefix()}*")

    def test_renovate_never_moves_the_runtime_off_that_sdk(self):
        allowed = [r.get("allowedVersions") for r in self.rules if ANDROID_ALL in r.get("matchPackageNames", [])
                   and r.get("allowedVersions")]
        self.assertEqual(allowed, [f"/^{self.prefix()}/"],
                         f"Renovate may move {ANDROID_ALL} to another SDK than the tests pin")


class RenovateRunsWhenItsWorkflowDoes(unittest.TestCase):
    """renovate.yml's cron (and the external scheduler's dispatch of it) IS the schedule. A `schedule` in renovate.json
    is a second window Renovate checks at run time: "before 6am on monday" against a 15:00 UTC run made every weekly
    run do nothing (2026-10-12 .. 11-09: green runs, no PR ever opened)."""

    def test_renovate_json_sets_no_schedule_of_its_own(self):
        with open(RENOVATE) as f:
            config = json.load(f)
        self.assertNotIn("schedule", config, "renovate.json's schedule gates every run of renovate.yml; drop it")
        for rule in config.get("packageRules", []):
            self.assertNotIn("schedule", rule, f"packageRule {rule.get('groupName')!r} carries its own schedule")


if __name__ == "__main__":
    unittest.main()
