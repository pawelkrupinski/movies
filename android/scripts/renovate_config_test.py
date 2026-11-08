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


if __name__ == "__main__":
    unittest.main()
