#!/usr/bin/env python3
"""THE SHELL MIGRATION GATE's tests (shell_gate.py), black-box over a scratch repository holding the real design rules
table: a new script past a limit is refused with the migration hint, a frozen one may not grow, the baseline may not
grow whatever the trailer, a fall is refused until recorded, a migrated tool's rows go with an update, and a keep needs
its date and why and goes stale once its script is within the limits."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from typing import Dict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
TOOL = os.path.join(HERE, "shell_gate.py")
RULES = os.path.join(ROOT, "test", "resources", "design-rules.tsv")
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
from harness import cli_harness  # noqa: E402  (scratch repositories through the isolating helper)


def branches(n: int) -> str:
    return "".join(f"[ -n \"$a{i}\" ] && echo {i}\n" for i in range(n))


SMALL = "#!/bin/bash\necho hello\n"
BIG = "#!/bin/bash\n" + branches(12)  # 12 top-level decisions: past the limit of 10


class ShellGateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.mkdtemp(prefix="shell-gate-")
        self.addCleanup(shutil.rmtree, self.dir)
        self.git("init", "-q", "-b", "main")
        with open(RULES) as f:
            rules = f.read()
        self.write({"test/resources/design-rules.tsv": rules, "scripts/small.sh": SMALL,
                    "scripts/big": BIG, "scripts/big-test.sh": BIG})
        self.assertEqual(0, self.gate("update").returncode)
        self.base = self.commit("the scripts")

    def git(self, *args: str) -> str:
        return cli_harness.scratch_git(self.dir, *args)

    def write(self, files: Dict[str, str]) -> None:
        for path, text in files.items():
            full = os.path.join(self.dir, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.git("add", "-A")

    def commit(self, message: str) -> str:
        self.git("add", "-A")
        self.git("commit", "-q", "--allow-empty", "-m", message)
        return self.git("rev-parse", "HEAD")

    def gate(self, *args: str) -> subprocess.CompletedProcess[str]:
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        return subprocess.run([sys.executable, "-I", "-B", TOOL, *args], cwd=self.dir, capture_output=True, text=True,
                              env=env)

    def assert_refused(self, *said: str, rng: str = "") -> str:
        done = self.gate("check", *(["--range", rng] if rng else []))
        self.assertEqual(1, done.returncode, done.stdout + done.stderr)
        for s in said:
            self.assertIn(s, done.stdout)
        return done.stdout

    def assert_passes(self) -> str:
        done = self.gate("check")
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)
        return done.stdout

    def test_todays_scripts_are_frozen_and_pass(self) -> None:
        self.assertIn("1 shell tools frozen", self.assert_passes())
        with open(os.path.join(self.dir, "test/resources/shell-baseline.tsv")) as f:
            frozen = f.read()
        self.assertIn("shell-decisions\tscripts/big:<top>\t12\n", frozen)
        self.assertNotIn("big-test.sh", frozen)  # a self-test is measured, never held

    def test_a_new_script_past_a_limit_is_refused_with_the_migration_hint(self) -> None:
        self.write({"scripts/new.sh": BIG})
        self.assert_refused("SHELL PAST A LIMIT", "shell-decisions scripts/new.sh:<top>: 12, past the limit 10", "migrate it to Python",
                            "scripts/shell-py-progress/migrate-tools.tsv")

    def test_a_frozen_script_growing_is_refused(self) -> None:
        self.write({"scripts/big": BIG + "[ -z \"$b\" ] || echo b\n"})
        self.assert_refused("shell-decisions scripts/big:<top>: 13, past its baseline 12")

    def test_a_raised_baseline_is_refused_whatever_the_trailer(self) -> None:
        self.write({"scripts/big": BIG + "[ -z \"$b\" ] || echo b\n"})
        self.assertEqual(0, self.gate("update").returncode)
        head = self.commit("grow it\n\nShell: it is only one more")
        self.assert_refused("THE SHELL BASELINE GREW", "shell-decisions scripts/big:<top> raised 12 -> 13",
                            rng=f"{self.base}..{head}")

    def test_a_fall_is_refused_until_recorded(self) -> None:
        self.write({"scripts/big": "#!/bin/bash\n" + branches(11)})
        self.assert_refused("SHELL FELL BELOW", "UNRECORDED", "scripts/design-rules/shell_gate.py update")
        self.assertEqual(0, self.gate("update").returncode)
        self.assert_passes()

    def test_a_tool_migrated_to_python_drops_its_rows_with_an_update(self) -> None:
        self.write({"scripts/big": "#!/usr/bin/env python3\nprint('hello')\n"})
        self.assert_refused("UNRECORDED", "shell-decisions scripts/big:<top> 12 -> within the limit")
        self.assertEqual(0, self.gate("update").returncode)
        self.assertIn("0 shell tools frozen", self.assert_passes())

    def test_a_keep_needs_its_date_and_why_and_leaves_the_queue(self) -> None:
        self.write({"test/resources/shell-keeps.tsv": "scripts/big\tlately\tshort\n"})
        self.assert_refused("THE SHELL KEEPS REFUSED", "not `<path><TAB><added YYYY-MM-DD>")
        why = "git runs it before any Python venv exists, so it stays shell"
        self.write({"test/resources/shell-keeps.tsv": f"scripts/big\t2026-11-28\t{why}\n"})
        self.assert_passes()
        queue = self.gate("queue").stdout
        self.assertIn(f"KEEP scripts/big\t2026-11-28\t{why}", queue)
        self.assertNotIn("\nscripts/big\t", "\n" + queue)

    def test_a_keep_of_a_script_within_the_limits_is_stale(self) -> None:
        why = "git runs it before any Python venv exists, so it stays shell"
        self.write({"test/resources/shell-keeps.tsv": f"scripts/small.sh\t2026-11-28\t{why}\n"})
        self.assert_refused("scripts/small.sh is within every shell limit now (or gone): strike its keep")


if __name__ == "__main__":
    unittest.main()
