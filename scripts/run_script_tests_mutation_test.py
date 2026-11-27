"""THE DERIVED READS' mutation test (scripts/run-script-tests): a copy of the runner whose selection no longer derives
what a test runs (script_tests_changed.py's Selector.touched without its Derived step) must fail
run_script_tests_test.py's DerivedReadsTest, and the unmutated copy must pass it -- so removing the derivation, or
the test, cannot go unseen.
Run: python3 scripts/run_script_tests_mutation_test.py"""
from __future__ import annotations

import glob
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.realpath(__file__))
DERIVED = "        return self.derived is not None and bool(self.changed & self.derived.reads(test))\n"


def copy_runner(into: str, mutate: bool) -> str:
    """The runner and its library under `into`/scripts, the derivation dropped when `mutate`: its path."""
    os.makedirs(os.path.join(into, "scripts/lib"))
    for f in ("run-script-tests", "floor-constructs.tsv", "stall_watch.py", "stall-report.sh", "linux-tree-sync.py"):
        shutil.copy2(os.path.join(HERE, f), os.path.join(into, "scripts", f))
    for f in glob.glob(os.path.join(HERE, "lib", "*.py")):
        shutil.copy2(f, os.path.join(into, "scripts/lib"))
    changed = os.path.join(into, "scripts/lib/script_tests_changed.py")
    with open(changed) as f:
        text = f.read()
    if DERIVED not in text:
        raise AssertionError("script_tests_changed.py no longer has the derivation line this mutates")
    if mutate:
        with open(changed, "w") as f:
            f.write(text.replace(DERIVED, "        return False\n"))
    return os.path.join(into, "scripts/run-script-tests")


def derived_reads_test(runner: str) -> int:
    env = {**os.environ, "CLI_TOOL_UNDER_TEST": runner, "PYTHONDONTWRITEBYTECODE": "1"}
    return subprocess.run([sys.executable, "-B", os.path.join(HERE, "run_script_tests_test.py"), "DerivedReadsTest"],
                          env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode


class MutationTest(unittest.TestCase):
    def test_the_derivation_removed_fails_the_derived_reads_test(self) -> None:
        with tempfile.TemporaryDirectory() as work:
            self.assertEqual(0, derived_reads_test(copy_runner(os.path.join(work, "kept"), False)))
            self.assertNotEqual(0, derived_reads_test(copy_runner(os.path.join(work, "mutant"), True)))


if __name__ == "__main__":
    unittest.main()
