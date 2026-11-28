"""scripts/run-script-tests, black-box (ported from run-script-tests-test.sh): THE TREE IS LEFT AS FOUND -- a test leaving a
file (or an ignored directory) in the checkout fails, POLLUTE, beside a clean one; a path no test makes again alone, or
one a writer beside the run keeps making, is said, not failed; macOS metadata is no leftover; RUN_SCRIPT_TESTS_TREE_CHECK=0
turns it off -- and STRESS: the named (or the concurrency-heavy) tests N times at once beside CPU burners.
Run: python3 scripts/run_script_tests_tree_test.py"""
from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import unittest

from run_script_tests_fixture import ScriptTestsCase
from harness import cli_harness

POLLUTER = ("sleep 1; echo x > scripts/left-behind.txt; mkdir -p scripts/tools/node_modules/pkg; "
            "echo y > scripts/tools/node_modules/pkg/i.js")
# made beside the run all along (the landing check's sbt writing target/): made again each time it is moved aside
WRITER = '''import os, sys, time
tree, stop = sys.argv[1], sys.argv[2]
while not os.path.exists(stop):
    if not os.path.exists(os.path.join(tree, "target")):
        os.makedirs(os.path.join(tree, "target"), exist_ok=True)
        open(os.path.join(tree, "target", "out"), "w").write("made")
    time.sleep(0.05)
'''


class TreeTest(ScriptTestsCase):
    def test_a_test_leaving_files_fails_named_and_they_stay_where_the_run_left_them(self) -> None:
        self.put(".gitignore", "node_modules/")
        self.put("scripts/polluter-test.sh", POLLUTER)
        self.put("scripts/clean-test.sh", 'd=$(mktemp -d); echo x > "$d/f"; rm -rf "$d"; sleep 2')
        done = self.runner("--jobs", "4", "polluter-test", "clean-test")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual((1, 1), (self.rows(done, "POLLUTE", "scripts/polluter-test.sh"),
                                  self.rows(done, "PASS", "scripts/clean-test.sh")))
        block = done.stdout.split("── POLLUTE scripts/polluter-test.sh", 1)[1].split("\n\n", 1)[0]
        self.assertEqual(["!! scripts/tools/", "?? scripts/left-behind.txt"],
                         [line.strip() for line in block.splitlines() if re.match(r"^    (\?\?|!!) ", line)])
        with open(os.path.join(self.tree, "scripts/left-behind.txt")) as f:
            self.assertEqual("x", f.read().strip())
        self.assertTrue(os.path.exists(os.path.join(self.tree, "scripts/tools/node_modules/pkg/i.js")))
        os.remove(os.path.join(self.tree, "scripts/left-behind.txt"))
        shutil.rmtree(os.path.join(self.tree, "scripts/tools"))
        done = self.runner("clean-test")
        self.assertEqual((0, False), (done.code, "left in the checkout" in done.stdout), done.output)
        done = self.runner("polluter-test", env={"RUN_SCRIPT_TESTS_TREE_CHECK": "0"})
        self.assertEqual((0, False), (done.code, "POLLUTE" in done.stdout), done.output)

    def test_macos_metadata_is_no_leftover(self) -> None:
        self.put("scripts/finder-test.sh", "touch .metadata_never_index scripts/.DS_Store; sleep 1")
        done = self.runner("finder-test")
        self.assertEqual((0, False), (done.code, "left in the checkout" in done.stdout), done.output)

    def test_a_path_no_test_makes_again_alone_is_said_and_not_failed(self) -> None:
        once = self.path("once-made")
        self.put("scripts/once-test.sh", f"[ -e '{once}' ] || {{ touch '{once}'; echo z > scripts/edited-beside.txt; }}")
        done = self.runner("once-test")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("    ?? scripts/edited-beside.txt   (running then: scripts/once-test.sh )\n", done.stdout)

    def test_a_path_a_writer_beside_the_run_keeps_making_is_no_tests(self) -> None:
        self.put(".gitignore", "target/")
        self.commit("ignore target")
        stop, started = self.path("stop"), self.path("beside.started")
        writer = self.write("writer.py", WRITER)
        self.put("scripts/quiet-test.sh", f"[ -e '{started}' ] || {{ touch '{started}'; {sys.executable} '{writer}' "
                                          f"'{self.tree}' '{stop}' > /dev/null 2>&1 < /dev/null & }}\n"
                                          f"while [ ! -e '{self.tree}/target/out' ]; do sleep 0.05; done")
        try:
            done = self.runner("quiet-test")
        finally:
            self.write("stop", "")
        self.assertEqual((0, False), (done.code, "POLLUTE" in done.stdout), done.output)
        self.assertIn("    !! target/   (running then: scripts/quiet-test.sh )\n", done.stdout)
        gone = lambda: subprocess.run(["pgrep", "-f", writer], stdout=subprocess.DEVNULL).returncode != 0  # noqa: E731
        self.assertTrue(cli_harness.wait_until(gone))


class StressTest(ScriptTestsCase):
    def setUp(self) -> None:
        super().setUp()
        lock = self.path("racy.lock")
        self.put("scripts/racy-test.sh", f"# script-test-stress: its copies share one lock\nmkdir '{lock}' 2> /dev/null "
                                         f"|| {{ echo collided; exit 1; }}\nsleep 3; rmdir '{lock}'")
        self.put("scripts/steady-test.sh", "# script-test-stress: never races\nexit 0")

    def test_stress_runs_a_test_n_times_at_once_and_catches_its_race(self) -> None:
        self.assertEqual(0, self.runner("racy-test").code)
        done = self.runner("--stress", "4", "--burn", "2", "racy-test")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(4, len(re.findall(r"(?m)^[A-Z]* .*scripts/racy-test\.sh \(run [1-4]/4\)$", done.stdout)))
        self.assertIn("\nSTRESS  scripts/racy-test.sh: 3 of 4 run(s) failed\n", done.stdout)
        self.assertIn("beside 2 CPU burner(s)", done.stdout)
        burners = subprocess.run(["ps", "-axo", "command"], capture_output=True, text=True).stdout
        runners = [line.split()[-1] for line in burners.splitlines() if "run-script-tests-burner" in line]
        self.assertEqual([], [r for r in runners if r.isdigit() and not cli_harness.alive(int(r))])

    def test_stress_with_no_names_runs_the_concurrency_heavy_ones(self) -> None:
        self.assertEqual("scripts/racy-test.sh scripts/steady-test.sh", self.listed("--stress", "2"))
        done = self.runner("--stress", "3", "--burn", "1", "steady-test")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("STRESS  scripts/steady-test.sh: 0 of 3 run(s) failed\n", done.stdout)
        done = self.runner("--changed", self.base, "steady-test")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("run-script-tests: concurrency-heavy, selected: scripts/steady-test.sh -- "
                      "`scripts/run-script-tests --stress 4 scripts/steady-test.sh`", done.stdout)
        self.assertEqual((64, 64), (self.runner("--stress", "2", "--linux", "racy-test").code,
                                    self.runner("--stress", "x", "racy-test").code))


if __name__ == "__main__":
    unittest.main()
