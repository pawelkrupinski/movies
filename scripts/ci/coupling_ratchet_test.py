#!/usr/bin/env python3
"""coupling_ratchet.py: measured coupling past the baseline fails (a new cross-module edge kind, a new package cycle
edge, a package's Ce past its row or the new-package limit, a class's fan-out past its row or reaching the limit, any package edge into a higher layer);
a baseline grown in a range passes only when a commit changing it carries `Coupling: <why>`; falls pass and are said.
COUPLING_RATCHET_UNDER_TEST names another copy of the tool (coupling-ratchet-mutation-test.sh's mutants)."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("COUPLING_RATCHET_UNDER_TEST") or os.path.join(HERE, "coupling_ratchet.py")
BASELINE = "test/resources/coupling-baseline.tsv"

ROWS = """# a comment
limit\tfanout\t30
limit\tnew-package-ce\t15
module-edge\tworker->common\t40
cycle-edge\tmodels->services.movies\t2
cycle-edge\tservices.movies->models\t9
ce\tmodels\t12
ce\tservices.movies\t20
fanout\tservices.movies.MovieService\t35
"""


def rows(*extra: str, drop: tuple = ()) -> str:
    kept = [line for line in ROWS.splitlines() if not any(line.startswith(d) for d in drop)]
    return "\n".join(kept + list(extra)) + "\n"


class CouplingRatchetTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({BASELINE: ROWS}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def check(self, current, rng=None):
        path = os.path.join(self.dir.name, "current.tsv")
        with open(path, "w") as f:
            f.write(current)
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        args = [sys.executable, "-I", TOOL, "check", "--current", path, "--baseline", BASELINE]
        if rng:
            args += ["--range", rng]
        done = subprocess.run(args, cwd=self.repo, env=env, capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def assert_fails(self, current, *said, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(1, rc, out)
        for s in said:
            self.assertIn(s, out)

    def assert_passes(self, current, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(0, rc, out)
        return out

    def test_the_baseline_itself_passes(self):
        self.assertIn("coupling: within", self.assert_passes(ROWS))

    def test_a_new_cross_module_edge_kind_fails(self):
        self.assert_fails(rows("module-edge\tcommon->worker\t1"), "a new cross-module edge kind: common->worker")

    def test_a_heavier_known_module_edge_passes(self):
        self.assert_passes(rows("module-edge\tworker->common\t400", drop=("module-edge",)))

    def test_a_new_package_cycle_edge_fails(self):
        self.assert_fails(rows("cycle-edge\tmodels->tools\t1"), "a new package cycle edge: models->tools")

    def test_a_package_ce_past_its_row_fails(self):
        self.assert_fails(rows("ce\tmodels\t13", drop=("ce\tmodels",)), "package models: Ce 13 past its baseline 12")

    def test_a_package_ce_at_its_row_passes(self):
        self.assert_passes(rows("ce\tmodels\t12", drop=("ce\tmodels",)))

    def test_a_new_package_past_the_new_package_limit_fails(self):
        self.assert_fails(rows("ce\tservices.fresh\t16"), "package services.fresh: Ce 16 past the new-package limit 15")

    def test_a_new_package_within_the_new_package_limit_passes(self):
        self.assert_passes(rows("ce\tservices.fresh\t15"))

    def test_a_class_fan_out_past_its_row_fails(self):
        self.assert_fails(rows("fanout\tservices.movies.MovieService\t36", drop=("fanout",)),
                          "class services.movies.MovieService: fan-out 36 past its baseline 35")

    def test_a_class_reaching_the_fan_out_limit_fails(self):
        self.assert_fails(rows("fanout\tworker.Wiring\t30"), "class worker.Wiring: fan-out 30 reaches the limit 30")

    def test_a_class_below_the_fan_out_limit_passes(self):
        self.assert_passes(rows("fanout\tworker.Wiring\t29"))

    def test_an_upward_layer_edge_fails_whatever_the_baseline_says(self):
        violation = "layer-violation\tmodels->services.movies\t2"
        self.commit({BASELINE: rows(violation)}, "a baseline that recorded the violation")
        self.assert_fails(rows(violation), "a package edge into a higher layer: models->services.movies")

    def test_an_unlayered_package_fails(self):
        self.assert_fails(rows("layer-violation\tfresh->?\t0"), "package fresh has no layer")

    def test_falls_pass_and_are_said(self):
        out = self.assert_passes(rows("ce\tmodels\t3", drop=("ce\tmodels", "cycle-edge", "fanout")))
        self.assertIn("coupling fell", out)
        self.assertIn("ce models 12 -> 3", out)
        self.assertIn("cycle-edge models->services.movies gone", out)
        self.assertIn("fanout services.movies.MovieService 35 -> below the limit", out)

    def test_a_grown_baseline_without_a_trailer_fails(self):
        grown = rows("module-edge\tcommon->worker\t1")
        head = self.commit({BASELINE: grown}, "use the worker from common")
        self.assert_fails(grown, "THE COUPLING BASELINE GREW", "module-edge common->worker added", rng=f"{self.base}..{head}")

    def test_a_grown_baseline_with_a_trailer_passes(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock\n\nCoupling: the clock is the one seam models needs")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_a_trailer_on_a_commit_not_changing_the_baseline_does_not_count(self):
        grown = rows("cycle-edge\tmodels->tools\t1")
        first = self.commit({BASELINE: grown}, "a cycle")
        head = self.commit({"README": "x\n"}, "docs\n\nCoupling: not this commit's")
        self.assert_fails(grown, "cycle-edge models->tools added", rng=f"{self.base}..{head}")
        self.assertTrue(first)

    def test_an_empty_trailer_does_not_count(self):
        grown = rows("fanout\tworker.Wiring\t31")
        head = self.commit({BASELINE: grown}, "wiring\n\nCoupling: ")
        self.assert_fails(grown, "fanout worker.Wiring raised <30 -> 31", rng=f"{self.base}..{head}")

    def test_a_raised_ce_row_without_a_trailer_fails(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock")
        self.assert_fails(grown, "ce models raised 12 -> 14", rng=f"{self.base}..{head}")

    def test_a_raised_limit_is_growth(self):
        grown = rows("limit\tfanout\t40", drop=("limit\tfanout",))
        head = self.commit({BASELINE: grown}, "looser")
        self.assert_fails(grown, "limit fanout raised 30 -> 40", rng=f"{self.base}..{head}")

    def test_a_new_package_row_within_the_limit_is_no_growth(self):
        grown = rows("ce\tservices.fresh\t4")
        head = self.commit({BASELINE: grown}, "a new package")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_a_shrunk_baseline_needs_no_trailer(self):
        shrunk = rows("ce\tmodels\t5", drop=("ce\tmodels", "cycle-edge"))
        head = self.commit({BASELINE: shrunk}, "models uses less")
        self.assert_passes(shrunk, rng=f"{self.base}..{head}")

    def test_a_malformed_row_is_an_error_not_a_pass(self):
        rc, out = self.check(ROWS + "ce\tmodels\n")
        self.assertEqual(2, rc, out)


if __name__ == "__main__":
    unittest.main()
