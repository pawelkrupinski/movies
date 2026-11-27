#!/usr/bin/env python3
"""js_quality_ratchet.py: a production row past its baseline row fails, and so does a row-less production key past its
kind's limit (a NEW function, file, clone set or cycle over a limit) and a measure whose limit rose past the baseline's;
a row of a test, a page test or a script (outside ratchet-areas.tsv's paths) is never held; a fall passes, is said, and
`update` records it (tests' rows left out); a baseline grown in a range passes only when the commit changing it carries
the kind's trailer (`Complexity:`, `Length:`, `Duplication:`, `Coupling:`) in its final trailer block.
JS_QUALITY_RATCHET_UNDER_TEST names another copy of the tool (js-quality-ratchet-mutation-test.sh's mutants)."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("JS_QUALITY_RATCHET_UNDER_TEST") or os.path.join(HERE, "js_quality_ratchet.py")
BASELINE = "test/resources/js-quality-baseline.tsv"
WEB = "web/src/main/assets/js"
DASH = "infra/version-dashboard/src"
AREAS = f"web-js\tglobals\t{WEB}\nversion-dashboard\timports\t{DASH}\n"

ROWS = f"""# a comment
limit\tcognitive\t10
limit\tcyclomatic\t10
limit\tinner-defs\t2
limit\tmethod-lines\t50
limit\tfile-lines\t500
limit\tcopies\t0
limit\tclone-tokens\t50
limit\tcycle\t0
cognitive\t{WEB}/a.js:render\t15
method-lines\t{DASH}/server.ts:main\t81
file-lines\t{WEB}/a.js\t578
copies\t{WEB}/a.js\t2
cycle\t{WEB}/a.js|{WEB}/b.js\t2
"""
TEST_ROWS = ("method-lines\tpage-tests-playwright/tests/x.spec.ts:test.describe('x') callback\t300\n"
             "cognitive\tscripts/js-quality/report.js:overLimitRows\t30\n"
             "copies\tinfra/version-dashboard/test/a.test.ts\t4\n")


def rows(*extra: str, drop: tuple = ()) -> str:
    kept = [line for line in ROWS.splitlines() if not any(line.startswith(d) for d in drop)]
    return "\n".join(kept + list(extra)) + "\n"


class JsQualityRatchetTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.areas = os.path.join(self.dir.name, "ratchet-areas.tsv")
        with open(self.areas, "w") as f:
            f.write(AREAS)
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

    def run_tool(self, command, current, *extra):
        path = os.path.join(self.dir.name, "current.tsv")
        with open(path, "w") as f:
            f.write(current)
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, command, "--current", path, "--areas", self.areas, *extra],
                              cwd=self.repo, env=env, capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def check(self, current, rng=None):
        return self.run_tool("check", current, *(["--range", rng] if rng else []))

    def assert_fails(self, current, *said, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(1, rc, out)
        for s in said:
            self.assertIn(s, out)

    def assert_passes(self, current, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(0, rc, out)
        return out

    def baseline(self):
        with open(os.path.join(self.repo, BASELINE)) as f:
            return f.read()

    # ── the measured rows against the baseline ──

    def test_the_baseline_itself_passes(self):
        self.assertIn("js quality: within", self.assert_passes(ROWS))

    def test_a_new_function_over_a_limit_fails(self):
        self.assert_fails(rows(f"cyclomatic\t{WEB}/c.js:filter\t12"),
                          f"cyclomatic {WEB}/c.js:filter: 12, past the limit 10 (no baseline row)")

    def test_a_new_dashboard_function_over_a_limit_fails(self):
        self.assert_fails(rows(f"inner-defs\t{DASH}/mobile/view.ts:platformBlock\t3"),
                          f"inner-defs {DASH}/mobile/view.ts:platformBlock: 3, past the limit 2")

    def test_a_frozen_row_rising_fails(self):
        self.assert_fails(rows(f"cognitive\t{WEB}/a.js:render\t16", drop=("cognitive",)),
                          f"cognitive {WEB}/a.js:render: 16, past its baseline 15")

    def test_a_frozen_row_at_its_value_passes(self):
        self.assert_passes(rows())

    def test_a_new_file_clone_and_cycle_fail(self):
        self.assert_fails(rows(f"file-lines\t{WEB}/big.js\t501", f"copies\t{WEB}/a.js|{WEB}/c.js\t2",
                               f"cycle\t{WEB}/c.js|{WEB}/d.js\t2"),
                          f"file-lines {WEB}/big.js: 501", f"copies {WEB}/a.js|{WEB}/c.js: 2", f"cycle {WEB}/c.js|{WEB}/d.js: 2")

    def test_a_clone_set_gaining_a_copy_fails(self):
        self.assert_fails(rows(f"copies\t{WEB}/a.js\t3", drop=("copies",)), f"copies {WEB}/a.js: 3, past its baseline 2")

    def test_fan_out_and_distance_are_the_design_rules_not_this_ratchets(self):
        self.assert_passes(rows(f"fanout\t{WEB}/boot.js\t99", f"distance\t{DASH}\t0.9", "limit\tfanout\t9"))

    def test_a_measure_whose_limit_rose_fails(self):
        self.assert_fails(rows("limit\tclone-tokens\t60", drop=("limit\tclone-tokens",)),
                          "the measure's clone-tokens limit 60 is past the baseline's 50")

    def test_a_missing_limit_is_an_error(self):
        rc, out = self.check(rows(drop=("limit\tclone-tokens",)))
        self.assertEqual(2, rc, out)
        self.assertIn("no `limit<TAB>clone-tokens<TAB>N` row", out)

    def test_a_malformed_row_is_an_error(self):
        rc, out = self.check(rows("cognitive\tno value"))
        self.assertEqual(2, rc, out)

    # ── tests, page tests, Playwright and scripts are reported, never held ──

    def test_rows_of_tests_and_scripts_are_not_gated(self):
        out = self.assert_passes(rows(TEST_ROWS.rstrip("\n")))
        self.assertIn("5 production rows", out)

    def test_update_leaves_tests_and_scripts_out_of_the_baseline(self):
        rc, out = self.run_tool("update", rows(TEST_ROWS.rstrip("\n")))
        self.assertEqual(0, rc, out)
        self.assertNotIn("page-tests-playwright", self.baseline())
        self.assertNotIn("overLimitRows", self.baseline())
        self.assertNotIn("version-dashboard/test", self.baseline())

    # ── a moved source keeps its rows ──

    def move_a_js(self):
        """a.js added, then renamed to moved.js by git (content unchanged): the base the rename tests start from."""
        body = "".join(f"const line{i} = {i};\n" for i in range(40))
        self.commit({f"{WEB}/a.js": body}, "add a.js")
        self.scratch("scratch_git", "mv", f"{WEB}/a.js", f"{WEB}/moved.js")
        self.scratch("scratch_git", "commit", "-qm", "move a.js")

    def test_a_row_follows_its_file_renamed_at_the_same_value(self):
        self.move_a_js()
        self.assert_passes(rows(f"cognitive\t{WEB}/moved.js:render\t15", drop=("cognitive",)))

    def test_a_renamed_files_row_rising_still_fails(self):
        self.move_a_js()
        self.assert_fails(rows(f"cognitive\t{WEB}/moved.js:render\t16", drop=("cognitive",)),
                          f"cognitive {WEB}/moved.js:render: 16, past the limit 10 (no baseline row)")

    def test_a_same_named_function_in_a_file_git_never_renamed_is_new(self):
        self.commit({f"{WEB}/other.js": "const x = 1;\n"}, "an unrelated file")
        self.assert_fails(rows(f"cognitive\t{WEB}/other.js:render\t15", drop=("cognitive",)),
                          f"cognitive {WEB}/other.js:render: 15, past the limit 10 (no baseline row)")

    def test_a_baseline_rekeyed_by_a_rename_needs_no_trailer(self):
        self.move_a_js()
        moved = rows(f"cognitive\t{WEB}/moved.js:render\t15", drop=("cognitive",))
        head = self.commit({BASELINE: moved}, "the baseline follows the rename")
        self.assert_passes(moved, rng=f"{self.base}..{head}")

    def test_a_baseline_row_raised_in_a_rename_needs_its_trailer(self):
        self.move_a_js()
        raised = rows(f"cognitive\t{WEB}/moved.js:render\t16", drop=("cognitive",))
        head = self.commit({BASELINE: raised}, "the baseline rises with the rename")
        self.assert_fails(raised, "THE JS/TS QUALITY BASELINE GREW", rng=f"{self.base}..{head}")

    # ── falls ──

    def test_a_fall_passes_is_said_and_update_records_it(self):
        fell = rows(f"cognitive\t{WEB}/a.js:render\t12", drop=("cognitive",))
        out = self.assert_passes(fell)
        self.assertIn("js quality fell (1 rows", out)
        self.assertIn(f"cognitive {WEB}/a.js:render 15 -> 12", out)
        rc, out = self.run_tool("update", fell)
        self.assertEqual(0, rc, out)
        written = self.baseline()
        self.assertTrue(written.startswith("# THE JS/TS QUALITY BASELINE"))
        self.assertIn(f"cognitive\t{WEB}/a.js:render\t12\n", written)
        self.assertNotIn("fanout", written)
        self.assertNotIn("distance", written)
        self.assertEqual(0, self.check(fell)[0])

    def test_a_shrunk_baseline_needs_no_trailer(self):
        fell = rows(drop=("cognitive",))
        head = self.commit({BASELINE: fell}, "render gets simpler")
        self.assert_passes(fell, rng=f"{self.base}..{head}")

    # ── a grown baseline and its trailer ──

    def test_a_grown_baseline_without_a_trailer_fails(self):
        grown = rows(f"cognitive\t{WEB}/c.js:filter\t12")
        head = self.commit({BASELINE: grown}, "a hairy filter")
        self.assert_fails(grown, "THE JS/TS QUALITY BASELINE GREW", "no `Complexity: <why>` trailer",
                          f"cognitive {WEB}/c.js:filter raised <=10 -> 12", rng=f"{self.base}..{head}")

    def test_a_trailer_admits_a_rise(self):
        grown = rows(f"cognitive\t{WEB}/c.js:filter\t12")
        head = self.commit({BASELINE: grown}, "a hairy filter\n\nComplexity: the URL grammar's one parser\n"
                                              "Co-Authored-By: A <a@b.c>")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_each_kind_needs_its_own_trailer(self):
        grown = rows(f"cycle\t{WEB}/c.js|{WEB}/d.js\t2")
        head = self.commit({BASELINE: grown}, "a cycle\n\nComplexity: not the coupling one")
        self.assert_fails(grown, "no `Coupling: <why>` trailer", f"cycle {WEB}/c.js|{WEB}/d.js raised <=0 -> 2",
                          rng=f"{self.base}..{head}")
        head = self.commit({BASELINE: grown + "\n"}, "the cycle again\n\nCoupling: auth and overlays share one modal")
        self.assert_fails(grown, "no `Coupling: <why>` trailer", rng=f"{self.base}..{head}")  # the first commit's rise
        self.assert_passes(grown, rng=f"{head}~1..{head}")

    def test_a_trailer_outside_the_final_block_does_not_count(self):
        grown = rows(f"method-lines\t{WEB}/c.js:build\t60")
        head = self.commit({BASELINE: grown}, "a long builder\n\nLength: the panel's one template\n\n"
                                              "Co-Authored-By: A <a@b.c>")
        self.assert_fails(grown, "no `Length: <why>` trailer", "not in the final trailer block", rng=f"{self.base}..{head}")

    def test_a_raised_limit_is_growth(self):
        raised = rows("limit\tmethod-lines\t60", drop=("limit\tmethod-lines",))
        head = self.commit({BASELINE: raised}, "longer functions")
        self.assert_fails(raised, "limit method-lines raised 50 -> 60", rng=f"{self.base}..{head}")

    def test_a_grown_test_row_in_the_baseline_is_no_growth(self):
        grown = rows(TEST_ROWS.rstrip("\n"))
        head = self.commit({BASELINE: grown}, "a hand-added test row")
        self.assert_passes(grown, rng=f"{self.base}..{head}")


if __name__ == "__main__":
    unittest.main()
