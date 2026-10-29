#!/usr/bin/env python3
"""convergence_reuse.py: main reuses a run's convergence verdict only when it provably applies -- the same tree key, pairs
and takes bases, main's inputs (both order phases among them), and every row main runs green in the source. Each guard
has a case here that fails without it.

    python3 scripts/ci/convergence_reuse_test.py
"""
import copy
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import convergence_reuse as reuse  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
with open(os.path.join(ROOT, reuse.WORKFLOW)) as f:
    WORKFLOW = f.read()
ROWS = reuse.expected_rows(WORKFLOW)
MAIN_INPUTS = reuse.leg_inputs({})
PAIRS = {"pl": "573 2026-10-04 enrichment-pl-573.tar.zst fill-pl-573-2783-1-convergence.tar.zst"}


def green_jobs(rows=ROWS):
    callers = {"leg " + r[r.index("("):] for r in rows}
    return [{"name": n, "status": "completed", "conclusion": "success"}
            for n in ["preflight", "timings"] + list(rows) + sorted(callers)]


def source(**over):
    run = {"id": 2821, "run_number": 1763, "path": ".github/workflows/identity-model-convergence.yml",
           "status": "completed", "conclusion": "success", "head_sha": "a" * 40}
    run.update(over)
    return run


def facts(**over):
    out = {"version": 1, "key": "k" * 64, "pairs": dict(PAIRS), "inputs": dict(MAIN_INPUTS), "order_phases": "both",
           "reused_from": ""}
    out.update(over)
    return out


def mine(**over):
    out = {"key": "k" * 64, "pairs": dict(PAIRS), "inputs": dict(MAIN_INPUTS), "order_phases": "both",
           "expected_rows": ROWS, "takes_bases": {"pl": ("b" * 40, "b" * 40, True)}}
    out.update(over)
    return out


class JudgeTest(unittest.TestCase):
    def assertReused(self, why):
        self.assertEqual(why, [])

    def assertNotReused(self, why, fragment):
        self.assertTrue(any(fragment in w for w in why), f"no reason mentioning {fragment!r} in {why}")

    def test_a_green_full_run_over_the_same_tree_pairs_inputs_and_bases_is_reused(self):
        self.assertReused(reuse.judge(source(), green_jobs(), facts(), mine()))

    def test_a_different_tree_is_not(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(key="x" * 64), mine()), "the tree differs")

    def test_a_p01_only_source_is_not_by_its_facts(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(order_phases="one"), mine()),
                             "order-independence phases 'one'")

    def test_a_p01_only_source_is_not_by_its_rows_either(self):
        rows = [r for r in ROWS if not r.startswith("order-independence-p12")]
        self.assertNotReused(reuse.judge(source(), green_jobs(rows), facts(), mine()),
                             "`order-independence-p12 (germany)` did not run there")

    def test_a_red_source_is_not(self):
        self.assertNotReused(reuse.judge(source(conclusion="failure"), green_jobs(), facts(), mine()),
                             "not a green completed run")

    def test_a_source_still_running_is_not(self):
        self.assertNotReused(reuse.judge(source(status="in_progress", conclusion=None), green_jobs(), facts(), mine()),
                             "not a green completed run")

    def test_a_red_row_is_not_even_when_the_run_reads_green(self):
        jobs = green_jobs()
        next(j for j in jobs if j["name"] == "takes (spain)")["conclusion"] = "failure"
        self.assertNotReused(reuse.judge(source(), jobs, facts(), mine()), "`takes (spain)` ended")

    def test_a_skipped_row_is_no_verdict(self):
        jobs = green_jobs()
        next(j for j in jobs if j["name"] == "convergence (poland)")["conclusion"] = "skipped"
        self.assertNotReused(reuse.judge(source(), jobs, facts(), mine()), "`convergence (poland)` ended")

    def test_a_partial_source_is_not_takes_only(self):
        rows = [r for r in ROWS if r.startswith("takes")]
        inputs = dict(MAIN_INPUTS, **{"takes-only": "true"})
        why = reuse.judge(source(), green_jobs(rows), facts(inputs=inputs), mine())
        self.assertNotReused(why, "takes-only")
        self.assertNotReused(why, "`convergence (poland)` did not run there")

    def test_a_drift_only_source_is_never_mains_verdict(self):
        # scripts/identity-drift-ci's pre-land run: the takes-drift rows alone, even when every one of them passed
        rows = [r for r in ROWS if r.startswith("takes-drift")]
        inputs = dict(MAIN_INPUTS, **{"drift-only": "true"})
        why = reuse.judge(source(), green_jobs(rows), facts(inputs=inputs), mine())
        self.assertNotReused(why, "drift-only")
        self.assertNotReused(why, "`takes (poland)` did not run there")
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(inputs=dict(MAIN_INPUTS, **{"drift-seeds": "2"})),
                                         mine()), "drift-seeds")

    def test_a_partial_source_is_not_some_countries(self):
        inputs = dict(MAIN_INPUTS, **{"takes-countries": "pl"})
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(inputs=inputs), mine()), "takes-countries")

    def test_a_missing_leg_caller_is_not(self):
        jobs = [j for j in green_jobs() if j["name"] != "leg (spain)"]
        self.assertNotReused(reuse.judge(source(), jobs, facts(), mine()), "`leg (spain)` did not run there")

    def test_a_named_recording_runner_or_jvm_options_is_not_mains_suite(self):
        for name in ("recording", "runner", "jvm-options"):
            inputs = dict(MAIN_INPUTS, **{name: "x"})
            self.assertNotReused(reuse.judge(source(), green_jobs(), facts(inputs=inputs), mine()), name)

    def test_the_takes_skip_gate_enforced_is_not_main_shadow_or_off_is(self):
        enforce = dict(MAIN_INPUTS, **{"takes-skip": "enforce"})
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(inputs=enforce), mine()), "takes-skip")
        off = dict(MAIN_INPUTS, **{"takes-skip": "off"})
        self.assertReused(reuse.judge(source(), green_jobs(), facts(inputs=off), mine()))

    def test_no_store_and_a_profile_change_no_verdict(self):
        inputs = dict(MAIN_INPUTS, **{"takes-no-store": "true", "profile": "true"})
        self.assertReused(reuse.judge(source(), green_jobs(), facts(inputs=inputs), mine()))

    def test_a_moved_pair_or_fill_is_not(self):
        pairs = {"pl": PAIRS["pl"] + " fill-pl-573-2900-1-convergence.tar.zst"}
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(pairs=pairs), mine()), "moved since: pl")

    def test_unreadable_pairs_are_not(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(pairs=None), mine()), "moved since")

    def test_another_takes_base_is_not(self):
        bases = {"pl": ("c" * 40, "b" * 40, False)}
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(), mine(takes_bases=bases)),
                             "judged against ccccccccc there")

    def test_an_unknown_takes_base_is_not(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(), mine(takes_bases={"pl": (None, "b", False)})),
                             "base is unknown")
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(), mine(takes_bases={"pl": ("b", None, False)})),
                             "main's base is unknown")

    def test_a_reuse_run_is_never_a_source(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(reused_from="2700"), mine()), "reused run 2700")

    def test_a_run_with_no_facts_is_not(self):
        self.assertNotReused(reuse.judge(source(), green_jobs(), None, mine()), "no reuse facts")

    def test_another_workflow_is_not(self):
        self.assertNotReused(reuse.judge(source(path=".github/workflows/identity-run.yml"), green_jobs(), facts(), mine()),
                             "no identity-model-convergence.yml run")

    def test_this_run_asking_for_less_than_mains_suite_reuses_nothing(self):
        inputs = dict(MAIN_INPUTS, **{"takes-only": "true"})
        self.assertNotReused(reuse.judge(source(), green_jobs(), facts(), mine(inputs=inputs)), "this run: takes-only")


class RowsTest(unittest.TestCase):
    def test_main_runs_every_country_with_both_order_phases_next_day_and_takes(self):
        self.assertEqual(len(ROWS), 27)
        for row in ("convergence (poland)", "takes (spain)", "order-independence-p01 (germany)",
                    "order-independence-p12 (united-kingdom)", "next-day (united-states)",
                    "order-independence-p12 (united-states)"):
            self.assertIn(row, ROWS)
        self.assertNotIn("order-independence-p01 (poland)", ROWS)


class InputsTest(unittest.TestCase):
    def test_a_dispatch_naming_nothing_hands_the_legs_mains_inputs(self):
        self.assertEqual(reuse.inputs_unlike_main(reuse.leg_inputs({}), "both"), [])

    def test_forgejos_string_booleans_and_defaults_are_normalised(self):
        legs = reuse.leg_inputs({"INPUT_TAKES_ONLY": "true", "INPUT_PROFILE": "", "INPUT_TAKES_SKIP": "",
                                 "INPUT_DRIFT_ONLY": "true"})
        self.assertEqual((legs["takes-only"], legs["profile"], legs["takes-skip"], legs["drift-only"], legs["drift-seeds"]),
                         ("true", "false", "shadow", "true", ""))


SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")


class KeyTest(unittest.TestCase):
    """Over a scratch repository made through scripts/scratch-git.sh -- never a raw git, which under the pre-push hook
    inherits its GIT_DIR and writes into the repository every worktree shares. key() reads it with every GIT_* variable
    of this process dropped, for the same reason."""

    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def git(self, *args):
        return self.scratch("scratch_git", *args)

    def commit(self, files):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.git("add", "-A")
        self.git("commit", "-qm", "c")
        return self.git("rev-parse", "HEAD")

    def test_a_change_outside_the_listed_paths_keeps_the_key_one_inside_moves_it(self):
        paths = "# what can move a verdict\nworker/src/main/**\n!worker/src/main/Ignored.scala\n.github/convergence-paths.txt\n"
        base = self.commit({".github/convergence-paths.txt": paths, "worker/src/main/A.scala": "a",
                            "docs/x.md": "x", "worker/src/main/Ignored.scala": "i"})
        docs = self.commit({"docs/x.md": "y", "worker/src/main/Ignored.scala": "j"})
        code = self.commit({"worker/src/main/A.scala": "b"})
        listing = self.commit({".github/convergence-paths.txt": paths + "docs/**\n"})
        k = [reuse.key(c, cwd=self.repo) for c in (base, docs, code, listing)]
        self.assertEqual(k[0], k[1])
        self.assertNotEqual(k[1], k[2])
        self.assertNotEqual(k[2], k[3])

    def test_a_file_mode_change_moves_the_key(self):
        base = self.commit({".github/convergence-paths.txt": "scripts/**\n", "scripts/a.sh": "echo"})
        self.git("update-index", "--chmod=+x", "scripts/a.sh")
        self.git("commit", "-qm", "x")
        self.assertNotEqual(reuse.key(base, cwd=self.repo), reuse.key("HEAD", cwd=self.repo))

    def test_patterns_read_as_matches_sh_reads_them(self):
        patterns = reuse.patterns_of("# c\n\n- 'web/**'\n\"build.sbt\"\n!web/x\n")
        self.assertEqual(patterns, ["web/**", "build.sbt", "!web/x"])
        self.assertTrue(reuse.listed("web/a", patterns))
        self.assertFalse(reuse.listed("webhook.md", patterns))
        self.assertFalse(reuse.listed("web/x", patterns))
        self.assertTrue(reuse.listed("build.sbt", patterns))


class NoFalseGreen(unittest.TestCase):
    """G7: a run whose takes or takes-drift rows did not all decide is no green, unless a proven reuse names its source."""

    def test_every_row_decided_is_complete(self):
        self.assertEqual(reuse.incomplete(green_jobs(), ROWS), [])

    def test_a_red_takes_row_is_decided(self):
        jobs = [dict(j, conclusion="failure") if j["name"] == "takes-drift-2 (germany)" else j for j in green_jobs()]
        self.assertEqual(reuse.incomplete(jobs, ROWS), [])

    def test_a_skipped_takes_row_is_no_verdict(self):
        jobs = [dict(j, conclusion="skipped") if j["name"] == "takes (united-kingdom)" else j for j in green_jobs()]
        self.assertEqual(len(reuse.incomplete(jobs, ROWS)), 1)

    def test_a_missing_or_cancelled_drift_row_is_no_verdict(self):
        jobs = [j for j in green_jobs() if j["name"] != "takes-drift-1 (poland)"]
        jobs = [dict(j, conclusion="cancelled") if j["name"] == "takes-drift-2 (spain)" else j for j in jobs]
        self.assertEqual(len(reuse.incomplete(jobs, ROWS)), 2)

    def test_other_rows_skipped_are_not_this_checks(self):
        jobs = [dict(j, conclusion="skipped") if j["name"].startswith("next-day") else j for j in green_jobs()]
        self.assertEqual(reuse.incomplete(jobs, ROWS), [])

    def test_a_proven_reuse_is_complete_with_every_row_skipped(self):
        jobs = [dict(j, conclusion="skipped") for j in green_jobs()]
        self.assertEqual(reuse.incomplete(jobs, ROWS, reused_from="2821"), [])
        self.assertNotEqual(reuse.incomplete(jobs, ROWS), [])

    def test_legs_handed_less_than_mains_suite_are_no_verdict(self):
        self.assertNotEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS, **{"takes-only": "true"})), [])
        self.assertEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS)), [])
        # the skip gate enforced still runs every takes row: its proof stands, the run decides
        self.assertEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS, **{"takes-skip": "enforce"})), [])


if __name__ == "__main__":
    unittest.main()
