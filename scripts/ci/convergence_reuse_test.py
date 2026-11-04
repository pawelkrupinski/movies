#!/usr/bin/env python3
"""convergence_reuse.py: main reuses a run's convergence verdict only when it provably applies -- the same tree key, pairs
and takes bases, main's inputs (both order phases among them), and every row main runs green in the source. Each guard
has a case here that fails without it.

    python3 scripts/ci/convergence_reuse_test.py
"""
import copy
import json
import os
import re
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
        # 34 until 2026-11-02, when each of the four split countries' order passes took a row of its own (+4 each), the
        # two comparisons keeping the two-pass rows' names
        self.assertEqual(len(ROWS), 50)
        for p in ("p01-pass0", "p01-pass1", "p12-pass1", "p12-pass2"):
            for country in ("poland", "germany", "united-kingdom", "united-states"):
                self.assertIn(f"order-independence-{p} ({country})", ROWS)
        self.assertNotIn("order-independence-p01-pass0 (spain)", ROWS)
        for row in ("convergence (poland)", "takes (spain)", "takes-base (spain)", "order-independence-p01 (germany)",
                    "order-independence-p12 (united-kingdom)", "next-day (united-states)",
                    "order-independence-p12 (united-states)"):
            self.assertIn(row, ROWS)
        self.assertIn("order-independence-p01 (poland)", ROWS)
        self.assertNotIn("order-independence (poland)", ROWS)


class InputsTest(unittest.TestCase):
    def test_a_dispatch_naming_nothing_hands_the_legs_mains_inputs(self):
        self.assertEqual(reuse.inputs_unlike_main(reuse.leg_inputs({}), "both"), [])

    def test_forgejos_string_booleans_and_defaults_are_normalised(self):
        legs = reuse.leg_inputs({"INPUT_TAKES_ONLY": "true", "INPUT_PROFILE": "", "INPUT_TAKES_SKIP": "",
                                 "INPUT_DRIFT_ONLY": "true"})
        self.assertEqual((legs["takes-only"], legs["profile"], legs["takes-skip"], legs["drift-only"], legs["drift-seeds"]),
                         ("true", "false", "shadow", "true", ""))


class LegMatrixTest(unittest.TestCase):
    """The `leg` matrix is the preflight's (cmd_legs): only the countries a run starts get a row, so none is skipped."""
    ROWS = """matrix:
        include:
            - { country: poland,        code: pl, job: 55, heap: 7g, mongo-cache-gb: 0.5, order: '', order-split: false, mongo-tmpfs-order: 'true' }
            - { country: germany,       code: de, job: 55, heap: 4g, mongo-cache-gb: 2, order: convergenceGermanyOrder, order-split: true }
            - { country: united-states, code: us, job: 55, heap: 10g }
    """

    def test_rows_keep_the_types_yaml_gives_them(self):
        rows = reuse.parse_rows(self.ROWS)
        self.assertEqual(rows[0], {"country": "poland", "code": "pl", "job": 55, "heap": "7g", "mongo-cache-gb": 0.5,
                                   "order": "", "order-split": False, "mongo-tmpfs-order": "true"})
        self.assertIs(rows[1]["order-split"], True)

    def test_a_full_run_starts_every_leg(self):
        m = reuse.leg_matrix(reuse.parse_rows(self.ROWS), reuse.leg_inputs({}))
        self.assertEqual(m["country"], ["poland", "germany", "united-states"])
        self.assertEqual(len(m["include"]), 3)

    def test_a_narrowed_takes_or_drift_run_starts_only_its_countries(self):
        for flag in ("INPUT_TAKES_ONLY", "INPUT_DRIFT_ONLY"):
            m = reuse.leg_matrix(reuse.parse_rows(self.ROWS), reuse.leg_inputs({flag: "true", "INPUT_TAKES_COUNTRIES": "pl, us"}))
            self.assertEqual(m["country"], ["poland", "united-states"])
            self.assertEqual([r["code"] for r in m["include"]], ["pl", "us"])

    def test_takes_countries_means_nothing_to_a_run_that_is_neither(self):
        m = reuse.leg_matrix(reuse.parse_rows(self.ROWS), reuse.leg_inputs({"INPUT_TAKES_COUNTRIES": "pl"}))
        self.assertEqual(len(m["country"]), 3)

    def test_the_workflows_own_rows_all_parse_with_the_keys_the_legs_read(self):
        import pathlib
        text = (pathlib.Path(__file__).resolve().parents[2] / ".github/workflows/identity-model-convergence.yml").read_text()
        rows = reuse.parse_rows(text.split("LEG_ROWS: |", 1)[1].split("run:", 1)[0])
        self.assertEqual([r["code"] for r in rows], ["pl", "de", "uk", "es", "us"])
        for r in rows:
            self.assertIsInstance(r["order-split"], bool)
            self.assertIsInstance(r["job"], int)


class RotatingSeedsTest(unittest.TestCase):
    """ROTATING SEEDS (drift_seeds.py): the legs are handed the run's two takes-drift seeds, or a dispatch's pins; a reuse
    carries the SOURCE's seeds by name, never this run's as if equal."""

    def run_inputs(self, env):
        out = os.path.join(tempfile.mkdtemp(), "legs.json")
        res = subprocess.run([sys.executable, os.path.join(ROOT, "scripts/ci/convergence_reuse.py"), "inputs"],
                             env=dict(os.environ, LEG_INPUTS_FILE=out, GITHUB_STEP_SUMMARY="", **env),
                             capture_output=True, text=True)
        lines = dict(line.split("=", 1) for line in res.stdout.splitlines() if "=" in line)
        if not os.path.exists(out):
            return res.returncode, lines, None
        with open(out) as f:
            return res.returncode, lines, json.load(f)

    def test_main_hands_the_legs_no_pin_so_each_row_replays_its_fixed_set(self):
        # FIXED SEEDS (2026-11-04): no seed moves with the run number; an unpinned row replays drift-gate-seeds.tsv's set
        code, lines, legs = self.run_inputs({"GITHUB_RUN_NUMBER": "2"})
        self.assertEqual((code, lines["drift-seed-1"], lines["drift-seed-2"]), (0, "", ""))
        self.assertEqual((legs["drift-seed-1"], legs["drift-seed-2"]), ("", ""))
        # the seeds are no input main lacks: main's own run is main's suite
        self.assertEqual(reuse.inputs_unlike_main(legs, "both"), [])

    def test_a_drift_only_dispatch_pins_one_row(self):
        code, lines, _ = self.run_inputs({"GITHUB_RUN_NUMBER": "2", "INPUT_DRIFT_ONLY": "true", "INPUT_DRIFT_SEEDS": ",37"})
        self.assertEqual((code, lines["drift-seed-1"], lines["drift-seed-2"]), (0, "", "37"))

    def test_malformed_pins_stop_the_run(self):
        self.assertEqual(self.run_inputs({"GITHUB_RUN_NUMBER": "2", "INPUT_DRIFT_SEEDS": "x"})[0], 2)

    def test_the_legs_row_expression_picks_the_pinned_row(self):
        self.assertIn("inputs.drift-seed-1 == '' && inputs.drift-seed-2 != '' && '[\"takes-drift-2\"]'",
                      reuse.PHASES_EXPRESSION)

    def test_a_runs_seeds_by_its_facts(self):
        self.assertEqual(reuse.seeds_of(dict(MAIN_INPUTS, **{"drift-seed-1": "37", "drift-seed-2": "38"})), "37,38")
        # a run from before the rotation replayed 1 and 2
        self.assertEqual(reuse.seeds_of(dict(MAIN_INPUTS)), "1,2")

    def test_a_reusing_runs_facts_name_no_seed_of_its_own(self):
        tmp = tempfile.mkdtemp()
        legs = os.path.join(tmp, "legs.json")
        with open(legs, "w") as f:
            json.dump(dict(MAIN_INPUTS, **{"drift-seed-1": "3", "drift-seed-2": "4"}), f)
        saved = (reuse.workflow_at, reuse.key, reuse.pairs)
        reuse.workflow_at, reuse.key, reuse.pairs = (lambda sha: WORKFLOW), (lambda sha: "k" * 64), (lambda wf: dict(PAIRS))
        try:
            import contextlib
            import io
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                reuse.cmd_facts({"sha": "a" * 40, "inputs": legs, "reused-from": "2821"})
            facts = json.loads(out.getvalue())
        finally:
            reuse.workflow_at, reuse.key, reuse.pairs = saved
        self.assertNotIn("drift-seed-1", facts["inputs"])
        self.assertEqual(facts["drift_seeds_from"], "2821")


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


class ListedInputs(unittest.TestCase):
    """The key and Main's kick read .github/convergence-paths.txt: every input a takes or takes-drift row reads is listed
    there, and every listed path exists -- an unlisted input lets a run's verdict be reused over a changed one (a false
    green) and leaves a push touching only it without a verdict at all."""

    CHECKS = ("scripts/ci/identity-drift-check.sh", "scripts/ci/identity-takes-check.sh")

    def setUp(self):
        with open(os.path.join(ROOT, reuse.PATHS_FILE)) as f:
            self.patterns = reuse.patterns_of(f.read())

    def inputs(self):
        out = set()
        for check in self.CHECKS:
            with open(os.path.join(ROOT, check)) as f:
                text = f.read()
            # the recorded corpus is the pinned pair's (hermetic-<code>.txt), not the tree's
            out |= {"test/resources/fixtures/" + m.replace("$cc", "pl")
                    for m in re.findall(r"\$fixtures/([A-Za-z0-9_./$-]+)", text) if not m.startswith("corpus")}
            for config, cls in re.findall(r"worker/(Test|IntegrationTest)/runMain ([A-Za-z.]+)", text):
                # an IntegrationTest main may live in the test tree, which its classpath holds
                trees = ("test",) if config == "Test" else ("it", "test")
                files = [f"worker/src/{t}/scala/" + cls.replace("integration.", "").replace(".", "/") + ".scala" for t in trees]
                out.add(next((f for f in files if os.path.exists(os.path.join(ROOT, f))), files[0]))
        return out

    def test_every_input_of_the_takes_and_drift_rows_is_listed(self):
        inputs = self.inputs()
        self.assertIn("worker/src/test/scala/scripts/IdentityDrift.scala", inputs)
        self.assertEqual([p for p in sorted(inputs) if not reuse.listed(p, self.patterns)], [])

    def test_every_listed_path_exists(self):
        tracked = reuse.git("ls-files").splitlines()
        missing = [p for p in self.patterns if not p.startswith("!") and not any(reuse._under(t, p) for t in tracked)]
        self.assertEqual(missing, [])


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

    def test_github_com_names_a_called_row_after_its_caller(self):
        # github.com: `leg (poland) / takes (poland)`, and no caller job (run 41215027020's verdict read every row absent)
        jobs = [dict(j, name=f"leg {j['name'][j['name'].index('('):]} / {j['name']}") if j["name"] in ROWS else j
                for j in green_jobs() if not j["name"].startswith("leg ")]
        self.assertEqual(reuse.incomplete(jobs, ROWS), [])
        jobs = [dict(j, conclusion="skipped") if j["name"].endswith("/ takes (united-kingdom)") else j for j in jobs]
        self.assertEqual(len(reuse.incomplete(jobs, ROWS)), 1)

    def test_github_com_callers_take_their_worst_row(self):
        jobs = [{"name": "leg (poland) / takes (poland)", "status": "completed", "conclusion": "success"},
                {"name": "leg (poland) / convergence (poland)", "status": "completed", "conclusion": "failure"}]
        self.assertEqual(reuse.jobs_by_name(jobs)["leg (poland)"]["conclusion"], "failure")
        self.assertEqual(reuse.jobs_by_name(jobs)["takes (poland)"]["conclusion"], "success")

    def test_drift_rows_of_a_recorded_github_com_run(self):
        """Run 41250027249 (a drift-only dispatch): github.com's `leg (germany) / takes-drift-1 (germany)` rows."""
        with open(os.path.join(ROOT, "scripts/ci/testdata/github-drift-run-41250027249-jobs.json")) as f:
            jobs = json.load(f)["jobs"]
        self.assertEqual(sorted(reuse.drift_rows(jobs)), [
            "takes-drift-1 (germany)\tcompleted\tfailure", "takes-drift-1 (united-kingdom)\tcompleted\tsuccess",
            "takes-drift-2 (germany)\tcompleted\tsuccess", "takes-drift-2 (united-kingdom)\tcompleted\tfailure"])

    def test_a_proven_reuse_is_complete_with_every_row_skipped(self):
        jobs = [dict(j, conclusion="skipped") for j in green_jobs()]
        self.assertEqual(reuse.incomplete(jobs, ROWS, reused_from="2821"), [])
        self.assertNotEqual(reuse.incomplete(jobs, ROWS), [])

    def test_legs_handed_less_than_mains_suite_are_no_verdict(self):
        self.assertNotEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS, **{"takes-only": "true"})), [])
        self.assertEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS)), [])
        # the skip gate enforced still runs every takes row: its proof stands, the run decides
        self.assertEqual(reuse.incomplete(green_jobs(), ROWS, inputs=dict(MAIN_INPUTS, **{"takes-skip": "enforce"})), [])


class DriftTwice(unittest.TestCase):
    """G10: the daily run's determinism double-run is a leg input like the others, a boolean, false on every push's run."""

    def test_it_defaults_false_and_reads_forgejos_string_boolean(self):
        self.assertEqual(reuse.leg_inputs({})["drift-twice"], "false")
        self.assertEqual(reuse.leg_inputs({"INPUT_DRIFT_TWICE": "true"})["drift-twice"], "true")

    def test_a_double_run_is_still_mains_suite(self):
        self.assertEqual(reuse.inputs_unlike_main(dict(MAIN_INPUTS, **{"drift-twice": "true"}), "both"), [])


if __name__ == "__main__":
    unittest.main()
