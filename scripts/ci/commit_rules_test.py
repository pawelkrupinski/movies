#!/usr/bin/env python3
"""commit_rules.py over a scratch repository (scripts/scratch-git.sh).

allowlists: an allow-list row lands only at a check's birth (`New-Check:`), each such row TEMPORARY with an owner and
when it goes; otherwise a list only shrinks, and the known-flips baseline only shrinks, whatever the trailers.
driftguards: a drift/flip red fix lands with its drift slice or `Drift-Slice: none -- <why>`; a red fix changing a resolver rule names the design doc that measured it; a drift harness change carries
its self-test or `Drift-Harness-Refactor: <why>`."""

from __future__ import annotations

import gzip
import json
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.path.join(HERE, "commit_rules.py")
LIST = "test/resources/fixtures/identity-flips-allow.tsv"
HEAD = "# reviewed flips\ncountry\tvenue\trawTitle\treason\n"
BASELINE = "test/resources/fixtures/identity-flips-baseline.tsv"
BHEAD = "# known flips\ncountry\tseed\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n"
BROW = "de\t2\tWerkstattkino e.V.\tCAMOUFLAGE\ttmdb:1 -> tmdb:2 -> tmdb:1\t2026-10-29\tfixer\tconvergence #2527\n"
PREDICATE = "identity/src/main/scala/services/identity/TakeFlips.scala"
GATE = "# the drift gate\nscripts/ci/identity-drift-check.sh\nidentity/src/main/scala/services/identity/TakeFlips.scala\n" + LIST + "\n"
BIRTH = "TEMPORARY owner: pawel; remove when the flip predicate reads a re-scrape as no input change; found at birth"


class RatchetTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n", LIST: HEAD,
                                 "other-allow.tsv": HEAD, ".github/identity-drift-gate-paths.txt": GATE}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full) or self.repo, exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def commit_bytes(self, path, data, message):
        full = os.path.join(self.repo, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "wb") as f:
            f.write(data)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def run_rules(self, rule_set, rng):
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", TOOL, rule_set, "--range", rng], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        return done.returncode, done.stderr

    def ratchet(self, rng):
        return self.run_rules("allowlists", rng)

    def guards(self, rng):
        return self.run_rules("driftguards", rng)[0]

    def test_a_row_added_after_birth_is_refused(self):
        head = self.commit({LIST: HEAD + "de\t*\tCAMOUFLAGE\tlooks intended\n"}, "list a flip")
        rc, err = self.ratchet(f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("an allow-list only shrinks", err)
        self.assertIn("CAMOUFLAGE", err)

    def test_a_checks_birth_lists_what_it_finds_as_temporary(self):
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "a new predicate\n"},
                           "the flip gate\n\nNew-Check: flips")
        self.assertEqual((0, ""), self.ratchet(f"{self.base}..{head}"))

    def test_new_check_without_a_new_check_is_refused(self):
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n"}, "list it\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("adds no check", err)

    def test_a_new_convergence_row_kind_is_a_new_check(self):
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n",
                            ".github/workflows/country-convergence-leg.yml": "a new row kind\n"}, "rows\n\nNew-Check: a row")
        self.assertEqual((0, ""), self.ratchet(f"{self.base}..{head}"))

    def test_a_birth_row_in_a_red_fix_is_refused(self):
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "a new predicate\n"},
                           "the flip gate\n\nNew-Check: flips\nFixes-Red: 3726")
        rc, err = self.ratchet(f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("Fixes-Red", err)

    def test_a_flip_a_list_named_before_is_known_and_refused(self):
        born = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "v1\n"}, "gate\n\nNew-Check: flips")
        fixed = self.commit({LIST: HEAD}, "fix CAMOUFLAGE")
        again = self.commit({LIST: HEAD + f"de\tWerkstattkino e.V.\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "v2\n"},
                            "tighter\n\nNew-Check: flips v2")
        rc, err = self.ratchet(f"{fixed}..{again}")
        self.assertEqual(1, rc, err)
        self.assertIn("known flip", err)
        self.assertNotEqual(born, again)

    def test_a_flip_a_drift_slice_holds_is_known_and_refused(self):
        seed = "P\x00Werkstattkino e.V.\x00CAMOUFLAGE\x00"
        sliced = self.commit_bytes("test/resources/fixtures/drift-slices/de/camouflage/slice.json.gz",
                                   gzip.compress(json.dumps({"country": "de", "seeds": [seed]}).encode()), "slice")
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "a new predicate\n"},
                           "gate\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{sliced}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("known flip", err)

    def test_a_new_listing_at_a_new_predicates_birth_lands(self):
        seed = "P\x00Werkstattkino e.V.\x00CAMOUFLAGE\x00"
        sliced = self.commit_bytes("test/resources/fixtures/drift-slices/de/camouflage/slice.json.gz",
                                   gzip.compress(json.dumps({"country": "de", "seeds": [seed]}).encode()), "slice")
        head = self.commit({LIST: HEAD + f"uk\t*\tRose (2023)\t{BIRTH}\n", PREDICATE: "a new predicate\n"},
                           "gate\n\nNew-Check: flips")
        self.assertEqual((0, ""), self.ratchet(f"{sliced}..{head}"))

    def test_a_birth_row_without_an_owner_and_an_end_is_refused(self):
        head = self.commit({LIST: HEAD + "de\t*\tCAMOUFLAGE\tTEMPORARY: found at birth\n", PREDICATE: "p\n"},
                           "gate\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("TEMPORARY with an owner and when it goes", err)

    def test_removing_a_row_always_lands(self):
        born = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "p\n"}, "gate\n\nNew-Check: flips")
        fixed = self.commit({LIST: HEAD}, "fix CAMOUFLAGE")
        self.assertEqual((0, ""), self.ratchet(f"{born}..{fixed}"))

    def test_an_unregistered_file_is_not_judged_here(self):
        # AllowListRatchetSpec holds every allow-list registered; this tool judges the registry's
        head = self.commit({"other-allow.tsv": HEAD + "x\ty\tz\twhy\n"}, "other")
        self.assertEqual((0, ""), self.ratchet(f"{self.base}..{head}"))

    def test_an_unreadable_range_is_no_verdict(self):
        self.assertEqual(2, self.ratchet("nosuch..alsonot")[0])


    def test_the_baseline_is_born_once_with_its_rows(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n"}, "register")
        born = self.commit({BASELINE: BHEAD + BROW}, "the known flips")
        self.assertEqual((0, ""), self.ratchet(f"{reg}..{born}"))

    def test_a_row_added_to_the_baseline_is_refused_but_at_a_tightened_checks_birth(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        more = BROW.replace("CAMOUFLAGE", "Titans")
        for message, touches in (("one more", True), ("fix\n\nFixes-Red: 2527", True), ("gate\n\nNew-Check: flips", False)):
            files = {BASELINE: BHEAD + BROW + more}
            if touches:
                files[PREDICATE] = message + "\n"
            head = self.commit(files, message)
            rc, err = self.ratchet(f"{reg}..{head}")
            self.assertEqual(1, rc, message)
            self.assertIn("only shrinks", err)
            self.scratch("scratch_git", "reset", "-q", "--hard", reg)

    def test_a_tightened_checks_birth_baselines_what_it_finds(self):
        # the owner, 2026-10-30: a229501a7's precise `none` revealed flips the blind predicate hid -- baselined at its birth
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        found = BROW.replace("CAMOUFLAGE", "Parkway Drive: Home")
        head = self.commit({BASELINE: BHEAD + BROW + found, PREDICATE: "a precise none\n"}, "flip check\n\nNew-Check: flips")
        self.assertEqual((0, ""), self.ratchet(f"{reg}..{head}"))

    def test_a_tightened_checks_birth_baselines_no_flip_fixed_before(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        fixed = self.commit({BASELINE: BHEAD}, "CAMOUFLAGE no longer flips")
        head = self.commit({BASELINE: BHEAD + BROW, PREDICATE: "a precise none\n"}, "flip check\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{fixed}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("known flip", err)
        self.assertNotEqual(reg, head)

    def test_a_tightened_checks_birth_baselines_a_listing_a_drift_slice_holds(self):
        # a slice holds a listing's past flip; the tightened check sees a new one of it (CinemaxX "Titans", a229501a7)
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        seeds = ["P\x00CinemaxX Dammtor\x00Titans\x00"]
        sliced = self.commit_bytes("test/resources/fixtures/drift-slices/de/titans/slice.json.gz",
                                   gzip.compress(json.dumps({"country": "de", "seeds": seeds}).encode()), "the Titans slice")
        found = BROW.replace("Werkstattkino e.V.", "CinemaxX Dammtor").replace("CAMOUFLAGE", "Titans")
        head = self.commit({BASELINE: BHEAD + BROW + found, PREDICATE: "a precise none\n"}, "flip check\n\nNew-Check: flips")
        self.assertEqual((0, ""), self.ratchet(f"{sliced}..{head}"))
        self.assertNotEqual(reg, head)

    def test_removing_a_fixed_flips_row_lands(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        fixed = self.commit({BASELINE: BHEAD}, "CAMOUFLAGE no longer flips")
        self.assertEqual((0, ""), self.ratchet(f"{reg}..{fixed}"))

    def test_a_baseline_made_again_after_its_removal_is_refused(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        os.remove(os.path.join(self.repo, BASELINE))
        gone = self.commit({}, "dropped")
        again = self.commit({BASELINE: BHEAD + BROW}, "made again")
        rc, err = self.ratchet(f"{gone}..{again}")
        self.assertEqual(1, rc, err)
        self.assertNotEqual(reg, again)

    def test_a_baselined_flip_is_known_to_a_newborn_check(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n", PREDICATE: "a new predicate\n"},
                           "gate\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{reg}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("known flip", err)


    # ── found in review (2026-10-30) ──
    def test_a_row_added_while_resolving_a_merge_is_judged(self):
        # diff-tree prints nothing for a merge: the rows its resolution added went unjudged
        base = self.commit({"a.txt": "a\n"}, "base")
        self.scratch("scratch_git", "checkout", "-q", "-b", "side")
        self.commit({"side.txt": "side\n"}, "side")
        self.scratch("scratch_git", "checkout", "-q", "-")
        self.commit({"a.txt": "b\n"}, "main moves")
        self.scratch("scratch_git", "merge", "-q", "--no-ff", "--no-commit", "side")
        with open(os.path.join(self.repo, LIST), "a") as f:
            f.write("de\t*\tCAMOUFLAGE\tlooks intended\n")
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", "merge, listing a flip")
        rc, err = self.ratchet(f"{base}..HEAD")
        self.assertEqual(1, rc, err)
        self.assertIn("only shrinks", err)

    def test_a_baseline_shrink_is_no_check_added(self):
        gate = GATE + BASELINE + "\n"
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW,
                           ".github/identity-drift-gate-paths.txt": gate}, "born")
        head = self.commit({BASELINE: BHEAD, LIST: HEAD + f"uk\t*\tNew One\t{BIRTH}\n"}, "shrink\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{reg}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("adds no check", err)

    # ── driftguards ──
    RULE = "identity/src/main/scala/services/identity/FeedLink.scala"
    HARNESS = "worker/src/test/scala/scripts/IdentityDrift.scala"

    def test_a_commit_fixing_no_red_passes_the_guards(self):
        head = self.commit({"identity/src/main/A.scala": "a\n", self.RULE: "rule\n"}, "Identity: a feature\n\nNo red.")
        self.assertEqual(0, self.guards(f"{self.base}..{head}"))

    def test_a_red_fix_changing_a_resolver_or_title_rule_fails(self):
        head = self.commit({self.RULE: "rule\n"}, "overruled\n\nFixes-Red: 2473")
        self.assertEqual(1, self.guards(f"{self.base}..{head}"))
        head = self.commit({"common/src/main/resources/title-rules.tsv": "row\n"}, "Title rules: a row\n\nFixes-Red: 2473")
        self.assertEqual(1, self.guards(f"{head}^..{head}"))

    def test_a_red_fix_changing_a_rule_passes_with_the_design_doc_that_measured_it(self):
        head = self.commit({self.RULE: "rule\n", "docs/design/feed-links.md": "measured\n"},
                           "overruled\n\nFixes-Red: 2473\nIdentity-Design: docs/design/feed-links.md")
        self.assertEqual(0, self.guards(f"{self.base}..{head}"))

    def test_an_identity_design_trailer_naming_no_document_fails(self):
        head = self.commit({self.RULE: "rule\n"}, "overruled\n\nFixes-Red: 2473\nIdentity-Design: measured, trust me")
        rc, err = self.run_rules("driftguards", f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("names no file", err)

    def test_a_red_fix_in_a_mechanism_needs_no_design_doc(self):
        head = self.commit({"identity/src/main/scala/services/identity/agreement/AgreementStage.scala": "s\n"},
                           "Agreement: the budget no longer lags\n\nFixes-Red: 2473")
        self.assertEqual(0, self.guards(f"{self.base}..{head}"))

    def test_a_harness_change_carries_its_self_test(self):
        head = self.commit({self.HARNESS: "h\n"}, "Drift harness: the queue files a held answer first")
        self.assertEqual(1, self.guards(f"{self.base}..{head}"))
        head = self.commit({self.HARNESS: "h2\n", "worker/src/test/scala/scripts/IdentityDriftSpec.scala": "s\n"}, "and its case")
        self.assertEqual(0, self.guards(f"{head}^..{head}"))
        head = self.commit({"scripts/ci/identity-drift-check.sh": "x\n"}, "one helper\n\nDrift-Harness-Refactor: no behaviour moves")
        self.assertEqual(0, self.guards(f"{head}^..{head}"))

    def test_the_flip_predicate_is_harness_and_a_later_commits_test_does_not_count(self):
        flips = "identity/src/main/scala/services/identity/TakeFlips.scala"
        first = self.commit({flips: "f\n"}, "Flips: a re-scrape is no input change")
        self.commit({"identity/src/test/scala/services/identity/TakeFlipsSpec.scala": "t\n"}, "its spec, later")
        self.assertEqual(1, self.guards(f"{first}^..HEAD"))

    def test_a_drift_or_flip_red_fix_lands_with_its_slice(self):
        a = "identity/src/main/A.scala"
        head = self.commit({a: "1\n"}, "Identity: the agreement keeps its verdicts\n\nFixes-Red: 3726")
        self.assertEqual(0, self.guards(f"{head}^..{head}"), "a red fix not about drift")
        head = self.commit({a: "2\n"}, "Identity: CAMOUFLAGE no longer flips\n\nFixes-Red: 3726")
        self.assertEqual(1, self.guards(f"{head}^..{head}"), "a flip fix without its slice")
        head = self.commit({a: "3\n"}, "Identity: the incremental state ends where a whole resolve does (drift)\n\nFixes-Red: 3726")
        self.assertEqual(1, self.guards(f"{head}^..{head}"), "a drift fix without its slice")
        fix = self.commit({a: "4\n"}, "Identity: CAMOUFLAGE no longer flips\n\nFixes-Red: 3726")
        self.commit({"test/resources/fixtures/drift-slices/de/camouflage/slice.json.gz": "s\n"}, "Drift slice: CAMOUFLAGE")
        self.assertEqual(0, self.guards(f"{fix}^..HEAD"), "its slice later in the push")
        head = self.commit({a: "5\n"}, "CI: the drift row times out later\n\nFixes-Red: 3134\nDrift-Slice: none -- a CI script, no listing")
        self.assertEqual(0, self.guards(f"{head}^..{head}"), "a reasoned Drift-Slice: none")
        head = self.commit({a: "6\n"}, "CI: the drift row times out later\n\nFixes-Red: 3134\nDrift-Slice: none")
        self.assertEqual(1, self.guards(f"{head}^..{head}"), "Drift-Slice: none without a reason")

    def test_an_unreadable_range_is_no_verdict_to_the_guards(self):
        self.assertEqual(2, self.guards("nosuch..HEAD"))


if __name__ == "__main__":
    unittest.main()
