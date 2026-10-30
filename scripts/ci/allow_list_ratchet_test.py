#!/usr/bin/env python3
"""allow_list_ratchet.py over a scratch repository (scripts/scratch-git.sh): an allow-list row lands only at a check's
birth (`New-Check:`), each such row TEMPORARY with an owner and when it goes; otherwise a list only shrinks."""

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
TOOL = os.path.join(HERE, "allow_list_ratchet.py")
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

    def ratchet(self, rng):
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", TOOL, "--range", rng], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        return done.returncode, done.stderr

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


if __name__ == "__main__":
    unittest.main()
