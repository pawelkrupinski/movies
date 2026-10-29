#!/usr/bin/env python3
"""allow_list_ratchet.py over a scratch repository (scripts/scratch-git.sh): an allow-list row lands only at a check's
birth (`New-Check:`), each such row TEMPORARY with an owner and when it goes; otherwise a list only shrinks."""

from __future__ import annotations

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
BIRTH = "TEMPORARY owner: pawel; remove when the flip predicate reads a re-scrape as no input change; found at birth"


class RatchetTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n", LIST: HEAD,
                                 "other-allow.tsv": HEAD}, "base")

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

    def commit_removing(self, path, message):
        os.remove(os.path.join(self.repo, path))
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
        head = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n"}, "the flip gate\n\nNew-Check: flips")
        self.assertEqual((0, ""), self.ratchet(f"{self.base}..{head}"))

    def test_a_birth_row_without_an_owner_and_an_end_is_refused(self):
        head = self.commit({LIST: HEAD + "de\t*\tCAMOUFLAGE\tTEMPORARY: found at birth\n"}, "gate\n\nNew-Check: flips")
        rc, err = self.ratchet(f"{self.base}..{head}")
        self.assertEqual(1, rc, err)
        self.assertIn("TEMPORARY with an owner and when it goes", err)

    def test_removing_a_row_always_lands(self):
        born = self.commit({LIST: HEAD + f"de\t*\tCAMOUFLAGE\t{BIRTH}\n"}, "gate\n\nNew-Check: flips")
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

    def test_a_row_added_to_the_baseline_is_refused_whatever_the_trailers(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        more = BROW.replace("CAMOUFLAGE", "Titans")
        for n, message in enumerate(("one more", "gate\n\nNew-Check: flips", "fix\n\nFixes-Red: 2527")):
            head = self.commit({BASELINE: BHEAD + BROW + more, "x.txt": f"{n}\n"}, message)
            rc, err = self.ratchet(f"{reg}..{head}")
            self.assertEqual(1, rc, message)
            self.assertIn("only shrinks", err)
            self.scratch("scratch_git", "reset", "-q", "--hard", reg)

    def test_removing_a_fixed_flips_row_lands(self):
        reg = self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        fixed = self.commit({BASELINE: BHEAD}, "CAMOUFLAGE no longer flips")
        self.assertEqual((0, ""), self.ratchet(f"{reg}..{fixed}"))

    def test_a_baseline_made_again_after_its_removal_is_refused(self):
        self.commit({".github/allow-lists.txt": f"# the registry\n{LIST}\n{BASELINE}\n", BASELINE: BHEAD + BROW}, "born")
        gone = self.commit_removing(BASELINE, "dropped")
        again = self.commit({BASELINE: BHEAD + BROW}, "made again")
        rc, err = self.ratchet(f"{gone}..{again}")
        self.assertEqual(1, rc, err)
        self.assertIn("only shrinks", err)


if __name__ == "__main__":
    unittest.main()
