#!/usr/bin/env python3
"""family_reach.py: a change to the family check's own tools reaches every family; a change that reads no take reaches
none; a change the what-if cannot attribute (the agreement stage, code outside the identity model) reaches every family;
and a commit is priced by the reached families' walks.

    python3 scripts/ci/family_reach_test.py
"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import family_reach  # noqa: E402


SCRATCH_GIT = os.path.join(os.path.dirname(HERE), "scratch-git.sh")


def scratch(helper, repo, *args):
    """scripts/scratch-git.sh's isolating helpers: a raw git would inherit a hook's GIT_DIR."""
    subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, repo, *args],
                   check=True, capture_output=True, text=True)


def repo_with(change):
    """A throwaway repository: a base commit, then a head commit writing `change` ({path: text})."""
    d = os.path.join(tempfile.mkdtemp(), "repo")
    scratch("scratch_repo", d, "-q")
    def commit(files, message):
        for path, text in files.items():
            os.makedirs(os.path.dirname(os.path.join(d, path)), exist_ok=True)
            with open(os.path.join(d, path), "w") as f:
                f.write(text)
        scratch("scratch_git", d, "add", "-A")
        scratch("scratch_git", d, "commit", "-qm", message)
    commit({"README.md": "x\n"}, "base")
    commit(change, "head")
    return d


class ReachTest(unittest.TestCase):
    def test_the_checks_own_tools_reach_every_family(self):
        d = repo_with({"worker/src/test/scala/services/identity/FamilyConvergence.scala": "object A { def a = 1 }\n"})
        patterns, why = family_reach.reach(d, "HEAD~1", "HEAD")
        self.assertEqual(patterns, ["*"])
        self.assertIn("own tools", why)

    def test_a_change_that_reads_no_take_reaches_none(self):
        d = repo_with({"docs/x.md": "words\n", "worker/src/test/scala/x/ASpec.scala": "class ASpec\n"})
        self.assertEqual(family_reach.reach(d, "HEAD~1", "HEAD")[0], [])

    def test_the_agreement_stage_and_code_outside_the_model_reach_every_family(self):
        d = repo_with({"identity/src/main/scala/services/identity/agreement/Agreement.scala": "object Agreement { def a = 1 }\n"})
        self.assertEqual(family_reach.reach(d, "HEAD~1", "HEAD")[0], ["*"])
        d = repo_with({"worker/src/main/scala/x/Wiring.scala": "object Wiring { def a = 1 }\n"})
        self.assertEqual(family_reach.reach(d, "HEAD~1", "HEAD")[0], ["*"])

    def test_a_commit_is_priced_by_the_reached_families_walks(self):
        walks = [{"family": "a", "seconds": "10", "rules": "accept:sole-work join:x"},
                 {"family": "b", "seconds": "30", "rules": "pooled:no-film"},
                 {"family": "c", "seconds": "5", "rules": ""}]
        self.assertEqual(family_reach.price(walks, ["accept:sole-*"]), (1, 10.0, 3, 45.0))
        self.assertEqual(family_reach.price(walks, ["*"]), (3, 45.0, 3, 45.0))
        self.assertEqual(family_reach.price(walks, []), (0, 0, 3, 45.0))


if __name__ == "__main__":
    unittest.main()
