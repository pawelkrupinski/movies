#!/usr/bin/env python3
"""scripts/review-commit, black-box (cli_harness), over a scratch repository and a linked worktree: it commits the
review answers from the worktree and refuses the root checkout, says so when there is nothing to commit, refuses
other staged files, takes -m and prints its --help. Ported from review-commit-test.sh; the same cases hold the shell
tool and its Python rewrite. Run: python3 scripts/review_commit_test.py"""
from __future__ import annotations

import os
import shutil
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TSV = "test/resources/fixtures/identity-judgements.tsv"
ROW = "pl\tKino\tFranz\ttmdb:1\tright\topen\t\t\t\t\treview page: right\n"


class ReviewCommitTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.root = self.repo("movies", "-b", "main")
        self.copy_scripts("movies")
        shutil.copy2(os.path.join(cli_harness.ROOT, "scripts", "review-commit"), self.path("movies/scripts/review-commit"))
        self.write("movies/scripts/ci/identity_judgements_lint.py", "#!/usr/bin/env python3\n")
        self.write(f"movies/{TSV}", "country\n")
        self.write(f"movies/{TSV}.used-until", "2026-01-01T00:00:00Z\n")
        self.commit_tree(self.root, "base")
        self.wt = self.path("wt")
        self.git(self.root, "worktree", "add", "-q", "-b", "review/labels", self.wt)

    def answer(self, where: str) -> None:
        with open(os.path.join(where, TSV), "a") as f:
            f.write(ROW)

    def review_commit(self, where: str, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, tool=os.path.join(where, "scripts/review-commit"), cwd=where)

    def test_the_root_checkout_is_refused_naming_review_server_and_nothing_is_committed(self) -> None:
        self.answer(self.root)
        done = self.review_commit(self.root)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("review-server", done.stderr)
        self.assertEqual("base", self.git(self.root, "log", "--format=%s", "-1"))

    def test_a_linked_worktree_commits_the_answers(self) -> None:
        self.answer(self.wt)
        self.write("wt/" + TSV + ".used-until", "2026-02-01T00:00:00Z\n")
        done = self.review_commit(self.wt)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("Judgements: the user's review answers", self.git(self.wt, "log", "--format=%s", "-1"))
        self.assertIn("1 rows written or rewritten, 0 replaced or taken back (0 of them agent judgements",
                      self.git(self.wt, "log", "--format=%B", "-1"))
        self.assertIn("land it: ~/bin/agentctl land", done.stdout)

    def test_m_names_the_subject(self) -> None:
        self.answer(self.wt)
        self.assertEqual(0, self.review_commit(self.wt, "-m", "Mine").code)
        self.assertEqual("Mine", self.git(self.wt, "log", "--format=%s", "-1"))

    def test_nothing_to_commit_says_so(self) -> None:
        done = self.review_commit(self.wt)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("no answers to commit", done.stdout)

    def test_other_staged_files_are_refused(self) -> None:
        self.answer(self.wt)
        self.write("wt/other.txt", "x\n")
        self.git(self.wt, "add", "other.txt")
        done = self.review_commit(self.wt)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("other files are staged", done.stderr)
        self.assertIn("other.txt", done.stderr)

    def test_help_prints_the_whole_header(self) -> None:
        done = self.review_commit(self.wt, "--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("REVIEW_LABELS_FILE=", done.stdout)

    def test_an_unknown_argument_is_refused(self) -> None:
        done = self.review_commit(self.wt, "--nonsense")
        self.assertEqual(2, done.code)
        self.assertIn("unknown argument --nonsense", done.stderr)


if __name__ == "__main__":
    unittest.main()
