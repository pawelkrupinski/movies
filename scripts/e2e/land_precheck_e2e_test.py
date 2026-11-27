#!/usr/bin/env python3
"""END TO END: scripts/land-precheck, the cheap rejects before `agentctl land` queues a change, in a worktree of a clone of
this checkout (the tools as the working tree has them; scripts/e2e/e2e_world.py) with a bare origin. The real
`git replay` / `git merge-tree` judge the rebase; the real pre-push hook runs its commit-rule checks (PREPUSH_ONLY) as a
black box. The lints run through the real scripts/sbt-warm over the stub sbt (scripts/e2e/sbt_stub.py).

  - a change on a main that moved over the same line: `precheck rebase FAIL` naming the file, the lints SKIPped
    (`refused already`), exit 1;
  - a docs-only change: rebase PASS, rules PASS, lints PASS, `PRECHECK PASS`, exit 0, and the worktree and refs as they were;
  - the lints naming a failing spec: `precheck lints FAIL` with that spec's lines, exit 1; sbt never reaching a server:
    UNCHECKED, exit 2 (the landing check then judges the change);
  - a row added to a registered allow-list (the ratchet only shrinks): `precheck rules FAIL` with the rule's reason,
    exit 1, the lints SKIPped (sbt never called).
Mutations (scripts/e2e/mutations): a rebase that conflicts read as PASS; a rules refusal (exit 1) read as PASS."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402
import sbt_stub  # noqa: E402

ALLOW = "test/resources/fixtures/identity-flips-allow.tsv"


class LandPrecheckE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.movies_clone()
        self.tree = self.repo.worktree("change")
        self.sbt = sbt_stub.StubSbt(self.w)
        self.addCleanup(self.sbt.stop, self.tree)

    def precheck(self) -> e2e_world.cli_harness.Done:
        # KINOWO_GITOPS_REPO names no repository: the manifests fetch fails offline, as the hook then fetches for itself
        return self.w.run([os.path.join(self.tree, "scripts/land-precheck")], self.tree,
                          env={**self.sbt.env(), "KINOWO_GITOPS_REPO": os.path.join(self.w.root, "no-gitops")})

    def first_line(self, repo_dir: str) -> str:
        return self.w.read(os.path.join(repo_dir, "CLAUDE.md")).split("\n", 1)[1]

    def test_a_conflicting_change_is_refused_in_seconds_and_names_its_file(self) -> None:
        self.w.write(self.tree, "CLAUDE.md", "# mine\n" + self.first_line(self.tree))
        self.w.commit(self.tree, "my heading")
        other = self.repo.worktree("other")
        self.w.write(other, "CLAUDE.md", "# theirs\n" + self.first_line(other))
        self.w.commit(other, "their heading")
        self.w.git(other, "push", "-q", "--no-verify", "origin", "HEAD:main")
        self.w.git(self.tree, "fetch", "-q", "origin")
        done = self.precheck()
        self.assertRan(done, 1, "precheck rebase FAIL", "conflicts with origin/main in: CLAUDE.md",
                       "precheck lints SKIP (0 s): refused already by rebase", "PRECHECK FAIL: rebase")
        self.assertEqual([], self.w.calls("sbt"), "the lints ran over a refused change")

    def test_a_clean_docs_change_passes_and_touches_nothing(self) -> None:
        self.w.write(self.tree, "docs/e2e-probe.md", "# a note\n")
        self.w.commit(self.tree, "a note")
        head, refs = self.w.git(self.tree, "rev-parse", "HEAD"), self.w.git(self.tree, "for-each-ref")
        done = self.precheck()
        self.assertRan(done, 0, "precheck rebase PASS", "precheck rules PASS", "precheck lints PASS", "PRECHECK PASS")
        self.assertEqual((head, refs), (self.w.git(self.tree, "rev-parse", "HEAD"), self.w.git(self.tree, "for-each-ref")))
        self.assertEqual("", self.w.git(self.tree, "status", "--porcelain"))
        self.assertIn("Test/testOnly", " ".join(c["argv"][-1] for c in self.w.calls("sbt")), "no lint spec was run")
        self.sbt.set_mode("spec-fails")
        done = self.precheck()
        self.assertRan(done, 1, "precheck lints FAIL", "names no wall clock *** FAILED ***", "Foo.scala uses Instant.now",
                       "PRECHECK FAIL: lints")
        self.sbt.set_mode("never-ran")
        self.assertRan(self.precheck(), 2, "precheck lints UNCHECKED", "PRECHECK UNCHECKED: lints")

    def test_a_row_added_to_an_allow_list_is_refused_by_the_commit_rules(self) -> None:
        self.w.git(self.tree, "sparse-checkout", "add", "/" + ALLOW)
        path = os.path.join(self.tree, ALLOW)
        self.w.write(self.tree, ALLOW, self.w.read(path) + "de\tSomeVenue\tSome Title\tTEMPORARY owner: nobody; remove when never; a row\n")
        self.w.commit(self.tree, "a finding listed")
        done = self.precheck()
        self.assertRan(done, 1, "precheck rules FAIL", "an allow-list only shrinks",
                       "precheck lints SKIP (0 s): refused already by rules", "PRECHECK FAIL: rules")
        self.assertEqual([], self.w.calls("sbt"), "the lints ran over a refused change")


if __name__ == "__main__":
    unittest.main()
