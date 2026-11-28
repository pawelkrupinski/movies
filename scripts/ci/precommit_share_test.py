#!/usr/bin/env python3
"""scripts/ci/precommit-share.sh, black-box: each landing-check job's hook environment, and the checks the hook runs
there -- the hook's own selection, so a job set up only for a non-empty share runs what it always ran. The tool runs
from a scratch repository beside copies of the hook and what it reads (as on CI, from the checkout). Ported from
precommit-share-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/precommit_share.py", "scripts/ci/precommit-share.sh")
COPIED = ("scripts/hooks/pre-push", "scripts/hooks/local-tests.txt", "scripts/hooks-path.sh", "scripts/tool-conventions.sh",
          "scripts/ci/commit_rules.py", "scripts/ci/ci-row-scope.sh", "scripts/ci/ci_row_scope.py",
          "scripts/ci/precommit-share.sh", "scripts/ci/precommit_share.py", "build.sbt",
          "scripts/hooks/pre-push-files.txt") + tuple(
    line for line in open(os.path.join(cli_harness.ROOT, "scripts/hooks/pre-push-files.txt")).read().splitlines()
    if line and not line.startswith("#"))


class PrecommitShareTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        for f in COPIED:
            if os.path.exists(os.path.join(cli_harness.ROOT, f)):
                target = os.path.join(self.checkout, f)
                os.makedirs(os.path.dirname(target), exist_ok=True)
                shutil.copy2(os.path.join(cli_harness.ROOT, f), target)
        self.sandbox("repo", "scripts/ci/precommit_share.py", "scripts/ci/commit_rules.py", "scripts/ci/ci_row_scope.py")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "base")
        self.git(self.checkout, "update-ref", "refs/remotes/origin/main", "HEAD")
        self.TOOL = os.path.join(self.checkout, os.path.relpath(TOOL, cli_harness.ROOT))
        self.n = 0

    def change(self, path: str) -> None:
        """A branch off main touching `path`."""
        self.n += 1
        self.git(self.checkout, "checkout", "-q", "-B", f"c{self.n}", "refs/remotes/origin/main")
        full = os.path.join(self.checkout, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a") as f:
            f.write(f"x {self.n}\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", f"touches {path}")

    def share(self, what: str, share: str, ref: Optional[str] = "check/x") -> cli_harness.Done:
        return self.run_tool(what, share, cwd=self.checkout, env={
            "GITHUB_REF_NAME": ref, "PREPUSH_ONLY": None, "PREPUSH_ELSEWHERE": None, "PREPUSH_HEAVY_ROWS": None})

    def checks(self, share: str, ref: str = "check/x") -> str:
        done = self.share("checks", share, ref)
        self.assertEqual(0, done.code, done.output)
        return done.stdout.rstrip("\n")

    def env_of(self, share: str, ref: str = "check/x") -> str:
        done = self.share("env", share, ref)
        self.assertEqual(0, done.code, done.output)
        return " ".join(done.stdout.split("\n")).strip()

    def merge_base(self) -> str:
        return self.git(self.checkout, "merge-base", "HEAD", "origin/main")

    def test_an_identity_change_leaves_identity_to_its_own_jobs(self) -> None:
        self.change("identity/src/main/scala/services/identity/Resolver.scala")
        self.assertEqual("driftguards sbt itrest itidentity", self.checks("rest"))
        self.assertEqual("identity", self.checks("identity"))
        self.assertEqual("driftguards sbt identity", self.checks("plan"))
        self.assertEqual(f"PREPUSH_MODE=--checks-only PREPUSH_RANGE={self.merge_base()}..HEAD PREPUSH_HEAVY_ROWS=1 "
                         "PREPUSH_ELSEWHERE=identity", self.env_of("rest"))
        self.assertTrue(self.env_of("identity").endswith("PREPUSH_ONLY=identity"))

    def test_a_batchs_fast_check_is_all_the_rests(self) -> None:
        self.change("identity/src/main/scala/services/identity/Resolver.scala")
        self.assertEqual("sbt|", f"{self.checks('rest', 'check/fast-ab12')}|{self.checks('identity', 'check/fast-ab12')}")
        self.assertTrue(self.env_of("rest", "check/fast-ab12").endswith("PREPUSH_ELSEWHERE="))

    def test_a_combined_tree_verification_and_its_areas(self) -> None:
        self.change("identity/src/main/scala/services/identity/Resolver.scala")
        full = self.git(self.checkout, "rev-parse", "origin/main")
        self.git(self.checkout, "update-ref", "refs/remotes/origin/main", "HEAD")
        self.change("worker/src/test/resources/fixture.json")
        verify = f"check/verify-{full}-x"
        self.assertEqual("sbt|", f"{self.checks('rest', verify)}|{self.checks('identity', verify)}")
        self.assertEqual(f"PREPUSH_MODE=--verify-only PREPUSH_RANGE={full}..HEAD PREPUSH_HEAVY_ROWS=1 PREPUSH_ELSEWHERE=",
                         self.env_of("rest", verify))
        self.assertEqual(self.env_of("rest", verify), self.env_of("rest", f"check/red-verify-{full}-x"))
        self.assertEqual(self.env_of("rest", "check/fast-ab12"), self.env_of("rest", "check/red-fast-ab12"))
        self.change("identity/src/main/scala/services/identity/Other.scala")
        areas = f"check/verifyareas-{full}-x"
        self.assertEqual(self.checks("rest", f"check/verify-{full}-x"), self.checks("rest", areas))
        self.assertEqual("identity", self.checks("identity", areas))
        self.assertIn("identity", self.checks("plan", areas).split(" "))
        self.assertEqual(f"PREPUSH_MODE=--checks-only PREPUSH_RANGE={full}..HEAD PREPUSH_ONLY=identity",
                         self.env_of("identity", areas))
        self.assertEqual(self.env_of("rest", f"check/verify-{full}-x"), self.env_of("rest", areas))
        self.assertEqual("identity", self.checks("identity", f"check/red-verifyareas-{full}-x"))

    def test_a_branch_naming_no_commit_fails(self) -> None:
        self.change("docs/notes.md")
        done = self.share("env", "rest", f"check/verify-{'0' * 40}-x")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("names no commit", done.stderr)

    def test_a_page_change_has_no_share_of_its_own(self) -> None:
        self.change("web/src/main/twirl/views/x.scala.html")
        self.assertEqual("jsquality configquality sbt itrest|", f"{self.checks('rest')}|{self.checks('identity')}")

    def test_a_docs_change_has_no_identity_or_heavy_share(self) -> None:
        self.change("docs/notes.md")
        self.assertEqual("", self.checks("identity"))
        self.assertEqual([], [c for c in self.checks("rest").split(" ") if c in ("itrest", "itidentity", "e2ecorpus")])

    def test_unknown_shares_and_commands_are_refused(self) -> None:
        for what, share in (("checks", "oldrefs"), ("checks", "page"), ("checks", "everything"), ("env", "heavy"),
                            ("everything", "rest")):
            with self.subTest(what=what, share=share):
                done = self.share(what, share)
                self.assertEqual(2, done.code, done.output)
                self.assertIn("usage: ", done.stderr)
        self.assertEqual(2, self.run_tool("env", cwd=self.checkout).code)


if __name__ == "__main__":
    unittest.main()
