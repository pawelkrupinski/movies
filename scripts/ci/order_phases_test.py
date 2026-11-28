#!/usr/bin/env python3
"""scripts/ci/order-phases.sh, black-box (cli_harness), against a fake `gh`: `one` only for a known change off main
touching no order path; `both` on main and on every failure, ambiguity or order path -- it never drops a row it cannot
prove it may. Ported from order-phases-test.sh case for case; the same cases hold the shell tool and its Python
rewrite. Run: python3 scripts/ci/order_phases_test.py"""
from __future__ import annotations

import os
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

# STUB_FILES: the changed files (one a line); STUB_GH=fail fails the read, =hang never answers, =nolist answers no list.
GH = """case "${STUB_GH:-}" in
  fail) echo "HTTP 502" >&2; exit 1 ;;
  hang) exec sleep 86400 ;;
  nolist) exit 5 ;;  # what gh's --jq exits when the error() in the tool's filter fires
esac
printf '%s' "${STUB_FILES:-}" | sed '/^$/d'
"""
ORDER_FILES = [
    "common/src/main/x", "worker/src/main/x", "scrapers/src/main/x",
    "e2e/src/test/scala/services/movies/GermanyConvergenceSpec.scala", "build.sbt", "project/plugins.sbt",
    ".github/workflows/country-convergence-leg.yml", ".github/order-independence-paths.txt",
    "scripts/ci/order-phases.sh", "scripts/ci/order_phases.py",
]


class OrderPhasesTest(cli_harness.ToolCase):
    TOOL = cli_harness.tool_path("scripts/ci/order_phases.py", "scripts/ci/order-phases.sh")

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH)

    def phases(self, ref: str, sha: str, files: str = "", gh: str = "", repo: Optional[str] = "o/r",
               paths: Optional[str] = None) -> cli_harness.Done:
        env = {"GH_REPO": repo, "ORDER_PHASES_TIMEOUT": "2", "STUB_FILES": files, "STUB_GH": gh, "ORDER_PATHS": paths}
        return self.run_tool(ref, sha, env=env)

    def assertPhases(self, expected: str, done: cli_harness.Done) -> None:
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(expected + "\n", done.stdout, done.stderr)

    def test_main_runs_both_whatever_changed_without_asking(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/main", "abc", "README.md"))
        self.assertEqual([], self.fakes.calls("gh"))

    def test_a_branch_changing_only_docs_and_scripts_runs_one_comparing_its_commit_with_main(self) -> None:
        self.assertPhases("one", self.phases("refs/heads/agent", "abc", "docs/x.md\nscripts/identity-measure"))
        self.assertEqual(["api", "repos/o/r/compare/main...abc"], self.fakes.calls("gh")[0][:2])

    def test_a_branch_touching_the_resolver_runs_both_saying_which_file(self) -> None:
        done = self.phases("refs/heads/agent", "abc", "docs/x.md\nidentity/src/main/scala/A.scala")
        self.assertPhases("both", done)
        self.assertIn("identity/src/main/scala/A.scala is order code", done.stderr)

    def test_a_branch_touching_any_order_path_runs_both(self) -> None:
        for path in ORDER_FILES:
            with self.subTest(path):
                self.assertPhases("both", self.phases("refs/heads/agent", "abc", path))

    def test_a_change_that_cannot_be_read_runs_both(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", gh="fail"))

    def test_a_read_that_hangs_is_cut_off_and_runs_both(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", gh="hang"))

    def test_an_answer_with_no_file_list_runs_both(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", gh="nolist"))

    def test_a_list_that_may_be_cut_short_runs_both_and_one_below_it_runs_one(self) -> None:
        names = lambda n: "\n".join(f"docs/{i}.md" for i in range(1, n + 1))  # noqa: E731
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", names(100)))
        self.assertPhases("one", self.phases("refs/heads/agent", "abc", names(99)))

    def test_no_commit_or_no_repository_runs_both(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/agent", ""))
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", repo=""))

    def test_an_unreadable_path_list_runs_both(self) -> None:
        self.assertPhases("both", self.phases("refs/heads/agent", "abc", "docs/x.md", paths="/nonexistent"))

    def test_a_branch_identical_to_main_runs_one(self) -> None:
        self.assertPhases("one", self.phases("refs/heads/agent", "abc", ""))


if __name__ == "__main__":
    unittest.main()
