#!/usr/bin/env python3
"""scripts/ci/commit-rules-ci.sh, black-box, over a scratch repository and its bare origin: the range starts at
refs/verified/commit-rules (else $BEFORE, else nothing to judge); both rule sets run whatever the commits touched; a
broken rule fails; a run on main whose rules hold advances the ref. The tool runs from the scratch repository, beside
copies of what it calls (as on CI, from the checkout). Ported from commit-rules-ci-test.sh; the same cases hold the
shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/commit_rules_ci.py", "scripts/ci/commit-rules-ci.sh")
# what the tool calls, copied beside it: whichever of them exist
COPIED = ("scripts/ci/commit-rules-ci.sh", "scripts/ci/commit_rules_ci.py", "scripts/ci/verified-ref.sh",
          "scripts/ci/verified_ref.py", "scripts/ci/commit_rules.py", "scripts/ci/trailers.py",
          "scripts/lib/tool_git.py", "scripts/lib/tool_process.py")
LIST = "test/resources/fixtures/identity-flips-allow.tsv"
REF = "refs/verified/commit-rules"


class CommitRulesCiTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.origin, self.checkout = self.origin_and_checkout()
        for f in COPIED:
            if os.path.exists(os.path.join(cli_harness.ROOT, f)):
                self.copy(f)
        self.put(".github/allow-lists.txt", f"# the registry\n{LIST}\n")
        self.put(LIST, "# reviewed flips\ncountry\tvenue\trawTitle\treason\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "base")
        self.base = self.git(self.checkout, "rev-parse", "HEAD")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.TOOL = os.path.join(self.checkout, os.path.relpath(TOOL, cli_harness.ROOT))
        self.out = ""

    def copy(self, relative: str) -> None:
        target = os.path.join(self.checkout, relative)
        os.makedirs(os.path.dirname(target), exist_ok=True)
        shutil.copy2(os.path.join(cli_harness.ROOT, relative), target)

    def put(self, relative: str, text: str, append: bool = False) -> None:
        full = os.path.join(self.checkout, relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a" if append else "w") as f:
            f.write(text)

    def commit(self, path: str, line: str, message: str) -> str:
        self.put(path, line + "\n", append=True)
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def verify(self, commit: str) -> None:
        self.git(self.checkout, "push", "-q", "-f", "origin", f"{commit}:{REF}")

    def verified(self) -> str:
        """The commit the ref records: a marker commit's `verified` file, or (its first form) the commit it is."""
        try:
            oid = self.git(self.origin, "rev-parse", "-q", "--verify", REF)
        except Exception:
            return "none"
        try:
            return self.git(self.origin, "cat-file", "blob", f"{oid}:verified")
        except Exception:
            return oid

    def ci(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> int:
        done = self.run_tool(*args, cwd=self.checkout, env={"GITHUB_REF": None, "GITHUB_EVENT_NAME": None,
                                                            "BEFORE": None, **(env or {})})
        self.out = done.output
        return done.code

    def on_main(self, *args: str) -> int:
        return self.ci(*args, env={"GITHUB_REF": "refs/heads/main", "GITHUB_EVENT_NAME": "push"})

    def test_a_range_breaking_no_rule_passes_from_the_ref_and_only_a_push_to_main_advances_it(self) -> None:
        self.commit("docs/x.md", "a doc", "docs")
        self.verify(self.base)
        self.assertEqual(0, self.ci(), self.out)
        self.assertIn(f"commit rules over {self.base[:9]}..", self.out)
        self.assertIn("  allowlists: every commit holds", self.out)
        self.assertIn("  driftguards: every commit holds", self.out)
        self.assertEqual(self.base, self.verified())
        head = self.git(self.checkout, "rev-parse", "HEAD")
        self.assertEqual(0, self.on_main(), self.out)
        self.assertEqual(head, self.verified())

    def test_a_row_added_to_an_allow_list_fails_from_the_ref_before_or_a_named_base(self) -> None:
        self.verify(self.base)
        self.commit(LIST, "de\t*\tCAMOUFLAGE\tlooks intended", "list a flip, pushed --no-verify")
        self.assertEqual(1, self.ci(), self.out)
        self.assertIn("an allow-list only shrinks", self.out)
        self.assertIn("::error title=commit rules (allowlists)::", self.out)
        self.assertEqual(1, self.on_main())
        self.assertEqual(self.base, self.verified())
        self.git(self.origin, "update-ref", "-d", REF)
        self.assertEqual(1, self.ci(env={"BEFORE": self.base}), self.out)
        self.verify(self.base)
        self.assertEqual(1, self.ci("--base", self.base), self.out)

    def test_a_red_fix_changing_a_resolver_rule_fails_the_driftguards_set(self) -> None:
        self.verify(self.base)
        self.commit("identity/src/main/scala/services/identity/FeedLink.scala", "rule", "overruled\n\nFixes-Red: 2473")
        self.assertEqual(1, self.ci(), self.out)
        self.assertRegex(self.out, r"driftguards: .*RULE code")

    def test_a_gating_checks_registry_out_of_step_fails(self) -> None:
        self.verify(self.base)
        self.copy("scripts/ci/gating_checks.py")
        self.put(".github/gating-checks.tsv", "kind\tworkflow\tname\tproven\tnote\njob\tmain.yml\tnew\tunproven\n")
        self.assertEqual(1, self.ci(), self.out)
        self.assertIn("::error title=commit rules (gating checks)::", self.out)

    def test_a_range_over_a_workflow_change_advances_through_markers_only_forward(self) -> None:
        """Main #2301: GitHub refuses github.token any ref update whose new commits change a workflow file; the origin
        here refuses the same way (scripts/ci/github-workflow-refusal.pre-receive)."""
        hooks = self.path("origin-hooks")
        os.makedirs(hooks)
        shutil.copy2(os.path.join(cli_harness.ROOT, "scripts/ci/github-workflow-refusal.pre-receive"),
                     os.path.join(hooks, "pre-receive"))
        self.verify(self.base)
        workflow = self.commit(".github/workflows/daily.yml", "on: push", "a workflow's schedule")
        self.git(self.checkout, "push", "-q", "-f", "origin", "main")
        self.git(self.origin, "config", "core.hooksPath", hooks)
        self.assertEqual(0, self.on_main(), self.out)
        self.assertEqual(workflow, self.verified())
        self.commit("docs/y.md", "a doc", "docs after the workflow")
        self.assertEqual(0, self.ci(), self.out)
        self.assertIn(f"over {workflow[:9]}..", self.out)
        self.git(self.checkout, "push", "-q", "origin", "main")
        newest = self.git(self.checkout, "rev-parse", "HEAD")
        self.assertEqual(0, self.on_main(), self.out)
        self.assertEqual(newest, self.verified())
        self.git(self.checkout, "reset", "-q", "--hard", workflow)
        self.assertEqual(0, self.on_main(), self.out)
        self.assertEqual(newest, self.verified())

    def test_nothing_to_judge_from_passes_saying_so(self) -> None:
        for env in ({"BEFORE": "0" * 40}, {}):
            with self.subTest(env=env):
                self.assertEqual(0, self.ci(env=env), self.out)
                self.assertIn("commit-rules-ci: no green Main run and no push base: nothing to judge", self.out)

    def test_a_base_that_is_no_ancestor_judges_from_the_merge_base_and_advances_the_ref(self) -> None:
        other = self.commit("docs/z.md", "z", "elsewhere")
        self.git(self.checkout, "reset", "-q", "--hard", self.base)
        self.commit("docs/w.md", "w", "here")
        self.assertEqual(0, self.on_main("--base", other), self.out)
        self.assertIn(f"commit-rules-ci: {other} is no ancestor of HEAD (history rewritten): judging from the merge-base",
                      self.out)
        self.assertIn("  allowlists: every commit holds", self.out)
        self.assertEqual(self.git(self.checkout, "rev-parse", "HEAD"), self.verified())

    def test_a_rewritten_history_still_judges_the_commits_since_the_merge_base(self) -> None:
        other = self.commit("docs/z.md", "z", "elsewhere")
        self.git(self.checkout, "reset", "-q", "--hard", self.base)
        self.commit(LIST, "pl\tv\tt\twhy", "adds an allow row")
        self.assertNotEqual(0, self.ci("--base", other), self.out)

    def test_a_base_sharing_no_history_with_head_is_no_verdict_exit_2(self) -> None:
        self.git(self.checkout, "checkout", "-q", "--orphan", "island")
        self.git(self.checkout, "commit", "-qm", "island", "--allow-empty")
        island = self.git(self.checkout, "rev-parse", "HEAD")
        self.git(self.checkout, "checkout", "-q", "main")
        self.assertEqual(2, self.ci("--base", island), self.out)

    def test_an_origin_that_cannot_be_read_is_no_verdict_exit_2(self) -> None:
        self.git(self.checkout, "remote", "set-url", "origin", self.path("no-such-origin"))
        self.assertEqual(2, self.ci(), self.out)

    def test_an_unknown_argument_or_a_valueless_base_is_refused_exit_2(self) -> None:
        self.assertEqual(2, self.ci("--everything"))
        self.assertIn("commit-rules-ci: unknown argument --everything", self.out)
        self.assertNotEqual(0, self.ci("--base"))


if __name__ == "__main__":
    unittest.main()
