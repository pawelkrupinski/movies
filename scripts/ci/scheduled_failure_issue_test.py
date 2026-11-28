#!/usr/bin/env python3
"""scripts/ci/scheduled-failure-issue.sh, black-box, against a fake `gh` that answers `issue list` as the real one does
with --json: a lookalike the fuzzy search also finds (a title that merely CONTAINS the searched one), then
$STUB_OPEN_ISSUE under the exact title. Ported from scheduled-failure-issue-test.sh; the same cases hold the shell tool
and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/scheduled_failure_issue.py", "scripts/ci/scheduled-failure-issue.sh")
GH = r"""
if argv[:2] == ["issue", "list"]:
    if env.get("STUB_LIST_FAILS"):
        err("HTTP 502")
        sys.exit(1)
    search = argv[argv.index("--search") + 1]
    title = search[1:-len('" in:title')]
    found = [{"number": 99, "title": "Re: " + title}]
    if env.get("STUB_OPEN_ISSUE"):
        found.append({"number": int(env["STUB_OPEN_ISSUE"]), "title": title})
    out(json.dumps(found))
"""
TITLE = "Scheduled workflow failing: OG cards"


class ScheduledFailureIssueTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)

    def report(self, open_issue: str, *args: str) -> List[List[str]]:
        """The gh calls after `issue list`, of the tool reporting `args` while issue `open_issue` (or none) is open."""
        self.fakes.clear()
        done = self.run_tool(*args, env={"STUB_OPEN_ISSUE": open_issue, "GH_REPO": "o/r"})
        self.assertEqual(0, done.code, done.output)
        calls = self.fakes.calls("gh")
        self.assertEqual(["issue", "list", "--state", "open", "--search", f'"Scheduled workflow failing: {args[1]}" in:title', "--json",
                          "number,title"], calls[0])
        return calls[1:]

    def test_a_first_failure_opens_an_issue_assigned_to_the_one_passed(self) -> None:
        [create] = self.report("", "failed", "OG cards", "https://run/1", "pawelkrupinski")
        self.assertEqual(["issue", "create", "--title", TITLE, "--assignee", "pawelkrupinski", "--body"], create[:-1])
        self.assertEqual("The scheduled workflow **OG cards** failed: https://run/1\n\nThis issue stays open until the "
                         "workflow next passes on its schedule, which closes it; further failures are added as "
                         "comments. Opened by `scripts/ci/scheduled-failure-issue.sh`.", create[-1])

    def test_no_assignee_opens_it_unassigned(self) -> None:
        [create] = self.report("", "failed", "OG cards", "https://run/1")
        self.assertEqual(["issue", "create", "--title", TITLE, "--body"], create[:-1])

    def test_a_failure_while_one_is_open_comments_on_it_instead_of_opening_a_second(self) -> None:
        self.assertEqual([["issue", "comment", "7", "--body", "Failed again: https://run/2"]],
                         self.report("7", "failed", "OG cards", "https://run/2", "pawelkrupinski"))

    def test_an_issue_whose_title_only_contains_the_workflows_is_not_its_issue(self) -> None:
        [create] = self.report("", "failed", "OG cards", "https://run/1", "pawelkrupinski")
        self.assertEqual(["issue", "create", "--title", TITLE], create[:4])

    def test_a_workflow_name_with_a_quote_still_finds_its_open_issue(self) -> None:
        self.assertEqual([["issue", "comment", "7", "--body", "Failed again: https://run/5"]],
                         self.report("7", "failed", 'Say "cheese"', "https://run/5", "pawelkrupinski"))

    def test_overdue_work_opens_the_same_issue_on_a_green_run_and_comments_while_open(self) -> None:
        [create] = self.report("", "overdue", "OG cards", "https://run/6", "pawelkrupinski")
        self.assertEqual(["issue", "create", "--title", TITLE, "--assignee", "pawelkrupinski", "--body"], create[:-1])
        self.assertEqual("The scheduled workflow **OG cards** passed, but found work past its deadline: https://run/6 "
                         "(its log's warnings name it). Not a regression, so not a red run.\n\nThis issue stays open "
                         "until a run finds nothing overdue, which closes it. Opened by "
                         "`scripts/ci/scheduled-failure-issue.sh`.", create[-1])
        self.assertEqual([["issue", "comment", "7", "--body", "Still overdue: https://run/7"]],
                         self.report("7", "overdue", "OG cards", "https://run/7", "pawelkrupinski"))

    def test_an_alert_opens_the_same_issue_and_refreshes_its_body_while_open(self) -> None:
        [create] = self.report("", "alert", "Decodo probe", "https://run/8", "pawelkrupinski", "0/7 ports")
        self.assertEqual(["issue", "create", "--title", "Scheduled workflow failing: Decodo probe", "--assignee",
                          "pawelkrupinski", "--body"], create[:-1])
        [edit] = self.report("7", "alert", "Decodo probe", "https://run/9", "pawelkrupinski", "0/7 ports")
        self.assertEqual(["issue", "edit", "7", "--body"], edit[:-1])
        self.assertEqual("The workflow **Decodo probe** found a problem that is not a regression of ours (a third "
                         "party's, or a backlog): 0/7 ports.\n\nLatest run: https://run/9 (refreshed while it lasts). "
                         "The run itself is green: nothing a commit made or can mend, so no red build and no fixer. "
                         "This issue closes itself at the next run that finds nothing. Opened by "
                         "`scripts/ci/scheduled-failure-issue.sh`.", edit[-1])

    def test_an_alert_naming_nothing_points_at_the_runs_warnings(self) -> None:
        [edit] = self.report("7", "alert", "Decodo probe", "https://run/9", "pawelkrupinski")
        self.assertIn("(a third party's, or a backlog): see the run's warnings.\n", edit[-1])

    def test_a_pass_closes_the_open_issue_and_does_nothing_with_none_open(self) -> None:
        self.assertEqual([["issue", "close", "7", "--comment", "Passed again: https://run/3"]],
                         self.report("7", "recovered", "OG cards", "https://run/3"))
        self.assertEqual([], self.report("", "recovered", "OG cards", "https://run/4"))

    def test_an_unknown_verdict_is_refused(self) -> None:
        done = self.run_tool("nonsense", "w", "u")
        self.assertEqual(2, done.code)
        self.assertIn("unknown verdict 'nonsense' (failed|overdue|alert|recovered)", done.stderr)

    def test_a_missing_argument_is_refused_naming_it(self) -> None:
        done = self.run_tool("failed", "OG cards")
        self.assertEqual(1, done.code)
        self.assertIn("run url", done.stderr)
        self.assertEqual([], self.fakes.calls("gh"))

    def test_an_issue_list_that_fails_fails_the_report_opening_nothing(self) -> None:
        done = self.run_tool("failed", "OG cards", "https://run/1", env={"STUB_LIST_FAILS": "1"})
        self.assertEqual(1, done.code)
        self.assertEqual([["issue", "list"]], [call[:2] for call in self.fakes.calls("gh")])


if __name__ == "__main__":
    unittest.main()
