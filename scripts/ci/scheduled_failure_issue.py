#!/usr/bin/env python3
"""Turn a failed SCHEDULED workflow run into a GitHub issue assigned to the repo owner, and close it again when the
workflow next passes.

WHY. A scheduled workflow that fails notifies, at most, whoever last edited its `cron:` line -- by email, if their
settings allow, with nothing anyone tracks. That is how the OG-card generator ran with BLANK proxy credentials and
nobody heard: it now exits non-zero on them, and a red run that reaches nobody is the same silence with a different
colour. An issue assigned to the owner is a notification GitHub always sends (web, mobile, email) and a record that
stays open until the workflow is green again. One issue per workflow: a failure while one is open adds a comment, never
a second issue.

OVERDUE is the same issue for a run that passed but found work past its deadline (a check whose lateness is a task, not
a regression: the owner, 2026-10-27, "CI red should mean only a regression"): it opens or comments the same way, and the
next pass with nothing overdue closes it.

ALERT is the same issue for a run that found a THIRD PARTY's or a BACKLOG's problem -- a vendor down, a quota spent,
untriaged findings piling up -- which no commit of ours made or can mend (ci-failures-2026-10-30, recommendation 2: the
Decodo probe went red 21 times in a row for an outage only the owner could act on). The run stays green; the issue opens
once, its body is REFRESHED (no comment, no new notification) while the condition lasts, and the next clean run closes
it -- the probe that raised it clears it.

The issue is found by its EXACT title: `gh issue list --search` is fuzzy, so an issue that merely mentions the title is
not this one.

Usage (needs GH_TOKEN with `issues: write` and GH_REPO=<owner>/<repo>):
  scheduled-failure-issue.sh failed   "<workflow name>" "<run url>" "<assignee>"
  scheduled-failure-issue.sh overdue  "<workflow name>" "<run url>" "<assignee>"
  scheduled-failure-issue.sh alert    "<workflow name>" "<run url>" "<assignee>" ["<what it found>"]
  scheduled-failure-issue.sh recovered "<workflow name>" "<run url>"

Stdlib only. .github/actions/report-scheduled-run runs it by the name scripts/ci/scheduled-failure-issue.sh; tested by
scheduled_failure_issue_test.py beside it against a fake `gh`."""
from __future__ import annotations

import ci_machine
import json
import subprocess
import sys
from typing import Callable, Dict, List, Optional, Sequence

VERDICTS = "failed|overdue|alert|recovered"
REQUIRED = (VERDICTS, "workflow name", "run url")
NOTHING_NAMED = "see the run's warnings"
SIGNED = "Opened by `scripts/ci/scheduled-failure-issue.sh`."


class Failed(Exception):
    """A gh call failed: the tool exits with its status."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def gh(*args: str, capture: bool = False) -> str:
    sys.stdout.flush()
    done = ci_machine.run(["gh", *args], stdout=subprocess.PIPE if capture else None, text=True)
    if done.returncode != 0:
        raise Failed(done.returncode)
    return done.stdout or ""


class Report:
    def __init__(self, workflow: str, run_url: str, assignee: str, found: str) -> None:
        self.workflow = workflow
        self.run_url = run_url
        self.assignee = assignee
        self.found = found
        self.title = f"Scheduled workflow failing: {workflow}"
        self.open_issue = self.open_issue_titled()

    def open_issue_titled(self) -> Optional[str]:
        """The open issue's number under exactly this title, else None."""
        listed = gh("issue", "list", "--state", "open", "--search", f'"{self.title}" in:title', "--json",
                    "number,title", capture=True)
        try:
            issues: List[Dict[str, object]] = json.loads(listed) if listed.strip() else []
        except ValueError:
            raise Failed(5) from None  # what jq's parse error exited with
        return next((str(issue["number"]) for issue in issues if issue.get("title") == self.title), None)

    def create(self, body: str) -> None:
        assignee = ["--assignee", self.assignee] if self.assignee else []
        gh("issue", "create", "--title", self.title, *assignee, "--body", body)

    def comment_or_create(self, comment: str, body: str) -> None:
        if self.open_issue:
            gh("issue", "comment", self.open_issue, "--body", comment)
        else:
            self.create(body)

    def overdue(self) -> None:
        self.comment_or_create(
            f"Still overdue: {self.run_url}",
            f"The scheduled workflow **{self.workflow}** passed, but found work past its deadline: {self.run_url} (its "
            "log's warnings name it). Not a regression, so not a red run.\n\nThis issue stays open until a run finds "
            f"nothing overdue, which closes it. {SIGNED}")

    def alert(self) -> None:
        body = (f"The workflow **{self.workflow}** found a problem that is not a regression of ours (a third party's, "
                f"or a backlog): {self.found or NOTHING_NAMED}.\n\n"
                f"Latest run: {self.run_url} (refreshed while it lasts). The run itself is green: nothing a commit made "
                "or can mend, so no red build and no fixer. This issue closes itself at the next run that finds "
                f"nothing. {SIGNED}")
        if self.open_issue:
            gh("issue", "edit", self.open_issue, "--body", body)
        else:
            self.create(body)

    def failed(self) -> None:
        self.comment_or_create(
            f"Failed again: {self.run_url}",
            f"The scheduled workflow **{self.workflow}** failed: {self.run_url}\n\nThis issue stays open until the "
            f"workflow next passes on its schedule, which closes it; further failures are added as comments. {SIGNED}")

    def recovered(self) -> None:
        if self.open_issue:
            gh("issue", "close", self.open_issue, "--comment", f"Passed again: {self.run_url}")


ACTIONS: Dict[str, Callable[[Report], None]] = {
    "overdue": Report.overdue, "alert": Report.alert, "failed": Report.failed, "recovered": Report.recovered}


def main(argv: Sequence[str]) -> int:
    for index, what in enumerate(REQUIRED):
        if len(argv) <= index or not argv[index]:
            print(f"scheduled-failure-issue.sh: {index + 1}: {what}", file=sys.stderr)
            return 1
    padded = [*argv, "", ""]
    try:
        report = Report(padded[1], padded[2], padded[3], padded[4])
        action = ACTIONS.get(argv[0])
        if action is None:
            print(f"unknown verdict '{argv[0]}' ({VERDICTS})", file=sys.stderr)
            return 2
        action(report)
    except Failed as failure:
        return failure.code
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
