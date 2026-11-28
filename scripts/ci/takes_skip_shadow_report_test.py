#!/usr/bin/env python3
"""scripts/ci/takes-skip-shadow-report.sh, black-box (cli_harness), against a fake `gh`: READY only with >= 20 SKIPs
and none disagreeing; a read that fails is exit 2, never a tally. Ported from takes-skip-shadow-report-test.sh; the
same cases hold the shell tool and its Python rewrite. Run: python3 scripts/ci/takes_skip_shadow_report_test.py"""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

# STUB_COMMITS commits on page 1, none after; each with STUB_STATUS's description (DISAGREE on the commit STUB_BAD
# names); STUB_GH=fail fails every read.
GH = """[ "${STUB_GH:-}" = fail ] && exit 1
case "$*" in
  *"commits?sha="*"page=1"*) for i in $(seq 1 "${STUB_COMMITS:-0}"); do printf 'c%039d\\n' "$i"; done ;;
  *"commits?sha="*) ;;
  *statuses*) sha=${1#repos/o/r/commits/}; sha=${sha%%/*}
              d=${STUB_STATUS:-"shadow: gate SKIP, takes unmoved -- agree"}
              [ "$sha" = "${STUB_BAD:-}" ] && d="shadow: gate SKIP, takes moved -- DISAGREE"
              printf 'takes skip gate (pl)\\tsuccess\\t%s\\n' "$d" ;;
esac
"""


class TakesSkipShadowReportTest(cli_harness.ToolCase):
    TOOL = cli_harness.tool_path("scripts/ci/takes_skip_shadow_report.py", "scripts/ci/takes-skip-shadow-report.sh")

    def setUp(self) -> None:
        super().setUp()
        # the fake's first argument is `api`: the url is its second, as the real gh's
        self.fakes.script("gh", 'shift\n' + GH)

    def tally(self, commits: int, **env: str) -> cli_harness.Done:
        return self.run_tool("main", "100", env={"GH_REPO": "o/r", "STUB_COMMITS": str(commits), **env})

    def test_twenty_skips_none_disagreeing_is_ready(self) -> None:
        done = self.tally(20)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("READY -- 20 SKIPs, none disagreeing", done.stdout)

    def test_nineteen_is_not_yet(self) -> None:
        self.assertEqual(1, self.tally(19).code)

    def test_one_disagreeing_among_thirty_is_never_ready_and_counted(self) -> None:
        done = self.tally(30, STUB_BAD="c%039d" % 7)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("gate SKIP 30 (disagreeing 1)", done.stdout)

    def test_runs_alone_are_not_ready(self) -> None:
        done = self.tally(30, STUB_STATUS="shadow: gate RUN, takes moved -- agree")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("gate SKIP 0 (disagreeing 0), gate RUN 30", done.stdout)

    def test_a_status_line_names_the_commit_context_state_and_description(self) -> None:
        done = self.tally(1)
        self.assertIn("c00000000 takes skip gate (pl) success shadow: gate SKIP, takes unmoved -- agree", done.stdout)

    def test_a_read_that_fails_is_no_tally(self) -> None:
        done = self.tally(30, STUB_GH="fail")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("could not list main's commits (page 1)", done.stderr)

    def test_no_repository_named_is_no_tally(self) -> None:
        done = self.run_tool(env={"GH_REPO": ""})
        self.assertEqual(2, done.code)
        self.assertIn("GH_REPO names no repository", done.stderr)


if __name__ == "__main__":
    unittest.main()
