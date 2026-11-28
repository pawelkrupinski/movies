#!/usr/bin/env python3
"""ablation_preflight.py: identity-rule-ablation.yml's inputs checked before a row takes a runner -- every kind a rule
kind, the rounds a whole number, every `only` rule of a kind being ablated (a title rule's has no colon), and every
recording held where identity-fetch-recording.sh finds it (scripts/ci/recording-held.sh, against a fake gh) -- each
fault an `::error::` line, all of them said before the exit 1. Written as the workflow's inline step was and held to
it: CLI_TOOL_UNDER_TEST=<that block as a script> runs these cases against the block itself."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/ablation_preflight.py")
# gh: the archive release holds run 573's corpus; any other run's artifacts are listed as $FAKE_ARTIFACTS names
GH = """
if argv[:2] == ["release", "view"]:
    out("scrapes-pl-573.tar.zst")
    exit(0)
if argv[0] == "api" and "/runs/404/" in argv[1]:
    err("HTTP 404: Not Found")
    exit(1)
for name in env.get("FAKE_ARTIFACTS", "").split():
    out(name)
exit(0)
"""
GOOD: Dict[str, Optional[str]] = {"KINDS": "title,accept", "ROUNDS": "3", "ONLY": "", "RECORDINGS": "573",
                                  "GITHUB_REPOSITORY": "o/r", "GH_TOKEN": "t"}


class AblationPreflightTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)

    def preflight(self, **inputs: Optional[str]) -> cli_harness.Done:
        return self.run_tool(env={**GOOD, **inputs}, cwd=cli_harness.ROOT)

    def errors(self, done: cli_harness.Done) -> list:
        return [line for line in done.stdout.splitlines() if line.startswith("::error::")]

    def test_good_inputs_pass(self) -> None:
        done = self.preflight()
        self.assertEqual((0, []), (done.code, self.errors(done)), done.output)
        self.assertEqual(["release", "view", "identity-recordings", "-R", "o/r"], self.fakes.calls("gh")[0][:5])

    def test_a_kind_that_is_no_rule_kind(self) -> None:
        done = self.preflight(KINDS="title,merge,,veto")
        self.assertEqual(1, done.code)
        self.assertEqual(["::error::kinds: 'merge' is not one of title, accept, veto, join, apart",
                          "::error::kinds: '' is not one of title, accept, veto, join, apart"], self.errors(done))

    def test_a_trailing_comma_is_no_kind(self) -> None:
        self.assertEqual(0, self.preflight(KINDS="title,").code)

    def test_rounds_must_be_a_whole_number(self) -> None:
        for rounds in ("x", "1.5", "", "-1"):
            done = self.preflight(ROUNDS=rounds)
            self.assertEqual(["::error::rounds: '%s' is not a whole number" % rounds], self.errors(done), rounds)

    def test_an_only_rule_of_a_kind_not_ablated(self) -> None:
        done = self.preflight(KINDS="accept", ONLY="accept:a,veto:v,The Title")
        self.assertEqual(1, done.code)
        self.assertEqual(["::error::only: 'veto:v' is a veto rule, and kinds is 'accept' -- it would never be ablated",
                          "::error::only: 'The Title' is a title rule, and kinds is 'accept' -- it would never be ablated"],
                         self.errors(done))

    def test_recordings_each_checked_every_fault_said(self) -> None:
        done = self.preflight(RECORDINGS="573,600,404", FAKE_ARTIFACTS="scrape-fixtures-pl", KINDS="nope")
        self.assertEqual(1, done.code)
        self.assertEqual(["::error::kinds: 'nope' is not one of title, accept, veto, join, apart",
                          "::error::neither the archive release identity-recordings nor this forge holds recording 404"],
                         self.errors(done))
        done = self.preflight(RECORDINGS="600")
        self.assertEqual(["::error::recording 600 holds no scrape-fixtures-* corpus (expired or deleted)"],
                         self.errors(done))


if __name__ == "__main__":
    unittest.main()
