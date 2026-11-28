#!/usr/bin/env python3
"""takes_skip_gate.Gate over its fakes: the repository's diffs from a scripted runner, the files in memory, the proof a
runner that writes what a proof says -- the decisions without a git or a process (the black-box cases, over real ones,
are takes_skip_gate_test.py's)."""
from __future__ import annotations

import gzip
import os
import sys
import unittest
from typing import Callable, List, Mapping

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import tool_git  # noqa: E402
import tool_process  # noqa: E402
from seams.tool_files import Files  # noqa: E402

import takes_skip_gate  # noqa: E402
from takes_skip_gate import Gate, Run  # noqa: E402

BASE, HEAD = "a" * 40, "b" * 40
RULES = "common/src/main/resources/title-rules.tsv"
TRACE = "/work/base.jsonl.gz"
SHADOW = {"TAKES_SKIP_MODE": "shadow"}
GOOD = "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 0 could not be made again\n"


class ScriptedRunner:
    """git answers by its arguments after `-C .`; a proof writes `says` into its log and exits `code`."""

    def __init__(self, files: Files, git: Mapping[str, tool_process.Captured], says: str = GOOD, code: int = 0,
                 differ: str = "") -> None:
        self.files, self.git, self.says, self.code, self.differ = files, git, says, code, differ
        self.ran: List[tool_process.Command] = []

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        args = " ".join(command.argv[command.argv.index(".") + 1:])
        key = args.split(" -- ")[0].removesuffix(" --")
        return self.git.get(key, tool_process.Captured(0, "", ""))

    def run(self, command: tool_process.Command) -> int:
        self.ran.append(command)
        assert command.log is not None
        self.files.write_text(command.log, self.says)
        if self.differ:
            self.files.write_text(command.argv[command.argv.index("--out") + 1], self.differ)
        return self.code


def captured(stdout: str, code: int = 0) -> tool_process.Captured:
    return tool_process.Captured(code, stdout, "")


class GateTest(unittest.TestCase):
    def gate(self, changed: str = RULES + "\n", trace: bytes = gzip.compress(b'{"a":1}\n'),
             env: Mapping[str, str] = SHADOW, **runner: object) -> Callable[[], str]:
        files = Files.in_memory()
        if trace:
            files.write_text(TRACE, "")
            files.disk.write(files.path(TRACE), trace)
        diffs = {f"diff --name-only --no-renames {BASE} {HEAD}": captured(changed),
                 f"diff --name-only {BASE} {HEAD}": captured("")}
        self.runner = ScriptedRunner(files, diffs, **runner)  # type: ignore[arg-type]
        gate = Gate(tool_git.Git(self.runner, env, "."), files, self.runner, env)
        return lambda: gate.decide([BASE, HEAD, TRACE, "--", "proof"])

    def refused(self, decide: Callable[[], str], why: str) -> None:
        with self.assertRaises(Run) as raised:
            decide()
        self.assertIn(why, str(raised.exception))

    def test_a_title_rule_change_the_proof_clears_is_skip(self) -> None:
        self.assertEqual("measure: footprint, 0 readings differ (2 readings)", self.gate()())
        self.assertEqual(["proof", "--out"], [*self.runner.ran[0].argv[:1], self.runner.ran[0].argv[1]])

    def test_a_change_to_anything_else_is_run_before_any_proof_runs(self) -> None:
        self.refused(self.gate(changed="web/app/Foo.scala\n"), "neither a title-rule file nor inert")
        self.assertEqual([], self.runner.ran)

    def test_a_trace_that_is_no_gzip_is_run(self) -> None:
        self.refused(self.gate(trace=b"not a gzip"), "is not a whole gzip")

    def test_a_proof_that_lists_readings_though_it_exited_0_is_run(self) -> None:
        self.refused(self.gate(differ="row\n"), "though it exited 0")

    def test_a_proof_that_fails_or_says_nothing_is_run(self) -> None:
        self.refused(self.gate(code=1), "the proof exited 1")
        self.refused(self.gate(says="nothing\n"), "the proof said no")

    def test_an_unset_mode_is_run(self) -> None:
        self.refused(self.gate(env={}), "TAKES_SKIP_MODE is unset")


class PureTest(unittest.TestCase):
    def test_the_placeholders_are_no_title_rule_and_markdown_is_inert(self) -> None:
        self.assertFalse(takes_skip_gate.eligible(takes_skip_gate.PLACEHOLDERS))
        self.assertTrue(takes_skip_gate.inert("notes.md"))

    def test_a_proofs_last_verdict_line_is_read(self) -> None:
        self.assertEqual(2, takes_skip_gate.proof_verdict(GOOD))
        self.assertEqual(None, takes_skip_gate.gzip_lines(b"x"))


if __name__ == "__main__":
    unittest.main()
