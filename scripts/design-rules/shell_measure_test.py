#!/usr/bin/env python3
"""THE SHELL MEASURE's tests (shell_measure.py): a script's code lines, and per function and top level its decisions,
nesting and lines -- quoted text, comments and here-documents read as no keyword."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import shell_measure  # noqa: E402


def measured(text: str) -> Dict[str, int]:
    return {f"{kind} {key.partition(':')[2] or key}": value for kind, key, value in shell_measure.rows("t.sh", text)}


class ShellMeasureTest(unittest.TestCase):
    def test_code_lines_leave_out_blanks_and_comments_but_count_a_here_document(self) -> None:
        text = "#!/bin/bash\n# a comment\n\necho one\ncat <<'EOF'\nif this were code\nEOF\n"
        self.assertEqual(4, measured(text)["shell-lines t.sh"])

    def test_each_branch_loop_arm_and_logic_is_a_decision(self) -> None:
        text = ("if a; then b; elif c; then d; fi\nwhile x; do y; done\nfor i in 1 2; do :; done\n"
                "case $1 in\n  a) one ;;\n  b) two ;;\nesac\n[ -n \"$x\" ] && y || z\n")
        self.assertEqual(8, measured(text)["shell-decisions <top>"])  # if elif while for, two arms, && ||

    def test_keywords_in_quotes_comments_and_arguments_decide_nothing(self) -> None:
        text = "echo \"if done\" 'while x'\necho for # if\nrun --if done\n"
        self.assertEqual(0, measured(text)["shell-decisions <top>"])

    def test_a_quote_left_open_across_lines_hides_its_keywords(self) -> None:
        text = "python3 -c '\nif x:\n    for y in z: pass\n'\n"
        self.assertEqual(0, measured(text)["shell-decisions <top>"])

    def test_a_here_string_is_no_here_document(self) -> None:
        text = "f() {\n  while read -r a; do :; done <<< \"$x\"\n}\ng() {\n  if a; then b; fi\n}\n"
        rows = measured(text)
        self.assertEqual(1, rows["shell-decisions f"])
        self.assertEqual(1, rows["shell-decisions g"])

    def test_nesting_is_the_deepest_block(self) -> None:
        text = "f() {\n  if a; then\n    for x in y; do\n      case $x in\n        1) if b; then c; fi ;;\n      esac\n    done\n  fi\n}\n"
        self.assertEqual(4, measured(text)["shell-nesting f"])
        self.assertEqual(0, measured(text)["shell-nesting <top>"])

    def test_a_function_ends_at_the_brace_at_its_indent_and_counts_its_code_lines(self) -> None:
        text = "f() {\n  a\n\n  # note\n  b\n}\nfunction g {\n  c\n}\nh() { one; }\nafter\n"
        rows = measured(text)
        self.assertEqual(4, rows["shell-function-lines f"])
        self.assertEqual(3, rows["shell-function-lines g"])
        self.assertEqual(1, rows["shell-function-lines h"])
        self.assertNotIn("shell-function-lines <top>", rows)

    def test_a_repeated_name_is_numbered(self) -> None:
        rows = measured("f() { :; }\nf() { :; }\n")
        self.assertIn("shell-decisions f", rows)
        self.assertIn("shell-decisions f#2", rows)

    def test_an_assignment_with_parentheses_is_no_function(self) -> None:
        self.assertNotIn("shell-decisions a", measured("a=(one two)\n"))


if __name__ == "__main__":
    unittest.main()
