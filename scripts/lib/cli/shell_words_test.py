#!/usr/bin/env python3
"""shell_words.py: the one reader the claude-hooks tokenize with; a continuation never reads as a separator."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from cli import shell_words  # noqa: E402


class CommandWordsTest(unittest.TestCase):
    def test_a_backslash_newline_continues_the_line(self) -> None:
        self.assertEqual(shell_words.command_words("scripts/sbt-warm testUnit \\\n 2>&1 | tail -5"),
                         ["scripts/sbt-warm", "testUnit", "2", ">&", "1", "|", "tail", "-5"])

    def test_a_heredoc_body_is_no_command(self) -> None:
        self.assertEqual(shell_words.command_words("cat <<EOF\nrm -rf /\nEOF\necho done"),
                         ["cat", "<<", "EOF", "\n", "echo", "done"])

    def test_adjacent_operators_are_split_apart(self) -> None:
        self.assertEqual(shell_words.tokens("(a)|b&&c"), ["(", "a", ")", "|", "b", "&&", "c"])

    def test_an_unbalanced_quote_is_a_value_error(self) -> None:
        with self.assertRaises(ValueError):
            shell_words.command_words("echo 'open")


if __name__ == "__main__":
    unittest.main()
