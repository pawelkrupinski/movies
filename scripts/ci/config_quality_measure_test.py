#!/usr/bin/env python3
"""THE DECLARATIVE-CONFIG MEASURES' tests (config_quality_measure.py): a file's lines, comments left out of the clone
tokens, the clone groups of 50+ tokens across files, and each language's files."""
from __future__ import annotations

import os
import sys
import unittest

sys.path[:0] = [os.path.dirname(os.path.abspath(__file__))]
import config_quality_measure as measure  # noqa: E402

NIX = measure.LANGUAGES["nix"]
BLOCK = "services.x = {\n" + "".join(f"  opt{i} = \"v{i}\";\n" for i in range(20)) + "};\n"


class ConfigQualityMeasureTest(unittest.TestCase):
    def test_a_files_lines_are_its_physical_lines(self) -> None:
        self.assertIn(("file-lines", "a.nix", 22), measure.rows(NIX, {"a.nix": BLOCK}))

    def test_a_block_written_twice_is_a_clone_group_and_a_comment_changes_nothing(self) -> None:
        rows = measure.rows(NIX, {"a.nix": BLOCK, "b.nix": "# a note\n" + BLOCK.replace("{\n", "{ # another\n", 1)})
        self.assertIn(("copies", "a.nix|b.nix", 2), rows)

    def test_a_short_repeat_is_no_clone(self) -> None:
        self.assertFalse([r for r in measure.rows(NIX, {"a.nix": "a = 1;\n", "b.nix": "a = 1;\n"}) if r[0] == "copies"])

    def test_each_language_holds_its_own_files(self) -> None:
        self.assertTrue(measure.LANGUAGES["terraform"].holds("infra/main.tf"))
        self.assertTrue(measure.LANGUAGES["twirl"].holds("web/src/main/twirl/views/a.scala.html"))
        self.assertFalse(measure.LANGUAGES["twirl"].holds("docs/a.scala.html"))
        self.assertFalse(NIX.holds("a.tf"))

    def test_twirl_and_html_comments_are_no_tokens(self) -> None:
        twirl = measure.LANGUAGES["twirl"]
        self.assertEqual(["<", "p", ">"], [t for t, _ in measure.tokens_of(twirl, "@* c *@<p><!-- x -->")])


if __name__ == "__main__":
    unittest.main()
