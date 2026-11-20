#!/usr/bin/env python3
"""dead_code.py: the deadCode rows ranked -- unreferenced before test-only, classes before methods, the largest first --
with each candidate's size read from its source (a class to its last indented line, a method to its closing line), and
a name it cannot find sized `?`."""

import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import dead_code  # noqa: E402

SOURCE = """package tools

object HttpRead {
  def checkJsonObject(body: String): Int =
    body.length

  def checkJsonArray(body: String): Int = {
    val trimmed = body.trim
    trimmed.length
  }
}

final class Leftover(x: Int) {
  def twice: Int = x * 2
}
"""


class RankTest(unittest.TestCase):
    def rows(self, d):
        os.makedirs(os.path.join(d, "http/src/main/scala/tools"))
        with open(os.path.join(d, "http/src/main/scala/tools/HttpRead.scala"), "w") as f:
            f.write(SOURCE)
        path = os.path.join(d, "rows.tsv")
        src = "http/src/main/scala/tools/HttpRead.scala"
        with open(path, "w") as f:
            f.write(f"method\ttest-only\ttools.HttpRead.checkJsonObject\thttp\t{src}\n"
                    f"method\tunreferenced\ttools.HttpRead.checkJsonArray\thttp\t{src}\n"
                    f"method\tunreferenced\ttools.HttpRead.vanished\thttp\t{src}\n"
                    f"class\tunreferenced\ttools.Leftover\thttp\t{src}\n")
        return dead_code.read_rows(path)

    def test_unreferenced_classes_lead_then_methods_by_size_then_test_only(self):
        with tempfile.TemporaryDirectory() as d:
            ranked = dead_code.ranked(self.rows(d), d)
            self.assertEqual([(r["name"], r["lines"]) for r in ranked], [
                ("tools.Leftover", 3), ("tools.HttpRead.checkJsonArray", 4), ("tools.HttpRead.vanished", 0),
                ("tools.HttpRead.checkJsonObject", 2)])

    def test_the_report_sections_and_counts(self):
        with tempfile.TemporaryDirectory() as d:
            out = dead_code.report(dead_code.ranked(self.rows(d), d))
            self.assertEqual(out[0], "# dead-code: 3 unreferenced, 1 test-only")
            self.assertIn("| 1 | class | `tools.Leftover` | 3 | http/src/main/scala/tools/HttpRead.scala |", out)
            self.assertIn("| 3 | method | `tools.HttpRead.vanished` | ? | http/src/main/scala/tools/HttpRead.scala |", out)

    def test_an_empty_list_says_none(self):
        self.assertIn("(none)", dead_code.report([]))


if __name__ == "__main__":
    unittest.main()
