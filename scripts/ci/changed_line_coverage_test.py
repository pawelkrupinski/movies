#!/usr/bin/env python3
"""changed_line_coverage.py over a hand-made `git diff -U0` and JaCoCo report.
Run: python3 scripts/ci/changed_line_coverage_test.py"""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import changed_line_coverage as clc  # noqa: E402

DIFF = """diff --git a/worker/src/main/scala/services/A.scala b/worker/src/main/scala/services/A.scala
--- a/worker/src/main/scala/services/A.scala
+++ b/worker/src/main/scala/services/A.scala
@@ -10,0 +11,3 @@ class A
+  val x = 1
+  // a comment
+  def f = x
@@ -40 +43 @@ class A
-  old
+  new
diff --git a/worker/src/main/scala/services/Gone.scala b/worker/src/main/scala/services/Gone.scala
--- a/worker/src/main/scala/services/Gone.scala
+++ /dev/null
@@ -1,2 +0,0 @@
-gone
-gone
diff --git a/common/src/main/scala/models/B.scala b/common/src/main/scala/models/B.scala
--- a/common/src/main/scala/models/B.scala
+++ b/common/src/main/scala/models/B.scala
@@ -1,0 +2 @@
+  val b = 2
"""

REPORT = """<report name="r">
  <package name="services">
    <sourcefile name="A.scala">
      <line nr="11" mi="0" ci="3" mb="0" cb="0"/>
      <line nr="13" mi="2" ci="0" mb="0" cb="0"/>
      <line nr="43" mi="1" ci="0" mb="0" cb="0"/>
    </sourcefile>
  </package>
  <package name="models">
    <sourcefile name="B.scala"><line nr="2" mi="0" ci="1" mb="0" cb="0"/></sourcefile>
  </package>
</report>"""


class ChangedLineCoverageTest(unittest.TestCase):
    def test_changed_lines_are_the_added_and_modified_ones_of_surviving_files(self):
        changed = clc.changed_lines(DIFF)
        self.assertEqual(changed["worker/src/main/scala/services/A.scala"], {11, 12, 13, 43})
        self.assertEqual(changed["common/src/main/scala/models/B.scala"], {2})
        self.assertNotIn("worker/src/main/scala/services/Gone.scala", changed)

    def test_a_hunk_header_without_a_count_is_one_line(self):
        self.assertEqual(clc.changed_lines("+++ b/x/src/main/scala/C.scala\n@@ -5 +7 @@\n")["x/src/main/scala/C.scala"], {7})

    def test_coverage_is_read_per_package_path_and_file(self):
        cov = clc.covered_lines(REPORT)
        self.assertEqual(cov["services/A.scala"], {11: 3, 13: 0, 43: 0})
        self.assertEqual(cov["models/B.scala"], {2: 1})

    def test_only_executable_changed_lines_count_and_the_worst_file_comes_first(self):
        rows = clc.intersect(clc.changed_lines(DIFF), clc.covered_lines(REPORT))
        self.assertEqual(rows, [
            ([13, 43], 1, "worker/src/main/scala/services/A.scala"),   # line 12 is a comment: not executable
            ([], 1, "common/src/main/scala/models/B.scala"),
        ])

    def test_a_file_the_report_does_not_know_is_left_out(self):
        rows = clc.intersect({"web/src/main/scala/controllers/C.scala": {1}}, clc.covered_lines(REPORT))
        self.assertEqual(rows, [])


if __name__ == "__main__":
    unittest.main()
