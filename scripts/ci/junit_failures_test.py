#!/usr/bin/env python3
"""junit_failures.py over hand-made JUnit in ScalaTest's shape (one suite per file) and
Playwright's (several suites in one <testsuites>). Run: python3 scripts/ci/junit_failures_test.py"""
import io
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import junit_failures  # noqa: E402


def suite(cases, name="S"):
    body = "".join(f'<testcase classname="{c}" name="{n}">{inner}</testcase>' for c, n, inner in cases)
    return f'<testsuite name="{name}">{body}</testsuite>'


class JunitFailuresTest(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())

    def write(self, rel, xml):
        p = self.dir / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(xml)

    def listing(self, *paths):
        out = io.StringIO()
        with redirect_stdout(out):
            code = junit_failures.main(["x", *map(str, paths)])
        return code, out.getvalue()

    def test_failures_and_errors_listed_once_passes_and_skips_not(self):
        self.write("unit/TEST-A.xml", suite([("A", "fails", "<failure/>"), ("A", "passes", ""),
                                             ("A", "skipped", "<skipped/>"), ("A", "errors", "<error/>")]))
        self.write("it/nested/TEST-B.xml", f'<testsuites>{suite([("A", "fails", "<failure/>")])}'
                                           f'{suite([("B", "also", "<failure/>")], "T")}</testsuites>')
        self.assertEqual(self.listing(self.dir / "unit", self.dir / "it"),
                         (0, "A\tfails\nA\terrors\nB\talso\n"))

    def test_a_cut_off_report_is_skipped_not_fatal(self):
        self.write("TEST-cut.xml", "<testsuite><testcase classname='C' name='n'>")
        self.write("TEST-ok.xml", suite([("D", "x", "<failure/>")]))
        self.assertEqual(self.listing(self.dir), (0, "D\tx\n"))

    def test_no_path_is_a_usage_error(self):
        self.assertEqual(self.listing()[0], 2)


if __name__ == "__main__":
    unittest.main()
