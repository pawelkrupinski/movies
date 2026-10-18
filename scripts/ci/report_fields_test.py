#!/usr/bin/env python3
"""report_fields.py: a Markdown table cell written whole, an ISO-8601 instant with a `Z` read as UTC.

    python3 scripts/ci/report_fields_test.py
"""
import datetime
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from report_fields import cell, parse_instant  # noqa: E402


class CellTest(unittest.TestCase):
    def test_a_pipe_is_escaped_and_a_newline_flattened_so_the_value_stays_one_cell(self):
        self.assertEqual(cell("Kino | Lalka\nII"), "Kino \\| Lalka II")

    def test_a_value_that_is_no_string_is_written_as_one(self):
        self.assertEqual(cell(42), "42")


class ParseInstantTest(unittest.TestCase):
    def test_a_z_is_utc(self):
        self.assertEqual(parse_instant("2026-10-14T05:00:00Z"),
                         datetime.datetime(2026, 10, 14, 5, 0, tzinfo=datetime.timezone.utc))

    def test_an_offset_is_kept(self):
        self.assertEqual(parse_instant("2026-10-14T07:00:00+02:00").timestamp(),
                         parse_instant("2026-10-14T05:00:00Z").timestamp())


if __name__ == "__main__":
    unittest.main()
