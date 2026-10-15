#!/usr/bin/env python3
"""identity_takes.py: the take files read one way.

    python3 scripts/ci/identity_takes_test.py
"""
import gzip
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_takes as t  # noqa: E402


class TablesTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)

    def write(self, name, text):
        path = os.path.join(self.dir.name, name)
        with (gzip.open(path, "wt", encoding="utf-8") if name.endswith(".gz") else open(path, "w", encoding="utf-8")) as f:
            f.write(text)
        return path

    def test_a_table_is_its_rows_by_the_header_with_their_lines(self):
        path = self.write("x.tsv", "# a comment\nstatus\tfilm\ttitle\n\nRIGHT\ttmdb:1\tA\nstatus\tfilm\ttitle\nLOST\n")
        rows = t.table(path)
        self.assertEqual([{"status": "RIGHT", "film": "tmdb:1", "title": "A"}, {"status": "LOST"}], rows)
        self.assertEqual([4, 6], [r.line for r in rows])

    def test_a_gzipped_file_reads_the_same(self):
        self.assertEqual([{"a": "1", "b": "2"}], t.table(self.write("x.tsv.gz", "a\tb\n1\t2\n")))

    def test_an_empty_file_is_no_rows(self):
        self.assertEqual([], t.table(self.write("x.tsv", "")))
        self.assertEqual([], t.table(self.write("y.tsv", "status\tfilm\n")))

    def test_the_expected_matches_skip_a_row_naming_no_film(self):
        path = self.write("e.tsv", "country\tvenue\trawTitle\tfilm\ttitle\npl\tKino\tLalka\ttmdb:1\tLalka (1968)\n"
                                   "pl\tKino\tEmil\ttmdb:2\nuk\tBEAM\tShort\n")
        self.assertEqual([(2, "pl", "Kino", "Lalka", "tmdb:1", "Lalka (1968)"), (3, "pl", "Kino", "Emil", "tmdb:2", "")],
                         t.expected_matches(path))


if __name__ == "__main__":
    unittest.main()
