#!/usr/bin/env python3
"""identity_takes.py: the take files read one way, and a take's move by its film and by its verdicts.

    python3 scripts/ci/identity_takes_test.py
"""
import gzip
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_takes as t  # noqa: E402
from tool_files import Files  # noqa: E402


class TablesTest(unittest.TestCase):
    def setUp(self):
        self.files = Files.in_memory()

    def write(self, name, text):
        path = self.files.path(name)
        if name.endswith(".gz"):
            self.files.write_text(name, "")
            self.files.disk.write(path, gzip.compress(text.encode("utf-8")))
        else:
            self.files.write_text(name, text)
        return path

    def test_a_table_is_its_rows_by_the_header_with_their_lines(self):
        path = self.write("x.tsv", "# a comment\nstatus\tfilm\ttitle\n\nRIGHT\ttmdb:1\tA\nstatus\tfilm\ttitle\nLOST\n")
        rows = t.table(self.files, path)
        self.assertEqual([{"status": "RIGHT", "film": "tmdb:1", "title": "A"}, {"status": "LOST"}], rows)
        self.assertEqual([4, 6], [r.line for r in rows])

    def test_a_gzipped_file_reads_the_same(self):
        self.assertEqual([{"a": "1", "b": "2"}], t.table(self.files, self.write("x.tsv.gz", "a\tb\n1\t2\n")))

    def test_an_empty_file_is_no_rows(self):
        self.assertEqual([], t.table(self.files, self.write("x.tsv", "")))
        self.assertEqual([], t.table(self.files, self.write("y.tsv", "status\tfilm\n")))

    def test_the_expected_matches_skip_a_row_naming_no_film(self):
        path = self.write("e.tsv", "country\tvenue\trawTitle\tfilm\ttitle\npl\tKino\tLalka\ttmdb:1\tLalka (1968)\n"
                                   "pl\tKino\tEmil\ttmdb:2\nuk\tBEAM\tShort\n")
        self.assertEqual([(2, "pl", "Kino", "Lalka", "tmdb:1", "Lalka (1968)"), (3, "pl", "Kino", "Emil", "tmdb:2", "")],
                         t.expected_matches(self.files, path))



class MoveTest(unittest.TestCase):
    def test_a_move_by_its_film(self):
        self.assertEqual([None, None, "lost", "gained", "changed"],
                         [t.moved(1, 1), t.moved(None, None), t.moved(1, None), t.moved(None, 1), t.moved(1, 2)])

    def test_a_move_by_its_verdicts_a_wrong_take_gone_first(self):
        cases = {("WRONG", "RIGHT"): "cured", ("WRONG", "-"): "cured", ("RIGHT", "WRONG"): "lost", ("RIGHT", "-"): "lost",
                 ("-", "WRONG"): "new-wrong", ("UNJUDGED", "WRONG"): "new-wrong", ("-", "RIGHT"): "won",
                 ("WRONG", "WRONG"): "moved (unjudged)", ("RIGHT", "RIGHT"): "moved (unjudged)", ("-", "UNJUDGED"): "moved (unjudged)"}
        self.assertEqual(cases, {k: t.outcome(*k) for k in cases})
        self.assertEqual(set(cases.values()), set(t.OUTCOMES))


if __name__ == "__main__":
    unittest.main()
