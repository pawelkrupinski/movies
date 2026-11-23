#!/usr/bin/env python3
"""header_tsv.py: rows by the header, `#` and blank lines aside, a `"` a character, a short row padded with ""; a
missing file an error unless missing_ok; written cells on one line."""
from __future__ import annotations

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import header_tsv  # noqa: E402


class HeaderTsvTest(unittest.TestCase):
    def test_rows_by_header(self) -> None:
        lines = ["# a comment\n", "\n", "venue\trawTitle\tfilm\n", 'Kino\t"Nosferatu" live\ttmdb:1\n', "Odeon\tx\n"]
        self.assertEqual((["venue", "rawTitle", "film"],
                          [{"venue": "Kino", "rawTitle": '"Nosferatu" live', "film": "tmdb:1"},
                           {"venue": "Odeon", "rawTitle": "x", "film": ""}]), header_tsv.table(lines))
        self.assertEqual(([], []), header_tsv.table(["# only a comment\n"]))

    def test_read_and_write_round_trip(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "t.tsv")
            with self.assertRaises(FileNotFoundError):
                header_tsv.read(path)
            self.assertEqual([], header_tsv.read(path, missing_ok=True))
            header_tsv.write(path, ["a", "b"], [{"a": "1\t2", "b": "x\ny"}, {"a": 3}])
            with open(path, encoding="utf-8") as f:
                self.assertEqual("a\tb\n1 2\tx y\n3\t\n", f.read())
            self.assertEqual([{"a": "1 2", "b": "x y"}, {"a": "3", "b": ""}], header_tsv.read(path))


if __name__ == "__main__":
    unittest.main()
