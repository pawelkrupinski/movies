#!/usr/bin/env python3
"""atomic.py: a write replaces the file whole; a failed write keeps the old file and leaves no temporary; two writers
never share a temporary name; `mode` is the result's permissions; the directory is made."""
from __future__ import annotations

import json
import os
import stat
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import atomic  # noqa: E402


class AtomicTest(unittest.TestCase):
    def setUp(self) -> None:
        self.directory = tempfile.mkdtemp()
        self.path = os.path.join(self.directory, "state.json")

    def test_write_text_replaces_the_file_and_leaves_nothing_beside_it(self) -> None:
        atomic.write_text(self.path, "old")
        atomic.write_text(self.path, "new")
        with open(self.path) as f:
            self.assertEqual("new", f.read())
        self.assertEqual(["state.json"], os.listdir(self.directory))

    def test_a_failed_write_keeps_the_old_file_and_removes_its_temporary(self) -> None:
        atomic.write_text(self.path, "old")
        with self.assertRaises(RuntimeError), atomic.replacing(self.path) as out:
            out.write("half")
            raise RuntimeError("boom")
        with open(self.path) as f:
            self.assertEqual("old", f.read())
        self.assertEqual(["state.json"], os.listdir(self.directory))

    def test_two_open_writers_have_two_temporaries(self) -> None:
        with atomic.replacing(self.path) as first, atomic.replacing(self.path) as second:
            first.write("a")
            second.write("b")
            self.assertEqual(2, len(os.listdir(self.directory)))

    def test_mode_is_the_results_and_the_directory_is_made(self) -> None:
        nested = os.path.join(self.directory, "a", "b", "out.txt")
        atomic.write_text(nested, "x", mode=0o644)
        self.assertEqual(0o644, stat.S_IMODE(os.stat(nested).st_mode))

    def test_write_bytes_replaces_the_file_whole_and_a_failed_one_keeps_the_old(self) -> None:
        atomic.write_bytes(self.path, b"old\xff", mode=0o600)
        with open(self.path, "rb") as f:
            self.assertEqual(b"old\xff", f.read())
        self.assertEqual(0o600, stat.S_IMODE(os.stat(self.path).st_mode))
        with self.assertRaises(TypeError):
            atomic.write_bytes(self.path, "not bytes")  # type: ignore[arg-type]
        with open(self.path, "rb") as f:
            self.assertEqual(b"old\xff", f.read())
        self.assertEqual(["state.json"], os.listdir(self.directory))

    def test_write_json_passes_its_options_on(self) -> None:
        atomic.write_json(self.path, {"b": 1, "a": 2}, sort_keys=True)
        with open(self.path) as f:
            self.assertEqual('{"a": 2, "b": 1}', f.read())
            f.seek(0)
            self.assertEqual({"a": 2, "b": 1}, json.load(f))


if __name__ == "__main__":
    unittest.main()
