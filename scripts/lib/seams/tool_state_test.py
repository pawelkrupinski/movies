#!/usr/bin/env python3
"""tool_state.py: ONE contract suite for the state files (JSON, TSV) run over both disks, and ONE for the key-value
store run over both tables (SQLite on a real file, in memory): a missing state is the default, a torn one refused, a
save atomic and canonical, a key normalised and checked, a value JSON both ways, keys sorted and filtered. SQLite's
own boundary beside it: two stores over one file see each other's writes."""
from __future__ import annotations

import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from seams import tool_files  # noqa: E402
from seams import tool_state  # noqa: E402
from seams.tool_files import Files  # noqa: E402
from seams.tool_state import JsonState, KeyValue, TsvState  # noqa: E402


def temporary_directory(test: unittest.TestCase) -> str:
    directory = tempfile.mkdtemp()
    test.addCleanup(shutil.rmtree, directory)
    return directory


class Contract:
    """Holds the suites out of unittest's discovery: only their subclasses run."""

    class StateFiles(unittest.TestCase):
        def files(self) -> Files:
            raise NotImplementedError

        def setUp(self) -> None:
            self.f = self.files()

        def test_a_missing_json_state_is_the_default_and_a_torn_one_is_refused(self) -> None:
            state = JsonState(self.f, "state/s.json")
            self.assertEqual({"n": 0}, state.load({"n": 0}))
            self.f.write_text("state/s.json", '{"n": 1')
            with self.assertRaises(tool_state.StateUnreadable):
                state.load()

        def test_a_json_save_is_canonical_and_round_trips(self) -> None:
            state = JsonState(self.f, "state/s.json")
            state.save({"b": [1, 2], "a": "Łódź"})
            self.assertEqual('{\n  "a": "Łódź",\n  "b": [\n    1,\n    2\n  ]\n}\n', self.f.read_text("state/s.json"))
            self.assertEqual({"a": "Łódź", "b": [1, 2]}, state.load())
            self.assertEqual(["s.json"], self.f.list("state"))

        def test_tsv_rows_skip_comments_and_a_cell_stays_on_its_line(self) -> None:
            state = TsvState(self.f, "t.tsv")
            self.assertEqual([], state.rows())
            state.save([["a", "b\tc"], ["d", 3]], comment="written by a test\nsecond line")
            self.assertEqual("# written by a test\n# second line\na\tb c\nd\t3\n", self.f.read_text("t.tsv"))
            self.assertEqual([["a", "b c"], ["d", "3"]], state.rows())

    class Store(unittest.TestCase):
        def store(self) -> KeyValue:
            raise NotImplementedError

        def setUp(self) -> None:
            self.kv = self.store()

        def test_a_value_round_trips_as_json_and_a_missing_key_is_the_default(self) -> None:
            self.assertIsNone(self.kv.get("a"))
            self.assertEqual(0, self.kv.get("a", 0))
            self.kv.put("a", {"x": [1, "ł"], "n": None})
            self.assertEqual({"x": [1, "ł"], "n": None}, self.kv.get("a"))
            self.kv.put("a", 2)
            self.assertEqual(2, self.kv.get("a"))

        def test_a_key_is_nfc_normalised_and_an_empty_or_nul_key_refused(self) -> None:
            self.kv.put("Łódz", 1)  # o + combining acute
            self.assertEqual(1, self.kv.get("Łódz"))
            self.assertEqual(["Łódz"], self.kv.keys())
            for bad in ("", "a\0b"):
                with self.assertRaises(ValueError):
                    self.kv.put(bad, 1)

        def test_delete_says_whether_the_key_was_there(self) -> None:
            self.kv.put("a", 1)
            self.assertTrue(self.kv.delete("a"))
            self.assertFalse(self.kv.delete("a"))
            self.assertIsNone(self.kv.get("a"))

        def test_keys_are_sorted_and_filtered_by_prefix(self) -> None:
            for key in ("run/2", "run/1", "job/1"):
                self.kv.put(key, True)
            self.assertEqual(["job/1", "run/1", "run/2"], self.kv.keys())
            self.assertEqual(["run/1", "run/2"], self.kv.keys("run/"))

        def test_a_value_json_cannot_hold_is_refused(self) -> None:
            with self.assertRaises(TypeError):
                self.kv.put("a", object())
            self.assertIsNone(self.kv.get("a"))


class LocalStateFilesTest(Contract.StateFiles):
    def files(self) -> Files:
        return Files(tool_files.LocalDisk(), temporary_directory(self))


class InMemoryStateFilesTest(Contract.StateFiles):
    def files(self) -> Files:
        return Files.in_memory()


class SqliteStoreTest(Contract.Store):
    def store(self) -> KeyValue:
        self.path = os.path.join(temporary_directory(self), "state.sqlite")
        return KeyValue.sqlite(self.path)

    def test_two_stores_over_one_file_see_each_others_writes(self) -> None:
        self.kv.put("a", 1)
        other = KeyValue.sqlite(self.path)
        self.assertEqual(1, other.get("a"))
        other.put("b", 2)
        self.assertEqual(["a", "b"], self.kv.keys())

    def test_a_table_name_is_checked(self) -> None:
        with self.assertRaises(ValueError):
            tool_state.SqliteTable(self.path, "kv; DROP TABLE kv")


class InMemoryStoreTest(Contract.Store):
    def store(self) -> KeyValue:
        return KeyValue.in_memory()


if __name__ == "__main__":
    unittest.main()
