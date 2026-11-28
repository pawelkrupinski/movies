#!/usr/bin/env python3
"""tool_files.py: ONE contract suite run against both disks -- the real one over a temporary directory and the
in-memory one -- so the in-memory `Files` a unit test hands a tool behaves as the real one does: text round-trips as
UTF-8, a missing file is told apart, a write makes its directories, a replacement is whole or nothing, a listing is
sorted and filtered, `under` walks every file, a removal of nothing is fine unless asked otherwise, paths are taken
against the base. Each disk's own boundary (the real one's atomic temporary, the in-memory one's clock) beside it."""
from __future__ import annotations

import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from seams import tool_files  # noqa: E402
from seams.tool_files import Files  # noqa: E402


class Contract:
    """Holds the suite out of unittest's discovery: only its subclasses run."""

    class FilesContract(unittest.TestCase):
        """The behaviour both disks owe. A subclass gives `files()`: a fresh, empty Files with its base an existing
        directory."""

        def files(self) -> Files:
            raise NotImplementedError

        def setUp(self) -> None:
            self.f = self.files()

        def test_text_round_trips_as_utf8_and_a_relative_path_is_against_the_base(self) -> None:
            self.f.write_text("a.txt", "Łódź\n")
            self.assertEqual("Łódź\n", self.f.read_text("a.txt"))
            self.assertEqual("Łódź\n", self.f.read_text(self.f.path("a.txt")))
            self.assertEqual("Łódź\n", self.f.read_text("sub/../a.txt"))

        def test_a_script_is_copied_whole_with_its_directories_and_the_text_it_had(self) -> None:
            self.f.write_text("a.py", "#!/usr/bin/env python3\nprint(1)\n")
            self.f.write_text("b.txt", "plain\n")
            self.f.copy_script("a.py", "x/y/a.py")
            self.f.copy_script("b.txt", "x/b.txt")
            self.assertEqual("#!/usr/bin/env python3\nprint(1)\n", self.f.read_text("x/y/a.py"))
            self.assertEqual("plain\n", self.f.read_text("x/b.txt"))

        def test_a_missing_file_raises_or_gives_the_default(self) -> None:
            with self.assertRaises(FileNotFoundError):
                self.f.read_text("none.txt")
            self.assertEqual("fallback", self.f.read_text_or("none.txt", "fallback"))
            with self.assertRaises(FileNotFoundError):
                self.f.mtime("none.txt")

        def test_a_write_makes_its_directories(self) -> None:
            self.f.write_text("a/b/c.txt", "x")
            self.f.replace_text("d/e.txt", "y")
            self.assertTrue(self.f.is_dir("a/b") and self.f.is_file("a/b/c.txt") and self.f.is_file("d/e.txt"))
            self.assertFalse(self.f.is_file("a/b") or self.f.is_dir("a/b/c.txt") or self.f.exists("nowhere"))

        def test_a_replacement_is_whole_and_a_block_that_raises_leaves_the_old_file(self) -> None:
            self.f.replace_text("state.json", "old")
            self.f.replace_text("state.json", "new")
            self.assertEqual("new", self.f.read_text("state.json"))
            with self.assertRaises(RuntimeError), self.f.replacing("state.json") as out:
                out.write("half")
                raise RuntimeError("boom")
            self.assertEqual("new", self.f.read_text("state.json"))
            with self.f.replacing("state.json") as out:
                out.write("one ")
                out.write("two")
            self.assertEqual("one two", self.f.read_text("state.json"))
            self.assertEqual(["state.json"], self.f.list("."))  # no temporary left beside it

        def test_a_listing_is_sorted_and_filtered_and_a_missing_directory_raises(self) -> None:
            for name in ("b.tsv", "a.tsv", "c.txt"):
                self.f.write_text(f"d/{name}", "")
            self.f.write_text("d/sub/x.tsv", "")
            self.assertEqual(["a.tsv", "b.tsv", "c.txt", "sub"], self.f.list("d"))
            self.assertEqual(["a.tsv", "b.tsv"], self.f.list("d", suffix=".tsv"))
            with self.assertRaises(FileNotFoundError):
                self.f.list("none")

        def test_under_walks_every_file_below_a_directory_sorted(self) -> None:
            for name in ("d/z.txt", "d/sub/deeper/y.txt", "d/sub/x.txt", "other/w.txt"):
                self.f.write_text(name, "")
            self.assertEqual([self.f.path(p) for p in ("d/sub/deeper/y.txt", "d/sub/x.txt", "d/z.txt")], self.f.under("d"))

        def test_remove_of_nothing_is_fine_unless_asked_otherwise(self) -> None:
            self.f.write_text("a.txt", "x")
            self.f.remove("a.txt")
            self.assertFalse(self.f.exists("a.txt"))
            self.f.remove("a.txt")
            with self.assertRaises(FileNotFoundError):
                self.f.remove("a.txt", missing_ok=False)

        def test_a_rename_moves_a_directory_whole_but_never_onto_a_filled_one(self) -> None:
            self.f.write_text("part/a/x.gz", "x")
            self.assertTrue(self.f.rename("part", "cache"))
            self.assertEqual(self.f.read_text("cache/a/x.gz"), "x")
            self.assertFalse(self.f.exists("part"))
            self.f.write_text("other/y", "y")
            self.assertFalse(self.f.rename("other", "cache"))
            self.assertEqual(self.f.read_text("other/y"), "y")

        def test_bytes_and_size_are_the_files_own(self) -> None:
            self.f.write_text("a.txt", "Łódź")
            self.assertEqual("Łódź".encode(), self.f.read_bytes("a.txt"))
            self.assertEqual(len("Łódź".encode()), self.f.size("a.txt"))
            with self.assertRaises(FileNotFoundError):
                self.f.size("none.txt")

        def test_a_scratch_directory_is_new_empty_and_gone_with_its_contents_after_the_block(self) -> None:
            with self.f.scratch("proof-") as one, self.f.scratch("proof-") as two:
                self.assertNotEqual(one, two)
                self.assertTrue(self.f.is_dir(one) and self.f.list(one) == [])
                self.f.write_text(f"{one}/deep/x.txt", "x")
            self.assertFalse(self.f.exists(one) or self.f.exists(two))

        def test_mtime_is_a_written_files(self) -> None:
            self.f.write_text("a.txt", "x")
            self.assertIsInstance(self.f.mtime("a.txt"), float)


class LocalDiskContractTest(Contract.FilesContract):
    def files(self) -> Files:
        self.directory = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.directory)
        return Files(tool_files.LocalDisk(), self.directory)

    def test_its_mtime_is_the_files_own(self) -> None:
        self.f.write_text("a.txt", "x")
        self.assertEqual(os.path.getmtime(os.path.join(self.directory, "a.txt")), self.f.mtime("a.txt"))

    def test_a_copied_script_is_executable_only_with_a_shebang(self) -> None:
        f = self.files()
        f.write_text("a.py", "#!/usr/bin/env python3\n")
        f.write_text("b.txt", "x\n")
        f.copy_script("a.py", "c/a.py")
        f.copy_script("b.txt", "c/b.txt")
        self.assertTrue(os.access(f.path("c/a.py"), os.X_OK))
        self.assertFalse(os.access(f.path("c/b.txt"), os.X_OK))

    def test_a_replacement_keeps_its_mode(self) -> None:
        self.f.replace_text("run.sh", "#!/bin/sh\n", mode=0o755)
        self.assertEqual(0o755, os.stat(os.path.join(self.directory, "run.sh")).st_mode & 0o777)


class InMemoryDiskContractTest(Contract.FilesContract):
    def files(self) -> Files:
        self.clock = [100.0]
        return Files.in_memory("/work", now=lambda: self.clock[0])

    def test_its_mtime_is_the_injected_clocks_at_the_write(self) -> None:
        self.f.write_text("a.txt", "x")
        self.clock[0] = 250.0
        self.assertEqual(100.0, self.f.mtime("a.txt"))
        self.f.replace_text("a.txt", "y")
        self.assertEqual(250.0, self.f.mtime("a.txt"))

    def test_a_write_into_no_directory_or_over_one_fails_as_the_disk_would(self) -> None:
        disk = tool_files.InMemoryDisk()
        with self.assertRaises(FileNotFoundError):
            disk.write("/no/such/dir/a.txt", b"")
        disk.make_directories("/d")
        with self.assertRaises(IsADirectoryError):
            disk.write("/d", b"")


if __name__ == "__main__":
    unittest.main()
