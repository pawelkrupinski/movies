#!/usr/bin/env python3
"""aot-class-list.py against heap dumps cut short, as a dump interrupted mid-write is.

    python3 scripts/test_aot_class_list.py
"""
import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT = pathlib.Path(__file__).with_name("aot-class-list.py")


def run(dump: pathlib.Path):
    return subprocess.run([sys.executable, str(SCRIPT), str(dump)], capture_output=True, text=True, timeout=10)


class TruncatedDumpTest(unittest.TestCase):
    def test_an_empty_or_header_cut_dump_fails_instead_of_spinning(self):
        with tempfile.TemporaryDirectory() as tmp:
            for name, content in (("empty.hprof", b""), ("cut.hprof", b"JAVA PROFILE 1.0")):
                dump = pathlib.Path(tmp, name)
                dump.write_bytes(content)
                result = run(dump)   # used to loop on read(1) == b'' forever: TimeoutExpired
                self.assertNotEqual(result.returncode, 0, name)
                self.assertIn("truncated", result.stderr, name)


if __name__ == "__main__":
    unittest.main()
