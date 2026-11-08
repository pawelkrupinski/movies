#!/usr/bin/env python3
"""zinc_orphans.py: which class files a checkout's Zinc analysis vouches for, and when it deletes nothing.

    python3 scripts/zinc_orphans_test.py
"""
import pathlib
import subprocess
import sys
import tempfile
import unittest
import zipfile

SCRIPT = pathlib.Path(__file__).with_name("zinc_orphans.py")
TARGET = "worker/target/scala-3.10.0"


def string(s: str) -> bytes:
    b, n, varint = s.encode(), len(s.encode()), bytearray()
    while True:
        varint.append((n & 0x7F) | (0x80 if n > 0x7F else 0))
        n >>= 7
        if not n:
            # a printable field tag ('"', field 4) right after a string: a scan to the next control byte would run on
            return b"\x0a" + bytes(varint) + b + b'"'


class ZincOrphansTest(unittest.TestCase):
    def build(self, tmp, products, files, analysis=None):
        checkout = pathlib.Path(tmp)
        (checkout / TARGET / "zinc").mkdir(parents=True)
        for f in files:
            (checkout / TARGET / "classes" / f).parent.mkdir(parents=True, exist_ok=True)
            (checkout / TARGET / "classes" / f).write_text("x")
        zip_path = checkout / TARGET / "zinc" / "inc_compile_3.zip"
        if analysis is not None:
            zip_path.write_bytes(analysis)
        else:
            with zipfile.ZipFile(zip_path, "w") as z:
                z.writestr("inc_compile.bin", b"".join(string(f"${{BASE}}/{TARGET}/classes/{p}") for p in products))
        return checkout

    def remaining(self, checkout):
        root = checkout / TARGET / "classes"
        return sorted(str(p.relative_to(root)) for p in root.rglob("*") if p.is_file())

    def run_on(self, checkout):
        return subprocess.run([sys.executable, str(SCRIPT), str(checkout)], capture_output=True, text=True, timeout=20)

    def test_keeps_the_listed_products_their_tasty_and_resources_and_drops_the_rest(self):
        long_name = "tools/" + "A" * 140 + "$.class"   # a path over 127 bytes: a two-byte length
        with tempfile.TemporaryDirectory() as tmp:
            checkout = self.build(tmp, ["tools/Kept.class", long_name],
                                  ["tools/Kept.class", "tools/Kept.tasty", long_name, "tools/Orphan.class",
                                   "tools/Orphan.tasty", "scripts/Gone$.class", "logback.xml"])
            result = self.run_on(checkout)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(self.remaining(checkout), sorted(["logback.xml", long_name, "tools/Kept.class", "tools/Kept.tasty"]))
            self.assertIn("removed 3 class file(s)", result.stderr)

    def test_an_unreadable_analysis_deletes_nothing(self):
        with tempfile.TemporaryDirectory() as tmp:
            checkout = self.build(tmp, [], ["tools/A.class"], analysis=b"not a zip")
            result = self.run_on(checkout)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(self.remaining(checkout), ["tools/A.class"])
            self.assertIn("unreadable", result.stderr)

    def test_an_analysis_listing_nothing_in_the_directory_deletes_nothing(self):
        with tempfile.TemporaryDirectory() as tmp:
            checkout = self.build(tmp, [], ["tools/A.class"])
            self.assertEqual(self.run_on(checkout).returncode, 0)
            self.assertEqual(self.remaining(checkout), ["tools/A.class"])


if __name__ == "__main__":
    unittest.main()
