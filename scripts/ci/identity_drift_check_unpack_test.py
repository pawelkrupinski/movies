#!/usr/bin/env python3
"""identity_drift_check.unpacked: a release asset's archive is unpacked under its target and nowhere else -- a member
that climbs out with `..` or is an absolute path makes it fail (False), never writes outside."""
from __future__ import annotations

import io
import os
import sys
import tarfile
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "..", "lib")]
import identity_drift_check  # noqa: E402


def archive(path: str, names: list[str]) -> None:
    with tarfile.open(path, "w") as t:
        for name in names:
            info = tarfile.TarInfo(name)
            info.size = 2
            t.addfile(info, io.BytesIO(b"x\n"))


class UnpackedTest(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.into = os.path.join(self.dir.name, "into")
        self.packed = os.path.join(self.dir.name, "a.tar")

    def test_a_plain_member_is_unpacked(self) -> None:
        archive(self.packed, ["ok.txt"])
        self.assertTrue(identity_drift_check.unpacked(self.packed, self.into))
        self.assertTrue(os.path.exists(os.path.join(self.into, "ok.txt")))

    def test_a_member_climbing_out_of_the_target_fails_and_writes_nothing_outside(self) -> None:
        archive(self.packed, ["../escaped.txt"])
        self.assertFalse(identity_drift_check.unpacked(self.packed, self.into))
        self.assertFalse(os.path.exists(os.path.join(self.dir.name, "escaped.txt")))


    def test_the_data_filter_is_asked_for_by_name_not_left_to_the_pythons_default(self) -> None:
        # Python 3.14 made "data" the default; before it the default extracted anywhere: the call names it
        archive(self.packed, ["ok.txt"])
        seen: list[object] = []
        real = tarfile.TarFile.extractall

        def spy(tar: tarfile.TarFile, *args: object, **kwargs: object) -> None:
            seen.append(kwargs.get("filter"))
            real(tar, *args, **kwargs)  # type: ignore[arg-type]

        tarfile.TarFile.extractall = spy  # type: ignore[method-assign]
        try:
            identity_drift_check.unpacked(self.packed, self.into)
        finally:
            tarfile.TarFile.extractall = real  # type: ignore[method-assign]
        self.assertEqual(["data"], seen)


if __name__ == "__main__":
    unittest.main()
