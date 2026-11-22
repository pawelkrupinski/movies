#!/usr/bin/env python3
"""seeded_order's one promise: a seed names ONE order, whichever machine computes it. The golden orders below are the
contract seeded-order-test.sh held the shell function to (asserted on the Linux runner and on a Mac alike), so a seed
printed by either version replays the same order; posix_cksum is held to `cksum`'s own answers."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from seeded_order import posix_cksum, seeded_order  # noqa: E402

LINES = ["a", "b", "c", "d", "e", "f"]


class SeededOrderTest(unittest.TestCase):
    def test_a_seed_names_a_fixed_order_on_every_platform(self) -> None:
        self.assertEqual(["c", "d", "f", "a", "b", "e"], seeded_order("17234567890", LINES))

    def test_a_different_seed_another_one(self) -> None:
        self.assertEqual(["d", "c", "a", "f", "e", "b"], seeded_order("1", LINES))

    def test_the_same_seed_twice_is_the_same_order(self) -> None:
        self.assertEqual(seeded_order("99", LINES), seeded_order("99", list(reversed(LINES))))

    def test_every_line_comes_back_exactly_once_and_blank_lines_are_dropped(self) -> None:
        self.assertEqual(LINES, sorted(seeded_order("7", LINES)))
        self.assertEqual(["x", "y"], sorted(seeded_order("3", ["x", "", "y"])))

    def test_a_name_with_regex_and_shell_metacharacters_passes_through_untouched(self) -> None:
        self.assertEqual(["KinowoCoreTests.A/test$(x)*[y]"], seeded_order("5", ["KinowoCoreTests.A/test$(x)*[y]"]))

    def test_posix_cksum_is_cksums_crc(self) -> None:
        # `printf '' | cksum`, `printf 'a' | cksum`, `printf '123456789' | cksum`
        self.assertEqual(4294967295, posix_cksum(b""))
        self.assertEqual(1220704766, posix_cksum(b"a"))
        self.assertEqual(930766865, posix_cksum(b"123456789"))


if __name__ == "__main__":
    unittest.main()
