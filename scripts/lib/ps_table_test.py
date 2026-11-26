#!/usr/bin/env python3
"""ps_table.py: the table parsed from ps text (args collapsed, junk lines skipped), read through the injected runner,
the tree under some pids, and ps clocks in seconds."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ps_table  # noqa: E402
import tool_process  # noqa: E402

TEXT = """  PID  PPID ARGS
    1     0 /sbin/launchd
  100     1 bash   mirror.sh --x
  101   100 sleep 5
  102   101 sleep   6
  200     1 other
"""


class FakeRunner:
    def run(self, command: tool_process.Command) -> int:
        return 0

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        self.argv = list(command.argv)
        return tool_process.Captured(0, TEXT, "")


class PsTableTest(unittest.TestCase):
    def test_parse_keeps_processes_collapses_args_and_skips_other_lines(self) -> None:
        table = ps_table.parse(TEXT)
        self.assertEqual([1, 100, 101, 102, 200], sorted(table))
        self.assertEqual(ps_table.Process(100, 1, "bash mirror.sh --x"), table[100])
        self.assertEqual(["sleep", "6"], table[102].words)

    def test_read_asks_ps_for_every_process_through_the_runner(self) -> None:
        runner = FakeRunner()
        self.assertEqual(5, len(ps_table.ProcessTable(runner, {}).read()))
        self.assertEqual(["ps", "-Ao", "pid=,ppid=,args="], runner.argv)

    def test_descendants_are_every_depth_and_not_the_roots(self) -> None:
        table = ps_table.parse(TEXT)
        self.assertEqual({101, 102}, ps_table.descendants(table, {100}))
        self.assertEqual(set(), ps_table.descendants(table, {200}))

    def test_clock_seconds_reads_ps_clocks(self) -> None:
        self.assertEqual(5.0, ps_table.clock_seconds("00:05"))
        self.assertEqual(3723.5, ps_table.clock_seconds("1:02:03.50"))
        self.assertEqual(86400 + 3661, ps_table.clock_seconds("1-01:01:01"))

    def test_clock_seconds_of_a_garbled_clock_is_zero(self) -> None:
        for garbled in ("", "-", "ab:cd", "1-x:00", "12:3x", "::"):
            self.assertEqual(0.0, ps_table.clock_seconds(garbled), garbled)


if __name__ == "__main__":
    unittest.main()
