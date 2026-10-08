#!/usr/bin/env python3
"""dumpheap.py's pure halves: the namespace pid it reads from /proc/<pid>/status and the attach
protocol v1 request it writes. The attach itself needs a Linux node and a live HotSpot JVM — no test
layer here reaches it. Run: python3 scripts/ops/dumpheap_test.py"""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import dumpheap  # noqa: E402


class DumpheapTest(unittest.TestCase):
    def test_a_containers_jvm_is_the_last_nspid(self):
        self.assertEqual(dumpheap.ns_pid("Name:\tjava\nPid:\t812345\nNSpid:\t812345\t1\nPPid:\t812300\n"), 1)
        self.assertEqual(dumpheap.ns_pid("NSpid:\t4242\n"), 4242)

    def test_without_nspid_the_namespace_pid_is_1(self):
        self.assertEqual(dumpheap.ns_pid("Name:\tjava\n"), 1)

    def test_a_request_is_version_command_and_exactly_three_arguments(self):
        self.assertEqual(dumpheap.request("dumpheap", "/data/heapdumps/x.hprof", "-live"),
                         b"1\0dumpheap\0/data/heapdumps/x.hprof\0-live\0\0")
        self.assertEqual(dumpheap.request("inspectheap", "-live"), b"1\0inspectheap\0-live\0\0\0")


if __name__ == "__main__":
    unittest.main()
