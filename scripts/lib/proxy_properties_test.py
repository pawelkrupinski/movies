#!/usr/bin/env python3
"""proxy_properties.py: key=value lines (comments and blanks skipped, spaces trimmed) and the pool's host and ports; the
committed file reads."""
from __future__ import annotations

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import proxy_properties  # noqa: E402


class ProxyPropertiesTest(unittest.TestCase):
    def write(self, text: str) -> str:
        handle, path = tempfile.mkstemp()
        with os.fdopen(handle, "w") as f:
            f.write(text)
        self.addCleanup(os.unlink, path)
        return path

    def test_read_skips_comments_and_blanks_and_trims(self) -> None:
        path = self.write("# pool\n\nhost = gate.example\nports=10001, 10002 ,\nnote=a=b\nno equals\n")
        self.assertEqual({"host": "gate.example", "ports": "10001, 10002 ,", "note": "a=b"}, proxy_properties.read(path))

    def test_the_pool_is_the_host_and_its_non_empty_ports(self) -> None:
        path = self.write("host=gate.example\nports=10001, 10002 ,\n")
        self.assertEqual(("gate.example", ["10001", "10002"]), proxy_properties.read_pool(path))

    def test_the_committed_pool_reads(self) -> None:
        host, ports = proxy_properties.read_pool()
        self.assertTrue(host and ports)


if __name__ == "__main__":
    unittest.main()
