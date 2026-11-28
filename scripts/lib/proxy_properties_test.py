#!/usr/bin/env python3
"""proxy_properties.py: key=value lines (comments and blanks skipped, spaces trimmed) and the pool's host and ports; the
committed file reads."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import proxy_properties  # noqa: E402
from seams.tool_files import Files  # noqa: E402


class ProxyPropertiesTest(unittest.TestCase):
    def test_read_skips_comments_and_blanks_and_trims(self) -> None:
        text = "# pool\n\nhost = gate.example\nports=10001, 10002 ,\nnote=a=b\nno equals\n"
        self.assertEqual({"host": "gate.example", "ports": "10001, 10002 ,", "note": "a=b"}, proxy_properties.parse(text))

    def test_the_pool_is_the_host_and_its_non_empty_ports(self) -> None:
        self.assertEqual(("gate.example", ["10001", "10002"]),
                         proxy_properties.pool("host=gate.example\nports=10001, 10002 ,\n"))

    def test_the_committed_pool_reads(self) -> None:
        host, ports = proxy_properties.pool(Files.here().read_text(proxy_properties.DEFAULT))
        self.assertTrue(host and ports)


if __name__ == "__main__":
    unittest.main()
