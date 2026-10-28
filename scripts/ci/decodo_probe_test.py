#!/usr/bin/env python3
"""decodo-probe.py offline: the pool parses, and a dead proxy fails a port without leaking credentials.

Run: python3 scripts/ci/decodo_probe_test.py
"""
import os
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import script_module  # noqa: E402

probe = script_module.load(os.path.join(HERE, "decodo-probe.py"))


class DecodoProbeTest(unittest.TestCase):
    def test_pool_is_read_from_the_committed_properties(self):
        host, ports = probe.proxy_pool()
        self.assertEqual(host, "isp.decodo.com")
        self.assertIn("10001", ports)

    def test_refused_proxy_fails_the_port_and_hides_credentials(self):
        os.environ["DECODO_PROXY_USER"], os.environ["DECODO_PROXY_PASS"] = "u", "s3cret"
        ok, text = probe.probe("127.0.0.1", "1", "u", "s3cret", timeout=2)
        self.assertFalse(ok)
        self.assertTrue(text.startswith("FAIL"))
        self.assertNotIn("s3cret", text)


if __name__ == "__main__":
    unittest.main()
