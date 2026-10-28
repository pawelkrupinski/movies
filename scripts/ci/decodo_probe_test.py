#!/usr/bin/env python3
"""decodo-probe.py offline: the pool parses, and a dead proxy fails a port without leaking credentials.

Run: python3 scripts/ci/decodo_probe_test.py
"""
import contextlib
import io
import os
import sys
import unittest
from unittest import mock

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

    def test_every_port_failing_is_an_error_annotation_naming_the_cause(self):
        out = io.StringIO()
        with mock.patch.dict(os.environ, {"DECODO_PROXY_USER": "u", "DECODO_PROXY_PASS": "s3cret"}), \
                mock.patch.object(probe, "proxy_pool", return_value=("127.0.0.1", ["1", "2"])), \
                mock.patch.object(probe, "probe", return_value=(False, "FAIL Tunnel connection failed: 503")), \
                contextlib.redirect_stdout(out):
            self.assertEqual(probe.main(), 1)
        self.assertIn("::error::DECODO DOWN: 0/2 ports reached https://kinowo.net/ through 127.0.0.1 "
                      "(2x FAIL Tunnel connection failed: 503)", out.getvalue())

    def test_missing_credentials_is_an_error_annotation(self):
        out = io.StringIO()
        with mock.patch.dict(os.environ, {"DECODO_PROXY_USER": "", "DECODO_PROXY_PASS": ""}), \
                contextlib.redirect_stdout(out):
            self.assertEqual(probe.main(), 2)
        self.assertTrue(out.getvalue().startswith("::error::DECODO_PROXY_USER"))


if __name__ == "__main__":
    unittest.main()
