#!/usr/bin/env python3
"""
inhibition_cases.py's WAIT, against a fake Alertmanager whose inhibitor lags behind its API.

A real Alertmanager answers the first GET after a post with every alert 'active' when its inhibitor
has not cached the sources yet (inhibition_cases.py's docstring). The fake does exactly that for a
while after each post, then suppresses the targets the cases expect suppressed plus the sentinel's.
The harness must wait the lag out, and must refuse to judge the cases when the sentinel never
reads suppressed -- otherwise every negative case passes on an empty cache.

Run: python3 infra/test/test_inhibition_cases.py   (also run by test_alertmanager.sh)
"""
import contextlib
import io
import json
import pathlib
import sys
import threading
import time
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import inhibition_cases  # noqa: E402


def key(labels):
    return tuple(sorted(labels.items()))


class LaggingAlertmanager:
    """The API answers at once; inhibition shows only `lag` seconds after the last post."""

    def __init__(self, lag, suppresses_sentinel=True):
        self.lag = lag
        self.alerts = {}
        self.last_post = None
        self.muted = {key(target) for _, _, target, expected in inhibition_cases.CASES if expected}
        if suppresses_sentinel:
            self.muted.add(key(inhibition_cases.SENTINEL_TARGET))
        fake = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_):
                pass

            def reply(self, body):
                data = json.dumps(body).encode()
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(data)

            def do_GET(self):
                if self.path == "/-/ready":
                    return self.reply("OK")
                caught_up = fake.last_post is not None and time.time() - fake.last_post >= fake.lag
                self.reply([{"labels": labels,
                             "status": {"state": "suppressed" if caught_up and k in fake.muted else "active"}}
                            for k, labels in fake.alerts.items()])

            def do_POST(self):
                for alert in json.loads(self.rfile.read(int(self.headers["Content-Length"]))):
                    fake.alerts[key(alert["labels"])] = alert["labels"]
                fake.last_post = time.time()
                self.reply("")

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def port(self):
        return str(self.server.server_address[1])

    def close(self):
        self.server.shutdown()
        self.server.server_close()


def run_harness(fake):
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        status = inhibition_cases.main(fake.port())
    return status, out.getvalue()


class WaitsForTheInhibitor(unittest.TestCase):
    def setUp(self):
        inhibition_cases.SETTLE_SECONDS = 5

    def test_a_lagging_inhibitor_is_waited_out_not_read_as_active(self):
        fake = LaggingAlertmanager(lag=1.0)
        try:
            status, out = run_harness(fake)
        finally:
            fake.close()
        self.assertEqual(status, 0, out)
        self.assertNotIn("FAILED", out)

    def test_a_sentinel_that_is_never_suppressed_fails_instead_of_passing_the_negatives(self):
        fake = LaggingAlertmanager(lag=0.0, suppresses_sentinel=False)
        inhibition_cases.SETTLE_SECONDS = 1
        try:
            status, out = run_harness(fake)
        finally:
            fake.close()
        self.assertEqual(status, 1, out)
        self.assertIn("sentinel was never suppressed", out)
        self.assertNotIn("  ok  ", out)


if __name__ == "__main__":
    unittest.main(verbosity=2)
