#!/usr/bin/env python3
"""identity_live_drift.py over two hand-made country reports: the gauges it pushes, and a push that fails.
Run: python3 scripts/ci/identity_live_drift_test.py"""
import contextlib
import io
import json
import os
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import identity_live_drift as drift  # noqa: E402

DE = {"country": "de", "recordedAt": "2026-10-23T21:31:00Z", "listings": 9000, "compared": 6100, "unsettled": 40,
      "drifted": {"lost": 4, "gained": 0, "changed": 3}}
PL = {"country": "pl", "recordedAt": "2026-10-23T21:31:00Z", "listings": 7000, "compared": 5000, "unsettled": 12,
      "drifted": {"lost": 0, "gained": 1, "changed": 0}}
NOW = 1792800000  # 2026-10-24T00:00:00Z


class Response:
    def __init__(self, status):
        self.status = status

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


class IdentityLiveDriftTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        for report in (DE, PL):
            sub = os.path.join(self.dir, "identity-live-drift-%s" % report["country"])
            os.makedirs(sub)
            with open(os.path.join(sub, "live-drift-%s.json" % report["country"]), "w", encoding="utf-8") as handle:
                json.dump(report, handle)

    def series(self, payload):
        out = {}
        for metric in payload["resourceMetrics"][0]["scopeMetrics"][0]["metrics"]:
            for point in metric["gauge"]["dataPoints"]:
                labels = tuple((a["key"], a["value"]["stringValue"]) for a in point["attributes"])
                out[(metric["name"], labels)] = (point["asDouble"], point["timeUnixNano"])
        return out

    def test_every_country_and_kind_is_a_series_stamped_when_pushed(self):
        series = self.series(drift.otlp_payload(drift.reports(self.dir), NOW * 10**9))
        stamp = str(NOW * 10**9)
        self.assertEqual(series[("kinowo_identity_drift_listings", (("country", "de"), ("kind", "lost")))], (4.0, stamp))
        self.assertEqual(series[("kinowo_identity_drift_listings", (("country", "de"), ("kind", "gained")))], (0.0, stamp))
        self.assertEqual(series[("kinowo_identity_drift_listings", (("country", "pl"), ("kind", "gained")))], (1.0, stamp))
        self.assertEqual(series[("kinowo_identity_drift_unsettled", (("country", "de"),))], (40.0, stamp))
        self.assertEqual(series[("kinowo_identity_drift_compared", (("country", "pl"),))], (5000.0, stamp))
        self.assertEqual(series[("kinowo_identity_drift_recorded_timestamp_seconds", (("country", "de"),))],
                         (1792791060.0, stamp))
        self.assertEqual(len(series), 2 * 3 + 2 * 3)

    def test_every_gauge_it_pushes_is_named_in_a_type_header(self):
        # deploy.GrafanaMetricCoverageSpec reads the `# TYPE` headers to know these families exist
        names = {m["name"] for m in drift.otlp_payload(drift.reports(self.dir), 0)["resourceMetrics"][0]["scopeMetrics"][0]["metrics"]}
        for name in names:
            self.assertIn("# TYPE %s gauge" % name, drift.__doc__)

    def test_it_pushes_with_the_token_and_fails_when_the_push_does(self):
        sent = []

        def opener(request, timeout):
            sent.append(request)
            return Response(200)

        env = {"GRAFANA_URL": "https://grafana.example/", "GRAFANA_ANNOTATION_TOKEN": "t0ken"}
        with unittest.mock.patch.dict(os.environ, env), contextlib.redirect_stdout(io.StringIO()) as out:
            self.assertEqual(drift.main([self.dir, "--push"], now=NOW, opener=opener), 0)
        self.assertEqual(sent[0].full_url, "https://grafana.example/ci/otlp/v1/metrics")
        self.assertEqual(sent[0].get_header("Authorization"), "Bearer t0ken")
        self.assertIn("de: 7 of 6100 compared listings drifted (lost 4, gained 0, changed 3)", out.getvalue())
        with unittest.mock.patch.dict(os.environ, env), contextlib.redirect_stdout(io.StringIO()) as out:
            self.assertEqual(drift.main([self.dir, "--push"], now=NOW, opener=lambda request, timeout: Response(502)), 1)
        self.assertIn("::error::drift counts not pushed: HTTP 502", out.getvalue())
        with unittest.mock.patch.dict(os.environ, {}, clear=True), contextlib.redirect_stdout(io.StringIO()) as out:
            self.assertEqual(drift.main([self.dir, "--push"], now=NOW), 1)
        self.assertIn("unset", out.getvalue())

    def test_no_report_is_a_failure(self):
        with contextlib.redirect_stdout(io.StringIO()) as out:
            self.assertEqual(drift.main([tempfile.mkdtemp()]), 1)
        self.assertIn("::error::", out.getvalue())


if __name__ == "__main__":
    unittest.main()
