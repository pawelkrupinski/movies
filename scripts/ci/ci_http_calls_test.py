#!/usr/bin/env python3
"""ci_http_calls.py over hand-written TSVs, as tools.CiHttpCalls writes them.
Run: python3 scripts/ci/ci_http_calls_test.py"""
import io
import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import ci_http_calls as calls  # noqa: E402


def tree(files):
    directory = tempfile.mkdtemp()
    for name, text in files.items():
        Path(directory, name).write_text(text)
    return directory


class CiHttpCallsTest(unittest.TestCase):
    def test_sums_every_process_file_by_service_phase_and_outcome(self):
        d = tree({"1.tsv": "country\toutcome\tphase\tservice\tcount\npl\tsuccess\tenrich\ttmdb\t5\npl\thttp_429\tenrich\ttmdb\t2\n",
                  "2.tsv": "country\toutcome\tphase\tservice\tcount\npl\tsuccess\tenrich\ttmdb\t3\npl\tsuccess\tenrich\tomdb\t1\n",
                  "3.tsv.partial": "country\toutcome\tphase\tservice\tcount\npl\tsuccess\tenrich\ttmdb\t100\n"})
        self.assertEqual(calls.read(d), {("tmdb", "enrich", "success"): 8, ("tmdb", "enrich", "http_429"): 2,
                                         ("omdb", "enrich", "success"): 1})

    def test_the_phase_stands_in_for_a_counter_without_a_service_label(self):
        d = tree({"1.tsv": "country\toutcome\tphase\tcount\nde\tsuccess\tscrape\t4\n"})
        self.assertEqual(calls.read(d), {("scrape", "scrape", "success"): 4})

    def test_summary_is_a_table_a_row_per_service_success_first(self):
        md = calls.summary_markdown({("tmdb", "enrich", "success"): 8, ("tmdb", "enrich", "http_429"): 2,
                                     ("omdb", "enrich", "success"): 1}, "pl convergence")
        self.assertIn("| service | success | http_429 | total |", md)
        self.assertIn("| tmdb | 8 | 2 | 10 |", md)
        self.assertIn("| omdb | 1 | 0 | 1 |", md)
        self.assertIn("| **all** | 9 | 2 | 11 |", md)
        self.assertLess(md.index("| tmdb"), md.index("| omdb"))

    def test_payload_is_one_gauge_under_job_ci_recording_with_the_run_labels(self):
        payload = calls.otlp_payload({("tmdb", "enrich", "success"): 8}, "pl", "convergence", "42", "Record", now=1.5)
        resource = payload["resourceMetrics"][0]
        self.assertEqual(resource["resource"]["attributes"][0]["value"]["stringValue"], "ci-recording")
        metric = resource["scopeMetrics"][0]["metrics"][0]
        self.assertEqual(metric["name"], "kinowo_ci_http_calls")
        point = metric["gauge"]["dataPoints"][0]
        self.assertEqual(point["asDouble"], 8.0)
        self.assertEqual(point["timeUnixNano"], "1500000000")
        labels = {a["key"]: a["value"]["stringValue"] for a in point["attributes"]}
        self.assertEqual(labels, {"country": "pl", "service": "tmdb", "phase": "enrich", "outcome": "success",
                                  "row": "convergence", "run_id": "42", "workflow": "Record"})

    def test_nothing_recorded_reports_nothing_and_pushes_nothing(self):
        out = io.StringIO()
        os.environ.pop("GITHUB_STEP_SUMMARY", None)
        self.assertEqual(calls.main(["--dir", tree({}), "--country", "pl", "--push"], out), 0)
        self.assertIn("No external calls", out.getvalue())

    def test_unset_secrets_skip_the_push(self):
        self.assertIn("not pushed", calls.push({}, "", ""))

    def test_a_refused_push_warns_and_never_raises(self):
        def refuse(request, timeout):
            raise OSError("connection refused")
        self.assertIn("::warning::", calls.push({}, "https://g", "t", opener=refuse))


if __name__ == "__main__":
    unittest.main()
