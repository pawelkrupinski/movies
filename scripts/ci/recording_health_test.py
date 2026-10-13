#!/usr/bin/env python3
"""Unit tests for recording_health.py, over fabricated enrichment trees shaped as a recording leaves them.

    python3 scripts/ci/recording_health_test.py
"""
import gzip
import importlib.util
import json
import os
import pathlib
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "recording_health", pathlib.Path(__file__).with_name("recording_health.py"))
health = importlib.util.module_from_spec(spec)
spec.loader.exec_module(health)

SINCE = 1_760_000_000          # the recording's start, epoch seconds
BEFORE = SINCE - 86_400        # a day earlier: restored from the last recording
DURING = SINCE + 600           # ten minutes in: recorded by this one


class Tree:
    """A country's enrichment tree: recorded responses under <host>/..., remembered failures under
    .enrichment-cache/ as FileEnrichmentCacheStore writes them (gzip of key, kind, fetchedAt, status, method, message)."""

    def __init__(self, root):
        self.root = pathlib.Path(root)
        self.root.mkdir(parents=True, exist_ok=True)
        self.n = 0

    def response(self, host, at, count=1):
        for _ in range(count):
            self.n += 1
            path = self.root / host / "path" / f"r{self.n}"
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("{}")
            os.utime(path, (at, at))
        return self

    def failure(self, url, status, at, count=1, method="GET", message="", request="BYTES"):
        for i in range(count):
            self.n += 1
            body = f"{request} {url}?n={self.n}\nfailed\n{at * 1000}\n{status}\n{method}\n{message or f'HTTP {status}'}"
            path = self.root / ".enrichment-cache" / "ab" / "cd" / f"{self.n:060x}.entry"
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(gzip.compress(body.encode()))
        return self


class RecordingHealthTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.tree = Tree(self.dir.name + "/enrichment-uk")

    def tearDown(self):
        self.dir.cleanup()

    def manifest(self):
        return health.manifest(self.tree.root, SINCE)

    def test_counts_only_what_this_recording_asked(self):
        self.tree.response("api.themoviedb.org", BEFORE, 50).response("api.themoviedb.org", DURING, 7)
        self.tree.failure("https://www.metacritic.com/movie/x", 404, BEFORE, 3)
        self.tree.failure("https://www.metacritic.com/movie/y", 404, DURING, 2)
        origins = self.manifest()["origins"]
        self.assertEqual(origins["api.themoviedb.org"], {"tree": 57, "answered": 7, "notFound": 0, "refused": 0, "refusals": {}})
        self.assertEqual(origins["www.metacritic.com"], {"tree": 0, "answered": 0, "notFound": 2, "refused": 0, "refusals": {}})

    def test_a_not_found_is_an_answer_and_a_403_a_timeout_or_an_open_circuit_a_refusal(self):
        url = "https://www.cineworld.co.uk/api/movies"
        self.tree.failure(url, 403, DURING, 3).failure(url, 404, DURING, 2).failure(url, 503, DURING)
        self.tree.failure(url, "", DURING, message="java.net.http.HttpTimeoutException: request timed out")
        self.tree.failure(url, "", DURING, message="tools.CircuitOpenException: www.cineworld.co.uk is refusing")
        origin = self.manifest()["origins"]["www.cineworld.co.uk"]
        self.assertEqual(origin["notFound"], 2)
        self.assertEqual(origin["refused"], 6)
        self.assertEqual(origin["refusals"], {"403": 3, "503": 1, "HttpTimeoutException": 1, "CircuitOpenException": 1})

    def test_an_origin_refusing_most_of_what_it_was_asked_is_flagged_without_any_previous_recording(self):
        # Recording 38355584482: every Cineworld detail read refused, nothing of it in the tree.
        self.tree.response("api.themoviedb.org", DURING, 1378)
        self.tree.failure("https://www.cineworld.co.uk/api/gatsby-source-boxofficeapi/movies", 403, DURING, 139)
        flags = health.judge(self.manifest(), None)
        self.assertEqual([(f.origin, f.reason) for f in flags], [("www.cineworld.co.uk", "refusing")])

    def test_a_few_refusals_or_a_minority_of_them_are_not_flagged(self):
        self.tree.failure("https://a.example/x", 403, DURING, health.MIN_REQUESTS - 1)
        self.tree.response("b.example", DURING, 60).failure("https://b.example/x", 503, DURING, 40)
        self.assertEqual(health.judge(self.manifest(), None), [])

    def test_not_found_answers_never_make_an_origin_refusing(self):
        # The rating-site slug probes answer 404 for most of what they ask, by design.
        self.tree.response("www.rottentomatoes.com", DURING, 5).failure("https://www.rottentomatoes.com/m/x", 404, DURING, 90)
        self.assertEqual(health.judge(self.manifest(), None), [])

    def test_an_origin_whose_responses_were_all_in_the_last_recording_and_none_in_this_is_flagged(self):
        previous = {"origins": {"www.cineworld.co.uk": {"tree": 139, "answered": 139, "notFound": 0, "refused": 0, "refusals": {}},
                                "letterboxd.com": {"tree": 3, "answered": 0, "notFound": 0, "refused": 0, "refusals": {}}}}
        self.tree.response("api.themoviedb.org", DURING, 10)
        flags = health.judge(self.manifest(), previous)
        self.assertEqual([(f.origin, f.reason) for f in flags], [("www.cineworld.co.uk", "gone")])

    def test_an_origin_still_holding_responses_it_was_not_asked_again_is_healthy(self):
        previous = {"origins": {"www.cineworld.co.uk": {"tree": 139, "answered": 139, "notFound": 0, "refused": 0, "refusals": {}}}}
        self.tree.response("www.cineworld.co.uk", BEFORE, 139)
        self.assertEqual(health.judge(self.manifest(), previous), [])

    def test_the_table_names_each_flagged_origin_with_its_share_and_what_it_answered_last_time(self):
        self.tree.failure("https://www.cineworld.co.uk/api/movies", 403, DURING, 139)
        previous = {"origins": {"www.cineworld.co.uk": {"tree": 139, "answered": 139, "notFound": 0, "refused": 0, "refusals": {}}}}
        manifest = self.manifest()
        table = health.table(manifest, previous, health.judge(manifest, previous))
        row = next(line for line in table.splitlines() if "www.cineworld.co.uk" in line)
        self.assertIn("**REFUSING**", row)
        self.assertIn("139/139 (100%)", row)
        self.assertIn("403×139", row)
        self.assertIn("0/139 (0%)", row)

    def test_main_writes_the_manifest_and_fails_on_a_flag(self):
        self.tree.failure("https://www.cineworld.co.uk/api/movies", 403, DURING, 139)
        out = pathlib.Path(self.dir.name) / "health.json"
        summary = pathlib.Path(self.dir.name) / "summary.md"
        code = health.main([str(self.tree.root), "--since", "2025-10-09T08:53:20Z", "--write", str(out),
                            "--summary", str(summary), "--title", "united-kingdom"])
        self.assertEqual(code, 1)
        self.assertEqual(json.loads(out.read_text())["origins"]["www.cineworld.co.uk"]["refused"], 139)
        self.assertIn("united-kingdom", summary.read_text())

    def test_main_passes_a_healthy_recording_against_its_previous_manifest(self):
        self.tree.response("api.themoviedb.org", DURING, 30)
        previous = pathlib.Path(self.dir.name) / "previous.json"
        previous.write_text(json.dumps({"origins": {"api.themoviedb.org": {"tree": 20, "answered": 20, "notFound": 0, "refused": 0, "refusals": {}}}}))
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE), "--previous", str(previous)]), 0)

    def test_a_missing_previous_manifest_falls_back_to_the_absolute_floor(self):
        self.tree.response("api.themoviedb.org", DURING, 30)
        missing = pathlib.Path(self.dir.name) / "none.json"
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE), "--previous", str(missing)]), 0)


if __name__ == "__main__":
    unittest.main()
