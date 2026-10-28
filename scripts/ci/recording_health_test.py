#!/usr/bin/env python3
"""Unit tests for recording_health.py, over fabricated enrichment trees shaped as a recording leaves them.

    python3 scripts/ci/recording_health_test.py
"""
import gzip
import json
import os
import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import recording_health as health  # noqa: E402

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
        self.assertEqual(origins["api.themoviedb.org"], {"tree": 57, "answered": 7, "notFound": 0, "refused": 0, "refusals": {}, "proxyDown": 0})
        self.assertEqual(origins["www.metacritic.com"], {"tree": 0, "answered": 0, "notFound": 2, "refused": 0, "refusals": {}, "proxyDown": 0})

    def test_an_entry_whose_compressed_body_is_damaged_is_skipped_as_undecodable(self):
        self.tree.failure("https://www.cineworld.co.uk/api/movies", 403, DURING)
        path = next((self.tree.root / ".enrichment-cache").rglob("*.entry"))
        data = path.read_bytes()
        path.write_bytes(data[:10] + b"\xff\xff\xff" + data[13:])  # a valid gzip header, an invalid deflate block
        self.assertEqual({}, self.manifest()["origins"])

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

    def test_a_best_effort_origin_refusing_everything_warns_and_never_fails_the_recording(self):
        # Run 469: OMDb's key out of its daily quota answered 401 to all 141 asks. OMDb is the one optional
        # source -- the user's call, 2026-10-20 -- so it is named in the summary but the leg stays healthy.
        self.tree.response("api.themoviedb.org", DURING, 30)
        self.tree.failure("https://www.omdbapi.com/", 401, DURING, 141)
        flags = health.judge(self.manifest(), None)
        self.assertEqual([(f.origin, f.reason, f.blocking) for f in flags], [("www.omdbapi.com", "refusing", False)])
        summary = pathlib.Path(self.dir.name) / "summary.md"
        code = health.main([str(self.tree.root), "--since", str(SINCE), "--summary", str(summary), "--title", "germany"])
        self.assertEqual(code, 0)
        text = summary.read_text()
        self.assertIn("OMDb unavailable: 141 call(s) refused", text)
        self.assertIn("**REFUSING (BEST EFFORT)**", text)

    def test_a_best_effort_origin_gone_from_the_tree_warns_too(self):
        previous = {"origins": {"www.omdbapi.com": {"tree": 191, "answered": 191, "notFound": 0, "refused": 0, "refusals": {}}}}
        self.tree.response("api.themoviedb.org", DURING, 10)
        flags = health.judge(self.manifest(), previous)
        self.assertEqual([(f.origin, f.reason, f.blocking) for f in flags], [("www.omdbapi.com", "gone", False)])

    def test_every_other_origin_still_fails_the_recording_beside_a_best_effort_one(self):
        self.tree.failure("https://www.omdbapi.com/", 401, DURING, 141)
        self.tree.failure("https://api.themoviedb.org/3/movie/1", 429, DURING, 40)
        code = health.main([str(self.tree.root), "--since", str(SINCE)])
        self.assertEqual(code, 1)
        flags = health.judge(self.manifest(), None)
        self.assertEqual([(f.origin, f.blocking) for f in flags], [("api.themoviedb.org", True), ("www.omdbapi.com", False)])

    def test_an_origin_refused_with_the_residential_proxy_unreachable_is_unknown_not_refusing(self):
        # Record scrape fixtures #2238: Cineworld 403s CI's own address, and the recording's direct -> proxy route
        # met Decodo answering `Tunnel failed, got: 503` on every ask. That is the proxy down, not a verdict on the
        # origin: UNKNOWN (exit 3), never a red the code is asked to fix.
        composite = ("java.lang.RuntimeException: All 2 backends failed for get https://www.cineworld.co.uk/api/movies:\n"
                     "  direct: HttpStatusException: HTTP 403 for GET https://www.cineworld.co.uk/api/movies\n"
                     "  proxy: IOException: Tunnel failed, got: 503")
        self.tree.response("api.themoviedb.org", DURING, 30)
        self.tree.failure("https://www.cineworld.co.uk/api/movies", "", DURING, 66, message=composite)
        flags = health.judge(self.manifest(), None)
        self.assertEqual([(f.origin, f.reason) for f in flags], [("www.cineworld.co.uk", "proxy-unreachable")])
        summary = pathlib.Path(self.dir.name) / "summary.md"
        code = health.main([str(self.tree.root), "--since", str(SINCE), "--summary", str(summary), "--title", "uk"])
        self.assertEqual(code, 3)
        self.assertIn("UNKNOWN: the residential proxy was unreachable", summary.read_text())
        self.assertIn("**PROXY-UNREACHABLE (UNKNOWN)**", summary.read_text())

    def test_an_origin_that_refused_through_a_working_proxy_too_is_still_refusing(self):
        composite = ("java.lang.RuntimeException: All 2 backends failed for get https://www.cineworld.co.uk/api/movies:\n"
                     "  direct: HttpStatusException: HTTP 403 for GET https://www.cineworld.co.uk/api/movies\n"
                     "  proxy: HttpStatusException: HTTP 403 for GET https://www.cineworld.co.uk/api/movies")
        self.tree.failure("https://www.cineworld.co.uk/api/movies", "", DURING, 66, message=composite)
        self.assertEqual([(f.origin, f.reason) for f in health.judge(self.manifest(), None)], [("www.cineworld.co.uk", "refusing")])
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE)]), 1)

    def test_a_real_refusal_beside_a_proxy_outage_is_still_a_red(self):
        down = "java.lang.RuntimeException: All 2 backends failed:\n  direct: HTTP 403\n  proxy: HttpConnectTimeoutException: HTTP connect timed out"
        self.tree.failure("https://www.cineworld.co.uk/api/movies", "", DURING, 30, message=down)
        self.tree.failure("https://api.themoviedb.org/3/movie/1", 429, DURING, 40)
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE)]), 1)

    def test_a_missing_previous_manifest_falls_back_to_the_absolute_floor(self):
        self.tree.response("api.themoviedb.org", DURING, 30)
        missing = pathlib.Path(self.dir.name) / "none.json"
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE), "--previous", str(missing)]), 0)


    def test_a_tree_it_cannot_read_is_no_verdict_never_a_healthy_one_nor_a_flag(self):
        # exit 2, which the publish step fails the leg on as unjudged: never 0 (an empty manifest published as the
        # next recording's baseline) nor 1 (an origin named as refusing that refused nothing)
        missing = pathlib.Path(self.dir.name) / "no-such-tree"
        self.assertEqual(health.main([str(missing), "--since", str(SINCE)]), 2)
        empty = pathlib.Path(self.dir.name) / "empty-tree"
        empty.mkdir()
        self.assertEqual(health.main([str(empty), "--since", str(SINCE)]), 2)
        self.tree.response("api.themoviedb.org", DURING, 30)
        torn = pathlib.Path(self.dir.name) / "torn.json"
        torn.write_text('{"origins": ')
        self.assertEqual(health.main([str(self.tree.root), "--since", str(SINCE), "--previous", str(torn)]), 2)

if __name__ == "__main__":
    unittest.main()
