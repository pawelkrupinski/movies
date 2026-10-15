#!/usr/bin/env python3
"""Unit tests for listing_history.py: take-change lines (JSON fields and the older text-only lines) read, grouped by the
deploy that logged them, the deploys read from Prometheus' worker image tags, the labels and the mirror's current state
shown — every outside call answered by a fake runner.

    python3 scripts/listing_history_test.py
"""
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import listing_history as history  # noqa: E402

GAINED = {
    "_time": "2026-10-14T06:51:29.723Z", "_msg": "identity take change: gained Casablanca-Programmkino | Der mit dem Wolf "
    "tanzt (OmU): none (X) -> tmdb 581 (Der mit dem Wolf tanzt - 35-jähriges Jubiläum) (fdf888627d354f3e); reason OwnMatch: …",
    "kind": "gained", "venue": "Casablanca-Programmkino", "listing": "Der mit dem Wolf tanzt (OmU)", "basis": "OwnMatch",
    "confidence": "0.928", "build": "56cd8fcd0c9f5d86720af4c3362d1d1cff53de52", "tmdb_after": "581",
    "title_after": "Der mit dem Wolf tanzt - 35-jähriges Jubiläum", "film_before": "fdf888627d354f3e",
    "film_after": "f00e6138193978a9", "projection_id": "2026-10-14T06:48:06.427Z", "projection_mode": "periodic",
    "pod": "worker-de-85b56b9f9f-925qs", "reason_first": "own match 581 at 60.1% by favoured-calibrated",
}
LOST_TEXT = {
    "_time": "2026-10-13T21:36:53.944Z",
    "_msg": "21:36:53 INFO  s.identity.IdentityProjection - identity take change: lost Casablanca-Programmkino | Der mit "
            "dem Wolf tanzt (OmU): tmdb 581 (Der mit dem Wolf tanzt - 35-jähriges Jubiläum) -> none (Der mit dem Wolf "
            "tanzt - 35-jähriges Jubiläum) (f00e6138193978a9); reason Vetoed: node Published(Apollo Aachen,…)",
}
OTHER_VENUE = dict(GAINED, venue="Casablanca Nürnberg", _msg=GAINED["_msg"].replace("Casablanca-Programmkino", "Casablanca Nürnberg"))


def prom(*images):
    return json.dumps({"status": "success", "data": {"result": [
        {"metric": {"image": f"ghcr.io/pawelkrupinski/movies-worker:{tag}"}, "values": [[first, "1"], [last, "1"]]}
        for tag, first, last in images]}})


A2E = ("main-20261013211040-a2e9df9", 1791926400, 1791931200)   # 10-13 21:20 → 22:40
CB = ("main-20261014063353-56cd8fc", 1791960600, 1791961500)    # 10-14 06:50 → 07:05


class Runner:
    """Answers each outside call by what it runs; records them."""

    def __init__(self, logs=(), images=(A2E, CB), mirror=None, freshness=(0, "de  kinowo_de_prod_mirror: synced (8s ago) — fresh\n"),
                 slice_takes=None, newest=(0, "38498856157\n")):
        self.calls = []
        self.newest = newest
        self.logs, self.images, self.mirror, self.freshness, self.slice_takes = logs, images, mirror, freshness, slice_takes or {}

    def __call__(self, argv, **_):
        self.calls.append(argv)
        text = " ".join(argv)
        if "logsql" in text:
            return 0, "\n".join(json.dumps(line) for line in self.logs) + "\n"
        if "query_range" in text:
            return 0, prom(*self.images)
        if argv[0].endswith("freshness"):
            return self.freshness
        if argv[0] == "mongosh":
            return 0, json.dumps(self.mirror or {"traces": [], "families": [], "venues": []})
        if argv[0] == "git":
            sha = argv[-1]
            return 0, f"{sha}{'0' * (40 - len(sha))}\tsubject of {sha}\n"
        if argv[0] == "bash" and "identity_newest_recording" in argv[2]:
            return self.newest
        if argv[0].endswith("label-lookup"):
            return 0, f"label of {argv[argv.index('--title') + 1]}\n"
        if argv[0].endswith("identity-slice"):
            ref = argv[argv.index("--ref") + 1]
            view = "ci" if "--families" in argv else "mirror" if "--prod-mirror" in argv else "prod"
            film = self.slice_takes.get((ref, view), self.slice_takes.get(ref, "none"))
            prod = "PROD Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU): tmdb:581 via Agreed imdb\n" if view == "mirror" else ""
            return 0, f"== «x»\nmodel: something\nTAKE Casablanca-Programmkino: {film} via X — -\n{prod}"
        raise AssertionError(f"unexpected call {argv}")


def run(runner, *extra):
    args = history.parse_args(["de", "--venue", "casablanca-programm", "--title", "wolf tanzt", *extra])
    return history.report(args, runner)


class ParseTest(unittest.TestCase):
    def test_a_json_line_is_read_by_its_fields(self):
        change = history.parse_change(GAINED)
        self.assertEqual((change.kind, change.venue, change.listing, change.before, change.after, change.build[:7]),
                         ("gained", "Casablanca-Programmkino", "Der mit dem Wolf tanzt (OmU)", "none", "tmdb 581", "56cd8fc"))

    def test_an_older_text_line_is_read_from_its_message(self):
        change = history.parse_change(LOST_TEXT)
        self.assertEqual((change.kind, change.venue, change.listing, change.before, change.after, change.basis, change.build),
                         ("lost", "Casablanca-Programmkino", "Der mit dem Wolf tanzt (OmU)", "tmdb 581", "none", "Vetoed", ""))

    def test_the_logsql_selects_take_changes_of_the_country_by_venue_and_title(self):
        query = history.logsql("de", "Casablanca (Nürnberg)", "wolf", "7d")
        self.assertIn('_time:7d "identity take change" country:de', query)
        # Go's RE2 escapes punctuation only (a space escaped is an error), inside a Go-quoted LogsQL string
        self.assertIn(r"[^|]*Casablanca \\(Nürnberg\\)[^|]*", query)

    def test_since_reads_days_and_hours(self):
        self.assertEqual((history.seconds("7d"), history.seconds("36h")), (7 * 86400, 36 * 3600))


class ReportTest(unittest.TestCase):
    def test_changes_are_grouped_by_the_deploy_that_logged_them(self):
        out = run(Runner(logs=[GAINED, LOST_TEXT, OTHER_VENUE]))
        self.assertIn("DEPLOY a2e9df9", out)
        self.assertIn("DEPLOY 56cd8fc", out)
        self.assertLess(out.index("DEPLOY a2e9df9"), out.index("10-13 21:36:53 lost"))
        self.assertLess(out.index("10-13 21:36:53 lost"), out.index("DEPLOY 56cd8fc"))
        self.assertIn("subject of 56cd8fc", out)
        self.assertNotIn("Nürnberg", out)   # another venue the regex let through is dropped
        self.assertIn("TAKES Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU): tmdb 581 → none (10-13 21:36, a2e9df9) → tmdb 581 (10-14 06:51, 56cd8fc)", out)

    def test_a_listing_flipping_within_one_deploy_is_called_out(self):
        back = dict(GAINED, _time="2026-10-14T07:01:00Z", kind="lost", tmdb_before="581", tmdb_after=None,
                    pod="worker-de-85b56b9f9f-x2", _msg=GAINED["_msg"].replace("gained", "lost"))
        back.pop("tmdb_after")
        out = run(Runner(logs=[GAINED, back]))
        self.assertIn("FLIPS within deploy 56cd8fc: 2 changes, 2 pods", out)

    def test_the_labels_of_every_spelling_come_from_label_lookup(self):
        mirror = {"traces": [{"listing": {"venue": "Casablanca-Programmkino", "rawTitle": "Der mit dem Wolf tanzt"},
                              "family": "F1", "film": 581, "basis": "OwnMatch"}], "families": [], "venues": []}
        runner = Runner(logs=[GAINED], mirror=mirror)
        out = run(runner)
        lookups = [a for a in runner.calls if a[0].endswith("label-lookup")]
        self.assertEqual([a[a.index("--title") + 1] for a in lookups], ["Der mit dem Wolf tanzt", "Der mit dem Wolf tanzt (OmU)"])
        self.assertTrue(all(a[1] == "de" and "--no-prod" in a and a[a.index("--venue") + 1] == "casablanca-programm"
                            for a in lookups))
        self.assertIn("  label of Der mit dem Wolf tanzt (OmU)", out)

    def test_the_mirror_s_current_state_is_shown_after_its_freshness(self):
        mirror = {"traces": [{"listing": {"venue": "Casablanca-Programmkino", "rawTitle": "Der mit dem Wolf tanzt (OmU)"},
                              "family": "F1", "film": 581, "basis": "Agreed", "rules": ["accept:x"], "vetoedBy": None,
                              "evidence": ["year.delta=0 +3.40"], "refusals": [], "candidates": ["581 60% rank 1"]}],
                  "families": [{"_id": "F1", "decisions": [{"members": [{"venue": "Casablanca-Programmkino",
                                                                        "rawTitle": "Der mit dem Wolf tanzt (OmU)"}],
                                                           "film": 581, "basis": "OwnMatch", "confidence": 0.6,
                                                           "explanation": ["own match 581"], "agreed": {}}]}],
                  "venues": [{"_id": "Casablanca-Programmkino", "films": [{"movie": {"title": "Der mit dem Wolf tanzt (OmU)",
                                                                                     "releaseYear": 2026}, "director": [],
                                                                           "showtimes": [{}, {}], "externalIds": {}}]}],
                  "agreements": [{"_id": "F1", "agreed": {"title": "Dances with Wolves", "families": ["imdb", "wikidata"]},
                                  "filled": None, "correction": None}],
                  "pins": [{"kind": "is-film", "tmdbId": 581, "author": "pawel", "reason": "checked"}]}
        runner = Runner(logs=[GAINED], mirror=mirror)
        out = run(runner)
        self.assertIn("synced (8s ago) — fresh", out)
        self.assertLess(out.index("fresh"), out.index("TRACE"))
        self.assertIn("TRACE Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU): film 581 via Agreed", out)
        self.assertIn("FAMILY F1: film 581 via OwnMatch at 60%", out)
        self.assertIn("LISTING Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU): year 2026, 2 showtimes", out)
        self.assertIn("AGREEMENT F1: agreed «Dances with Wolves» by imdb, wikidata", out)
        self.assertIn("PIN is-film 581 by pawel: checked", out)
        self.assertTrue(any(a[0].endswith("freshness") and "identity_traces" in a[3] for a in runner.calls))
        self.assertTrue(any(a[0] == "mongosh" and "127.0.0.1:28017/kinowo_de_prod_mirror" in " ".join(a) for a in runner.calls))

    def test_a_stale_mirror_is_said_so_and_still_read(self):
        out = run(Runner(logs=[GAINED], freshness=(3, "de  kinowo_de_prod_mirror: synced 9h ago — STALE\n")))
        self.assertIn("STALE", out)
        self.assertIn("the mirror is not fresh", out)

    def test_replay_decides_the_listing_at_each_deploy_s_commit_on_one_recording(self):
        runner = Runner(logs=[GAINED, LOST_TEXT], slice_takes={"a2e9df9" + "0" * 33: "none", "56cd8fc" + "0" * 33: "tmdb:581"})
        # CI's view: both deploys tmdb:581 (the filed answers are what moved it)
        runner.slice_takes[("a2e9df9" + "0" * 33, "ci")] = "tmdb:581"
        out = run(runner, "--replay", "--recording", "38498856157")
        slices = [a for a in runner.calls if a[0].endswith("identity-slice")]
        # each deploy once in each view: CI's (no filed answers), the capture's filed answers, and prod's state now
        self.assertEqual([(a[a.index("--ref") + 1][:7], "--families" in a, "--prod-mirror" in a) for a in slices],
                         [("a2e9df9", True, False), ("a2e9df9", False, False), ("a2e9df9", False, True),
                          ("56cd8fc", True, False), ("56cd8fc", False, False), ("56cd8fc", False, True)])
        self.assertTrue(all("--recording" in a and "38498856157" in a for a in slices))
        self.assertTrue(all("Casablanca-Programmkino|Der mit dem Wolf tanzt (OmU)" in a for a in slices))
        self.assertIn("REPLAY a2e9df9 Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU) (prod: tmdb 581 → none):", out)
        self.assertIn("  CI (no filed answers): TAKE Casablanca-Programmkino: tmdb:581", out)
        self.assertIn("no earlier deploy in the window to replay against", out)
        self.assertIn("REPRODUCED: a2e9df9 takes none, 56cd8fc tmdb 581", out)

    def test_replay_defaults_to_the_newest_recording_holding_the_country_s_corpus(self):
        runner = Runner(logs=[GAINED])
        run(runner, "--replay")
        newest = [a for a in runner.calls if a[0] == "bash"]
        self.assertEqual([(a[-2].endswith("scripts/identity-tools.sh"), a[-1]) for a in newest], [(True, "de")])
        slices = [a for a in runner.calls if a[0].endswith("identity-slice")]
        self.assertTrue(slices and all(a[a.index("--recording") + 1] == "38498856157" for a in slices))

    def test_no_recording_found_replays_nothing_and_says_why(self):
        runner = Runner(logs=[GAINED], newest=(1, "[identity] could not list the runs (gh failed or timed out)\n"))
        out = run(runner, "--replay")
        self.assertFalse(any(a[0].endswith("identity-slice") for a in runner.calls))
        self.assertIn("REPLAY: no recording named, and the newest green one holding a de corpus could not be read: "
                      "[identity] could not list the runs (gh failed or timed out)", out)

    def test_a_change_only_prod_s_filed_answers_reproduce_is_the_ci_gap(self):
        a2e, cb = "a2e9df9" + "0" * 33, "56cd8fc" + "0" * 33
        takes = {(a2e, "ci"): "none", (cb, "ci"): "none", (a2e, "prod"): "tmdb:1710881", (cb, "prod"): "none"}
        lost = dict(GAINED, kind="lost", tmdb_before="1710881", _msg=GAINED["_msg"].replace("gained", "lost"))
        lost.pop("tmdb_after")
        out = run(Runner(logs=[lost], slice_takes=takes), "--replay", "--recording", "1")
        ci, prod = out.index("CI (no filed answers)"), out.index("with prod's filed answers")
        self.assertIn("NOT REPRODUCED: a2e9df9 takes none too", out[ci:prod])
        self.assertIn("REPRODUCED: a2e9df9 takes tmdb 1710881, 56cd8fc none", out[prod:])

    def test_replay_reads_prod_s_state_now_from_the_mirror_and_its_trace_for_the_reason(self):
        mirror = {"traces": [{"listing": {"venue": "Casablanca-Programmkino", "rawTitle": "Der mit dem Wolf tanzt (OmU)"},
                              "family": "F1", "film": None, "basis": "BelowThreshold", "rules": ["refused:year"],
                              "blocker": "year.delta"}], "families": [], "venues": []}
        out = run(Runner(logs=[GAINED], mirror=mirror, slice_takes={("56cd8fc" + "0" * 33, "mirror"): "tmdb:581"}),
                  "--replay", "--recording", "1")
        self.assertIn("  prod now (mirror identity_traces): film none via BelowThreshold; rules refused:year; blocker year.delta", out)
        self.assertIn("  with prod's state now (--prod-mirror): TAKE Casablanca-Programmkino: tmdb:581", out)
        self.assertIn("      PROD Casablanca-Programmkino | Der mit dem Wolf tanzt (OmU): tmdb:581 via Agreed imdb", out)

    def test_an_unreadable_mirror_replays_without_prod_s_state(self):
        runner = Runner(logs=[GAINED], freshness=(2, "no mirror\n"))
        out = run(runner, "--replay", "--recording", "1")
        self.assertFalse(any("--prod-mirror" in a for a in runner.calls if a[0].endswith("identity-slice")))
        self.assertIn("  prod now (mirror identity_traces): the mirror is not readable", out)

    def test_a_change_the_replay_does_not_move_is_not_reproduced(self):
        runner = Runner(logs=[GAINED], slice_takes={"a2e9df9" + "0" * 33: "tmdb:581", "56cd8fc" + "0" * 33: "tmdb:581"})
        out = run(runner, "--replay", "--recording", "1")
        self.assertIn("NOT REPRODUCED: a2e9df9 takes tmdb 581 too", out)

    def test_one_billing_moved_at_many_venues_is_one_line_and_one_replay(self):
        many = [dict(GAINED, venue=f"Cineworld {i}", _msg=GAINED["_msg"].replace("Casablanca-Programmkino", f"Cineworld {i}"))
                for i in range(5)]
        runner = Runner(logs=many)
        args = history.parse_args(["de", "--title", "wolf tanzt", "--replay", "--recording", "1"])
        out = history.report(args, runner)
        self.assertIn("TAKES 5 venues (Cineworld 0, Cineworld 1, Cineworld 2, …) | Der mit dem Wolf tanzt (OmU): none → tmdb 581", out)
        self.assertIn("gained  Der mit dem Wolf tanzt (OmU) at 5 venues (Cineworld 0, …): none → tmdb 581", out)
        self.assertEqual(len([a for a in runner.calls if a[0].endswith("identity-slice") and "56cd8fc" in a[a.index("--ref") + 1]]), 3)

    def test_no_change_logged_says_so(self):
        out = run(Runner(logs=[]))
        self.assertIn("no take change logged", out)


if __name__ == "__main__":
    unittest.main()
