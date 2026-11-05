#!/usr/bin/env python3
"""Unit tests for flip_timeline.py: the timeline and the reversal classes over captured prod rows (identity_history from
the local mirror, VictoriaLogs projection and take-change lines; scripts/testdata/flip-timeline/*.json, written by
`flip-timeline --dump` and trimmed to the moments around the takes), and the gathering through a fake runner.

    python3 scripts/flip_timeline_test.py
"""
import contextlib
import io
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import flip_timeline as timeline  # noqa: E402

FIXTURES = os.path.join(os.path.dirname(os.path.abspath(__file__)), "testdata", "flip-timeline")


def fixture(name):
    with open(os.path.join(FIXTURES, f"{name}.json"), encoding="utf-8") as f:
        return json.load(f)


def reversal_lines(out):
    return [l.strip() for l in out.split("REVERSALS", 1)[1].splitlines()[1:] if l.strip()]


def verdicts(out):
    return [l.split()[0] for l in reversal_lines(out)[::2]]


# Lichtblick "Buddies" (DE drift seed 64, BUDDIES-STATE.md's lattice over drift-slices/de/programmkino-lichtblick-e-v-
# buddies-69181215): {Lichtblick} takes 1629957 by the lean fill; Lichtwerk joins, its node denies 1629957 -> none
# (Vetoed); Pollux joins, three venues -> venues.current fills 1629957 again. Same build, same pod, scrape-triggered, no
# ask filed: only the population moved. Prod never ran this order inside identity_history's week, so the rows are
# the lattice's steps written as prod would keep them.
L = "N\x00Programmkino Lichtblick e.V.\x00https://www.kinoheld.de/film/buddies-2026\x00Buddies"
W = "P\x00Lichtwerk im Ravensberger Park\x00Buddies\x00"
P = "P\x00Pollux by Cineplex Paderborn\x00Buddies\x00"
DAY = 1793750400000   # 2026-11-03T00:00:00Z


def _take(at, kind, before, after, stage, basis, decided_by, members, previous=""):
    return {"at": DAY + at, "id": L, "build": "2405daba00f", "after": {
        "kind": kind, "venue": "Programmkino Lichtblick e.V.", "listing": "Buddies", "tmdb_before": before,
        "tmdb_after": after, "stage": stage, "basis": basis, "decided_by": decided_by, "previous_decided_by": previous,
        "trigger": "scrape", "cause": "evidence", "members": members}}


def _projection(start_ms, seconds, trigger="scrape", agreement=1.0):
    start = DAY + start_ms
    return {"_time": timeline._iso((start + seconds * 1000) / 1000), "projection_id": timeline._iso(start / 1000),
            "projection_trigger": trigger, "projection_mode": "changed", "pod": "worker-de-1", "build": "2405daba00f",
            "_msg": f"identity projection: 1 listings → 1 films; wrote 1 in {seconds}s (listings 0.1s/1MB, snapshot 0.0s/1MB, "
                    f"resolve 0.4s/1MB, agreement {agreement}s/1MB, draft 0.1s/1MB)"}


def buddies_lattice():
    return {"cc": "de", "venue": "Lichtblick", "title": "Buddies", "since": "7d", "now": DAY / 1000 + 86400, "notes": [],
            "freshness": "de  kinowo_de_prod_mirror: synced (fixture)",
            "history": {"takes": [
                _take(3_600_000, "gained", None, 1629957, "fill", "Filled", "fill:model.lean", [L]),
                _take(7_200_000, "lost", 1629957, None, "model", "Vetoed", "veto:denied", [L, W], "fill:model.lean"),
                _take(10_800_000, "gained", None, 1629957, "fill", "Filled", "fill:venues.current", [L, W, P], "veto:denied")],
                "siblings": [{"at": DAY + 7_200_000, "id": W, "members": [L, W]},
                             {"at": DAY + 10_800_000, "id": W, "members": [L, W, P]},
                             {"at": DAY + 10_800_000, "id": P, "members": [L, W, P]}],
                "asks": [], "answers": [], "listings": [{"at": DAY + 10_799_000, "id": P}]},
            "changes": [], "histogram": {},
            "projections": [_projection(3_598_000, 2.0), _projection(7_198_000, 2.0), _projection(10_798_000, 2.0)]}


class CapturedCasesTest(unittest.TestCase):
    """The cases the await-answers investigation reconstructed by hand (AWAIT-ANSWERS-STATE.md, 2026-11-05)."""

    def test_geister_at_abaton_is_a_late_answer(self):
        out = timeline.render(fixture("geister"))
        self.assertEqual(verdicts(out), ["LATE-ANSWER"])
        # the dropping projection (scrape, 17:10:55.75) asked inline; metacritic's title answer was filed 8.8 s into
        # its agreement phase, past the 5 s wait, before the drop was written at 17:11:08.77
        self.assertIn("PROJ  scrape             17:10:55.75 -> 17:11:08.67", out)
        self.assertIn("ASK   metacritic title «Geister» answered · inline in 17:10:55.75 (sent ~17:10:57.45, +8.8 s)", out)
        self.assertIn("ASK   filmweb    title «Geister» answered · inline in 17:10:55.75 (sent ~17:10:57.45, +0.5 s)\n11-02", out)
        late = out.split("ASK   metacritic title «Geister»", 1)[1].splitlines()[1]
        self.assertIn("LATE: after projection 17:10:55.75 gave up waiting", late)
        self.assertIn("stage model · BelowThreshold · pooled:no-film (was accept:joined)", out)
        self.assertIn("PROJ  answer+scrape      17:11:08.85", out)
        self.assertIn("stage fill · Filled · fill:venues.current", out)
        self.assertIn("the answer-triggered projection 17:11:08.85 restored it by fill:venues.current", out)

    def test_hay_fever_at_arc_stockton_reversed_twice_on_a_late_poster_vote(self):
        out = timeline.render(fixture("hayfever"))
        # 12:00:39 and 13:39:22: both restored by poster:vote in an answer-triggered projection, nothing filed in the
        # history (posters are not kept there); between them a deploy (ee31642 -> 201e45b) took the film again
        self.assertEqual(verdicts(out), ["LATE-ANSWER", "DEPLOY/RESTART", "LATE-ANSWER"])
        lines = reversal_lines(out)
        self.assertIn("(10-30 12:00:39.85) -> tmdb 1791571 (+8.9 s)", lines[0])
        self.assertIn("inferred: restored by poster:vote", lines[1])
        self.assertIn("the build changed between them: ee31642 -> 201e45b", lines[3])
        self.assertIn("(10-30 13:39:22.67) -> tmdb 1791571 (+4.9 s)", lines[4])

    def test_eternity_dropped_by_a_boot_projection_is_a_deploy(self):
        out = timeline.render(fixture("eternity"))
        self.assertEqual(verdicts(out), ["DEPLOY/RESTART"])
        self.assertIn("PROJ  boot ", out)
        self.assertIn("the dropping projection was a boot (trigger deploy)", out)

    def test_buddies_moved_by_its_population_is_a_sibling_reversal(self):
        out = timeline.render(buddies_lattice())
        # none -> 1629957 -> none -> 1629957: two reversals, Lichtwerk's arrival and Pollux's
        self.assertEqual(verdicts(out), ["SIBLING/POPULATION", "SIBLING/POPULATION"])
        self.assertIn("joined Lichtwerk im Ravensberger Park | Buddies", out)
        self.assertIn("joined Pollux by Cineplex Paderborn | Buddies", out)


class ClassifyTest(unittest.TestCase):
    def test_nothing_moved_but_the_decision_is_a_rule_reversal(self):
        raw = buddies_lattice()
        for take in raw["history"]["takes"]:
            take["after"]["members"] = [L]
        raw["history"]["siblings"], raw["history"]["listings"] = [], []
        out = timeline.render(raw)
        self.assertEqual(verdicts(out), ["RULE", "RULE"])
        self.assertIn("veto:denied then fill:venues.current on the same inputs", out)

    def test_a_pod_change_with_the_same_build_is_a_restart(self):
        raw = buddies_lattice()
        raw["projections"][2]["pod"] = "worker-de-2"
        self.assertEqual(verdicts(timeline.render(raw)), ["SIBLING/POPULATION", "DEPLOY/RESTART"])

    def test_an_ask_filed_within_the_inline_wait_is_not_late(self):
        raw = buddies_lattice()
        raw["projections"][1] = _projection(7_198_000, 10.0, agreement=8.0)
        raw["projections"][2]["projection_trigger"] = "answer"
        # the dropping projection began 7198.0 s in, its agreement phase +0.5 s to +8.5 s; the inline wait ends at +5.5 s
        ask = lambda at: {"at": DAY + at, "id": "unanswered|rt|title|Buddies", "outcome": "answered", "failures": 0}
        raw["history"]["asks"] = [ask(7_198_000 + 4_000)]
        self.assertEqual(verdicts(timeline.render(raw)), ["SIBLING/POPULATION", "SIBLING/POPULATION"])
        raw["history"]["asks"] = [ask(7_198_000 + 4_000), ask(7_198_000 + 6_000)]
        out = timeline.render(raw)
        self.assertEqual(verdicts(out), ["SIBLING/POPULATION", "LATE-ANSWER"])
        self.assertIn("1 ask(s) of the cluster filed late, the last rt title «Buddies»", out)

    def test_the_agreement_phase_starts_after_the_phases_before_it(self):
        projection = timeline.parse_projection(_projection(0, 2.0))
        self.assertAlmostEqual(projection["agreement"][0] - projection["start"], 0.5)
        self.assertAlmostEqual(projection["agreement"][1] - projection["start"], 1.5)

    def test_a_take_only_the_log_holds_joins_the_timeline(self):
        raw = buddies_lattice()
        raw["history"]["takes"] = raw["history"]["takes"][:2]
        raw["changes"] = [{"_time": timeline._iso(DAY / 1000 + 10_800), "kind": "gained", "venue": "Programmkino Lichtblick e.V.",
                           "listing": "Buddies", "tmdb_after": "1629957", "stage": "fill", "decided_by": "fill:venues.current",
                           "projection_trigger": "scrape", "build": "2405daba00f", "members": "3"}]
        out = timeline.render(raw)
        self.assertIn("(VictoriaLogs only)", out)
        self.assertEqual(len(verdicts(out)), 2)


class GatherTest(unittest.TestCase):
    def test_it_reads_the_mirror_after_its_freshness_then_the_logs_and_dumps_what_it_read(self):
        calls = []
        history = buddies_lattice()["history"]

        def run(argv, **_):
            calls.append(argv)
            text = " ".join(argv)
            if argv[0].endswith("freshness"):
                return 0, "de  kinowo_de_prod_mirror: synced — fresh\n"
            if argv[0] == "mongosh":
                return 0, json.dumps(history) + "\n"
            if "query_range" not in text and "/api/v1/query" in text:
                return 0, json.dumps({"data": {"result": []}})
            if "logsql" in text and "identity projection" in text:
                return 0, "\n".join(json.dumps(p) for p in buddies_lattice()["projections"]) + "\n"
            if "logsql" in text:
                return 0, ""
            raise AssertionError(f"unexpected call {argv}")

        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()) as printed:
                timeline.main(["de", "--venue", "Lichtblick", "--title", "Buddies", "--dump", dump], run=run, now=DAY / 1000 + 86400)
            self.assertEqual(verdicts(printed.getvalue()), ["SIBLING/POPULATION", "SIBLING/POPULATION"])
            with contextlib.redirect_stdout(io.StringIO()) as again:
                timeline.main(["de", "--title", "x", "--from", dump], run=None)
            self.assertEqual(again.getvalue(), printed.getvalue())
        self.assertTrue(calls[0][0].endswith("local-mirror/freshness") and "identity_history" in calls[0])
        self.assertIn("kinowo_de_prod_mirror", calls[1][2])
        self.assertIn('TITLE = "Buddies"', calls[1][-1])
        self.assertTrue(any('"identity projection:"' in " ".join(c) for c in calls))

    def test_an_unreadable_mirror_is_said_not_guessed(self):
        def run(argv, **_):
            return (2, "no mirror\n") if argv[0].endswith("freshness") else (1, "ssh: no route")
        with contextlib.redirect_stdout(io.StringIO()) as printed:
            timeline.main(["uk", "--title", "Hay Fever"], run=run, now=DAY / 1000)
        out = printed.getvalue()
        self.assertIn("NOTE the mirror is not readable", out)
        self.assertIn("no take change of it kept", out)


if __name__ == "__main__":
    unittest.main()
