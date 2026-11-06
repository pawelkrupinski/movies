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
import re
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


class FakeMirror:
    """The local mirror and the logs as a runner: mongosh scripts answered by the marker comment each starts with, from
    the takes, siblings, asks and listings it is given — paging as the server does (PAGE rows after the cursor)."""

    def __init__(self, takes, siblings=(), asks=(), listings=(), projections=()):
        self.takes = sorted(takes, key=lambda t: (t["at"], t["oid"]))
        self.siblings, self.asks, self.listings, self.projections = list(siblings), list(asks), list(listings), list(projections)
        self.calls = []

    def scripts(self, marker):
        return [c[-1] for c in self.calls if c[0] == "mongosh" and f"// {marker}\n" in c[-1]]

    def __call__(self, argv, **_):
        self.calls.append(argv)
        text = " ".join(argv)
        if argv[0].endswith("freshness"):
            return 0, "de  kinowo_de_prod_mirror: synced (fixture)\n"
        if argv[0] == "mongosh":
            return 0, json.dumps(self.answer(argv[-1])) + "\n"
        if "logsql" in text and "identity projection" in text:
            return 0, "\n".join(json.dumps(p) for p in self.projections) + "\n"
        if "logsql" in text or "/api/v1/query" in text:
            return 0, ""
        raise AssertionError(f"unexpected call {argv}")

    def answer(self, script):
        const = lambda name, default=None: (re.search(rf"\b{name} = (\[.*?\]|\"[^\"]*\"|\d+)[,;]", script) or [None, default])[1]
        if "// takes\n" in script:
            page, at, oid = int(const("PAGE", 400)), int(const("AFTER_AT", 0)), json.loads(const("AFTER_ID", '""'))
            rows = [t for t in self.takes if (t["at"], t["oid"]) > (at, oid)] if oid else self.takes
            return [dict(t, readIds=[]) for t in rows[:page]]
        if "// siblings\n" in script:
            ids = set(json.loads(const("IDS")))
            return [s for s in self.siblings if s["id"] in ids]
        if "// asks\n" in script:
            return {"asks": self.asks, "answers": []}
        if "// listings\n" in script:
            ids = set(json.loads(const("IDS")))
            return [l for l in self.listings if l["id"] in ids]
        return []


def mirror_take(index, at, before=None, after=1, members=None, key=None):
    key = key or f"N\x00Cinema {index}\x00https://x/{index}\x00Film {index}"
    return {"at": at, "id": key, "oid": f"{index:024x}", "build": "2405daba00f", "after": {
        "kind": "gained", "venue": f"Cinema {index}", "listing": f"Film {index}", "tmdb_before": before, "tmdb_after": after,
        "stage": "fill", "basis": "Filled", "decided_by": "fill:venues.current", "trigger": "scrape", "cause": "evidence",
        "members": members if members is not None else [key]}}


class TitleSearchTest(unittest.TestCase):
    """«(IMAX) Sinners» (24 UK listings, a known late answer) was missed by a search that took the brackets literally
    on one side and the resolver's clean title on the other."""

    def test_a_parenthesised_title_finds_its_listing_with_or_without_the_brackets(self):
        pattern = re.compile(timeline.title_pattern("(IMAX) Sinners"), re.I)
        self.assertTrue(pattern.search("(IMAX) Sinners"))
        self.assertTrue(pattern.search("IMAX Sinners"))
        self.assertTrue(pattern.search("imax - sinners"))
        self.assertFalse(pattern.search("Sinners"))
        self.assertEqual(timeline.title_pattern("Geister"), "Geister")

    def test_the_search_the_mirror_and_the_logs_get_is_that_pattern(self):
        calls = []

        def run(argv, **_):
            calls.append(" ".join(argv))
            return (0, "") if not argv[0].endswith("freshness") else (0, "uk fresh")
        with contextlib.redirect_stdout(io.StringIO()):
            timeline.main(["uk", "--title", "(IMAX) Sinners", "--dump", os.devnull], run=run, now=DAY / 1000)
        self.assertTrue(any('TITLE = "IMAX[^0-9A-Za-z]+Sinners"' in c for c in calls))
        self.assertTrue(any("IMAX[^0-9A-Za-z]+Sinners" in c and "logsql" in c for c in calls))

    def test_a_take_change_only_the_log_holds_is_found_by_the_unbracketed_title(self):
        raw = buddies_lattice()
        raw["title"], raw["history"]["takes"] = "IMAX Buddies", []
        change = {"_time": timeline._iso(DAY / 1000 + 10_800), "kind": "gained", "venue": "Programmkino Lichtblick e.V.",
                  "listing": "(IMAX) Buddies", "tmdb_after": "1629957", "stage": "fill", "decided_by": "fill:venues.current",
                  "projection_trigger": "scrape", "build": "2405daba00f", "members": "3"}
        self.assertEqual(len(timeline.merge_logged([], [change], raw)), 1)

    def test_family_asks_name_the_clean_title(self):
        needle = re.compile(timeline.ask_needle("(IMAX) Sinners"), re.I)
        self.assertTrue(needle.search("unanswered|metacritic|record|sinners"))
        self.assertTrue(needle.search("unanswered|rt|title|Sinners"))
        self.assertTrue(needle.search("unanswered|rt|record|sinners-2025"))
        self.assertFalse(needle.search("unanswered|metacritic|record|two-sinners-and-a-mule"))


class EveryReversalTest(unittest.TestCase):
    """`flip-timeline <cc> --all`: every reversal of a country, classified; the fixtures are prod's take changes (and the
    asks and projections around them) of DE «Geister» and UK «(IMAX) Sinners», read 2026-11-06."""

    def table(self, raw):
        rows = timeline.analyse_all(raw)
        return {c: sum(1 for r in rows if r["class"] == c) for c in timeline.CLASSES}

    def test_every_geister_reversal_is_a_late_answer(self):
        counts = self.table(fixture("all-de-geister"))
        self.assertEqual(counts["LATE-ANSWER"], 56)
        self.assertEqual(sum(counts.values()), 56)

    def test_sinners_at_24_uk_cinemas_is_a_late_answer_but_for_the_two_a_boot_restored(self):
        counts = self.table(fixture("all-uk-sinners"))
        self.assertEqual((counts["LATE-ANSWER"], counts["DEPLOY/RESTART"], sum(counts.values())), (22, 2, 24))

    def test_the_count_table_and_a_tsv_row_per_reversal(self):
        tsv = io.StringIO()
        out = timeline.render_all(fixture("all-de-geister"), summary=True, tsv=tsv)
        self.assertIn("LATE-ANSWER             56    100%", out)
        self.assertIn("total                   56", out)
        self.assertNotIn("Geister", out)    # --summary: the table only
        lines = tsv.getvalue().splitlines()
        self.assertEqual(lines[0].split("\t"), list(timeline.TSV_COLUMNS))
        self.assertEqual(len(lines), 57)
        self.assertTrue(all(l.startswith("LATE-ANSWER\tde\t") for l in lines[1:]))
        full = timeline.render_all(fixture("all-de-geister"))
        self.assertIn("| Geister", full)
        self.assertIn("filed late", full)

    def test_a_take_that_went_back_after_a_day_is_a_new_decision_not_a_reversal(self):
        raw = buddies_lattice()
        raw["history"]["takes"][2]["at"] += 90_000_000
        rows = timeline.analyse_all(raw)
        self.assertEqual([r["restored_after_s"] for r in rows], [3600.0])

    def test_a_restore_with_no_projection_line_is_unclassified_and_says_why(self):
        raw = buddies_lattice()
        raw["projections"] = []
        for take in raw["history"]["takes"]:
            take["after"]["members"] = [L]
        raw["history"]["siblings"] = raw["history"]["listings"] = []
        rows = timeline.analyse_all(raw)
        self.assertEqual({r["class"] for r in rows}, {"UNCLASSIFIED"})
        self.assertIn("no projection line found", rows[0]["why"])

    def test_an_answer_triggered_restore_with_no_ask_named_by_its_title_is_unclassified(self):
        raw = buddies_lattice()
        for take in raw["history"]["takes"]:
            take["after"]["members"] = [L]
        raw["history"]["siblings"] = raw["history"]["listings"] = []
        raw["projections"][1]["projection_trigger"] = raw["projections"][2]["projection_trigger"] = "answer"
        rows = timeline.analyse_all(raw)
        self.assertEqual([r["class"] for r in rows], ["UNCLASSIFIED", "UNCLASSIFIED"])
        self.assertIn("answer-triggered projection, but no ask naming the listing's title", rows[0]["why"])

    def test_it_pages_all_changes_then_reads_asks_projections_and_listings_around_the_reversals_only(self):
        history = buddies_lattice()["history"]
        takes = [dict(t, oid=f"{i:024x}") for i, t in enumerate(history["takes"])]
        takes += [mirror_take(100 + i, DAY + 20_000_000 + i * 1000) for i in range(450)]    # one-way changes: no reversal
        for take in takes[3:]:
            take["after"]["members"] = [take["id"]]
        for take in takes[:3]:
            take["after"]["members"] = [L]
        mirror = FakeMirror(takes, projections=buddies_lattice()["projections"])
        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()) as printed:
                timeline.main(["de", "--all", "--summary", "--dump", dump], run=mirror, now=DAY / 1000 + 86400)
            self.assertIn("2 reversal(s) among 453 take change(s)", printed.getvalue())
            with contextlib.redirect_stdout(io.StringIO()) as again:
                timeline.main(["de", "--all", "--summary", "--from", dump], run=None)
            self.assertEqual(again.getvalue(), printed.getvalue())
        self.assertEqual(len(mirror.scripts("takes")), 2)
        self.assertEqual(len(mirror.scripts("all-asks")), 1)
        self.assertEqual(mirror.scripts("siblings"), [])
        self.assertIn("RULE", printed.getvalue())

    def test_all_takes_no_venue_or_title(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            timeline.parse_args(["de", "--all", "--title", "x"])


class GatherTest(unittest.TestCase):
    def test_it_reads_the_mirror_after_its_freshness_then_the_logs_and_dumps_what_it_read(self):
        history = buddies_lattice()["history"]
        mirror = FakeMirror([dict(t, oid=f"{i:024x}") for i, t in enumerate(history["takes"])], history["siblings"],
                            projections=buddies_lattice()["projections"])
        calls = mirror.calls
        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()) as printed:
                timeline.main(["de", "--venue", "Lichtblick", "--title", "Buddies", "--dump", dump], run=mirror, now=DAY / 1000 + 86400)
            self.assertEqual(verdicts(printed.getvalue()), ["SIBLING/POPULATION", "SIBLING/POPULATION"])
            with contextlib.redirect_stdout(io.StringIO()) as again:
                timeline.main(["de", "--title", "x", "--from", dump], run=None)
            self.assertEqual(again.getvalue(), printed.getvalue())
        self.assertTrue(calls[0][0].endswith("local-mirror/freshness") and "identity_history" in calls[0])
        self.assertIn("kinowo_de_prod_mirror", calls[1][2])
        self.assertIn('TITLE = "Buddies"', calls[1][-1])
        self.assertTrue(any('"identity projection:"' in " ".join(c) for c in calls))

    def test_a_countrys_week_of_take_changes_is_read_in_full_not_the_first_400(self):
        # DE keeps 918 take changes a week: an empty-title run read the first 400 and undercounted every class
        mirror = FakeMirror([mirror_take(i, DAY + i * 1000) for i in range(950)])
        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()):
                timeline.main(["de", "--venue", "Cinema", "--title", "", "--dump", dump], run=mirror, now=DAY / 1000 + 86400)
            with open(dump, encoding="utf-8") as f:
                kept = json.load(f)["history"]["takes"]
        self.assertEqual(len(kept), 950)
        self.assertEqual(len({t["id"] for t in kept}), 950)
        self.assertEqual(len(mirror.scripts("takes")), 3)

    def test_takes_changed_in_one_millisecond_are_paged_by_their_id_not_skipped(self):
        mirror = FakeMirror([mirror_take(i, DAY) for i in range(1000)])
        with contextlib.redirect_stdout(io.StringIO()):
            timeline.main(["de", "--title", "Film", "--dump", os.devnull], run=mirror, now=DAY / 1000 + 86400)
        self.assertEqual(len({c for s in mirror.scripts("takes") for c in re.findall(r'AFTER_ID = "(\w*)"', s)}), 3)

    def test_a_family_of_300_members_still_reads_its_siblings(self):
        # families over 120 members skipped their siblings' takes and listings, so most reversals never paired up
        members = [f"P\x00Cinema {i}\x00Film\x00" for i in range(300)]
        siblings = [{"at": DAY + 5000, "id": m, "members": members[:299]} for m in members[::40]]
        listings = [{"at": DAY + 6000, "id": members[7]}]
        mirror = FakeMirror([mirror_take(1, DAY + 9000, members=members)], siblings, listings=listings)
        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()) as printed:
                timeline.main(["de", "--title", "Film 1", "--dump", dump], run=mirror, now=DAY / 1000 + 86400)
            with open(dump, encoding="utf-8") as f:
                history = json.load(f)["history"]
        self.assertNotIn("over 120", printed.getvalue())
        self.assertEqual({s["id"] for s in history["siblings"]}, set(members[::40]))
        self.assertEqual(history["listings"], listings)
        self.assertEqual(len(mirror.scripts("siblings")), 2)    # 300 member ids, 200 to a call

    def test_a_siblings_unchanged_decisions_are_kept_once(self):
        members = [f"P\x00Cinema {i}\x00Film\x00" for i in range(3)]
        siblings = [{"at": DAY + at, "id": members[1], "members": members[:2 + (at > 3000)]} for at in (1000, 2000, 3000, 4000)]
        mirror = FakeMirror([mirror_take(1, DAY + 9000, members=members)], siblings)
        with tempfile.TemporaryDirectory() as tmp:
            dump = os.path.join(tmp, "dump.json")
            with contextlib.redirect_stdout(io.StringIO()):
                timeline.main(["de", "--title", "Film 1", "--dump", dump], run=mirror, now=DAY / 1000 + 86400)
            with open(dump, encoding="utf-8") as f:
                self.assertEqual([s["at"] - DAY for s in json.load(f)["history"]["siblings"]], [1000, 3000 + 1000])

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
