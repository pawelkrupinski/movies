#!/usr/bin/env python3
"""nix/files/identity-regressions.py's arithmetic, on the take-change lines of the incidents it was written for.

The script turns `identity take change` log lines into the gauges identity-regressions.rules alerts
on. What a "net loss" is, which listing counts as a flip, and what a build owes are all decided
here, offline, so a rewrite that stops seeing DE "Der kleine Prinz" or PL Peppa fails bin/check
rather than the next incident. Also pins the backtest's copy of the thresholds to the rule file's.

Run: python3 infra/test/test_identity_regressions.py   (also run by bin/check)
"""

import importlib.machinery
import importlib.util
import json
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
INFRA = os.path.dirname(HERE)
_PATH = os.path.join(INFRA, "nix", "files", "identity-regressions.py")
_LOADER = importlib.machinery.SourceFileLoader("identity_regressions", _PATH)
_SPEC = importlib.util.spec_from_loader("identity_regressions", _LOADER)
ir = importlib.util.module_from_spec(_SPEC)
_LOADER.exec_module(ir)

RULES = os.path.join(INFRA, "nix", "files", "monitoring", "rules", "identity-regressions.rules")
H = 3600


def line(t, kind, venue, listing, before=None, after=None, country="de", build="b1", title="T"):
    r = {"_time": "2026-10-20T00:00:00Z", "country": country, "kind": kind, "build": build,
         "venue": venue, "listing": listing, "title_before": title, "title_after": title}
    if before:
        r["tmdb_before"] = before
    if after:
        r["tmdb_after"] = after
    r["t"] = float(t)
    return r


class Parse(unittest.TestCase):
    def test_reads_nanosecond_times_and_keeps_only_take_changes(self):
        raw = [json.dumps({"_time": "2026-10-21T11:30:14.96901428Z", "kind": "lost", "country": "pl"}),
               json.dumps({"_time": "2026-10-21T11:30:00Z", "country": "pl"}),  # a summary line: no kind
               ""]
        recs = ir.parse(raw)
        self.assertEqual(len(recs), 1)
        self.assertAlmostEqual(recs[0]["t"], 1792582214.969, places=2)


class FilmNetLost(unittest.TestCase):
    def test_peppa_pl_counts_every_listing_left_with_no_film(self):
        recs = [line(100, "lost", f"Cinema City {i}", "Peppa", before="1778515", country="pl", build="96fb23c")
                for i in range(123)]
        out = ir.film_net_lost(recs, now=200)
        self.assertEqual(out[("pl", "1778515")]["count"], 123)
        self.assertEqual(out[("pl", "1778515")]["build"], "96fb23c")

    def test_prinz_moved_then_lost_is_charged_to_the_film_it_held_at_the_start(self):
        # 16:43 334199 -> 309809 (changed), 21:13 309809 -> nothing (lost): over 6h, 334199 lost 16.
        recs = []
        for i in range(16):
            recs.append(line(0, "changed", f"v{i}", "Der kleine Prinz", before="334199", after="309809"))
            recs.append(line(4.5 * H, "lost", f"v{i}", "Der kleine Prinz", before="309809"))
        out = ir.film_net_lost(recs, now=5 * H)
        self.assertEqual(out[("de", "334199")]["count"], 16)
        self.assertNotIn(("de", "309809"), out)

    def test_a_listing_moved_to_another_film_is_not_a_loss(self):
        recs = [line(10, "changed", f"v{i}", "Rose", before="1", after="2") for i in range(30)]
        self.assertEqual(ir.film_net_lost(recs, now=100), {})

    def test_a_loss_taken_back_within_the_window_is_not_a_loss(self):
        recs = []
        for i in range(30):
            recs.append(line(10, "lost", f"v{i}", "Carmen", before="7"))
            recs.append(line(70, "gained", f"v{i}", "Carmen", after="7"))
        self.assertEqual(ir.film_net_lost(recs, now=100), {})

    def test_listings_newly_matched_to_the_film_offset_its_losses(self):
        recs = [line(10, "lost", f"old{i}", "LOTR", before="120") for i in range(12)]
        recs += [line(20, "gained", f"new{i}", "LOTR (ext.)", after="120") for i in range(5)]
        self.assertEqual(ir.film_net_lost(recs, now=100)[("de", "120")]["count"], 7)

    def test_a_loss_older_than_six_hours_has_left_the_window(self):
        recs = [line(0, "lost", f"v{i}", "X", before="9") for i in range(20)]
        self.assertIn(("de", "9"), ir.film_net_lost(recs, now=6 * H - 1))
        self.assertEqual(ir.film_net_lost(recs, now=6 * H + 1), {})


class Cures(unittest.TestCase):
    """A loss of a take judged wrong is the fix: DE "Iconic Movie" lost tmdb 1217647 at 31 listings on 2026-10-22."""

    def judged(self):
        return ir.load_judgements(os.path.join(HERE, "identity-regressions-judgements.tsv"))

    def raw(self, venue, listing, tmdb, kind="lost", build="6450294"):
        return json.dumps({"_time": "2026-10-22T15:00:00Z", "country": "de", "kind": kind, "build": build,
                           "venue": venue, "listing": listing, "tmdb_before": tmdb, "title_before": listing})

    def test_loads_rows_keyed_as_the_gates_key_them(self):
        j = self.judged()
        self.assertEqual(ir.verdict(j, "de", "CinemaxX Augsburg", "Iconic  Movie ", "1217647"), "wrong")
        self.assertIsNone(ir.verdict(j, "de", "CinemaxX Augsburg", "ICONIC MOVIE", "1217647"))  # case is kept
        # A venue's row overrides "*"'s.
        self.assertEqual(ir.verdict(j, "de", "Cinecitta", "Über Unterbiberger - Vom Woid in die Welt", "1559083"), "wrong")
        self.assertEqual(ir.verdict(j, "de", "Kinopolis Freiberg", "Über Unterbiberger - Vom Woid in die Welt", "1559083"), "right")

    def test_a_cure_counts_in_neither_loss_and_is_published_apart(self):
        lines = [self.raw(f"v{i}", "Iconic Movie", "1217647") for i in range(31)]
        lines += [self.raw(f"w{i}", "Über Unterbiberger - Vom Woid in die Welt", "1559083", build="045d2fc") for i in range(14)]
        recs = ir.parse(lines, self.judged())
        now = recs[-1]["t"] + 1
        self.assertEqual(set(ir.film_net_lost(recs, now)), {("de", "1559083")})
        self.assertEqual(set(ir.build_net_lost(recs, now)), {("de", "045d2fc")})
        self.assertEqual(ir.cures(recs, now)[("de", "1217647")]["count"], 31)
        self.assertIn('kinowo_identity_cures{country="de",tmdb="1217647",title="Iconic Movie",build="6450294"} 31',
                      ir.render(recs, now))

    def test_without_judgements_nothing_is_a_cure(self):
        recs = ir.parse([self.raw("v", "Iconic Movie", "1217647")])
        self.assertFalse(recs[0]["cure"])


class BuildNetLost(unittest.TestCase):
    def test_a_build_owes_what_it_lost_minus_what_it_gained(self):
        recs = [line(10, "lost", f"v{i}", "Peppa", before="1778515", country="pl", build="96fb23c") for i in range(126)]
        recs += [line(20, "gained", f"g{i}", "Other", after="5", country="pl", build="96fb23c") for i in range(3)]
        recs += [line(20, "lost", "w", "Other", before="5", country="pl", build="other")]
        out = ir.build_net_lost(recs, now=100)
        self.assertEqual(out[("pl", "96fb23c")], {"count": 123, "tmdb": "1778515", "title": "T"})
        self.assertEqual(out[("pl", "other")]["count"], 1)

    def test_a_build_that_gained_more_than_it_lost_is_not_published(self):
        recs = [line(10, "lost", "a", "X", before="1"), line(10, "gained", "b", "Y", after="2"),
                line(10, "gained", "c", "Z", after="3")]
        self.assertEqual(ir.build_net_lost(recs, now=100), {})


class ListingFlips(unittest.TestCase):
    def test_ties_that_bind_counts_moves_between_the_same_two_films_either_way(self):
        recs = [line(i * 60, "changed", "Chichester", "The Ties That Bind Us", country="uk",
                     before=("1096642" if i % 2 == 0 else "1460982"), after=("1460982" if i % 2 == 0 else "1096642"))
                for i in range(4)]
        out = ir.listing_flips(recs, now=H)
        self.assertEqual(out[("uk", "1096642", "1460982")]["count"], 4)
        self.assertEqual(out[("uk", "1096642", "1460982")]["venue"], "Chichester")

    def test_pairs_are_kept_apart_and_film_to_nothing_is_not_a_flip(self):
        recs = [line(1, "changed", "M", "Rose", before="1176962", after="846864"),
                line(2, "changed", "M", "Rose", before="846864", after="1712584"),
                line(3, "lost", "M", "Rose", before="1712584"),
                line(4, "gained", "M", "Rose", after="1712584")]
        out = ir.listing_flips(recs, now=10)
        self.assertEqual({k: v["count"] for k, v in out.items()},
                         {("de", "1176962", "846864"): 1, ("de", "1712584", "846864"): 1})


class Render(unittest.TestCase):
    def test_writes_type_headers_escaped_labels_and_the_floor(self):
        recs = [line(10, "lost", f"v{i}", 'Say "Hi"\\', before="1", build="b9", title='Say "Hi"\\') for i in range(5)]
        recs += [line(10, "lost", f"w{i}", "Small", before="2") for i in range(4)]
        text = ir.render(recs, now=100)
        self.assertIn('# TYPE kinowo_identity_film_net_lost gauge', text)
        self.assertIn('kinowo_identity_film_net_lost{country="de",tmdb="1",title="Say \\"Hi\\"\\\\",build="b9"} 5', text)
        self.assertNotIn('tmdb="2"', text.split("# TYPE kinowo_identity_build_net_lost")[0])
        self.assertIn("kinowo_identity_regressions_last_success_timestamp_seconds 100", text)
        self.assertIn("kinowo_identity_regressions_lines 9", text)


class Backtest(unittest.TestCase):
    def test_an_episode_shorter_than_the_hold_does_not_fire(self):
        steps = [(0, {}), (300, {"k": {"count": 20}}), (600, {}), (900, {"j": {"count": 11}}),
                 (1200, {"j": {"count": 12}}), (1500, {"j": {"count": 12}}), (1800, {"j": {"count": 11}})]
        eps = ir.episodes(steps, hold=900)
        self.assertEqual([(e[0], e[1], e[2], e[3]["count"]) for e in eps], [("j", 900, 1800, 12)])


class ThresholdsMatchTheRules(unittest.TestCase):
    """The backtest replays THRESHOLDS; Prometheus alerts on the rule file. They must say the same."""

    def setUp(self):
        with open(RULES) as f:
            self.text = f.read()

    def rule(self, name):
        m = re.search(r"- alert: " + name + r"\n(.*?)(?=\n      (?:- alert: |#)|\Z)", self.text, re.S)
        self.assertIsNotNone(m, name)
        return m.group(1)

    def test_film(self):
        r = self.rule("IdentityFilmListingsLost")
        self.assertIn(f"kinowo_identity_film_net_lost >= {ir.THRESHOLDS['film_net_lost']}", r)
        self.assertIn(f"for: {ir.THRESHOLDS['film_for_seconds'] // 60}m", r)

    def test_build(self):
        r = self.rule("IdentityBuildNetLosses")
        t = ir.THRESHOLDS["build_net_lost"]
        named = {cc: int(n) for cc, n in re.findall(r'kinowo_identity_build_net_lost\{country="([a-z]+)"\} > (\d+)', r)}
        self.assertEqual(named, {cc: n for cc, n in t.items() if cc != "*"})
        rest = re.search(r'kinowo_identity_build_net_lost\{country!~"([a-z|]+)"\} > (\d+)', r)
        self.assertEqual(set(rest.group(1).split("|")), set(named))
        self.assertEqual(int(rest.group(2)), t["*"])
        self.assertIn(f"for: {ir.THRESHOLDS['build_for_seconds'] // 60}m", r)

    def test_flips(self):
        r = self.rule("IdentityListingFlipFlop")
        self.assertIn(f"kinowo_identity_listing_flips >= {ir.THRESHOLDS['listing_flips']}", r)
        self.assertNotIn("for:", r)


if __name__ == "__main__":
    unittest.main(verbosity=2)
