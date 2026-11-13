#!/usr/bin/env python3
"""identity_precision_sample.py: the showtime-weighted draw of served takes and the wrong-share estimate over it.

    python3 scripts/identity_precision_sample_test.py
"""
import collections
import os
import random
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_precision_sample as ps  # noqa: E402
from identity_judgements import HEADER  # noqa: E402


def takes(*showtimes):
    return [{"venue": f"V{i}", "rawTitle": f"T{i}", "film": f"tmdb:{i}", "showtimes": n} for i, n in enumerate(showtimes)]


def row(showtimes, verdict, inclusion=0.1, certain=False):
    return {"showtimes": showtimes, "verdict": verdict, "inclusion": inclusion, "certain": certain}


class Draw(unittest.TestCase):
    def test_draws_n_distinct_takes_and_the_seed_reproduces_them(self):
        population = takes(*range(1, 201))
        first = ps.draw(population, 20, random.Random("s"))
        again = ps.draw(takes(*range(1, 201)), 20, random.Random("s"))
        self.assertEqual(len(first), 20)
        self.assertEqual(len({t["film"] for t, _, _ in first}), 20)
        self.assertEqual([t["film"] for t, _, _ in first], [t["film"] for t, _, _ in again])

    def test_a_take_reaching_the_interval_is_certain_and_the_rest_share_what_is_left(self):
        drawn = ps.draw(takes(1000, 1, 1, 1, 1, 1, 1, 1, 1, 1), 3, random.Random(1))
        certain = [t for t, pi, c in drawn if c]
        self.assertEqual([t["film"] for t in certain], ["tmdb:0"])
        self.assertTrue(all(abs(pi - 2 / 9) < 1e-9 for _, pi, c in drawn if not c))
        self.assertEqual(len(drawn), 3)

    def test_each_take_is_drawn_as_often_as_its_inclusion_probability_says(self):
        population = takes(1, 2, 3, 4, 10, 10, 20)
        counts, runs = collections.Counter(), 20000
        stated = {}
        for i in range(runs):
            for t, pi, _ in ps.draw(takes(1, 2, 3, 4, 10, 10, 20), 3, random.Random(i)):
                counts[t["film"]] += 1
                stated[t["film"]] = pi
        for t in population:
            expected = stated.get(t["film"], 0)
            self.assertAlmostEqual(counts[t["film"]] / runs, expected, delta=0.02, msg=t["film"])

    def test_takes_without_upcoming_showtimes_are_never_drawn(self):
        drawn = ps.draw(takes(0, 0, 5, 5), 4, random.Random(0))
        self.assertEqual(sorted(t["film"] for t, _, _ in drawn), ["tmdb:2", "tmdb:3"])


class DrawBillings(unittest.TestCase):
    def billed(self):
        wide = [{"venue": f"V{i}", "rawTitle": "Avatar" if i % 2 else "AVATAR ", "film": "tmdb:1", "showtimes": 50}
                for i in range(40)]
        tail = [{"venue": f"W{i}", "rawTitle": f"Rare {i}", "film": f"tmdb:{100 + i}", "showtimes": 1} for i in range(9)]
        return wide + tail

    def test_a_billing_is_one_unit_however_many_venues_and_showtimes_bill_it(self):
        units = ps.billings(self.billed())
        self.assertEqual(len(units), 10)
        self.assertEqual(len(units[("avatar", "tmdb:1")]), 40)

    def test_every_billing_is_drawn_equally_often_and_the_seed_reproduces_the_draw(self):
        counts, runs = collections.Counter(), 6000
        for i in range(runs):
            drawn = ps.draw_billings(self.billed(), 3, random.Random(i))
            self.assertEqual(len({(t["rawTitle"].strip().lower(), t["film"]) for t, _, _ in drawn}), 3)
            self.assertTrue(all(abs(pi - 0.3) < 1e-9 and not certain for _, pi, certain in drawn))
            counts.update(t["film"] for t, _, _ in drawn)
        for film in ["tmdb:1", "tmdb:100", "tmdb:108"]:
            self.assertAlmostEqual(counts[film] / runs, 0.3, delta=0.03, msg=film)
        first = [t["venue"] for t, _, _ in ps.draw_billings(self.billed(), 5, random.Random("s"))]
        self.assertEqual(first, [t["venue"] for t, _, _ in ps.draw_billings(self.billed(), 5, random.Random("s"))])

    def test_n_beyond_the_billings_draws_each_once_and_unshown_takes_never(self):
        population = self.billed() + [{"venue": "X", "rawTitle": "Gone", "film": "tmdb:9", "showtimes": 0}]
        drawn = ps.draw_billings(population, 50, random.Random(0))
        self.assertEqual(len(drawn), 10)
        self.assertTrue(all(pi == 1.0 for _, pi, _ in drawn))


class Estimate(unittest.TestCase):
    def test_the_drawn_share_scales_to_the_showtimes_not_drawn_with_certainty(self):
        rows = [row(100, "wrong", 1.0, True), row(5, "wrong"), row(5, "right"), row(5, "right"), row(5, "right")]
        e = ps.estimate(rows, 1100)
        self.assertAlmostEqual(e["share"], (100 + 1000 * 0.25) / 1100)
        self.assertEqual((e["judged"], e["wrong"], e["unjudged"]), (5, 2, 0))
        self.assertLess(e["low"], e["share"])
        self.assertGreater(e["high"], e["share"])

    def test_an_unjudged_draw_is_left_out_of_the_share_and_counted(self):
        e = ps.estimate([row(5, "right"), row(5, "wrong"), row(5, "")], 100)
        self.assertAlmostEqual(e["share"], 0.5)
        self.assertEqual(e["unjudged"], 1)

    def test_the_per_take_rate_weights_each_draw_by_its_inverse_inclusion(self):
        e = ps.estimate([row(5, "wrong", 0.5), row(1, "right", 0.1)], 100)
        self.assertAlmostEqual(e["takeRate"], 2 / (2 + 10))

    def test_no_wrong_draw_still_has_an_upper_bound_alone_and_pooled(self):
        rows = [row(1, "right") for _ in range(150)]
        e = ps.estimate(rows, 1000)
        self.assertEqual(e["share"], 0.0)
        self.assertAlmostEqual(e["low"], 0.0)
        self.assertAlmostEqual(e["high"], 0.0250, places=3)
        pool = ps.pooled({"pl": e, "uk": ps.estimate(rows, 1000)}, {"pl": 1000, "uk": 1000})
        self.assertEqual(pool["share"], 0.0)
        self.assertAlmostEqual(pool["high"], ps.wilson(0, 300)[1], places=6)


class Files(unittest.TestCase):
    def test_a_sample_round_trips_through_its_tsv_line_breaks_and_tabs_aside(self):
        take = {"venue": "Kino", "rawTitle": "Lalka", "film": "tmdb:1", "showtimes": 7, "basis": "OwnMatch",
                "billing": {"synopsis": "a\tb\nc\rd e", "directors": ["X", "Y"], "ids": {"cc": "1"}},
                "record": {"title": "Lalka", "year": 2026}}
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "s.tsv")
            ps.write_sample(path, [ps.sample_row(1, "pl", take, 0.25, False, "right")])
            back = ps.read_sample(path)
        self.assertEqual(len(back), 1)
        self.assertEqual((back[0]["synopsis"], back[0]["billedDirectors"], back[0]["showtimes"]), ("a b c d e", "X; Y", 7))
        self.assertEqual((back[0]["inclusion"], back[0]["certain"]), (0.25, False))

    def test_judge_reads_the_venue_row_over_every_venues_and_infers_wrong_from_another_right_film(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "j.tsv")
            with open(path, "w", encoding="utf-8") as f:
                f.write(HEADER + "\n")
                f.write("pl\t*\tLalka\ttmdb:2\tright\topen\t\t\t\t2026-11-13\te\n")
                f.write("pl\tIluzjon\tLalka\ttmdb:1\tright\topen\t\t\t\t2026-11-13\te\n")
            labels = ps.labels_by_country(ps.read(path))
        rows = ps.judge([{"country": "pl", "venue": "Iluzjon", "rawTitle": "Lalka", "film": "tmdb:1"},
                         {"country": "pl", "venue": "Muza", "rawTitle": "Lalka", "film": "tmdb:1"},
                         {"country": "pl", "venue": "Muza", "rawTitle": "Lalka", "film": "tmdb:2"},
                         {"country": "uk", "venue": "Muza", "rawTitle": "Lalka", "film": "tmdb:2"}], labels)
        self.assertEqual([r["verdict"] for r in rows], ["right", "wrong", "right", ""])

    def test_the_report_reads_the_samples_back_with_the_judgements_of_now(self):
        with tempfile.TemporaryDirectory() as d:
            take = {"venue": "Kino", "rawTitle": "Lalka", "film": "tmdb:1", "showtimes": 7}
            ps.write_sample(os.path.join(d, "sample-pl.tsv"), [ps.sample_row(1, "pl", take, 0.5, False, None)])
            with open(os.path.join(d, "sample.json"), "w") as f:
                f.write('{"seed": "s", "n": 1, "now": "t", "countries": {"pl": {"showtimes": 14}}}')
            path = os.path.join(d, "j.tsv")
            with open(path, "w", encoding="utf-8") as f:
                f.write(HEADER + "\npl\tKino\tLalka\ttmdb:1\twrong\topen\t\t\t\t2026-11-13\te\n")
            text = ps.report(d, path, ["pl"])
        self.assertIn("100.00%", text.splitlines()[2])

    def test_a_billing_sample_reports_the_share_of_billings_wrong_not_of_showtimes(self):
        with tempfile.TemporaryDirectory() as d:
            rows = [ps.sample_row(i, "pl", {"venue": "Kino", "rawTitle": t, "film": "tmdb:1", "showtimes": s}, 0.5, False,
                                  None) for i, (t, s) in enumerate([("Lalka", 99), ("Faraon", 1)], 1)]
            ps.write_sample(os.path.join(d, "sample-pl.tsv"), rows)
            with open(os.path.join(d, "sample.json"), "w") as f:
                f.write('{"seed": "s", "n": 2, "now": "t", "unit": "billing", '
                        '"countries": {"pl": {"showtimes": 100, "billings": 4}}}')
            path = os.path.join(d, "j.tsv")
            with open(path, "w", encoding="utf-8") as f:
                f.write(HEADER + "\npl\tKino\tLalka\ttmdb:1\twrong\topen\t\t\t\t2026-11-13\te\n"
                        "pl\tKino\tFaraon\ttmdb:1\tright\topen\t\t\t\t2026-11-13\te\n")
            text = ps.report(d, path, ["pl"])
        self.assertIn("billings-wrong", text.splitlines()[1])
        self.assertIn("50.00%", text.splitlines()[2])


if __name__ == "__main__":
    unittest.main()
