#!/usr/bin/env python3
"""Unit tests for poster_eval.py and poster_hash.py over a tiny fixture dataset -- no mirror, no network:

  - the hash reproduces prod's filed hashes bit for bit (three images in scripts/poster_eval_fixtures/; needs vips);
  - the dataset keeps a cluster's real candidates and drops a feed's poster;
  - the vote, the veto and the vote a poster vetoes follow PosterEvidence; a variant's new FP and lost TP are listed;
  - coverage puts each listing in its bucket.

    python3 scripts/poster_eval_test.py
"""
import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
try:
    import numpy  # noqa: F401
except ImportError:  # the tool's own dependency, as vips is: the CI container has neither
    print("poster_eval_test: SKIPPED -- numpy is not installed (brew install numpy / apt install python3-numpy)")
    sys.exit(0)
import poster_eval as pe  # noqa: E402
import poster_hash  # noqa: E402

Fixtures = os.path.join(os.path.dirname(os.path.abspath(__file__)), "poster_eval_fixtures")


def flip(h, bits):
    """`h` with its lowest `bits` bits flipped: a poster `bits` away."""
    return h ^ ((1 << bits) - 1)


X, Y, Z = 0x5555_0000_ffff_0000, 0x0f0f_0f0f_0f0f_0f0f, 0x1234_5678_1234_5678


def listing(venue, raw, url, cands, take, feed=False):
    return {"t": "l", "venue": venue, "raw": raw, "url": url, "feed": feed, "cands": cands, "take": take, "decided": True}


def decision(venue, raw, film):
    return {"t": "d", "fam": f"P\u0000{venue}\u0000{raw}", "keys": [[venue, raw]], "film": film, "basis": "OwnMatch"}


def answer(kind, ref, hashes, unread=False):
    return {"t": "a", "id": f"poster|{kind}|{ref}", "hashes": [str(h) for h in hashes], "paths": [], "unread": unread}


# Cluster A: the right film 1 sits 3 bits from the venue poster, film 2 far. Cluster B: prod took 3, 30 bits off, while
# film 4 sits 2 bits away -- a vote for 4 and a veto of 3. Cluster C: two near-twins both within 4 bits -- no vote.
# Cluster D: one poster votes for 7, another vetoes it (8 within 8, 7 beyond 10 of that one).
Rows = {"pl": [
    listing("Kino A", "Film A", "https://a/a.jpg", [1, 2], 1),
    listing("Kino B", "Film B", "https://b/b.jpg", [3, 4], 3),
    listing("Kino C", "Film C", "https://c/c.jpg", [5, 6], None),
    listing("Kino D", "Film D", "https://d/d1.jpg", [7, 8], None),
    listing("Kino D2", "Film D", "https://d/d2.jpg", [7, 8], None),
    listing("Kino Feed", "Film F", "https://feed/f.jpg", [9], 9, feed=True),
    listing("Kino None", "Film N", None, [9], 9),
    listing("Kino Unread", "Film U", "https://u/u.jpg", [9], 9),
    decision("Kino A", "Film A", 1), decision("Kino B", "Film B", 3), decision("Kino C", "Film C", None),
    {"t": "d", "fam": "D", "keys": [["Kino D", "Film D"], ["Kino D2", "Film D"]], "film": None, "basis": "NoCandidate"},
    decision("Kino Feed", "Film F", 9), decision("Kino None", "Film N", 9), decision("Kino Unread", "Film U", 9),
    answer("venue", "https://a/a.jpg", [X]), answer("venue", "https://b/b.jpg", [Y]), answer("venue", "https://c/c.jpg", [Z]),
    answer("venue", "https://d/d1.jpg", [X]), answer("venue", "https://d/d2.jpg", [Y]), answer("venue", "https://u/u.jpg", [], unread=True),
    answer("film", 1, [flip(X, 3), Z]), answer("film", 2, [flip(X, 20)]), answer("film", 3, [flip(Y, 30)]), answer("film", 4, [flip(Y, 2)]),
    answer("film", 5, [flip(Z, 1)]), answer("film", 6, [flip(Z, 4)]), answer("film", 7, [flip(X, 2), flip(Y, 12)]),
    answer("film", 8, [flip(Y, 6)]), answer("film", 9, [Z]),
]}
JudgementRows = [("pl", "Kino A", "Film A", "tmdb:1", "right"), ("pl", "Kino B", "Film B", "tmdb:4", "right"),
                 ("pl", "Kino B", "Film B", "tmdb:3", "wrong")]


class PosterEvalTest(unittest.TestCase):
    def setUp(self):
        self.cache = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.cache)
        tsv = os.path.join(self.cache, "judgements.tsv")
        with open(tsv, "w") as f:
            f.write("\t".join(pe.Columns) + "\n")
            for row in JudgementRows:
                f.write("\t".join(row + ("open", "", "", "", "2026-11-11", "")) + "\n")
        self.rows = pe.read_judgements(tsv)
        # the unread venue poster is no image either: never fetched
        os.makedirs(os.path.join(self.cache, "img", "v"))
        open(os.path.join(self.cache, "img", pe.Images.venue_name("https://u/u.jpg") + ".fail"), "w").close()
        meta, arrays = pe.build(Rows)
        pe.save(os.path.join(self.cache, "dataset.npz"), meta, arrays)
        self.meta, self.arrays = pe.load(os.path.join(self.cache, "dataset.npz"))
        self.clusters = {c["keys"][0][1]: i for i, c in enumerate(self.meta["clusters"])}

    def run_variant(self, **variant):
        images, tmdb = pe.Images(self.cache), pe.TmdbLists(self.cache)
        return pe.run_variant(self.meta, self.arrays, images, tmdb, dict(pe.Baseline, **variant))

    def test_the_dataset_keeps_real_candidates_and_drops_a_feeds_poster(self):
        self.assertEqual(sorted(self.clusters), ["Film A", "Film B", "Film C", "Film D", "Film U"])
        self.assertEqual(self.meta["clusters"][self.clusters["Film A"]]["cands"], [1, 2])
        self.assertEqual(self.meta["clusters"][self.clusters["Film D"]]["urls"], ["https://d/d1.jpg", "https://d/d2.jpg"])

    def test_the_vote_and_veto_follow_poster_evidence(self):
        vote, take_vetoed, bits = self.run_variant()
        at = lambda title: self.clusters[title]  # noqa: E731
        self.assertEqual((vote[at("Film A")], bits[at("Film A")]), (1, 3))
        self.assertEqual(vote[at("Film B")], 4)
        self.assertTrue(take_vetoed[at("Film B")])
        self.assertFalse(take_vetoed[at("Film A")])
        self.assertEqual(vote[at("Film C")], -1, "two near-twins within 4 bits: no vote")
        self.assertEqual(vote[at("Film D")], -1, "d1 votes 7, d2 names 8 while 7 stays 12 bits off: vetoed")

    def test_a_stricter_vote_lists_its_lost_true_positive(self):
        labs = pe.labels(self.meta, self.rows)
        results = {"baseline": self.run_variant(), "variant": self.run_variant(vote=2)}
        lines = []
        pe.report(self.meta, labs, results, dict(pe.Baseline, vote=2), out=lines.append)
        text = "\n".join(lines)
        self.assertRegex(text, r"pl\s+baseline\s+5\s+2 \|\s+2\s+0\s+1\.000")
        self.assertRegex(text, r"pl\s+variant\s+5\s+2 \|\s+1\s+0\s+1\.000")
        self.assertIn("LOST TP (judged): 1\n  pl Kino A | Film A: baseline voted 1 at 3 bits, now none", text)
        self.assertIn("NEW FP (judged): 0", text)
        self.assertRegex(text, r"\|\s+0/1\s+1/1 ", "the judged wrong take (B's 3) rightly vetoed, the right one (A's 1) not")

    def test_coverage_buckets(self):
        lines = []
        pe.coverage(self.meta, self.rows, out=lines.append)
        text = "\n".join(lines)
        for bucket, count in [("1 no poster url", 1), ("2 feed poster", 1), ("4 asked, unread", 1), ("7 usable", 5)]:
            self.assertRegex(text, rf"{bucket}.*?\s{count}\s")

    def test_chosen_orders_by_language_then_votes(self):
        posters = [("/de.jpg", "de", 9), ("/en1.jpg", "en", 1), ("/en5.jpg", "en", 5), ("/none.jpg", None, 50), ("/pl.jpg", "pl", 0)]
        self.assertEqual(pe.chosen(posters, "pl", 4), ["/pl.jpg", "/en5.jpg", "/en1.jpg", "/none.jpg"])

    @unittest.skipUnless(shutil.which("vips"), "vips is not installed")
    def test_the_hash_is_prods_bit_for_bit(self):
        with open(os.path.join(Fixtures, "hashes.tsv")) as f:
            filed = [line.split() for line in f if not line.startswith("#")]
        self.assertEqual(len(filed), 3)
        for name, h in filed:
            self.assertEqual(poster_hash.hash_file(os.path.join(Fixtures, name)), int(h), name)


if __name__ == "__main__":
    result = unittest.main(exit=False, verbosity=1).result
    sys.exit(0 if result.wasSuccessful() else 1)
