#!/usr/bin/env python3
"""identity_judgements.py, the one Python reader of the identity judgements: its keys are the Scala reader's
(tools.IdentityJudgements), its verdicts UnmatchedClusters.verdict's, its views what the gates read."""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_judgements as j  # noqa: E402

HEAD = j.HEADER + "\n"
REPO = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")


def row(cc, venue, title, film, verdict, state="open", scope="", film_title="", reason="", judged="2026-10-14", evidence=""):
    return "\t".join([cc, venue, title, film, verdict, state, scope, film_title, reason, judged, evidence]) + "\n"


class JudgementsTest(unittest.TestCase):
    def test_a_billing_is_keyed_as_the_scala_reader_keys_it(self):
        # the cases tools.IdentityJudgementsSpec reads too
        with open(os.path.join(REPO, "test/resources/fixtures/identity-judgements-normalise.jsonl"), encoding="utf-8") as f:
            cases = [json.loads(line) for line in f if line.strip()]
        self.assertTrue(cases)
        for raw, normalised in cases:
            self.assertEqual(normalised, j.normalise(raw), repr(raw))

    def test_a_venues_row_overrides_every_venues(self):
        labels = {("*", "Lalka", "tmdb:1"): "wrong", ("Kino Iluzjon", "Lalka", "tmdb:1"): "right", ("Helios", "Tosca", "tmdb:2"): "wrong",
                  ("*", "Tosca", "tmdb:2"): "right"}
        self.assertEqual("right", j.label_of(labels, "Kino Iluzjon", "Lalka", "tmdb:1"))
        self.assertEqual("wrong", j.label_of(labels, "Helios", "Lalka", "tmdb:1"))
        self.assertEqual("wrong", j.label_of(labels, "Helios", "Tosca", "tmdb:2"))
        self.assertEqual("right", j.label_of(labels, "Rialto", "Tosca  ", "tmdb:2"))
        self.assertIsNone(j.label_of(labels, "Rialto", "Diuna", "tmdb:3"))

    def test_the_views(self):
        path = os.path.join(tempfile.mkdtemp(), "j.tsv")
        with open(path, "w", encoding="utf-8") as f:
            f.write(HEAD + row("de", "Caligari", "ES (ULRICH SCHAMONI)", "tmdb:1", "wrong", "known-wrong", "capture,served", reason="why")
                    + row("de", "*", "Halloween [OV]", "tmdb:2", "undecided", "open", "served", reason="which", judged="2026-10-13")
                    + row("pl", "Kino Powiśle", "OPERA-OTELLO", "tmdb:3", "right", "accepted-right-loss", reason="a trade"))
        rows = j.read(path)
        self.assertEqual({("de", "ES (ULRICH SCHAMONI)", "tmdb:1")}, j.tolerated(rows, "capture"))
        self.assertEqual({("de", "ES (ULRICH SCHAMONI)", "tmdb:1"), ("de", "Halloween [OV]", "tmdb:2")}, j.tolerated(rows, "served"))
        self.assertEqual([("Kino Powiśle", "OPERA-OTELLO", "tmdb:3")], j.accepted(rows, "pl"))
        self.assertEqual({"de": {("Caligari", "ES (ULRICH SCHAMONI)", "tmdb:1"): "wrong"}, "pl": {("Kino Powiśle", "OPERA-OTELLO", "tmdb:3"): "right"}},
                         dict(j.labels_by_country(rows)))

    def test_a_film_other_than_the_one_judged_right_for_the_listing_is_wrong_as_the_scala_verdict_reads_it(self):
        # tools.UnmatchedClustersFindingsSpec's Lalka: UnmatchedClusters.verdict calls a take WRONG when the judgements
        # name the listing's right film in its database and this is not it -- the suspect gate fails such a take, so the
        # lint's served check and the takes check must read it alike.
        path = os.path.join(tempfile.mkdtemp(), "j.tsv")
        with open(path, "w", encoding="utf-8") as f:
            f.write(HEAD + row("pl", "*", "Lalka", "tmdb:81315", "wrong") + row("pl", "*", "Lalka", "tmdb:1321666", "right")
                    + row("pl", "Kino Iluzjon", "Lalka", "tmdb:81315", "right") + row("pl", "*", "Diuna", "imdb:tt1160419", "right"))
        labels = j.labels_by_country(j.read(path))["pl"]
        self.assertEqual("right", j.label_of(labels, "Kino Iluzjon", "Lalka", "tmdb:81315"))
        self.assertEqual("wrong", j.label_of(labels, "Kino Kadr", "Lalka", "tmdb:81315"))
        self.assertEqual("right", j.label_of(labels, "Kino Kadr", "Lalka", "tmdb:1321666"))
        self.assertEqual("wrong", j.label_of(labels, "Kino Kadr", "Lalka", "tmdb:999"), "another TMDB film: not the right one")
        self.assertEqual("right", j.label_of(labels, "Kino Iluzjon", "Lalka", "tmdb:1321666"), "every venue's right film")
        self.assertIsNone(j.label_of(labels, "Kino Kadr", "Diuna", "tmdb:438631"), "a right IMDb film says nothing of a TMDB one")
        self.assertIsNone(j.label_of(labels, "Kino Kadr", "Tosca", "tmdb:1"))
        self.assertEqual({"tmdb:81315", "tmdb:1321666"}, j.right_films(labels, "Kino Iluzjon", "Lalka"))

    def test_judgements_of_no_file_is_no_judgement(self):
        self.assertEqual(({}, [], set()), j.judgements_of(None, "pl"))


if __name__ == "__main__":
    unittest.main()
