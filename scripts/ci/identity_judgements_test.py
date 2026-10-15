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

    def test_judgements_of_no_file_is_no_judgement(self):
        self.assertEqual(({}, [], set()), j.judgements_of(None, "pl"))


if __name__ == "__main__":
    unittest.main()
