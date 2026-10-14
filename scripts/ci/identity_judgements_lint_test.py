#!/usr/bin/env python3
"""identity_judgements_lint.py: each disagreement between the judgements and the files leaning on them fails, as the
2026-10-13/14 commits that landed them would have; consistent files pass; the grandfathered findings only shrink."""

import gzip
import json
import os
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_judgements_lint as lint  # noqa: E402

HEAD = "country\tvenue\trawTitle\tfilm\tverdict\tnote\n"


class JudgementsLintTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.files = {lint.LABELS: HEAD, lint.ACCEPTED: "country\tvenue\trawTitle\tbefore\tafter\tnote\n",
                      lint.EXPECTED_MATCHES: "country\tvenue\trawTitle\tfilm\ttitle\n",
                      lint.KNOWN_WRONG: "country\trawTitle\tfilm\ttitle\treasons\n",
                      lint.BASELINE: "country\trawTitle\tfilm\ttitle\treasons\n"}
        self.patcher = mock.patch.object(lint, "GRANDFATHERED", {})
        self.patcher.start()

    def tearDown(self):
        self.patcher.stop()

    def write(self, rel, text):
        path = os.path.join(self.root, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(text)
        return path

    def run_lint(self, extra=None, served=(), labels_before=None):
        files = dict(self.files, **(extra or {}))
        for rel, text in files.items():
            self.write(rel, text)
        return lint.lint(self.root, served, labels_before)

    def messages(self, **kw):
        findings, stale = self.run_lint(**kw)
        return [m for _, m in findings], stale

    def test_consistent_files_pass(self):
        found, stale = self.messages(extra={lint.LABELS: HEAD + "uk\t*\tFilm\ttmdb:1\tright\t\n"})
        self.assertEqual(([], []), (found, stale))

    def test_an_accepted_loss_of_a_take_its_note_calls_right_fails(self):
        # 19890b7a5: "a right take lost to a precision guard ... the wrong-over-missing trade" -- the four Romeo & Juliets
        accepted = self.files[lint.ACCEPTED] + ("uk\tGala Cinema Durham\tThe Royal Ballet: Romeo & Juliet\ttmdb:1702789\tnone\t"
                                                "a right take lost to a precision guard, the wrong-over-missing trade\n")
        found, _ = self.messages(extra={lint.ACCEPTED: accepted})
        self.assertEqual(1, len(found))
        self.assertIn("accepts losing tmdb:1702789 at Gala Cinema Durham", found[0])

    def test_an_accepted_move_off_a_take_labels_call_right_fails_and_off_a_wrong_one_passes(self):
        labels = HEAD + "uk\t*\tTosca\ttmdb:5\tright\t\nuk\t*\tCarmen\ttmdb:6\twrong\t\n"
        accepted = self.files[lint.ACCEPTED] + "uk\tPlaza\tTosca\ttmdb:5\ttmdb:9\tseason\nuk\tPlaza\tCarmen\ttmdb:6\tnone\tdrop\n"
        found, _ = self.messages(extra={lint.LABELS: labels, lint.ACCEPTED: accepted})
        self.assertEqual(1, len(found))
        self.assertIn("losing tmdb:5 at Plaza | Tosca", found[0])

    def test_a_listing_judged_both_right_and_wrong_fails(self):
        found, _ = self.messages(extra={lint.LABELS: HEAD + "pl\tHelios\tLalka\ttmdb:1\tright\t\npl\tHelios\tLalka\ttmdb:1\twrong\t\n"})
        self.assertEqual(["labels.tsv judges tmdb:1 both right and wrong for pl Helios | Lalka"], found)

    def test_a_venues_own_right_beside_every_venues_wrong_is_no_conflict(self):
        # PL "Lalka": Kino Iluzjon bills Has' 1968 film, every other venue the 2026 one -- read as the takes check reads it
        labels = HEAD + "pl\t*\tLalka\ttmdb:81315\twrong\t\npl\tKino Iluzjon\tLalka\ttmdb:81315\tright\t\n"
        self.assertEqual(([], []), self.messages(extra={lint.LABELS: labels}))

    def test_a_billing_judged_wrong_venue_by_venue_at_five_venues_fails_until_one_line_judges_every_venue(self):
        # 911d8f486: 81 of 82 Odeons' Bear Hunt double bill judged one by one; Odeon Luxe Peterborough was left out
        per_venue = "".join(f"uk\tOdeon {n}\tBear Hunt+Tiger\ttmdb:431591\twrong\ta double bill\n" for n in range(5))
        found, _ = self.messages(extra={lint.LABELS: HEAD + per_venue})
        self.assertEqual(1, len(found))
        self.assertIn("at 5 venues one by one", found[0])
        self.assertEqual(([], []), self.messages(extra={lint.LABELS: HEAD + per_venue + "uk\t*\tBear Hunt+Tiger\ttmdb:431591\twrong\t\n"}))
        four = "".join(f"uk\tOdeon {n}\tBear Hunt+Tiger\ttmdb:431591\twrong\t\n" for n in range(4))
        self.assertEqual(([], []), self.messages(extra={lint.LABELS: HEAD + four}))

    def test_an_expected_match_labels_call_wrong_fails(self):
        expected = self.files[lint.EXPECTED_MATCHES] + "de\tArsenal\tKaidan\ttmdb:30959\tKwaidan (1964)\n"
        found, _ = self.messages(extra={lint.EXPECTED_MATCHES: expected, lint.LABELS: HEAD + "de\t*\tKaidan\ttmdb:30959\twrong\t\n"})
        self.assertEqual(1, len(found))
        self.assertIn("expected-matches.tsv expects tmdb:30959", found[0])

    def test_a_hard_cluster_expectation_labels_call_wrong_fails(self):
        # c151636f1: PL's hard clusters regenerated to the 2007 "Ktoś całkiem obcy", wrong at every venue by labels.tsv
        hard = {lint.HARD_CLUSTERS.format(cc="pl"):
                "ktoscalkiemobcy|2007\tKtoś całkiem obcy (2007) tmdb=7183 imdb=tt0457433 address=ktos-calkiem-obcy "
                "cinemas=[Kino 60 Krzeseł, Kino Kryterium, Kino Luna]\n"}
        found, _ = self.messages(extra=dict(hard, **{lint.LABELS: HEAD + "pl\t*\tKtoś całkiem obcy\ttmdb:7183\twrong\t\n"}))
        self.assertEqual(["expected-hard-clusters-pl.txt expects 'ktoscalkiemobcy' as tmdb:7183, which labels.tsv calls wrong"], found)
        # a venue's label reaches only a cluster that venue is in
        found, _ = self.messages(extra=dict(hard, **{lint.LABELS: HEAD + "pl\tKino Muza\tKtoś całkiem obcy\ttmdb:7183\twrong\t\n"}))
        self.assertEqual([], found)
        found, _ = self.messages(extra=dict(hard, **{lint.LABELS: HEAD + "pl\tKino Luna\tKtoś całkiem obcy\ttmdb:7183\twrong\t\n"}))
        self.assertEqual(1, len(found))

    def served(self, decisions):
        path = os.path.join(self.root, "de.json.gz")
        with gzip.open(path, "wt", encoding="utf-8") as f:
            json.dump({"country": "de", "decisions": decisions}, f)
        return path

    def test_a_served_take_labels_call_wrong_fails_unless_a_baseline_lists_it(self):
        # 911d8f486: 48 billings labelled wrong that the DE and ES legs served and no baseline listed -- red after the push
        cap = self.served([{"film": 424523, "members": [{"venue": "Union Filmtheater", "rawTitle": "80ER PARTY"}]},
                           {"film": None, "members": [{"venue": "Union Filmtheater", "rawTitle": "Slam"}]}])
        labels = HEAD + "de\tUnion Filmtheater\t80ER PARTY\ttmdb:424523\twrong\ta party night\n"
        found, _ = self.messages(extra={lint.LABELS: labels}, served=[cap])
        self.assertEqual(1, len(found))
        self.assertIn("de serves tmdb:424523 for '80ER PARTY'", found[0])
        listed = self.files[lint.KNOWN_WRONG] + "de\t80ER PARTY\ttmdb:424523\t80er Clips (2004)\twhy it stands\n"
        self.assertEqual(([], []), self.messages(extra={lint.LABELS: labels, lint.KNOWN_WRONG: listed}, served=[cap]))

    def test_with_the_labels_a_push_started_from_only_its_new_wrong_judgements_of_served_takes_fail(self):
        cap = self.served([{"film": 424523, "members": [{"venue": "Union", "rawTitle": "80ER PARTY"}]},
                           {"film": 1576593, "members": [{"venue": "Thalia", "rawTitle": "Simply Christmas"}]}])
        old = "de\tUnion\t80ER PARTY\ttmdb:424523\twrong\t\n"
        before = self.write("before.tsv", HEAD + old)
        found, _ = self.messages(extra={lint.LABELS: HEAD + old + "de\tThalia\tSimply Christmas\ttmdb:1576593\twrong\t\n"},
                                 served=[cap], labels_before=before)
        self.assertEqual(1, len(found))
        self.assertIn("Simply Christmas", found[0])

    def test_a_grandfathered_finding_stands_and_one_no_longer_found_fails_as_stale(self):
        per_venue = "".join(f"uk\tV{n}\tX\ttmdb:1\twrong\t\n" for n in range(5))
        with mock.patch.object(lint, "GRANDFATHERED", {("scattered", "uk", "X", "tmdb:1"): "why",
                                                       ("conflict", "uk", "V", "Y", "tmdb:2"): "cured"}):
            findings, stale = self.run_lint(extra={lint.LABELS: HEAD + per_venue})
        self.assertEqual([], findings)
        self.assertEqual([("conflict", "uk", "V", "Y", "tmdb:2")], stale)

    def test_the_main_exit_codes(self):
        for rel, text in self.files.items():
            self.write(rel, text)
        self.assertEqual(0, lint.main(["--root", self.root]))
        self.write(lint.LABELS, HEAD + "pl\tH\tL\ttmdb:1\tright\t\npl\tH\tL\ttmdb:1\twrong\t\n")
        self.assertEqual(1, lint.main(["--root", self.root]))
        os.remove(os.path.join(self.root, lint.LABELS))
        self.assertEqual(2, lint.main(["--root", self.root]))

    def test_the_checked_in_judgements_are_consistent(self):
        self.patcher.stop()
        try:
            root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
            findings, stale = lint.lint(root)
            self.assertEqual(([], []), ([m for _, m in findings], stale))
        finally:
            self.patcher.start()


if __name__ == "__main__":
    unittest.main()
