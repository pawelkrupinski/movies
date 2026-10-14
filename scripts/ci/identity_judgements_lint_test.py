#!/usr/bin/env python3
"""identity_judgements_lint.py: each disagreement within the judgements, or between them and the files leaning on them,
fails, as the 2026-10-13/14 commits that landed them would have; consistent files pass. And identity_judgements.py, the
one reader: its keys are the Scala reader's, its views what the gates read."""

import gzip
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_judgements as j  # noqa: E402
import identity_judgements_lint as lint  # noqa: E402

HEAD = j.HEADER + "\n"
REPO = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")


def row(cc, venue, title, film, verdict, state="open", scope="", film_title="", reason="", judged="2026-10-14", evidence=""):
    return "\t".join([cc, venue, title, film, verdict, state, scope, film_title, reason, judged, evidence]) + "\n"


class JudgementsTest(unittest.TestCase):
    def test_the_labels_a_push_started_from_read_a_legacy_labels_file_too(self):
        # the push migrating the judgements into one file starts from a base whose labels are labels.tsv
        with tempfile.TemporaryDirectory() as d:
            legacy = os.path.join(d, "labels.tsv")
            with open(legacy, "w", encoding="utf-8") as f:
                f.write("country\tvenue\trawTitle\tfilm\tverdict\tnote\npl\tKino Oaza\tKtoś  całkiem obcy\ttmdb:7183\twrong\tx\n")
            self.assertEqual({("Kino Oaza", "Ktoś całkiem obcy", "tmdb:7183"): "wrong"}, dict(lint.labels_before(legacy)["pl"]))
            store = os.path.join(d, "identity-judgements.tsv")
            with open(store, "w", encoding="utf-8") as f:
                f.write(j.HEADER + "\npl\t*\tLalka\ttmdb:1\tright\topen\t\t\t\t2026-10-14\t\n")
            self.assertEqual({("*", "Lalka", "tmdb:1"): "right"}, dict(lint.labels_before(store)["pl"]))

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


class JudgementsLintTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.files = {lint.JUDGEMENTS: HEAD, lint.EXPECTED_MATCHES: "country\tvenue\trawTitle\tfilm\ttitle\n"}

    def write(self, rel, text):
        path = os.path.join(self.root, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(text)
        return path

    def run_lint(self, extra=None, served=(), before=None):
        for rel, text in dict(self.files, **(extra or {})).items():
            self.write(rel, text)
        return lint.lint(self.root, served, before)

    def messages(self, judgements=None, **kw):
        extra = dict(kw.pop("extra", {}), **({lint.JUDGEMENTS: HEAD + "".join(judgements)} if judgements is not None else {}))
        return [m for _, m in self.run_lint(extra=extra, **kw)]

    def test_consistent_files_pass(self):
        self.assertEqual([], self.messages([row("uk", "*", "Film", "tmdb:1", "right")]))

    def test_a_listing_judged_twice_fails_even_respaced(self):
        # two rows of one key: a listing judged both right and wrong, or a union merge that doubled one
        found = self.messages([row("pl", "Helios", "Lalka", "tmdb:1", "right"), row("pl", "Helios", "Lalka", "tmdb:1", "wrong")])
        self.assertEqual(1, len(found))
        self.assertIn("a second row for pl | Helios | Lalka | tmdb:1", found[0])

    def test_a_key_not_normalised_fails(self):
        # DE Caligari: "ES  (ULRICH SCHAMONI)" judged with two spaces, served with one a recording later
        found = self.messages([row("de", "*", "ES  (ULRICH SCHAMONI)", "tmdb:346364", "wrong")])
        self.assertEqual(1, len(found))
        self.assertIn("is not normalised", found[0])

    def test_the_vocabulary_and_what_each_state_asks(self):
        found = self.messages([row("xx", "*", "A", "tmdb:1", "right"), row("pl", "*", "B", "tmdb:Kinoteka", "right"),
                               row("pl", "*", "C", "tmdb:3", "maybe"), row("pl", "*", "D", "tmdb:4", "right", "known-wrong", "capture", reason="r"),
                               row("pl", "*", "E", "tmdb:5", "wrong", "known-wrong"), row("pl", "*", "F", "tmdb:6", "wrong", "known-wrong", "capture"),
                               row("pl", "*", "G", "tmdb:7", "wrong", scope="served"), row("pl", "*", "H", "tmdb:8", "wrong", judged="10-14")])
        for text in ("country 'xx'", "'tmdb:Kinoteka' is no id", "verdict 'maybe'", "its verdict must be wrong", "names no scope",
                     "known-wrong gives no reason", "scope is for a known-wrong row", "is no date"):
            self.assertTrue(any(text in m for m in found), text)

    def test_a_new_undecided_suspect_fails_one_standing_since_the_gate_was_armed_passes(self):
        armed = row("de", "*", "Es", "tmdb:1", "undecided", "open", "served", reason="no page says which", judged="2026-10-13")
        self.assertEqual([], self.messages([armed]))
        found = self.messages([row("de", "*", "Es", "tmdb:1", "undecided", "open", "served", reason="new", judged="2026-10-14")])
        self.assertEqual(1, len(found))
        self.assertIn("judge a new suspect right or wrong", found[0])

    def test_an_accepted_loss_of_a_right_take_fails_unless_the_policys_exception_says_so(self):
        # 19890b7a5: "a right take lost to a precision guard ... the wrong-over-missing trade" -- the four Romeo & Juliets
        found = self.messages([row("uk", "Gala Cinema Durham", "The Royal Ballet: Romeo & Juliet", "tmdb:1702789", "right", "accepted-loss",
                                   reason="a right take lost to a precision guard")])
        self.assertEqual(1, len(found))
        self.assertIn("accepted-loss of a take judged RIGHT", found[0])
        # its own reason calls it right, its verdict does not
        found = self.messages([row("uk", "Gala", "RBO: Romeo & Juliet", "tmdb:1702789", "undecided", "accepted-loss",
                                   reason="a right take lost to a guard")])
        self.assertEqual(1, len(found))
        self.assertIn("its own reason calls it right", found[0])
        # a "*" row calls it right
        found = self.messages([row("uk", "*", "Tosca", "tmdb:5", "right"), row("uk", "Plaza", "Tosca", "tmdb:5", "undecided", "accepted-loss", reason="season")])
        self.assertEqual(1, len(found))
        self.assertIn("the judgements call it right", found[0])
        # off a wrong take, and the policy's explicit exception, pass -- the exception named on every run
        exception = row("pl", "Kino Powiśle", "OPERA-OTELLO", "tmdb:1703634", "right", "accepted-right-loss", reason="the 11-for-2 trade")
        self.assertEqual([], self.messages([row("uk", "Plaza", "Carmen", "tmdb:6", "wrong", "accepted-loss", reason="drop"), exception]))
        self.assertEqual(["accepted-right-loss: pl Kino Powiśle | OPERA-OTELLO: losing tmdb:1703634 accepted -- the 11-for-2 trade"],
                         lint.notices(j.read_at(self.root)))

    def test_a_billing_judged_wrong_venue_by_venue_at_five_venues_fails_until_one_row_judges_every_venue(self):
        # 911d8f486: 81 of 82 Odeons' Bear Hunt double bill judged one by one; Odeon Luxe Peterborough was left out
        per_venue = [row("uk", f"Odeon {n}", "Bear Hunt+Tiger", "tmdb:431591", "wrong") for n in range(5)]
        found = self.messages(per_venue)
        self.assertEqual(1, len(found))
        self.assertIn("at 5 venues one by one", found[0])
        self.assertEqual([], self.messages(per_venue + [row("uk", "*", "Bear Hunt+Tiger", "tmdb:431591", "wrong")]))
        self.assertEqual([], self.messages(per_venue[:4]))

    def test_a_known_wrong_take_the_judgements_call_right_everywhere_fails(self):
        found = self.messages([row("uk", "*", "Romeo and Juliet", "tmdb:6003", "right"),
                               row("uk", "Roses", "Romeo and Juliet", "tmdb:6003", "wrong", "known-wrong", "capture", reason="relay")])
        self.assertEqual(1, len(found))
        self.assertIn("known-wrong here, but the judgements call it right at every venue", found[0])

    def test_an_expected_match_the_judgements_call_wrong_fails(self):
        expected = self.files[lint.EXPECTED_MATCHES] + "de\tArsenal\tKaidan\ttmdb:30959\tKwaidan (1964)\n"
        found = self.messages([row("de", "*", "Kaidan", "tmdb:30959", "wrong")], extra={lint.EXPECTED_MATCHES: expected})
        self.assertEqual(1, len(found))
        self.assertIn("expected-matches.tsv expects tmdb:30959", found[0])

    def test_a_hard_cluster_expectation_the_judgements_call_wrong_fails_unless_known_wrong_there(self):
        # c151636f1: PL's hard clusters regenerated to the 2007 "Ktoś całkiem obcy", wrong at every venue
        hard = {lint.HARD_CLUSTERS.format(cc="pl"):
                "ktoscalkiemobcy|2007\tKtoś całkiem obcy (2007) tmdb=7183 imdb=tt0457433 address=ktos-calkiem-obcy "
                "cinemas=[Kino 60 Krzeseł, Kino Kryterium, Kino Luna]\n"}
        found = self.messages([row("pl", "*", "Ktoś całkiem obcy", "tmdb:7183", "wrong")], extra=hard)
        self.assertEqual(["expected-hard-clusters-pl.txt expects 'ktoscalkiemobcy' as tmdb:7183, which the judgements call wrong"], found)
        # a venue's row reaches only a cluster that venue is in
        self.assertEqual([], self.messages([row("pl", "Kino Muza", "Ktoś całkiem obcy", "tmdb:7183", "wrong")], extra=hard))
        self.assertEqual(1, len(self.messages([row("pl", "Kino Luna", "Ktoś całkiem obcy", "tmdb:7183", "wrong")], extra=hard)))
        # known-wrong in scope hard-clusters it stands -- and once no expectation takes it, the scope is stale
        known = [row("pl", "*", "Ktoś całkiem obcy", "tmdb:7183", "wrong", "known-wrong", "hard-clusters", reason="fix the take, regenerate")]
        self.assertEqual([], self.messages(known, extra=hard))
        found = self.messages(known, extra={lint.HARD_CLUSTERS.format(cc="pl"): ""})
        self.assertEqual(1, len(found))
        self.assertIn("no hard-cluster expectation takes it any more", found[0])

    def served(self, decisions):
        path = os.path.join(self.root, "de.json.gz")
        with gzip.open(path, "wt", encoding="utf-8") as f:
            json.dump({"country": "de", "decisions": decisions}, f)
        return path

    def test_a_served_take_the_judgements_call_wrong_fails_unless_known_wrong(self):
        # 911d8f486: 48 billings labelled wrong that the DE and ES legs served and no baseline listed -- red after the push
        cap = self.served([{"film": 424523, "members": [{"venue": "Union Filmtheater", "rawTitle": "80ER PARTY"}]},
                           {"film": None, "members": [{"venue": "Union Filmtheater", "rawTitle": "Slam"}]}])
        found = self.messages([row("de", "Union Filmtheater", "80ER PARTY", "tmdb:424523", "wrong", evidence="a party night")], served=[cap])
        self.assertEqual(1, len(found))
        self.assertIn("de serves tmdb:424523 for '80ER PARTY'", found[0])
        for scope in ("served", "capture"):
            self.assertEqual([], self.messages([row("de", "Union Filmtheater", "80ER PARTY", "tmdb:424523", "wrong", "known-wrong", scope,
                                                    reason="why it stands")], served=[cap]))

    def test_a_billing_respaced_by_its_venue_is_the_same_listing_to_judge_and_to_tolerate(self):
        # DE Caligari: "ES (ULRICH SCHAMONI)" judged and tolerated, served as "ES  (ULRICH SCHAMONI)" a recording later
        cap = self.served([{"film": 346364, "members": [{"venue": "Caligari", "rawTitle": "ES  (ULRICH SCHAMONI)"}]}])
        wrong = row("de", "*", "ES (ULRICH SCHAMONI)", "tmdb:346364", "wrong")
        self.assertEqual(1, len(self.messages([wrong], served=[cap])))
        known = row("de", "*", "ES (ULRICH SCHAMONI)", "tmdb:346364", "wrong", "known-wrong", "served", reason="no fix")
        self.assertEqual([], self.messages([known], served=[cap]))

    def test_with_the_judgements_a_push_started_from_only_its_new_wrong_judgements_of_served_takes_fail(self):
        cap = self.served([{"film": 424523, "members": [{"venue": "Union", "rawTitle": "80ER PARTY"}]},
                           {"film": 1576593, "members": [{"venue": "Thalia", "rawTitle": "Simply Christmas"}]}])
        old = row("de", "Union", "80ER PARTY", "tmdb:424523", "wrong")
        before = self.write("before.tsv", HEAD + old)
        found = self.messages([old, row("de", "Thalia", "Simply Christmas", "tmdb:1576593", "wrong")], served=[cap], before=before)
        self.assertEqual(1, len(found))
        self.assertIn("Simply Christmas", found[0])

    def newly_wrong(self, replay):
        """Two takes the push newly judges wrong; the head's code replayed by `replay`."""
        cap = self.served([{"film": 1, "members": [{"venue": "Union", "rawTitle": "Der Diplomat"}]},
                           {"film": 2, "members": [{"venue": "Thalia", "rawTitle": "Blutgericht in Texas"}]}])
        before = self.write("before.tsv", HEAD)
        self.write(lint.JUDGEMENTS, HEAD + row("de", "Union", "Der Diplomat", "tmdb:1", "wrong") + row("de", "Thalia", "Blutgericht in Texas", "tmdb:2", "wrong"))
        self.write(lint.EXPECTED_MATCHES, self.files[lint.EXPECTED_MATCHES])
        return [m for _, m in lint.lint(self.root, [cap], before, replay)]

    def test_a_take_the_pushed_code_cures_is_no_served_finding(self):
        # the push judges a take wrong AND cures it: the last leg still served it, the head does not
        asked = []

        def replay(cc, listings):
            asked.append((cc, sorted(listings)))
            return {("Union", "Der Diplomat"): "tmdb:9", ("Thalia", "Blutgericht in Texas"): "tmdb:2"}
        found = self.newly_wrong(replay)
        self.assertEqual([("de", [("Thalia", "Blutgericht in Texas"), ("Union", "Der Diplomat")])], asked)
        self.assertEqual(1, len(found))
        self.assertIn("Blutgericht in Texas", found[0])
        self.assertIn("the pushed code still serves it", found[0])

    def test_a_replay_that_cannot_run_refuses_saying_it_could_not_replay(self):
        def replay(cc, listings):
            raise RuntimeError("no recording")
        found = self.newly_wrong(replay)
        self.assertEqual(2, len(found))
        self.assertTrue(all("COULD NOT REPLAY the pushed code (no recording)" in m
                            for m in found))

    def test_the_slice_replay_reads_identity_slices_takes_at_the_checked_out_code(self):
        # a stub identity-slice: the batch it is handed and the takes TSV it writes where --out says
        stub = self.write("identity-slice", "#!/usr/bin/env bash\n"
                          "while [ $# -gt 0 ]; do case $1 in --batch) b=$2;; --out) o=$2;; esac; shift; done\n"
                          "cp \"$b\" \"$(dirname \"$o\")/batch-seen\"\n"
                          "printf 'selector\\tkey\\tvenue\\trawTitle\\tfilm\\n' > \"$o\"\n"
                          "printf 'k\\tUnion|Der Diplomat\\tUnion\\tDer Diplomat\\tnone\\n' >> \"$o\"\n")
        os.chmod(stub, 0o755)
        takes = lint.slice_replay(stub)("de", [("Union", "Der Diplomat")])
        self.assertEqual({("Union", "Der Diplomat"): "none"}, takes)

    def test_the_main_exit_codes(self):
        for rel, text in self.files.items():
            self.write(rel, text)
        self.assertEqual(0, lint.main(["--root", self.root]))
        self.write(lint.JUDGEMENTS, HEAD + row("pl", "H", "L", "tmdb:1", "right") + row("pl", "H", "L", "tmdb:1", "wrong"))
        self.assertEqual(1, lint.main(["--root", self.root]))
        self.write(lint.JUDGEMENTS, "country\tvenue\trawTitle\tfilm\tverdict\tnote\n")
        self.assertEqual(2, lint.main(["--root", self.root]))
        os.remove(os.path.join(self.root, lint.JUDGEMENTS))
        self.assertEqual(2, lint.main(["--root", self.root]))

    def test_the_checked_in_judgements_are_consistent(self):
        self.assertEqual([], [m for _, m in lint.lint(REPO)])


if __name__ == "__main__":
    unittest.main()
