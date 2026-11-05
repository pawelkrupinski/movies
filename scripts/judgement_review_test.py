#!/usr/bin/env python3
"""judgement_review.py: what awaits the owner, how each answer is written (in place, lint-clean), the page and /land."""
import os
import sys
import tempfile
import threading
import unittest
import urllib.parse
import urllib.request
from http.server import HTTPServer

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, "ci"))
import identity_judgements as j  # noqa: E402
import judgement_review as r  # noqa: E402

REPO = os.path.join(HERE, "..")


def row(cc, venue, title, film, verdict, evidence="", state="open", scope="", film_title="", reason="", judged="2026-11-01"):
    return "\t".join([cc, venue, title, film, verdict, state, scope, film_title, reason, judged, evidence]) + "\n"


FIXTURE = ("# comment\n" + j.HEADER + "\n"
           + row("de", "Zeta", "Buddies", "tmdb:2", "right", "AGENT-JUDGED 2026-11-05 (a): because", film_title="Buddies (1985)")
           + row("de", "Alpha", "Buddies", "tmdb:2", "right", "AGENT-JUDGED 2026-11-05 (a): because", film_title="Buddies (1985)")
           + row("de", "Alpha", "Buddies", "tmdb:1", "wrong", "AGENT-JUDGED 2026-11-05 (a): short", "known-wrong", "rows", "Buddies (2026)", "kept")
           + row("de", "Alpha", "Plain", "tmdb:9", "right", "hand label")
           + row("de", "Alpha", "Judged", "tmdb:7", "wrong", "hand label"))
CANDIDATES = ("# c\ncountry\tvenue\trawTitle\tfilm\tfilmTitle\tflagged\tevidence\n"
              "de\tBeta\tSnake Eyes\ttmdb:5\tSnake Eyes (2021)\t2026-10-23\tlooks wrong: a short\n"
              "de\tAlpha\tJudged\ttmdb:7\tJudged (2000)\t2026-10-23\tanswered already\n")


class Fixture(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        fixtures = os.path.join(self.dir.name, "test/resources/fixtures")
        os.makedirs(fixtures)
        self.path = os.path.join(fixtures, "identity-judgements.tsv")
        self.candidates = os.path.join(fixtures, r.CANDIDATES)
        open(self.path, "w", encoding="utf-8").write(FIXTURE)
        open(self.candidates, "w", encoding="utf-8").write(CANDIDATES)

    def tearDown(self):
        self.dir.cleanup()

    def take(self, kind, film):
        return next(t for t in r.awaiting(self.path, self.candidates) if t.kind == kind and t.film == film)

    def rows(self):
        return {(x.venue, x.rawTitle, x.film): x for x in j.read(self.path)}


class AwaitingTest(Fixture):
    def test_lists_agent_judged_takes_by_billing_and_film_and_unanswered_candidates(self):
        takes = r.awaiting(self.path, self.candidates)
        self.assertEqual([("agent", "tmdb:2", ["Zeta", "Alpha"]), ("agent", "tmdb:1", ["Alpha"]), ("candidate", "tmdb:5", ["Beta"])],
                         [(t.kind, t.film, t.venues) for t in takes])

    def test_the_real_judgements_file_lists_the_known_takes(self):
        takes = r.awaiting(os.path.join(REPO, j.PATH), os.path.join(REPO, "test/resources/fixtures", r.CANDIDATES))
        agent = {(t.title, t.film) for t in takes if t.kind == "agent"}
        self.assertIn(("Buddies", "tmdb:1629957"), agent)
        self.assertIn(("Buddies", "tmdb:259433"), agent)
        self.assertIn(("The MET Opera: La Fancuilla del West", "tmdb:1703629"), agent)


class AnswerTest(Fixture):
    def answer(self, kind, take_film, action, **kw):
        r.answer(self.path, self.candidates, self.take(kind, take_film).id, action, today="2026-11-06", **kw)

    def test_confirm_keeps_the_verdict_and_the_state_and_notes_the_owner_in_every_venue_row(self):
        self.answer("agent", "tmdb:1", "confirm")
        x = self.rows()[("Alpha", "Buddies", "tmdb:1")]
        self.assertEqual(("wrong", "known-wrong", "rows", "2026-11-06"), (x.verdict, x.state, x.scope, x.judged))
        self.assertRegex(x.evidence, "^REVIEW PAGE 2026-11-06: owner confirmed wrong; was: wrong: AGENT-JUDGED")
        self.assertEqual(["tmdb:2", "tmdb:5"], [t.film for t in r.awaiting(self.path, self.candidates)])

    def test_reject_flips_the_verdict_and_clears_a_known_wrong_state(self):
        self.answer("agent", "tmdb:1", "reject")
        x = self.rows()[("Alpha", "Buddies", "tmdb:1")]
        self.assertEqual(("right", "open", "", ""), (x.verdict, x.state, x.scope, x.reason))
        self.answer("agent", "tmdb:2", "reject")
        self.assertEqual({"wrong"}, {x.verdict for k, x in self.rows().items() if k[2] == "tmdb:2"})

    def test_another_film_marks_the_take_wrong_and_adds_a_right_row_per_venue_without_a_second_row_for_a_key(self):
        self.answer("agent", "tmdb:2", "other", film="tmdb:3", film_title="Buddies (1986)")
        rows = self.rows()
        self.assertEqual("wrong", rows[("Zeta", "Buddies", "tmdb:2")].verdict)
        self.assertEqual(("right", "Buddies (1986)"), (rows[("Zeta", "Buddies", "tmdb:3")].verdict, rows[("Zeta", "Buddies", "tmdb:3")].filmTitle))
        self.assertIn(("Alpha", "Buddies", "tmdb:3"), rows)
        self.assertEqual([], j.problems(j.read(self.path)))

    def test_another_film_that_already_has_a_row_edits_it_in_place(self):
        self.answer("agent", "tmdb:1", "other", film="tmdb:2")
        self.assertEqual("right", self.rows()[("Alpha", "Buddies", "tmdb:2")].verdict)
        self.assertEqual(1, sum(1 for x in j.read(self.path) if x.key == ("de", "Alpha", "Buddies", "tmdb:2")))

    def test_a_candidate_answered_right_becomes_a_row_and_drops_off(self):
        self.answer("candidate", "tmdb:5", "right")
        x = self.rows()[("Beta", "Snake Eyes", "tmdb:5")]
        self.assertEqual(("right", "Snake Eyes (2021)"), (x.verdict, x.filmTitle))
        self.assertEqual([], [t for t in r.awaiting(self.path, self.candidates) if t.kind == "candidate"])

    def test_every_other_line_is_kept_byte_for_byte(self):
        self.answer("agent", "tmdb:2", "confirm")
        old, new = FIXTURE.split("\n"), open(self.path, encoding="utf-8").read().split("\n")
        self.assertEqual(len(old), len(new))
        self.assertEqual([0, 1, 4, 5, 6], [i for i in range(len(old)) if old[i] == new[i]][:5])
        self.assertEqual(old[:2], new[:2])
        self.assertEqual(old[4:], new[4:])

    def test_a_bad_film_id_is_refused_and_the_file_is_untouched(self):
        with self.assertRaises(r.AnswerError):
            self.answer("agent", "tmdb:2", "other", film="not an id")
        self.assertEqual(FIXTURE, open(self.path, encoding="utf-8").read())

    def test_the_lint_refusing_puts_the_file_back(self):
        # a known-wrong row with no scope left behind would fail the lint: force it through a doctored take
        original = j.problems
        j.problems = lambda rows: [(None, "boom")]
        try:
            with self.assertRaises(r.AnswerError):
                self.answer("agent", "tmdb:2", "confirm")
        finally:
            j.problems = original
        self.assertEqual(FIXTURE, open(self.path, encoding="utf-8").read())

    def test_an_answered_take_cannot_be_answered_twice(self):
        take_id = self.take("agent", "tmdb:2").id
        r.answer(self.path, self.candidates, take_id, "confirm")
        with self.assertRaises(r.AnswerError):
            r.answer(self.path, self.candidates, take_id, "confirm")


class ServerTest(Fixture):
    def serve(self, runner):
        server = HTTPServer(("127.0.0.1", 0), r.make_handler(self.path, runner))
        threading.Thread(target=server.serve_forever, daemon=True).start()
        self.addCleanup(server.shutdown)
        return f"http://127.0.0.1:{server.server_port}"

    def post(self, url, **form):
        return urllib.request.urlopen(urllib.request.Request(url, urllib.parse.urlencode(form).encode())).read().decode()

    def test_the_page_shows_the_cards_and_an_answer_posts_through(self):
        base = self.serve(lambda worktree: (True, "ok"))
        page = urllib.request.urlopen(base + "/").read().decode()
        self.assertIn("3 awaiting you", page)
        self.assertIn("AGENT-JUDGED 2026-11-05 (a): because", page)
        self.assertIn("flagged candidate", page)
        self.assertIn("Alpha: <span class=wrong>wrong</span> tmdb:1", page)  # the other film's judgement of the same billing
        self.assertIn("not saved", self.post(base + "/answer", id="nope", action="confirm"))
        self.assertIn("2 awaiting you", self.post(base + "/answer", id=self.take("agent", "tmdb:2").id, action="confirm"))

    def test_land_runs_the_runner_in_the_review_worktree(self):
        seen = []
        base = self.serve(lambda worktree: (seen.append(worktree), (False, "lint says no"))[1])
        out = self.post(base + "/land")
        self.assertIn("NOT landed", out)
        self.assertIn("lint says no", out)
        self.assertEqual([os.path.realpath(self.dir.name)], [os.path.realpath(w) for w in seen])

    def test_land_commands_are_the_commit_script_then_agentctl(self):
        self.assertEqual(["scripts/review-commit"], r.land_commands("/w")[0])
        self.assertEqual(["land"], r.land_commands("/w")[1][1:])

    def test_land_stops_at_the_first_failing_command(self):
        ok, log = r.land(self.dir.name, [["false"], ["echo", "never"]])
        self.assertFalse(ok)
        self.assertNotIn("never", log)


if __name__ == "__main__":
    unittest.main()
