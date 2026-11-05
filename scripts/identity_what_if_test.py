#!/usr/bin/env python3
"""identity_what_if.py: a change is attributed to the rules its callers reach, a change it cannot attribute selects every
family (saying why), the families a rule fired in are the ones re-resolved, and the diff buckets each moved take as
identity_measure does -- and --verify names a take the selection missed."""

import io
import os
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_what_if as w  # noqa: E402

ACCEPTANCE = """package services.identity

private[identity] trait AcceptanceRules {
  /** Why the rule takes it. */
  def soleWorkWhy(ranked: Seq[Int]): Boolean =
    ranked.sizeIs == 1 && plausible(ranked.head)

  def plausible(film: Int): Boolean = {
    val cut = 3
    film > cut
  }

  def scoring(film: Int): Double = film * 0.5
}
"""

RESOLVER = """package services.identity

object Resolver {
  def resolve(listings: Seq[Int]): Seq[Double] = listings.map(new AcceptanceRules {}.scoring)
  def decide(ranked: Seq[Int]): Boolean = new AcceptanceRules {}.soleWorkWhy(ranked)
}
"""

WORKER = """package services.identity

object Projection {
  def project(listings: Seq[Int]): Seq[Double] = Resolver.resolve(listings)
}
"""

ROOTS = "definition\tpatterns\twhy\nsoleWorkWhy\taccept:sole-work refused:sole-work:*\tthe rule\n"


SCRATCH_GIT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "scripts/scratch-git.sh")


class Repo:
    """A throwaway repository holding an identity source, its caller and a worker caller, with one commit to change --
    made through scripts/scratch-git.sh, never a raw git, which under the pre-push hook inherits its GIT_DIR and writes
    into the repository every worktree shares (the tool's own reads run with every GIT_* variable dropped, setUp)."""

    def __init__(self, root):
        self.root = root
        self.scratch("scratch_repo", "-q")
        self.write("identity/src/main/scala/services/identity/AcceptanceRules.scala", ACCEPTANCE)
        self.write("identity/src/main/scala/services/identity/Resolver.scala", RESOLVER)
        self.write("worker/src/main/scala/services/identity/Projection.scala", WORKER)
        self.write("docs/notes.md", "notes\n")
        self.base = self.commit("base")
        with open(os.path.join(root, "roots.tsv"), "w", encoding="utf-8") as f:
            f.write(ROOTS)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.root, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def git(self, *args):
        return self.scratch("scratch_git", *args)

    def write(self, path, text):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w", encoding="utf-8") as f:
            f.write(text)

    def commit(self, message):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)
        return self.git("rev-parse", "HEAD")

    def edit(self, path, old, new):
        full = os.path.join(self.root, path)
        with open(full, encoding="utf-8") as f:
            text = f.read()
        assert old in text, old
        self.write(path, text.replace(old, new))

    def attribute(self, head="HEAD"):
        head_ref = "." if head == "." else self.git("rev-parse", head)
        return w.attribute(w.changes_between(self.root, self.base, head_ref), w.Sources(self.root, self.base, head_ref),
                           w.read_roots(os.path.join(self.root, "roots.tsv")))


ACC = "identity/src/main/scala/services/identity/AcceptanceRules.scala"


class AttributeTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.tmp = tempfile.TemporaryDirectory()
        self.repo = Repo(os.path.join(self.tmp.name, "repo"))

    def tearDown(self):
        self.tmp.cleanup()
        os.environ.update(self.saved)

    def test_a_helper_only_a_rule_calls_moves_that_rule(self):
        self.repo.edit(ACC, "val cut = 3", "val cut = 4")
        self.repo.commit("cut")
        self.assertEqual(self.repo.attribute(), ({"accept:sole-work", "refused:sole-work:*"}, None))

    def test_the_rule_itself(self):
        self.repo.edit(ACC, "ranked.sizeIs == 1", "ranked.sizeIs <= 1")
        self.repo.commit("rule")
        self.assertEqual(self.repo.attribute()[0], {"accept:sole-work", "refused:sole-work:*"})

    def test_a_new_helper_a_rule_calls(self):
        self.repo.edit(ACC, "&& plausible(ranked.head)", "&& plausible(ranked.head) && dated(ranked.head)")
        self.repo.edit(ACC, "  def scoring", "  def dated(film: Int): Boolean =\n    film < 2027\n\n  def scoring")
        self.repo.commit("new helper")
        self.assertEqual(self.repo.attribute()[0], {"accept:sole-work", "refused:sole-work:*"})

    def test_code_the_worker_reaches_past_every_root_selects_all(self):
        self.repo.edit(ACC, "film * 0.5", "film * 0.6")
        self.repo.commit("scoring")
        patterns, why = self.repo.attribute()
        self.assertIsNone(patterns)
        self.assertIn("reaches the worker's project", why)

    def test_code_outside_the_identity_sources_selects_all(self):
        self.repo.edit("worker/src/main/scala/services/identity/Projection.scala", "Resolver.resolve(listings)",
                       "Resolver.resolve(listings.distinct)")
        self.repo.commit("worker")
        patterns, why = self.repo.attribute()
        self.assertIsNone(patterns)
        self.assertIn("outside the identity model's sources", why)

    def test_comments_docs_and_tests_reach_nothing(self):
        self.repo.edit(ACC, "/** Why the rule takes it. */", "/** Why the rule takes it, and when. */")
        self.repo.write("docs/notes.md", "more notes\n")
        self.repo.write("identity/src/test/scala/services/identity/AcceptanceSpec.scala", "class AcceptanceSpec\n")
        self.repo.commit("inert")
        self.assertEqual(self.repo.attribute(), (set(), None))

    def test_the_agreement_stage_is_not_the_models(self):
        self.repo.write("identity/src/main/scala/services/identity/agreement/Stage.scala", "object Stage { def x = 1 }\n")
        self.repo.commit("stage")
        self.assertEqual(self.repo.attribute(), (set(), None))

    def test_uncommitted_edits_of_the_checkout(self):
        self.repo.edit(ACC, "val cut = 3", "val cut = 5")
        self.assertEqual(self.repo.attribute(".")[0], {"accept:sole-work", "refused:sole-work:*"})


class HolderTest(unittest.TestCase):
    def test_a_local_definition_is_its_members(self):
        lines = ACCEPTANCE.splitlines()
        self.assertEqual(w.holder_at(lines, lines.index("    film > cut")), ("plausible", "def"))
        self.assertEqual(w.holder_at(lines, lines.index("    val cut = 3")), ("plausible", "def"))

    def test_a_line_after_a_one_line_local_def_is_the_outer_members(self):
        lines = ["object O {", "  def rule(x: Int): Boolean = {", "    def near(y: Int) =", "      y > 1",
                 "    near(x) &&", "      x < 3", "  }", "}"]
        self.assertEqual(w.holder_at(lines, 5), ("rule", "def"))


def row(key, family, take, rules=""):
    venue, title = key.split("|")
    return {"key": key, "venue": venue, "rawTitle": title, "family": family, "take": take, "rules": rules}


def judged(venue, title, film, verdict):
    return "\t".join(["de", venue, title, film, verdict, "open", "", "", "", "2026-11-05", ""]) + "\n"


class SelectAndDiffTest(unittest.TestCase):
    BASE = [row("A|Hen", "A|Hen", "tmdb:1", "accept:sole-work"), row("B|Hen", "A|Hen", "tmdb:1", "join:title"),
            row("C|Lalka", "C|Lalka", "tmdb:5", "accept:exact-top-hit"), row("D|Obcy", "D|Obcy", "none", "refused:sole-work:no-candidate"),
            row("E|Betty", "E|Betty", "pending", "search:unanswered")]

    def test_the_families_a_pattern_names_whole(self):
        keys, picked, families = w.select(self.BASE, ["accept:sole-work"])
        self.assertEqual((keys, picked, families), (["A|Hen", "B|Hen"], 1, 4))
        self.assertEqual(w.select(self.BASE, ["refused:sole-work:*"])[0], ["D|Obcy"])

    def judgements(self):
        tmp = tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False, encoding="utf-8")
        tmp.write("country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n")
        tmp.write(judged("*", "Hen", "tmdb:1", "right"))
        tmp.write(judged("*", "Lalka", "tmdb:6", "wrong"))
        tmp.write(judged("*", "Lalka", "tmdb:5", "right"))
        tmp.write(judged("*", "Obcy", "tmdb:7", "right"))
        tmp.close()
        self.addCleanup(os.unlink, tmp.name)
        from identity_judgements import read
        return read(tmp.name)

    def test_the_diff_buckets_as_the_measure_and_reads_the_base_where_the_head_did_not_resolve(self):
        head = [row("A|Hen", "A|Hen", "none"), row("B|Hen", "A|Hen", "tmdb:1"), row("C|Lalka", "C|Lalka", "tmdb:6"),
                row("D|Obcy", "D|Obcy", "tmdb:7")]
        prod = {"A|Hen": "tmdb:1", "B|Hen": "tmdb:1", "C|Lalka": "tmdb:5", "D|Obcy": "tmdb:7", "E|Betty": "missing"}
        report, rows = w.diff("de", self.BASE, head, prod, self.judgements())
        self.assertEqual(report["moved"], 3)
        self.assertEqual(report["reresolved"], 4)
        self.assertEqual(report["buckets"], {"right-lost": 1, "right->wrong": 1, "gained-right": 1})
        self.assertEqual(report["prod"], {"compared": 4, "base agrees": 3, "head agrees": 2, "toward prod": 1, "away from prod": 2})
        self.assertEqual([(r["venue"], r["bucket"]) for r in rows], [("A", "right-lost"), ("C", "right->wrong"), ("D", "gained-right")])
        self.assertIn("BLOCKING", w.render(report))

    def test_verify_names_a_take_outside_the_selection_the_whole_head_moves(self):
        head = [row("A|Hen", "A|Hen", "tmdb:1"), row("B|Hen", "A|Hen", "tmdb:1")]
        whole = [dict(r) for r in self.BASE]
        whole[2]["take"] = "tmdb:6"
        report, _ = w.diff("de", self.BASE, head, {}, self.judgements(), whole)
        self.assertEqual(report["unsound"], [{"venue": "C", "rawTitle": "Lalka", "what-if": "tmdb:5", "whole": "tmdb:6", "selected": False}])
        report, _ = w.diff("de", self.BASE, head, {}, self.judgements(), [dict(r) for r in self.BASE])
        self.assertEqual(report["unsound"], [])

    def test_a_take_the_head_awaits_an_answer_for_is_no_move(self):
        # the head's rules ask a search prod's store never held; the what-if refuses live asks, so the family is Pending.
        # A real resolve would ask it and answer: the listing is reported as awaiting, never as a lost take.
        head = [row("A|Hen", "A|Hen", "pending", "search:unanswered"), row("B|Hen", "A|Hen", "pending", "search:unanswered"),
                row("C|Lalka", "C|Lalka", "tmdb:5"), row("D|Obcy", "D|Obcy", "none"), row("E|Betty", "E|Betty", "pending", "search:unanswered")]
        report, rows = w.diff("de", self.BASE, head, {}, self.judgements())
        self.assertEqual((report["moved"], report["buckets"], rows), (0, {}, []))
        self.assertEqual(report["awaiting"], 2)
        self.assertIn("2 listing(s) await an answer", w.render(report))
        self.assertIn("CLEAN", w.render(report))
        # --verify compares the head's own takes, awaiting included, with the whole head resolve's
        whole = [dict(r) for r in head]
        report, _ = w.diff("de", self.BASE, head, {}, self.judgements(), whole)
        self.assertEqual(report["unsound"], [])

    def test_verify_lets_a_listing_the_head_corpus_drops_stand(self):
        # the head's rules fold two listings to one key: the head corpus (distinctBy key) holds one, so neither the
        # what-if's re-resolve nor the whole resolve takes the other -- the base's row stands in the what-if, absent in whole
        head = [row("A|Hen", "A|Hen", "tmdb:1"), row("B|Hen", "A|Hen", "tmdb:1")]
        whole = [dict(r) for r in self.BASE if r["key"] != "C|Lalka"]
        report, _ = w.diff("de", self.BASE, head, {}, self.judgements(), whole)
        self.assertEqual(report["unsound"], [])

    def test_a_clean_change(self):
        report, rows = w.diff("de", self.BASE, [row("A|Hen", "A|Hen", "tmdb:1")], {}, self.judgements())
        self.assertEqual((report["moved"], rows), (0, []))
        out = io.StringIO()
        with redirect_stdout(out):
            print(w.render(report))
        self.assertIn("CLEAN", out.getvalue())


if __name__ == "__main__":
    unittest.main()
