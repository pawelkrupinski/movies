#!/usr/bin/env python3
"""END TO END: scripts/identity-stamp-carry (`propose`, `verify`), in a worktree of a clone of this checkout (the tools as the
working tree has them; scripts/e2e/e2e_world.py). The stamps are made by the real `identity-stamp-carry stamps` -- the
ref laid out whole by identity-tools.sh from git, the real sbt command line run in it -- over the stub sbt of
scripts/e2e/sbt_stub.py, which answers the stamps task with digests the commits control (the hash of the paths and text
under identity/src/main/scala/e2e/<stamp>/). The real pure-move-check judges each range, the real git holds the history.

  - a pure move of a rules file: `propose` carries identity-rules (a row of its stamp, the build's digest of the head
    against the base's, proof pure-move), says venue-slot and agreement-rules are unchanged, and writes the range beside
    it; committed, `verify` proves the row again (exit 0, `proven`);
  - the row edited by hand -- its digest, its `equals` -- is refused by `verify` (exit 1), and so is the carry after a
    rebase over identity code (the base's digest moved: the row cites commits the push no longer holds the same);
  - a hand-written what-if verdict (done, nothing moved, no published asset) carries nothing: `verify` refuses a what-if row
    over it, naming the missing asset;
  - a move that also edits a line is no pure move: `propose` exits 1 REFUSED and writes no row.
Mutations (scripts/e2e/mutations): the pure-move proof re-run read as always holding; the row's digest or `equals` never
compared with the build's; the row's cited range taken as the pushed one; the published asset not asked for."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402
import sbt_stub  # noqa: E402

TABLE = "test/resources/identity-stamp-carry.tsv"
RANGES = "test/resources/identity-stamp-carry-ranges.tsv"
RULES = "identity/src/main/scala/e2e/identity-rules"
SCALA = "package e2e\n\nobject Probe {{\n  def answer: Int = {answer}\n}}\n"


class IdentityStampCarryE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.movies_clone()
        self.tree = self.repo.worktree("change")
        self.sbt = sbt_stub.StubSbt(self.w)
        self.addCleanup(self.sbt.stop, self.tree)
        self.w.git(self.tree, "sparse-checkout", "add", "/identity/src/main/scala/e2e")
        self.w.write(self.tree, f"{RULES}/Probe.scala", SCALA.format(answer=1))
        self.base = self.w.commit(self.tree, "a rules file")

    def carry(self, *args: str) -> e2e_world.cli_harness.Done:
        return self.w.run([os.path.join(self.tree, "scripts/identity-stamp-carry"), *args], self.tree, env=self.sbt.env(),
                          bound=240)

    def move(self, answer: int, message: str) -> str:
        """The rules file moved to a new name (its text as it was, or with `answer` changed): a commit."""
        self.w.git(self.tree, "mv", f"{RULES}/Probe.scala", f"{RULES}/Moved.scala")
        self.w.write(self.tree, f"{RULES}/Moved.scala", SCALA.format(answer=answer))
        return self.w.commit(self.tree, message)

    def rows(self) -> list:
        """The rows this scenario's range wrote (the table holds the repository's own besides)."""
        return [cols for cols in (line.split("\t") for line in self.w.read(os.path.join(self.tree, TABLE)).splitlines())
                if len(cols) == 6 and cols[4] == self.base]

    def test_a_pure_move_is_carried_and_a_hand_edited_or_rebased_row_is_refused(self) -> None:
        head = self.move(1, "move the rules file")
        done = self.carry("propose", f"{self.base}..{head}")
        self.assertRan(done, 0, "identity-rules:", "carries", "venue-slot: unchanged", "agreement-rules: unchanged",
                       "row(s) added to test/resources/identity-stamp-carry.tsv")
        [row] = self.rows()
        stamp, digest, equals, proof, base, cited = row
        self.assertEqual(("identity-rules", "pure-move", self.base, head), (stamp, proof, base, cited))
        stamps = {s: d for s, d in (line.split("\t") for line in self.carry("stamps", head).stdout.splitlines())}
        self.assertEqual(stamps["identity-rules"], digest, "the row's digest is not the build's of the head")
        self.assertNotEqual(digest, equals, "a move that left the digest alone has nothing to carry")
        self.assertIn(self.base, self.w.read(os.path.join(self.tree, RANGES)))
        landed = self.w.commit(self.tree, "carry the rules stamp", TABLE, RANGES)
        self.assertRan(self.carry("verify", f"{self.base}..{landed}"), 0, "proven")
        self.hand_edits_refused(digest, equals, landed)
        self.forged_verdict_refused(landed)
        self.rebase_refused(head, landed)

    def hand_edits_refused(self, digest: str, equals: str, landed: str) -> None:
        for old, why in ((digest, "digest is not"), (equals, "equals is not")):
            path = os.path.join(self.tree, TABLE)
            self.w.write(self.tree, TABLE, self.w.read(path).replace(old, "0" * 64))
            edited = self.w.commit(self.tree, f"a hand-edited row ({why})", TABLE)
            self.assertRan(self.carry("verify", f"{self.base}..{edited}"), 1, "REFUSED", why)
            self.w.write(self.tree, TABLE, self.w.read(path).replace("0" * 64, old))
            self.w.commit(self.tree, "the row as it was", TABLE)
            self.assertEqual("", self.w.git(self.tree, "diff", landed, "HEAD", "--", TABLE))

    def forged_verdict_refused(self, landed: str) -> None:
        """A what-if row over a verdict its author wrote (every country done, nothing moved, no published asset)."""
        self.w.write(self.tree, f"{RULES}/Moved.scala", SCALA.format(answer=2))
        edited = self.w.commit(self.tree, "an edit the carry cannot call pure")
        stamps = dict(line.split("\t") for line in self.carry("stamps", edited).stdout.splitlines())
        before = dict(line.split("\t") for line in self.carry("stamps", landed).stdout.splitlines())
        diff = self.w.git(self.tree, "diff", landed, edited)
        patch_id = self.w.git_run(self.tree, "patch-id", "--stable", stdin=diff + "\n").stdout.split()[0]
        record = "".join(f"{cc}\tdone\t2026-11-27T00:00:00Z\t-\t0\twhole,final\t-\n" for cc in ("pl", "de", "uk", "es", "us"))
        self.w.write(self.tree, f"test/resources/identity-what-if-verdicts/{edited}-{landed}.tsv",
                     record + f"patch-id\t{patch_id}\t{edited}\t{landed}\n")
        row = "\t".join(("identity-rules", stamps["identity-rules"], before["identity-rules"], "what-if", landed, edited))
        self.w.write(self.tree, TABLE, self.w.read(os.path.join(self.tree, TABLE)) + row + "\n")
        forged = self.w.commit(self.tree, "a what-if row over a hand-written verdict")
        self.assertRan(self.carry("verify", f"{landed}..{forged}"), 1, "REFUSED", "names no published asset")
        self.w.git(self.tree, "reset", "-q", "--hard", landed)

    def rebase_refused(self, head: str, landed: str) -> None:
        """Main moved over identity code under the carry commit: the pushed base's digest is no longer the row's."""
        other = self.repo.worktree("other", self.base)
        self.w.write(other, f"{RULES}/Other.scala", SCALA.format(answer=7))
        moved = self.w.commit(other, "main's own change to the rules")
        self.w.git(self.tree, "rebase", "-q", "--onto", moved, self.base, "HEAD")
        rebased = self.w.git(self.tree, "rev-parse", "HEAD")
        self.assertNotEqual(landed, rebased)
        self.assertRan(self.carry("verify", f"{moved}..{rebased}"), 1, "REFUSED", "digest is not")

    def test_a_move_that_also_edits_a_line_is_no_pure_move_and_writes_no_row(self) -> None:
        head = self.move(2, "move the rules file and change what it answers")
        table = self.w.read(os.path.join(self.tree, TABLE))
        done = self.carry("propose", f"{self.base}..{head}")
        self.assertRan(done, 1, "REFUSED: no proof holds", "pure-move:", "does not call")
        self.assertEqual(table, self.w.read(os.path.join(self.tree, TABLE)), "a row was written without a proof")
        self.assertEqual("", self.w.git(self.tree, "status", "--porcelain"))


if __name__ == "__main__":
    unittest.main()
