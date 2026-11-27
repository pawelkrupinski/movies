#!/usr/bin/env python3
"""END TO END: scripts/identity-slice --explain and scripts/identity-what-if over a pure move, in a worktree of a clone of this
checkout (the tools as the working tree has them; scripts/e2e/e2e_world.py), with a tiny capture of two listings. Each ref
is laid out whole from git by identity-tools.sh, its classpath made by the stub sbt (scripts/e2e/sbt_stub.py), and the JVM is
a stub `java` at the process boundary: its IdentityExplain reads the capture it is handed and takes a listing when the
ref's own rules file (identity/src/main/scala/e2e/rules/threshold.txt) lets its score through -- so what it prints is
the ref's code deciding the capture's listings. NO mirror, recording download or real JVM is reached.

  - `identity-slice de --explain --venue Kino --fixture DIR`: only the selected listing explained (the stub JVM was handed
    the selector and the capture), at this checkout and, with `--ref A --ref B`, at each ref's own tree: the two traces
    side by side, then the diff naming the TAKE that moved; the same ref twice says the traces are identical;
  - `identity-what-if --base A --head B pl de` over a range that only moves code: `PURE MOVE: what-if skipped`, exit 0, no JVM
    and no mirror, and a `pure-move` verdict recorded for each country under exactly that head and base.
Mutations (scripts/e2e/mutations): the selector never passed to the JVM; two different traces called identical; the
pure-move verdict recorded under the wrong head; only the first country recorded."""
import gzip
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402
import sbt_stub  # noqa: E402

RULES = "identity/src/main/scala/e2e/rules"
# the stub JVM's IdentityExplain: the capture's listings (venue, title, score) matching --venue / --title, each taken when the
# tree's rules file lets its score through
JAVA = r'''
if "scripts.IdentityExplain" not in argv:
    err("the stub java explains only: " + " ".join(argv)); sys.exit(2)
rest = argv[argv.index("scripts.IdentityExplain") + 1:]
opt = {rest[i]: rest[i + 1] for i in range(1, len(rest) - 1) if rest[i].startswith("--")}
import gzip
threshold = float(open("identity/src/main/scala/e2e/rules/threshold.txt").read())
for c in json.load(gzip.open(opt["--fixture"], "rt")):
    if opt.get("--venue", "").lower() in c["venue"].lower() and opt.get("--title", "").lower() in c["title"].lower():
        out("== " + c["venue"] + " | " + c["title"])
        out("agreement Capture")
        out("TAKE " + (c["film"] if c["score"] >= threshold else "none"))
'''
CAPTURE = [{"venue": "Kino Lalka", "title": "Lalka", "film": "tmdb:1", "score": 0.7},
           {"venue": "Cinema Two", "title": "Emil", "film": "tmdb:2", "score": 0.7}]
SCALA = "package e2e\n\nobject Probe {\n  def answer: Int = 1\n}\n"


class IdentityToolsE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.movies_clone()
        self.tree = self.repo.worktree("change")
        self.sbt = sbt_stub.StubSbt(self.w)
        self.addCleanup(self.sbt.stop, self.tree)
        self.w.shim("java", JAVA, reads_stdin=False)
        self.capture = os.path.join(self.w.root, "capture")
        self.w.files.disk.make_directories(self.capture)
        with gzip.open(os.path.join(self.capture, "de.json.gz"), "wt") as f:
            json.dump(CAPTURE, f)
        self.w.write(self.capture, "identity-judgements.tsv", "country\tvenue\trawTitle\tfilm\tverdict\n")
        self.w.git(self.tree, "sparse-checkout", "add", "/identity/src/main/scala/e2e")
        self.w.write(self.tree, f"{RULES}/threshold.txt", "0.5\n")
        self.w.write(self.tree, f"{RULES}/Probe.scala", SCALA)
        self.base = self.w.commit(self.tree, "the rules let score 0.7 through")

    def tool(self, name: str, *args: str) -> e2e_world.cli_harness.Done:
        return self.w.run([os.path.join(self.tree, "scripts", name), *args], self.tree, env=self.sbt.env(), bound=240)

    def test_slice_explain_takes_the_selected_listings_at_each_refs_own_code(self) -> None:
        self.w.write(self.tree, f"{RULES}/threshold.txt", "0.9\n")
        head = self.w.commit(self.tree, "the rules ask for more")
        done = self.tool("identity-slice", "de", "--explain", "--venue", "Kino", "--fixture", self.capture)
        self.assertRan(done, 0, "== Kino Lalka | Lalka", "TAKE none")
        self.assertNotIn("Cinema Two", done.output, "the selector never reached the JVM")
        [call] = [c for c in self.w.calls("java") if "scripts.IdentityExplain" in c["argv"]]
        self.assertIn(os.path.join(self.capture, "de.json.gz"), call["argv"])
        self.assertEqual(self.tree, os.path.realpath(call["cwd"]), "this checkout's code was not the JVM's")
        done = self.tool("identity-slice", "de", "--explain", "--venue", "Kino", "--fixture", self.capture,
                         "--ref", self.base, "--ref", head)
        self.assertRan(done, 0, "TAKE tmdb:1", "TAKE none", f"--- {self.base}", f"+++ {head}")
        self.assertRegex(done.output, r"-TAKE tmdb:1\n\+TAKE none")
        self.assertNotIn("Cinema Two", done.output)
        done = self.tool("identity-slice", "de", "--explain", "--fixture", self.capture, "--ref", self.base, "--ref", self.base)
        self.assertRan(done, 0, "the two refs' traces are identical", "Cinema Two | Emil")

    def test_what_if_over_a_pure_move_resolves_nothing_and_records_its_verdict(self) -> None:
        self.w.git(self.tree, "mv", f"{RULES}/Probe.scala", f"{RULES}/Moved.scala")
        head = self.w.commit(self.tree, "move the probe")
        done = self.tool("identity-what-if", "--base", self.base, "--head", head, "pl", "de")
        self.assertRan(done, 0, "PURE MOVE: what-if skipped")
        record = os.path.join(self.w.root, "identity", "what-if-verdicts", f"{head}-{self.base}.tsv")
        self.assertTrue(self.w.files.exists(record), "no verdict recorded under the move's head and base")
        verdicts = [line.split("\t")[:2] for line in self.w.read(record).splitlines()]
        self.assertEqual([["pl", "pure-move"], ["de", "pure-move"]], verdicts)
        self.assertEqual([], self.w.calls("java"), "a JVM was started for a pure move")


if __name__ == "__main__":
    unittest.main()
