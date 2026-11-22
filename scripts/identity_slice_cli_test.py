#!/usr/bin/env python3
# script-test-reads: scripts/identity-slice scripts/identity_slice*.py scripts/identity-slice-tool-files.txt scripts/identity-tools.sh scripts/identity-explain scripts/tool-conventions.sh scripts/jvm-gate.sh scripts/zinc_orphans.py scripts/lib/*.py scripts/ci/identity_*.py scripts/ci/population_walk_diff.py
"""scripts/identity-slice, black-box: a log line read into its listing and build, each ref laid out once with the tool's
files dropped in and run on ONE recording, the two refs' takes diffed and tallied against the labels, a ref the clone
does not hold refused by name; the keys this checkout's code, a ref's code and a slice's results are kept under; a new
ref's compile seeded from the nearest compiled one; --bisect, --help. java, sbt and gh are stubs; the repository is a
scratch one. The kept JVMs' cases are in identity_slice_cli_serve_test.py, the other modes' in
identity_slice_cli_modes_test.py. Ported from identity-slice-test.sh; unchanged across the swap to Python."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_slice_cli_fixture import JUDGEMENTS, SliceCase, count, tool_files, zinc_build  # noqa: E402

KEY = ("de", "--recording", "777", "--key", "Kino|Lalka")


class TwoRefsTest(SliceCase):
    def test_each_ref_is_laid_out_once_and_decides_the_same_recording(self) -> None:
        done = self.slice(*KEY, "--ref", "HEAD~1", "--ref", "HEAD")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(2, len(os.listdir(os.path.join(self.cache, "refs"))))
        # ...with the ref's judgements, which the worker's build bundles -- into a tree laid out before they were too
        judged = self.ref_tree(self.base, JUDGEMENTS)
        self.assertEqual(1, count("^country", self.content(judged)))
        os.remove(judged)
        subprocess.run(["bash", "-c", '. "$1/scripts/identity-tools.sh" && identity_tree_judgements "$2" "$3"', "_",
                        self.repo_dir, self.base, self.ref_tree(self.base)], env=self.tool_env(), check=True)
        self.assertEqual(1, count("^country", self.content(judged)))
        # ...with the tool's files dropped in, every one of them
        slice_file = "worker/src/test/scala/scripts/IdentitySlice.scala"
        self.assertEqual(f"// {slice_file}", self.content(self.ref_tree(self.base, slice_file)))
        self.assertEqual([], [f for f in tool_files() if not os.path.isfile(self.ref_tree(self.base, f))])
        calls = self.calls()
        self.assertEqual(2, calls.count(f"--recording {self.cache}/recordings/777/pair/test/resources/fixtures"))
        self.assertEqual(2, calls.count(f"--families {self.repo_dir}/test/resources/fixtures/identity-unmatched/de.json.gz"))
        self.assertEqual(1, calls.count(f"--ref {self.head}"))

    def test_the_moves_are_tallied_against_the_labels_each_with_its_verdicts(self) -> None:
        done = self.slice(*KEY, "--ref", "HEAD~1", "--ref", "HEAD")
        self.assertEqual(["TALLY cured 1, lost 1, new-wrong 1, won 0, moved (unjudged) 0"],
                         [line for line in done.stdout.splitlines() if line.startswith("TALLY")], done.output)
        self.assertEqual(1, count(r"Kino \| Emil: tmdb:2 .Emil \(2001\). via OwnMatch \[RIGHT\] -> none via "
                                  r"BelowThreshold \[-\]", done.stdout), done.output)

    def test_a_log_line_names_the_listing_exactly_and_decides_it_at_its_build(self) -> None:
        line = ('{"country":"de","venue":"Kino","listing":"Lalka","build":"%s","projection_mode":"changed",'
                '"kind":"lost"}' % self.base)
        done = self.slice("--log", line, "--recording", "777")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.calls().count("--key Kino|Lalka"))
        self.assertEqual(1, self.calls().count(f"--ref {self.base}"))

    def test_a_log_line_naming_no_listing_is_refused(self) -> None:
        self.assertEqual(2, self.slice("--log", '{"country":"de"}').code)

    def test_a_ref_the_clone_does_not_hold_is_refused_by_name(self) -> None:
        done = self.slice(*KEY, "--ref", "0123456789abcdef")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, count("no commit 0123456789abcdef", self.err), done.output)

    def test_the_script_edited_while_it_runs_finishes_as_it_started(self) -> None:
        original = self.path("tool.orig")
        shutil.copy2(self.tool, original)
        done = self.slice(*KEY, "--ref", "HEAD~1", "--ref", "HEAD", env={"STUB_REWRITE": self.tool})
        shutil.copy2(original, self.tool)
        self.assertEqual((0, 1), (done.code, count("^TALLY", done.stdout)), done.output)


class BatchTest(SliceCase):
    def setUp(self) -> None:
        super().setUp()
        self.write("b.tsv", "Kino\tLalka\n")

    def test_a_batch_file_is_handed_to_the_jvm_by_its_absolute_path(self) -> None:
        self.slice("de", "--recording", "777", "--batch", "b.tsv", "--ref", "HEAD")
        self.assertEqual(1, self.calls().count(f"--batch {self.work}/b.tsv"), self.err)

    def test_a_batch_meeting_requests_it_lacks_still_diffs_saying_so_at_each_ref(self) -> None:
        done = self.slice("de", "--recording", "777", "--batch", "b.tsv", "--ref", "HEAD~1", "--ref", "HEAD",
                          env={"STUB_INCOMPLETE": "1"})
        self.assertEqual((1, 2), (count("^TALLY", done.stdout), count("incomplete", self.err)), done.output)

    def test_out_keeps_the_takes_of_the_last_refs_batch(self) -> None:
        self.slice("de", "--recording", "777", "--batch", "b.tsv", "--out", self.path("takes.tsv"))
        self.assertEqual(1, count("^k\tKino\\|Sinn\tKino\tSinn\ttmdb:9", self.read("takes.tsv")), self.err)

    def test_an_out_in_a_missing_directory_is_a_usage_error(self) -> None:
        done = self.slice("de", "--recording", "777", "--batch", "b.tsv", "--out", self.path("nope/takes.tsv"))
        self.assertEqual((2, 1), (done.code, count(f"no directory {self.work}/nope", self.err)), done.output)


class CheckoutKeyTest(SliceCase):
    """This checkout's code (no --ref) is keyed by WHICH checkout, its commit and its uncommitted code: the
    whole-corpus index cached under that key is never another worktree's, an older commit's or an earlier edit's."""

    def ref_key(self, tool: str = "") -> str:
        self.clear_calls()
        self.slice(*KEY, tool=tool or None)
        return self.last(r"--ref (worktree[^ ]*)")

    def test_two_worktrees_at_different_commits_never_share_a_key(self) -> None:
        here = self.ref_key()
        self.git(self.repo_dir, "worktree", "add", "-q", self.path("wt2"), "HEAD~1")
        there = self.ref_key(os.path.join(self.path("wt2"), os.path.relpath(self.tool, self.repo_dir)))
        self.assertTrue(here and there and here != there, (here, there))

    def test_an_uncommitted_edit_of_the_code_is_another_key_and_undone_the_same(self) -> None:
        here = self.ref_key()
        slicing = "worker/src/test/scala/services/identity/IdentitySlicing.scala"
        with open(self.path("repo", slicing), "a") as f:
            f.write("// edited\n")
        self.assertNotEqual(here, self.ref_key())
        self.git(self.repo_dir, "checkout", "-q", "--", slicing)
        self.assertEqual(here, self.ref_key())

    def test_a_new_file_in_the_scrapers_or_geodata_module_is_another_key(self) -> None:
        # 2026-10-19: the scrapers and geodata modules reach the resolver (a venue's details, its catalogue links)
        here = self.ref_key()
        for module in ("scrapers", "geodata"):
            edited = self.write(f"repo/{module}/src/main/Edited.scala", "// new\n")
            self.assertNotEqual(here, self.ref_key(), module)
            os.remove(edited)

    def test_compiles_lays_a_ref_out_and_compiles_it_deciding_nothing(self) -> None:
        done = self.slice("--compiles", "HEAD~1")
        self.assertEqual((0, 0), (done.code, count("^java", self.calls())), done.output)
        self.assertIn("compiles with this checkout's tool files", self.err)


class KeyedCase(SliceCase):
    def key(self, *ref: str) -> str:
        self.clear_calls()
        self.slice(*KEY, *ref)
        return self.last(r"--index-key [^ ]*")

    def commit_docs(self, text: str) -> str:
        with open(self.path("repo/README.md"), "a") as f:
            f.write(text + "\n")
        return self.commit_all(text)


class CodeKeyTest(KeyedCase):
    """The whole-corpus index and the results are keyed by the code a ref runs, never its commit: a commit changing no
    code (a judgement, a doc) resolves nothing again (2026-10-15: 252 indexes under one recording, 12 distinct)."""

    def test_results_are_kept_per_recording_code_and_filed_answers_and_threads_reach_the_jvm(self) -> None:
        self.slice(*KEY, "--ref", "HEAD", "--threads", "3")
        results = f"--results {self.cache}/slice/777/results/code-[0-9a-f]{{24}}-none "
        self.assertEqual((1, 1), (count(results, self.calls()), self.calls().count("--threads 3")), self.err)

    def test_a_commit_changing_no_code_reads_its_parents_index(self) -> None:
        at_head = self.key("--ref", "HEAD")
        self.write("repo/" + JUDGEMENTS + ".note", "a judgement\n")
        docs = self.commit_docs("docs")
        self.assertEqual(at_head, self.key("--ref", docs))
        self.assertTrue(at_head.startswith("--index-key code-"), at_head)
        self.assertNotEqual(at_head, self.key("--ref", self.base), "build.sbt changed")

    def test_this_checkouts_code_is_its_commits_and_an_edit_of_it_other_code(self) -> None:
        at_head = self.key("--ref", "HEAD")
        mine = self.key()
        self.assertEqual(at_head, mine)
        self.write("repo/identity/src/main/Resolver.scala", "// edited\n")
        self.assertNotEqual(mine, self.key())

    def test_an_edit_of_the_tool_files_dropped_into_every_ref_is_other_code(self) -> None:
        at_head = self.key("--ref", "HEAD")
        with open(self.path("repo/worker/src/test/scala/scripts/IdentitySlice.scala"), "a") as f:
            f.write("// a new tool\n")
        self.assertNotEqual(at_head, self.key("--ref", "HEAD"))

    def test_fresh_decides_every_slice_again(self) -> None:
        self.slice(*KEY, "--ref", "HEAD", "--fresh")
        self.assertEqual(0, self.calls().count("--results"), self.err)


class CompileSeedTest(KeyedCase):
    """A new ref's tree is seeded from the nearest compiled one, and one with the very sources of a compiled ref keeps
    its classpath -- no sbt at all."""

    def compiled_docs(self) -> str:
        docs = self.commit_docs("no code")
        self.slice(*KEY, "--ref", docs)
        # ...never with the class files no build of the seed's produced (IntelliJ's JPS, 2026-10-18), and with every
        # module's build (2026-10-19: scrapers' and geodata's were not seeded)
        zinc_build(self.ref_tree(docs), "worker", "tools/Kept.class")
        self.write(self.ref_tree(docs, "worker/target/scala-3.x/classes/tools/Orphan.class"), "orphan\n")
        for module in ("scrapers", "geodata"):
            os.makedirs(self.ref_tree(docs, module, "target/scala-3.x/classes"))
        return docs

    def test_a_new_ref_is_seeded_from_the_nearest_compiled_one_and_compiles_nothing(self) -> None:
        docs = self.compiled_docs()
        again = self.commit_docs("no code again")
        self.clear_calls()
        self.slice(*KEY, "--ref", again)
        self.assertEqual(["scrapers", "geodata"], [m for m in ("scrapers", "geodata")
                                                    if os.path.isdir(self.ref_tree(again, m, "target/scala-3.x/classes"))])
        self.assertEqual(["Kept.class", "Kept.tasty"],
                         sorted(os.listdir(self.ref_tree(again, "worker/target/scala-3.x/classes/tools"))))
        self.assertEqual(1, self.err.count(f"seeding its compile from {self.cache}/refs/{docs} (0 source file(s) apart)"),
                         self.err)
        self.assertEqual(0, count(f"^sbt {self.cache}/refs/{again}", self.calls()))
        self.assertEqual(1, self.calls().count(f"-cp {self.cache}/refs/{again}/worker/target/scala-3.x/test-classes:/lib/a.jar"))

    def test_a_dropped_in_tool_file_differing_from_the_seeds_compiles(self) -> None:
        self.compiled_docs()
        third = self.commit_docs("no code, a third")
        with open(self.path("repo/worker/src/test/scala/services/identity/IdentitySlicing.scala"), "a") as f:
            f.write("// this checkout's own tool\n")
        self.clear_calls()
        self.slice(*KEY, "--ref", third)
        self.assertEqual(1, count(f"^sbt {self.cache}/refs/{third}", self.calls()), self.err)

    def test_a_call_finding_another_dropping_tool_files_in_waits_and_gives_up_naming_it(self) -> None:
        self.hold_lock(os.path.join(self.cache, "locks", f"tools-{self.head}.flock"))
        done = self.slice(*KEY, "--ref", "HEAD", env={"KINOWO_IDENTITY_LOCK_WAIT": "2"})
        self.assertEqual((1, 1), (done.code, count(f"gave up after .*tools-{self.head}", self.err)), done.output)


class BisectAndUsageTest(SliceCase):
    def test_bisect_names_the_first_commit_moving_the_take(self) -> None:
        done = self.slice(*KEY, "--bisect", f"{self.base}..{self.head}")
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"first commit moving the take: {self.head[:7]} head", done.stdout)
        self.assertIn("bisecting 1 commit(s) after", self.err)

    def test_bisect_over_commits_moving_nothing_says_so(self) -> None:
        self.write("repo/README.md", "x\n")
        later = self.commit_all("nothing")
        done = self.slice(*KEY, "--bisect", f"{self.head}..{later}")
        self.assertEqual((0, f"no commit of {self.head}..{later} moves the take\n"), (done.code, done.stdout), done.output)

    def test_help_prints_the_header_and_no_argument_is_a_usage_error(self) -> None:
        done = self.slice("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("\nReproduce ONE listing's identity take", "\n" + done.stdout, done.stdout[:300])
        self.assertTrue(done.stdout.endswith("pinning cases one after another costs ~2 s each.\n"), done.stdout[-200:])
        bare = self.slice()
        self.assertEqual((2, "", done.stdout), (bare.code, bare.stdout, bare.stderr))

    def test_an_unknown_argument_is_a_usage_error(self) -> None:
        done = self.slice("de", "--bogus")
        self.assertEqual(2, done.code)
        self.assertTrue(done.stderr.startswith("unknown argument --bogus\n"), done.stderr[:200])
        self.assertEqual(self.slice("--help").stdout, done.stderr[len("unknown argument --bogus\n"):])

    def test_a_flag_without_its_value_is_a_usage_error(self) -> None:
        done = self.slice("de", "--ref")
        self.assertEqual((2, "identity-slice: --ref needs a value\nidentity-slice: see identity-slice --help\n"),
                         (done.code, done.stderr))


if __name__ == "__main__":
    unittest.main()
