#!/usr/bin/env python3
"""lint_reads_trace.py: a command's reads are seen by the access time -- every file it opens (any reader: here `cat`
and `ls`), a listed directory, a test resource through its classpath copy -- and nothing it only stats, nor a main
resource served from the classpath; a read its registry row does not select is a miss, one it selects is not; the rows
it suggests read listed directories as `<dir>/*` (one keeping a kind of file as that kind and its family), three
modules sharing a tree as `*`, five files read by name in one directory as it, a spec reading nothing as `@code`. LINT_READS_TRACE_UNDER_TEST names another copy of the tool."""

import importlib.util
import os
import subprocess
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("LINT_READS_TRACE_UNDER_TEST") or os.path.join(HERE, "lint_reads_trace.py")


def load_tool():
    spec = importlib.util.spec_from_file_location("lint_reads_trace", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


T = load_tool()
# THE TRACE SEES READS BY ATIME, which a filesystem mounted noatime, or CI's Linux container's bind mount, never moves
# (2026-11-21: this test went red there). Where it does not, the cases reading atimes skip, saying so, and the tool
# itself refuses (test_a_filesystem_whose_atimes_never_move_is_refused, which runs everywhere).
FILE_READS, DIRECTORY_LISTINGS = T.atime_moves(tempfile.gettempdir())
NO_FILE_ATIMES = "file atimes do not move on this filesystem (noatime / a bind mount): the tool refuses to trace here"
NO_DIRECTORY_ATIMES = "directory atimes do not move on this filesystem: the tool refuses to trace here"

FILES = {
    ".github/workflows/a.yml": "a\n", ".github/workflows/b.yml": "b\n", "docs/notes.md": "n\n",
    "scripts/tool.sh": "echo\n", "worker/src/test/resources/fixture.txt": "f\n",
    "worker/src/main/resources/table.tsv": "t\n",
}
# a stand-in for pre-push --lint-plan: the spec is selected by a workflow or the fixture, nothing else
HOOK = """#!/bin/sh
while IFS= read -r path; do
  case "$path" in .github/workflows/*|worker/src/test/resources/fixture.txt) echo worker:deploy.ASpec ;; esac
done
"""


class LintReadsTraceTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        subprocess.run(["bash", "-c", '. "$1"; scratch_repo "$2" -q', "_", SCRATCH_GIT, self.repo], check=True)
        for path, text in FILES.items():
            os.makedirs(os.path.join(self.repo, os.path.dirname(path)), exist_ok=True)
            with open(os.path.join(self.repo, path), "w") as f:
                f.write(text)
        subprocess.run(["bash", "-c", '. "$1"; scratch_git "$2" add -A && scratch_git "$2" commit -qm base', "_",
                        SCRATCH_GIT, self.repo], check=True)
        # sbt's copies of the resources: a test one stands for its source, a main one is the code's own reading
        for config, rel in (("test-classes", "fixture.txt"), ("classes", "table.tsv")):
            copy = os.path.join(self.repo, "worker/target/scala-3.10.1", config, rel)
            os.makedirs(os.path.dirname(copy))
            with open(copy, "w") as f:
                f.write("copy\n")
        self.cwd = os.getcwd()
        os.chdir(self.repo)

    def tearDown(self):
        os.chdir(self.cwd)
        self.dir.cleanup()
        os.environ.update(self.saved)

    def observed(self, script):
        files, dirs = T.tracked()
        copies = T.resource_copies(files)
        reads, run = T.observe(["sh", "-c", script], files + dirs + list(copies), copies)
        self.assertEqual(run.returncode, 0, run.stderr)
        return [p + "/" if os.path.isdir(p) else p for p in reads]

    @unittest.skipUnless(FILE_READS and DIRECTORY_LISTINGS, NO_DIRECTORY_ATIMES if FILE_READS else NO_FILE_ATIMES)
    def test_a_read_by_any_reader_is_seen_and_a_stat_is_not(self):
        reads = self.observed("cat docs/notes.md >/dev/null; ls .github/workflows >/dev/null; test -f scripts/tool.sh")
        self.assertEqual(reads, [".github/workflows/", "docs/notes.md"])

    @unittest.skipUnless(FILE_READS, NO_FILE_ATIMES)
    def test_a_test_resource_through_the_classpath_is_its_source_and_a_main_one_is_no_read(self):
        reads = self.observed("cat worker/target/scala-3.10.1/test-classes/fixture.txt "
                              "worker/target/scala-3.10.1/classes/table.tsv >/dev/null")
        self.assertEqual(reads, ["worker/src/test/resources/fixture.txt"])

    def test_a_filesystem_whose_atimes_never_move_is_refused(self):
        """A trace that cannot see a read would report every row whole: it refuses, naming why, before running a spec."""
        self.assertIn("file read", T.blind((False, False)))
        self.assertIn("directory listed", T.blind((True, False)))
        self.assertIsNone(T.blind((True, True)))
        saved = T.atime_moves
        T.atime_moves = lambda root=".": (True, False)
        try:
            self.assertEqual(T.main(["worker:tools.ASpec"]), 2, "refused before any classpath or spec")
        finally:
            T.atime_moves = saved

    def test_the_probe_leaves_nothing_behind(self):
        T.atime_moves(self.repo)
        self.assertEqual(sorted(os.listdir(self.repo)), sorted({".git", "worker", ".github", "docs", "scripts"}))

    def test_a_read_the_row_does_not_select_is_a_miss(self):
        hook = os.path.join(self.dir.name, "hook")
        with open(hook, "w") as f:
            f.write(HOOK)
        os.chmod(hook, 0o755)
        observed = {"worker:deploy.ASpec": [".github/workflows/", ".github/workflows/a.yml", "docs/notes.md"]}
        self.assertEqual(T.misses(observed, hook), ["worker:deploy.ASpec\tdocs/notes.md"])

    @unittest.skipUnless(FILE_READS and DIRECTORY_LISTINGS, NO_DIRECTORY_ATIMES if FILE_READS else NO_FILE_ATIMES)
    def test_a_walk_lists_a_directory_holding_only_directories(self):
        self.assertEqual(self.observed("find .github >/dev/null"), [".github/", ".github/workflows/"])

    def test_the_suggested_row_reads_listed_directories_and_shared_trees_as_globs(self):
        files = [".github/workflows/a.yml", ".github/workflows/b.yml", "docs/notes.md", "web/src/main/twirl/x.html"] \
            + [f"{m}/src/{c}/scala/X.scala" for m in ("base", "common", "web") for c in ("main", "test", "it")]
        observed = {
            "worker:deploy.ASpec": [".github/workflows/", ".github/workflows/a.yml", ".github/workflows/b.yml",
                                    "docs/notes.md"],
            # a walk of every tree's scala/, three modules: their modules and configs read as `*`
            "common:tools.BSpec": [f"{m}/src/{c}/scala/" for m in ("base", "common", "web") for c in ("main", "test", "it")]
                                  + ["web/src/main/twirl/"],
            # three modules' test trees: the module read as `*`, the config kept (no production change selects it)
            "common:tools.CSpec": [f"{m}/src/test/scala/" for m in ("base", "common", "web")],
            "web:tools.DSpec": [],
        }
        self.assertEqual(T.suggest(observed, files), {
            "worker:deploy.ASpec": [".github/workflows/*", "docs/notes.md"],
            "common:tools.BSpec": ["*/src/*/scala/*", "web/src/main/twirl/*"],
            "common:tools.CSpec": ["*/src/test/scala/*"],
            "web:tools.DSpec": ["@code"],
        })

    def test_a_walk_keeping_one_kind_of_file_is_read_as_that_kind_and_its_family(self):
        files = ["worker/src/main/scala/A.scala", "worker/src/main/resources/t.tsv", ".github/workflows/a.yml",
                 ".github/workflows/README.md"]
        observed = {"worker:tools.ASpec": ["worker/src/main/", "worker/src/main/scala/", "worker/src/main/resources/",
                                           "worker/src/main/scala/A.scala"],
                    "worker:deploy.BSpec": [".github/workflows/", ".github/workflows/a.yml"]}
        self.assertEqual(T.suggest(observed, files), {
            "worker:tools.ASpec": ["worker/src/main/*.java", "worker/src/main/*.scala"],
            "worker:deploy.BSpec": [".github/workflows/*.yaml", ".github/workflows/*.yml"],
        })

    def test_files_read_by_name_in_one_directory_are_read_as_it(self):
        pages = [f"test/resources/fixtures/kinoteka/film/{n}" for n in range(6)]
        self.assertEqual(T.suggest({"scrapers:clients.ASpec": pages + ["data/x.json"]}, pages),
                         {"scrapers:clients.ASpec": ["data/x.json", "test/resources/fixtures/kinoteka/film/*"]})


if __name__ == "__main__":
    unittest.main()
