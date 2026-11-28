#!/usr/bin/env python3
"""scripts/restore-country-corpus.sh, black-box: a country's pinned recorded pair -- the corpus artifact of its
recording run and the enrichment tree with its fills -- laid into a scratch RESTORE_ROOT, against a fake `gh` serving
a fake release and recording run; no pin, an expired corpus, a malformed pin, a failed read and an unknown country
each fail loudly. Ported from restore-country-corpus-test.sh case for case; the same cases hold the shell tool and its
Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import gzip
import os
import re
import subprocess
import sys
import tarfile
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/restore_country_corpus.py", "scripts/restore-country-corpus.sh")
FIXTURES = "out/test/resources/fixtures"
# gh: release assets by pattern, a run's artifact by name and the release's asset listing (for the fills) from
# $STUB_RELEASE and $STUB_RUNS; anything absent answers as gh does, and $GH_FAILS ($GH_RUN_FAILS) fails every call (every run
# download) as an outage would
GH = r'''
import fnmatch, glob, shutil
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
if env.get("GH_FAILS"):
    err(env["GH_FAILS"]); exit(1)
verb = " ".join(argv[:2])
if verb == "release view":
    out("\n".join(sorted(os.listdir(env["STUB_RELEASE"])))); exit(0)
if verb == "release download":
    matches = [f for f in sorted(os.listdir(env["STUB_RELEASE"])) if fnmatch.fnmatch(f, flag("--pattern"))]
    if not matches:
        err("no assets match the file pattern"); exit(1)
    os.makedirs(flag("--dir"), exist_ok=True)
    for f in matches:
        shutil.copy(os.path.join(env["STUB_RELEASE"], f), flag("--dir"))
    exit(0)
if verb == "run download":
    if env.get("GH_RUN_FAILS"):
        err(env["GH_RUN_FAILS"]); exit(1)
    src = os.path.join(env["STUB_RUNS"], argv[2], flag("--name"))
    if not os.path.isdir(src):
        err("no artifact matches any of the names or patterns provided"); exit(1)
    os.makedirs(flag("--dir"), exist_ok=True)
    for f in glob.glob(os.path.join(src, "*")):
        shutil.copy(f, flag("--dir"))
    exit(0)
err("unexpected gh " + " ".join(argv)); exit(2)
'''


class RestoreCountryCorpusTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)
        os.makedirs(self.path("release"))
        os.makedirs(self.path("runs"))
        # A recorded pair for `de`: the corpus artifact of run 42, the tree asset, one fill.
        self.write("src/test/resources/fixtures/corpus/prod-coverage-de.json", "{}\n")
        with gzip.open(self.path("src/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz"), "wt") as f:
            f.write("corpus\n")
        self.write("src/test/resources/fixtures/enrichment-de/tmdb/one.json", "answer\n")
        self.tar("src", "test/resources/fixtures/corpus", "runs/42/scrape-fixtures-de/scrapes-de.tar.gz")
        self.tar("src", "test/resources/fixtures/enrichment-de", "release/enrichment-de-42.tar.gz")
        self.write("fill/test/resources/fixtures/enrichment-de/tmdb/two.json", "filled\n")
        with open(self.path("release/fill-de-42-77-1-convergence.tar.zst"), "wb") as sink:
            tar = subprocess.Popen(["tar", "-C", self.path("fill"), "-cf", "-", "test"], stdout=subprocess.PIPE)
            subprocess.run(["zstd", "-q", "-c"], stdin=tar.stdout, stdout=sink, check=True)
            assert tar.stdout is not None
            tar.stdout.close()
            self.assertEqual(0, tar.wait())
        self.pin("de", "42 2026-10-11T02:00:00Z enrichment-de-42.tar.gz")
        self.out = self.err = ""

    def tar(self, base: str, member: str, archive: str) -> None:
        os.makedirs(os.path.dirname(self.path(archive)), exist_ok=True)
        with tarfile.open(self.path(archive), "w:gz") as t:
            t.add(self.path(base, member), arcname=member)

    def pin(self, code: str, line: str) -> None:
        self.write(f"release/hermetic-{code}.txt", line + "\n")

    def restore(self, *args: str, **env: str) -> int:
        """The tool into $WORK/out, a stale local tree there first: its exit; its stdout and stderr in self.out,
        self.err."""
        self.write(f"{FIXTURES}/enrichment-{args[0] if args else 'de'}/stale.json", "stale\n")
        done = self.run_tool(*args, env={"STUB_RELEASE": self.path("release"), "STUB_RUNS": self.path("runs"),
                                         "RESTORE_ROOT": self.path("out"), "CONVERGENCE_FILL_LIST_BACKOFF": "0",
                                         "GH_DOWNLOAD_BACKOFF": "0", "GH_DOWNLOAD_ATTEMPTS": "1", **env})
        self.out, self.err = done.stdout, done.stderr
        return done.code

    def last_error(self) -> str:
        lines = [line for line in self.err.split("\n") if line]
        return lines[-1] if lines else ""

    def restored(self, relative: str) -> str:
        full = self.path(FIXTURES, relative)
        return self.read(os.path.join(FIXTURES, relative)) if os.path.exists(full) else "absent"

    def test_the_pinned_pair_restores(self) -> None:
        self.assertEqual(0, self.restore("de"), self.err)
        with gzip.open(self.path(FIXTURES, "corpus/cinema-scrapes-de.json.gz"), "rt") as f:
            self.assertEqual("corpus\n", f.read(), "the corpus lands where CorpusFixture looks")
        self.assertEqual("answer\n", self.restored("enrichment-de/tmdb/one.json"), "the tree lands as the fixtures")
        self.assertEqual("filled\n", self.restored("enrichment-de/tmdb/two.json"), "the pair's fill is laid over it")
        self.assertEqual("absent", self.restored("enrichment-de/stale.json"), "a local tree is replaced, not merged")

    def test_what_it_says_of_the_pair(self) -> None:
        """Its own three lines, restore-enrichment-tree.sh's between them"""
        self.assertEqual(0, self.restore("de"), self.err)
        lines = self.out.rstrip("\n").split("\n")
        self.assertEqual("==> de: corpus from recording run 42 (2026-10-11T02:00:00Z), tree enrichment-de-42.tar.gz",
                         lines[0])
        self.assertRegex(lines[1], r"^    corpus: \S+ test/resources/fixtures/corpus/cinema-scrapes-de\.json\.gz$")
        self.assertEqual("    tree:   2 files in test/resources/fixtures/enrichment-de "
                         "(fills: fill-de-42-77-1-convergence.tar.zst)", lines[-1])

    def test_no_pinned_pair_fails_naming_the_recorder(self) -> None:
        self.assertEqual(1, self.restore("uk"))
        self.assertIn("no recorded pair pinned for uk (hermetic-uk.txt in release convergence-fixtures)",
                      self.last_error())
        self.assertIn('Record one: gh workflow run "Record scrape fixtures" --ref main', self.last_error())
        self.assertEqual("", self.out)

    def test_an_expired_corpus_fails_rather_than_replaying_anything_else(self) -> None:
        self.pin("es", "43 2026-10-11T02:00:00Z enrichment-es-43.tar.gz")
        self.assertEqual(1, self.restore("es"))
        self.assertIn("recording run 43's scrape-fixtures-es artifact is gone (14-day retention)", self.last_error())
        self.assertFalse(os.path.exists(self.path(FIXTURES, "corpus")), "nothing unpacked")

    def test_a_pin_without_a_tree_fails_as_malformed(self) -> None:
        self.pin("us", "44 2026-10-11T02:00:00Z")
        self.assertEqual(1, self.restore("us"))
        self.assertEqual("restore-country-corpus: hermetic-us.txt is malformed", self.last_error())

    def test_a_failed_read_is_no_absent_pin(self) -> None:
        self.assertEqual(1, self.restore("de", GH_FAILS="HTTP 401: Bad credentials"))
        self.assertEqual("restore-country-corpus: could not read release convergence-fixtures for hermetic-de.txt "
                         "(gh auth?)", self.last_error())

    def test_a_failed_corpus_download_is_said(self) -> None:
        self.assertEqual(1, self.restore("de", GH_RUN_FAILS="HTTP 401: Bad credentials"))
        self.assertEqual("restore-country-corpus: could not download scrape-fixtures-de from run 42", self.last_error())

    def test_an_artifact_holding_no_archive_is_gone(self) -> None:
        os.remove(self.path("runs/42/scrape-fixtures-de/scrapes-de.tar.gz"))
        self.write("runs/42/scrape-fixtures-de/other.txt", "x\n")
        self.assertEqual(1, self.restore("de"))
        self.assertIn("recording run 42's scrape-fixtures-de artifact is gone", self.last_error())

    def test_all_restores_every_country_in_order_stopping_at_the_first_failure(self) -> None:
        self.assertEqual(1, self.restore("all"))
        self.assertIn("no recorded pair pinned for pl", self.last_error())
        self.assertEqual("", self.out, "pl comes first")

    def test_the_release_tag_is_overridable(self) -> None:
        self.assertEqual(1, self.restore("uk", FIXTURE_RELEASE_TAG="other-tag"))
        self.assertIn("(hermetic-uk.txt in release other-tag)", self.last_error())

    def test_an_unknown_country_or_none_is_a_usage_error(self) -> None:
        for args in (["xx"], []):
            self.assertEqual(64, self.restore(*args), args)
            self.assertRegex(self.last_error(), r"^usage: \S*restore.country.corpus\S* <pl\|de\|uk\|es\|us\|all>$")
            self.assertEqual([], self.fakes.calls("gh"), "nothing read")


if __name__ == "__main__":
    unittest.main()
