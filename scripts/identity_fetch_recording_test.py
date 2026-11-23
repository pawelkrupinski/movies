#!/usr/bin/env python3
"""scripts/identity-fetch-recording.sh, black-box, against a fake gh holding one recording run's assets and artifact:
the pair laid out (tree, overlay, corpus, fills), --corpus-only / --no-overlay, the archive release, a read that fails
once asked again, a hung gh cut off, a missing piece named, the usage. Ported from identity-fetch-recording-test.sh
case for case; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_fetch_recording.py", "scripts/identity-fetch-recording.sh")
# `release download` hands over the asset its --pattern names when the release holds it; `run download` copies the
# named artifact's files into --dir. Files in $STUB make it fail: `down` every release read, `blip` the next one,
# `run-blip` the next run download (leaving half a file).
GH = r'''cmd="$1 $2"; shift 2
tag=""; [ "$cmd" = "release download" ] && { tag="$1"; shift; }
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; --name) name="$2" ;; esac; shift; done
case "$cmd" in
  "release download")
    [ -f "$STUB/down" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
    [ -f "$STUB/blip" ] && { rm -f "$STUB/blip"; echo "HTTP 500: Internal Server Error" >&2; exit 1; }
    assets="$STUB/assets"; [ "$tag" = convergence-fixtures ] || assets="$STUB/assets-$tag"
    [ -f "$assets/$pattern" ] || { echo "no assets match the file pattern" >&2; exit 1; }
    mkdir -p "$dir"; cp "$assets/$pattern" "$dir/" ;;
  "run download")
    [ -f "$STUB/run-blip" ] && { rm -f "$STUB/run-blip"; mkdir -p "$dir"; echo half > "$dir/scrapes-uk.tar.gz"; echo "HTTP 502" >&2; exit 1; }
    [ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
    mkdir -p "$dir"; cp "$STUB/artifacts/$name/"* "$dir/" ;;
  *) exit 1 ;;
esac'''
FIXTURES = os.path.join("pair", "test", "resources", "fixtures")


class FetchRecordingTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.pack("assets/enrichment-uk-77.tar.zst", "enrichment-uk/api.themoviedb.org/search", "tree", "zstd")
        self.pack("assets/identity-overlay-uk-77.tar.gz", "enrichment-uk/identity/answers", "overlay", "gzip")
        self.pack("artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz", "corpus/cinema-scrapes-uk.json.gz", "corpus", "gzip")

    def pack(self, archive: str, path: str, content: str, compressor: str) -> None:
        """An archive of test/resources/fixtures/<path> holding `content`, packed by `compressor`."""
        with tempfile.TemporaryDirectory(dir=self.work) as source:
            self.write(os.path.relpath(os.path.join(source, "test/resources/fixtures", path), self.work), content + "\n")
            os.makedirs(os.path.dirname(self.path(archive)), exist_ok=True)
            subprocess.run(["bash", "-c", f'tar -cf - test | {compressor} -q -c > "$1"', "pack", self.path(archive)],
                           cwd=source, check=True)

    def fetch(self, *args: str, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        return self.run_tool(*args, env={"STUB": self.work, "GH_DOWNLOAD_BACKOFF": "0", **(env or {})})

    def laid(self, dest: str, path: str) -> Optional[str]:
        full = self.path(dest, FIXTURES, path)
        if not os.path.isfile(full):
            return None
        with open(full) as f:
            return f.read().strip()

    def test_a_runs_pair_is_laid_out_tree_overlay_and_corpus(self) -> None:
        done = self.fetch("77", self.path("dest"), "uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("tree", self.laid("dest", "enrichment-uk/api.themoviedb.org/search"))   # from zstd
        self.assertEqual("overlay", self.laid("dest", "enrichment-uk/identity/answers"))       # from gzip
        self.assertEqual("corpus", self.laid("dest", "corpus/cinema-scrapes-uk.json.gz"))
        self.assertIn("uk: ", done.stdout)
        self.assertIn("laid out so far", done.stdout)

    def test_corpus_only_lays_out_the_scrape_corpus_alone(self) -> None:
        done = self.fetch("--corpus-only", "79", self.path("corpus-only"), "uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("corpus", self.laid("corpus-only", "corpus/cinema-scrapes-uk.json.gz"))
        self.assertFalse(os.path.isdir(self.path("corpus-only", FIXTURES, "enrichment-uk")))

    def test_a_scrape_download_an_earlier_run_left_behind_is_fetched_again(self) -> None:
        self.write("dest/archive/scrape-uk/scrapes-uk.tar.gz", "junk\n")
        self.assertEqual(0, self.fetch("77", self.path("dest"), "uk").code)
        self.assertEqual("corpus", self.laid("dest", "corpus/cinema-scrapes-uk.json.gz"))

    def test_a_scrape_archive_without_the_countrys_corpus_file_lays_no_corpus_out(self) -> None:
        # identity_recorded reads cinema-scrapes-<cc>.json.gz as "laid out whole"
        self.pack("artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz", "corpus/other.json", "other", "gzip")
        done = self.fetch("--corpus-only", "80", self.path("no-corpus"), "uk")
        self.assertEqual(1, done.code, done.output)
        corpus = self.path("no-corpus", FIXTURES, "corpus")
        self.assertEqual([], os.listdir(corpus) if os.path.isdir(corpus) else [])
        self.assertEqual(1, done.output.count("holds no cinema-scrapes-uk.json.gz"))

    def test_a_gh_that_hangs_is_cut_off_and_the_fetch_fails(self) -> None:
        self.fakes.script("gh", "sleep 20")
        self.assertEqual(1, self.fetch("--corpus-only", "81", self.path("slow"), "uk",
                                       env={"KINOWO_IDENTITY_DOWNLOAD_TIMEOUT": "1"}).code)

    def test_no_overlay_lays_out_the_tree_without_the_identity_overlay(self) -> None:
        self.assertEqual(0, self.fetch("--no-overlay", "77", self.path("plain"), "uk").code)
        self.assertEqual("tree", self.laid("plain", "enrichment-uk/api.themoviedb.org/search"))
        self.assertIsNone(self.laid("plain", "enrichment-uk/identity/answers"))

    def test_a_run_without_an_overlay_is_laid_out_too(self) -> None:
        os.remove(self.path("assets/identity-overlay-uk-77.tar.gz"))
        self.assertEqual(0, self.fetch("77", self.path("no-overlay"), "uk").code)

    def test_a_run_the_release_holds_no_tree_for_fails_naming_it(self) -> None:
        done = self.fetch("78", self.path("missing"), "uk")
        self.assertEqual(1, done.code)
        self.assertEqual(1, done.output.count("holds enrichment-uk-78"))

    def test_a_release_that_cannot_be_read_fails_not_as_a_missing_tree(self) -> None:
        self.write("down", "")
        done = self.fetch("77", self.path("down-dest"), "uk")
        self.assertEqual(1, done.code)
        self.assertEqual(1, done.output.count("uk: could not read release convergence-fixtures"))

    def test_a_release_read_and_a_run_download_failing_once_are_asked_again(self) -> None:
        self.write("blip", "")
        self.write("run-blip", "")
        self.assertEqual(0, self.fetch("77", self.path("blip-dest"), "uk").code)
        self.assertEqual("tree", self.laid("blip-dest", "enrichment-uk/api.themoviedb.org/search"))
        self.assertEqual("corpus", self.laid("blip-dest", "corpus/cinema-scrapes-uk.json.gz"))

    def test_a_recording_only_the_archive_release_holds_is_laid_out_from_it(self) -> None:
        # github.com's 38713274617: its tree and its corpus from the archive, no run read
        self.pack("assets-identity-recordings/enrichment-uk-90.tar.zst", "enrichment-uk/api.themoviedb.org/search",
                  "archived-tree", "zstd")
        self.pack("assets-identity-recordings/scrapes-uk-90.tar.gz", "corpus/cinema-scrapes-uk.json.gz",
                  "archived-corpus", "gzip")
        done = self.fetch("--no-overlay", "90", self.path("archived"), "uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("archived-tree", self.laid("archived", "enrichment-uk/api.themoviedb.org/search"))
        self.assertEqual("archived-corpus", self.laid("archived", "corpus/cinema-scrapes-uk.json.gz"))
        self.assertEqual(1, done.output.count("uk: tree from the archive release identity-recordings"))

    def test_a_run_neither_release_holds_names_both(self) -> None:
        done = self.fetch("91", self.path("neither"), "uk")
        self.assertEqual(1, done.code)
        self.assertEqual(1, done.output.count(
            "neither release convergence-fixtures nor identity-recordings holds enrichment-uk-91"))

    def test_fills_lay_each_named_fill_of_the_country_over_its_tree(self) -> None:
        # a hermetic leg replays its pair WITH fills laid over the tree (convergence-fill.sh unpack)
        self.pack("assets/fill-uk-77-5-1-convergence.tar.zst", "enrichment-uk/api.themoviedb.org/search", "filled", "zstd")
        self.pack("assets/fill-de-77-5-1-convergence.tar.zst", "enrichment-de/api.themoviedb.org/search",
                  "other-country", "zstd")
        done = self.fetch("--no-overlay", "--fills", "fill-uk-77-5-1-convergence.tar.zst fill-de-77-5-1-convergence.tar.zst",
                          "77", self.path("filled"), "uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("filled", self.laid("filled", "enrichment-uk/api.themoviedb.org/search"))  # a later file wins
        self.assertFalse(os.path.exists(self.path("filled", FIXTURES, "enrichment-de")))  # another country's is not laid
        self.assertEqual(1, done.output.count("uk: laid fill fill-uk-77-5-1-convergence.tar.zst over the tree"))

    def test_a_named_fill_the_release_does_not_hold_fails_naming_it(self) -> None:
        done = self.fetch("--fills", "fill-uk-77-6-1-convergence.tar.zst", "77", self.path("fill-missing"), "uk")
        self.assertEqual(1, done.code)
        self.assertEqual(1, done.output.count("release convergence-fixtures holds no fill fill-uk-77-6-1-convergence.tar.zst"))

    def test_help_prints_the_usage_and_too_few_arguments_is_a_usage_error(self) -> None:
        done = self.fetch("--help")
        self.assertEqual(0, done.code)
        self.assertEqual(1, done.output.count("<recording run id> <dest dir>"))
        self.assertEqual(2, self.fetch("77").code)
        self.assertEqual(2, self.fetch("--fills").code)


if __name__ == "__main__":
    shutil.which("zstd") or sys.exit("needs zstd")
    unittest.main()
