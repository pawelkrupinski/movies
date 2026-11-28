#!/usr/bin/env python3
"""scripts/ci/archive-recording.sh, black-box, against a fake gh whose releases and run artifacts are directories: a
recording's trees and corpora are copied into the archive release under the names the readers ask for, a partial upload
replaced, an asset already there at its size skipped, a country the recording lacks skipped, and a missing corpus or an
empty recording failing. Ported from archive-recording-test.sh; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/archive_recording.py", "scripts/ci/archive-recording.sh")
GH = r"""
root="$STUB"
case "$1 $2" in
  "release view") [ -f "$root/view-fails-$3" ] && { echo "HTTP 502" >&2; exit 1; }
    for f in "$root/$3"/*; do [ -e "$f" ] && printf '%s\t%s\n' "$(basename "$f")" "$(wc -c < "$f" | tr -d ' ')"; done; exit 0 ;;
  "release download")
    while [ "$#" -gt 0 ]; do case "$1" in --pattern) p=$2 ;; --dir) d=$2 ;; esac; shift; done
    cp "$root/convergence-fixtures/$p" "$d/" ;;
  "release upload") echo "upload $(basename "$4")" >> "$root/uploads"; cp "$4" "$root/$3/" ;;
  "run download")
    run=$3
    while [ "$#" -gt 0 ]; do case "$1" in --name) n=$2 ;; --dir) d=$2 ;; esac; shift; done
    [ -d "$root/runs/$run/$n" ] || { echo "no artifact matches" >&2; exit 1; }
    cp -R "$root/runs/$run/$n/." "$d/" ;;
  *) exit 2 ;;
esac
"""


class ArchiveRecordingTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.write("stub/convergence-fixtures/enrichment-pl-528.tar.zst", "tree-pl\n")
        self.write("stub/convergence-fixtures/enrichment-de-528.tar.zst", "tree-de\n")
        self.write("stub/runs/528/scrape-fixtures-pl/scrapes-pl.tar.gz", "corpus-pl\n")
        self.write("stub/runs/528/scrape-fixtures-de/scrapes-de.tar.zst", "corpus-de\n")
        self.write("stub/identity-recordings/enrichment-de-528.tar.zst", "part\n")
        self.out = ""

    def archive(self, *runs: str) -> int:
        done = self.run_tool(*runs, env={"STUB": self.path("stub"), "GH_RELEASE_RETRY_DELAY": "0"})
        self.out = done.output
        return done.code

    def archived(self) -> str:
        return " ".join(sorted(os.listdir(self.path("stub/identity-recordings"))))

    def uploads(self) -> str:
        return self.read("stub/uploads") if os.path.exists(self.path("stub/uploads")) else ""

    def test_a_recordings_trees_and_corpora_are_archived_under_the_readers_names_a_partial_upload_replaced(self) -> None:
        self.assertEqual(0, self.archive("528"), self.out)
        self.assertEqual("enrichment-de-528.tar.zst enrichment-pl-528.tar.zst scrapes-de-528.tar.zst "
                         "scrapes-pl-528.tar.gz", self.archived())
        self.assertEqual("tree-de\n", self.read("stub/identity-recordings/enrichment-de-528.tar.zst"))
        self.assertIn("recording 528\n", self.out)
        self.assertIn("  enrichment-pl-528.tar.zst: archived (8 bytes)", self.out)
        self.assertIn("  scrapes-pl-528.tar.gz: archived (10 bytes)", self.out)
        self.assertIn("  uk: convergence-fixtures holds no enrichment-uk-528 -- not a recording of uk (or already "
                      "pruned): skipped", self.out)

    def test_a_second_run_uploads_nothing(self) -> None:
        self.assertEqual(0, self.archive("528"))
        os.remove(self.path("stub/uploads"))
        self.assertEqual(0, self.archive("528"), self.out)
        self.assertEqual("", self.uploads())
        self.assertIn("  enrichment-pl-528.tar.zst: archived already", self.out)
        self.assertIn("  scrapes-pl-528: archived already", self.out)

    def test_a_tree_only_in_the_archive_counts(self) -> None:
        self.assertEqual(0, self.archive("528"))
        os.remove(self.path("stub/convergence-fixtures/enrichment-pl-528.tar.zst"))
        os.remove(self.path("stub/convergence-fixtures/enrichment-de-528.tar.zst"))
        self.assertEqual(0, self.archive("528"), self.out)
        self.assertIn("  enrichment-pl-528: only in identity-recordings already", self.out)

    def test_a_tree_whose_corpus_artifact_is_gone_fails_naming_it(self) -> None:
        self.assertEqual(0, self.archive("528"))
        os.remove(self.path("stub/identity-recordings/scrapes-pl-528.tar.gz"))
        os.remove(self.path("stub/runs/528/scrape-fixtures-pl/scrapes-pl.tar.gz"))
        os.rmdir(self.path("stub/runs/528/scrape-fixtures-pl"))
        self.assertEqual(1, self.archive("528"))
        self.assertIn("::error::recording 528 holds a pl tree but its scrape-fixtures-pl artifact could not be read",
                      self.out)

    def test_an_artifact_without_its_corpus_fails(self) -> None:
        os.remove(self.path("stub/runs/528/scrape-fixtures-pl/scrapes-pl.tar.gz"))
        self.assertEqual(1, self.archive("528"))
        self.assertIn("::error::528's scrape-fixtures-pl holds no scrapes-pl.tar.*", self.out)

    def test_a_run_holding_nothing_fails(self) -> None:
        self.assertEqual(1, self.archive("999"))
        self.assertIn("::error::recording 999: nothing archived (no tree in convergence-fixtures or "
                      "identity-recordings)", self.out)

    def test_what_is_no_run_id_fails_and_the_rest_still_run(self) -> None:
        self.assertEqual(1, self.archive("abc", "528"))
        self.assertIn("::error::not a run id: abc", self.out)
        self.assertIn("scrapes-de-528.tar.zst", self.archived())

    def test_an_unreadable_release_fails(self) -> None:
        self.write("stub/view-fails-identity-recordings", "")
        self.assertEqual(1, self.archive("528"))
        self.assertIn("::error::could not read release identity-recordings", self.out)

    def test_no_run_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.archive())
        self.assertIn("usage: ", self.out)


if __name__ == "__main__":
    unittest.main()
