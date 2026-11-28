#!/usr/bin/env python3
"""scripts/ci/convergence-preflight.sh, black-box: a missing pin, pinned tree, named recording or its corpus fails the
run in seconds; a gh read that fails is no evidence and passes. gh is a fake answering from files under $STUB.
Ported from convergence-preflight-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/convergence_preflight.py", "scripts/ci/convergence-preflight.sh")
GH = r"""
case "$1 $2" in
    "release view") [ -f "$STUB/release-fails" ] && { echo "HTTP 502" >&2; exit 1; }
        [ "$3" = identity-recordings ] && [ -f "$STUB/archive-fails" ] && { echo "HTTP 504" >&2; exit 1; }
        [ "$3" = identity-recordings ] && [ -f "$STUB/archived" ] && { cat "$STUB/archived"; exit 0; }
        cat "$STUB/assets" ;;
    "release download")
        [ -f "$STUB/download-fails" ] && { echo "HTTP 500" >&2; exit 1; }
        dir=""; while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
        mkdir -p "$dir" && cp "$STUB"/pins/* "$dir"/ 2>/dev/null; exit 0 ;;
    "api "*)
        run=$(echo "$2" | sed -E 's|.*/runs/([0-9]+)/artifacts.*|\1|')
        [ -f "$STUB/api-fails" ] && { echo "HTTP 503" >&2; exit 1; }
        [ -f "$STUB/runs/$run" ] || { echo "gh: Not Found (HTTP 404)" >&2; exit 1; }
        cat "$STUB/runs/$run" ;;
esac
"""


class ConvergencePreflightTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.write("stub/assets", "".join(f"{a}\n" for a in (
            "hermetic-pl.txt", "hermetic-de.txt", "enrichment-pl-573.tar.zst", "enrichment-de-573.tar.zst",
            "enrichment-pl-528.tar.zst")))
        self.pin("pl", "573 2026-10-20T13:35:26Z enrichment-pl-573.tar.zst")
        self.pin("de", "573 2026-10-20T13:35:31Z enrichment-de-573.tar.*")
        self.write("stub/runs/573", "scrape-fixtures-pl\nscrape-fixtures-de\n")
        self.write("stub/runs/528", "scrape-fixtures-pl\n")
        self.out = ""

    def pin(self, code: str, line: str) -> None:
        self.write(f"stub/pins/hermetic-{code}.txt", line + "\n")

    def preflight(self, *args: str) -> int:
        done = self.run_tool(*args, env={"STUB": self.path("stub"), "GH_REPO": "o/r",
                                          "FIXTURE_RELEASE_TAG": "convergence-fixtures"})
        self.out = done.output
        return done.code

    def said(self, text: str) -> int:
        return sum(1 for line in self.out.split("\n") if text in line)

    def test_every_pinned_pair_there_passes_saying_so(self) -> None:
        self.assertEqual(0, self.preflight("pl,de"), self.out)
        self.assertIn("every leg's pair is there (countries pl,de)", self.out)
        self.assertEqual(["release", "view", "convergence-fixtures", "--json", "assets", "--jq", ".assets[].name"],
                         self.fakes.calls("gh")[0])
        self.assertEqual(["release", "view", "identity-recordings", "--json", "assets", "--jq", ".assets[].name"],
                         self.fakes.calls("gh")[1])

    def test_a_named_recording_with_its_tree_and_corpus_passes(self) -> None:
        self.assertEqual(0, self.preflight("pl", "528"), self.out)
        self.assertIn("every leg's pair is there (recording 528, countries pl)", self.out)
        self.assertEqual(["api", "repos/o/r/actions/runs/528/artifacts?per_page=100", "--jq",
                          ".artifacts[] | select(.expired != true) | .name"], self.fakes.calls("gh")[-1])

    def test_a_country_with_no_pin_fails_saying_so_as_the_leg_would(self) -> None:
        self.assertEqual(1, self.preflight("pl,us"))
        self.assertEqual(1, self.said("::error::No recorded pair pinned for us (release asset hermetic-us.txt)"))
        self.assertIn("1 problem(s): no leg would replay its pair, so none starts", self.out)

    def test_a_pinned_tree_the_release_no_longer_keeps_fails_naming_it(self) -> None:
        self.pin("de", "573 2026-10-20T13:35:31Z enrichment-de-500.tar.zst")
        self.assertEqual(1, self.preflight("de"))
        self.assertEqual(1, self.said("::error::de: the pinned tree enrichment-de-500.tar.zst is not in release "
                                      "convergence-fixtures"))

    def test_a_malformed_pin_fails(self) -> None:
        self.pin("de", "573")
        self.assertEqual(1, self.preflight("de"))
        self.assertEqual(1, self.said("::error::hermetic-de.txt is malformed: '573'"))

    def test_a_recording_this_forge_never_held_fails(self) -> None:
        self.assertEqual(1, self.preflight("pl", "38713274617"))
        self.assertEqual(1, self.said("recording run 38713274617 is not on this forge"))

    def test_a_recording_without_that_countrys_corpus_fails_naming_tree_and_corpus(self) -> None:
        self.assertEqual(1, self.preflight("de", "528"))
        self.assertEqual(1, self.said("release convergence-fixtures holds no enrichment-de-528.tar.*"))
        self.assertEqual(1, self.said("recording run 528 holds no scrape-fixtures-de"))
        self.assertIn("2 problem(s)", self.out)

    def test_a_recording_whose_tree_and_corpus_the_archive_release_keeps_passes(self) -> None:
        self.write("stub/archived", "enrichment-de-528.tar.zst\nscrapes-de-528.tar.gz\n")
        self.assertEqual(0, self.preflight("de", "528"), self.out)
        self.assertEqual([], [c for c in self.fakes.calls("gh") if c[0] == "api"])

    def test_an_unreadable_archive_is_no_evidence_an_archived_recording_is_gone(self) -> None:
        # Identity model convergence #1505: the archive read failed, the archived run 573 is no run of this forge's
        # API, and five legs whose pair the archive held were refused.
        self.write("stub/archive-fails", "")
        os.remove(self.path("stub/runs/573"))
        self.assertEqual(0, self.preflight("pl,de"), self.out)
        self.assertEqual(1, self.said("::warning::could not read release identity-recordings (HTTP 504"))
        self.assertEqual(1, self.said("::warning::recording run 573 is no run of this forge's and the archive was not "
                                      "read -- not checked"))

    def test_the_artifacts_of_a_run_are_listed_once_for_every_country(self) -> None:
        self.assertEqual(0, self.preflight("pl,de"), self.out)
        self.assertEqual(1, len([c for c in self.fakes.calls("gh") if c[0] == "api"]))

    def test_an_unreadable_artifacts_list_is_no_evidence_it_passes_warning_once(self) -> None:
        self.write("stub/api-fails", "")
        self.assertEqual(0, self.preflight("pl,de"), self.out)
        self.assertEqual(1, self.said("::warning::could not list recording run 573's artifacts (HTTP 503"))

    def test_an_unreadable_release_is_no_evidence_it_passes(self) -> None:
        self.write("stub/release-fails", "")
        self.assertEqual(0, self.preflight("pl,us"), self.out)
        self.assertEqual(1, self.said("::warning::could not read release convergence-fixtures (HTTP 502"))

    def test_unreadable_pins_are_no_evidence_they_pass(self) -> None:
        self.write("stub/download-fails", "")
        self.assertEqual(0, self.preflight("pl,us"), self.out)
        self.assertEqual(1, self.said("::warning::could not download the pins (HTTP 500"))

    def test_empty_codes_are_skipped(self) -> None:
        self.assertEqual(0, self.preflight("pl,,de"), self.out)

    def test_no_codes_is_refused(self) -> None:
        self.assertNotEqual(0, self.preflight())
        self.assertIn("usage: convergence-preflight.sh <codes> [recording run]", self.out)
        self.assertEqual([], self.fakes.calls("gh"))


if __name__ == "__main__":
    unittest.main()
