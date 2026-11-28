#!/usr/bin/env python3
"""scripts/identity-repro, black-box: a production listing to a verdict -- found in the recording's corpus (case and
accents aside) and sliced, or why not -- over a fabricated recording in a scratch cache and a fake identity-slice whose
output is set; --wait waits for a newer recording in flight (its id and ETA said, then agentctl wait's backoff) before
reproducing on prod-latest. Ported from identity-repro-test.sh case for case; the same cases hold the shell tool and
its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import gzip
import json
import os
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_repro.py", "scripts/identity-repro")
FIXTURES = "cache/recordings/42/pair/test/resources/fixtures"
CORPUS = [
    {"_id": "Cinema City Arkadia", "scrapedAt": "2026-10-23T00:10:00Z",
     "films": [{"movie": {"title": "Świąteczna zabawa z Peppą"}}, {"movie": {"title": "Rose"}}]},
    {"_id": "Cinema City Bonarka", "scrapedAt": "2026-10-23T00:11:00Z",
     "films": [{"movie": {"title": "Świąteczna zabawa z Peppą"}}]},
    {"_id": "Kino Muranów", "scrapedAt": "2026-10-23T00:12:00Z",
     "films": [{"movie": {"title": "Rose"}}, {"movie": {"title": "Rosemary's Baby"}}]}]
# identity-slice: its arguments noted, $SLICE_OUT printed (escapes read), exit $SLICE_EXIT
SLICE = '''printf '%s\\n' "$@" > "$WORK/args"
printf '%b' "${SLICE_OUT:-}"
exit "${SLICE_EXIT:-0}"'''
# gh: prod-latest names recording 42; `run list` answers $RUNS, `run view` in progress for $PENDING reads then $CONCL
# (each through the caller's --jq, as gh applies it)
GH = r'''
import subprocess
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
def answer(text):
    if not flag("--jq"):
        out(text); exit(0)
    done = subprocess.run(["jq", "-r", flag("--jq")], input=text, capture_output=True, text=True)
    sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
verb = " ".join(argv[:2])
if verb == "release download":
    with open(os.path.join(flag("--dir"), "prod-latest-pl.txt"), "w") as f:
        f.write("42 2026-10-23T00:30:00Z\n")
    exit(0)
if verb == "run list":
    answer(env.get("RUNS") or "[]")
if verb == "run view":
    views = os.path.join(env["WORK"], "views")
    n = (int(open(views).read()) if os.path.exists(views) else 0) + 1
    with open(views, "w") as f:
        f.write(str(n))
    pending = n <= int(env.get("PENDING") or 0)
    answer(json.dumps({"status": "in_progress" if pending else "completed",
                       "conclusion": "" if pending else env.get("CONCL") or "success"}))
exit(1)
'''


def rec(run: int, status: str, conclusion: str, ended: str) -> Dict[str, object]:
    return {"databaseId": run, "status": status, "conclusion": conclusion, "headBranch": "main",
            "createdAt": "2026-10-23T00:00:00Z", "updatedAt": f"2026-10-23T{ended}:00Z"}


class IdentityReproTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        os.makedirs(self.path(FIXTURES, "enrichment-pl"))
        os.makedirs(self.path(FIXTURES, "corpus"))
        with gzip.open(self.path(FIXTURES, "corpus", "cinema-scrapes-pl.json.gz"), "wt", encoding="utf-8") as f:
            f.write(json.dumps(CORPUS, ensure_ascii=False) + "\n")
        self.write(f"{FIXTURES}/corpus/prod-coverage-pl.json", '{"recordedAt": "2026-10-23T00:30:00Z"}\n')
        self.slice = self.fakes.script("slice", SLICE, shell="/usr/bin/env bash")
        self.gh = self.fakes.program("gh", GH)
        self.fakes.script("sleep", 'echo "$1" >> "$WORK/slept"')
        self.out = self.err = ""

    def repro(self, *args: str, **env: str) -> int:
        """The tool over recording 42, no mirror: its exit; its stdout and stderr in self.out, self.err."""
        return self.invoke(*args, "--recording", "42", "--no-mirror", **env)

    def invoke(self, *args: str, **env: str) -> int:
        for name in ("args", "views", "slept"):
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))
        done = self.run_tool(*args, env={"WORK": self.work, "KINOWO_IDENTITY_CACHE": self.path("cache"),
                                         "IDENTITY_REPRO_SLICE": self.slice, "KINOWO_GH": self.gh, **env})
        self.out, self.err = done.stdout, done.stderr
        return done.code

    def verdict(self) -> str:
        return self.out.split(":")[0].split("\n")[0]

    def sliced(self) -> str:
        """identity-slice's arguments, a space between ("" when it was not run)."""
        return " ".join(self.read("args").split("\n")).strip() if os.path.exists(self.path("args")) else ""

    def test_a_listing_the_corpus_holds_is_sliced_by_its_exact_key(self) -> None:
        self.assertEqual(0, self.repro("pl", "arkadia", "swiateczna ZABAWA"), self.err)
        self.assertEqual("pl --key Cinema City Arkadia|Świąteczna zabawa z Peppą --recording 42 --emit-case",
                         self.sliced())
        self.assertEqual("REPRODUCIBLE", self.verdict(), "it asked nothing the recording lacked")
        self.assertEqual(0, self.repro("pl", "arkadia", "rose", "--no-emit"), self.err)
        self.assertEqual("pl --key Cinema City Arkadia|Rose --recording 42", self.sliced(), "--no-emit pins no case")

    def test_what_cannot_be_sliced(self) -> None:
        self.assertEqual((2, "AMBIGUOUS", ""), (self.repro("pl", "muranow", "Ros"), self.verdict(), self.sliced()))
        self.assertEqual((3, "NOT-IN-RECORDING"), (self.repro("pl", "Kino Lalka", "Rose"), self.verdict()))
        # ...and so is a title its venue does not list, naming the venue's scrape
        self.assertEqual(3, self.repro("pl", "Bonarka", "Rose"))
        self.assertIn("Cinema City Bonarka scraped 2026-10-23T00:11:00Z", self.out)
        self.assertEqual(2, self.repro("xx", "arkadia", "zabawa"), "an unknown country")

    def test_what_the_slice_said(self) -> None:
        self.assertEqual((4, "NEEDS-JUDGEMENT"), (self.repro(
            "pl", "arkadia", "zabawa", SLICE_OUT="--emit-case REFUSED for x: judge them first\\n"), self.verdict()))
        self.assertEqual((5, "NEEDS-ANSWERS"), (self.repro(
            "pl", "arkadia", "zabawa", SLICE_OUT="UNANSWERED by the recording (read as unknown): tmdb 1778515\\n"),
            self.verdict()))
        self.assertIn("tmdb 1778515", self.out)
        # a slice taking what prod serves REPRODUCES it, its gaps named
        self.assertEqual((0, "REPRODUCIBLE"), (self.repro("pl", "arkadia", "zabawa", SLICE_OUT=(
            "-- prod's stored state (mirror):\\nUNANSWERED by the recording (read as unknown): imdb x\\n")),
            self.verdict()))
        self.assertIn("imdb x", self.out)
        self.repro("pl", "arkadia", "zabawa", SLICE_OUT=(
            "-- prod's stored state (mirror):\\nPROD TAKES OTHERWISE for 1 of 1: Cinema City Arkadia | Rose: slice "
            "none, prod tmdb:7\\n"))
        self.assertIn("slice none, prod tmdb:7", self.out, "prod taking otherwise is named beside the verdict")
        self.assertEqual((1, "FAILED"), (self.repro("pl", "arkadia", "zabawa", SLICE_EXIT="1"), self.verdict()))

    def wait_repro(self, runs: List[Dict[str, object]], *args: str, **env: str) -> int:
        return self.invoke("pl", "arkadia", "zabawa", "--no-mirror", "--wait", *args, RUNS=json.dumps(runs), **env)

    def slept(self) -> str:
        return " ".join(self.read("slept").split()) if os.path.exists(self.path("slept")) else ""

    def test_wait_for_a_newer_recording_in_flight(self) -> None:
        runs = [rec(50, "in_progress", "", "00:00"), rec(41, "completed", "success", "01:00"),
                rec(40, "completed", "failure", "01:20")]
        self.assertEqual(0, self.wait_repro(runs, PENDING="2"), self.err)
        self.assertRegex(self.err, "recording run 50 in flight \\(newer than prod-latest.s 42\\): ETA ~80 min "
                                   "\\(median of 2 recent runs\\)")
        self.assertIn("recording run 50 finished", self.err)
        self.assertEqual("60 90", self.slept(), "the backoff")
        # ...then reproduces on prod-latest
        self.assertEqual(("REPRODUCIBLE", "pl --key Cinema City Arkadia|Świąteczna zabawa z Peppą --recording 42 "
                                          "--emit-case"), (self.verdict(), self.sliced()))

    def test_wait_when_the_recording_fails_or_none_is_newer(self) -> None:
        self.assertEqual(0, self.wait_repro([rec(50, "in_progress", "", "00:00")], CONCL="failure"), self.err)
        self.assertRegex(self.err, "recording run 50 ended failure: reproducing on prod-latest.s 42")
        self.assertEqual(0, self.wait_repro([rec(41, "in_progress", "", "00:00")]), self.err)
        self.assertRegex(self.err, "no recording newer than prod-latest.s \\(run 42\\) in flight")
        self.assertEqual("", self.slept(), "no wait")
        self.assertEqual(2, self.wait_repro([], "--recording", "42"), "--wait with a named recording")


if __name__ == "__main__":
    unittest.main()
