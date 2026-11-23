#!/usr/bin/env python3
"""scripts/identity-ratchet-report, black-box, against a fake `gh`: a run's identity-ratchet artifact (the table, its
counts, --explain, --tsv, --status, --country, --limit), the cached download a second read uses, a PR read through the
run its body names, an older run whose findings only its pull-request job's ANSI-coloured log holds, a download that
fails (not an absent artifact), a gh that hangs (cut off by its timeout, nothing left running), a findings file and
--local read as they are, and the usage errors. Ported from identity-ratchet-report-test.sh case for case, plus the
filters and sources it left out; the same cases hold the tool across its rewrite."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = os.path.join(cli_harness.ROOT, "scripts", "identity-ratchet-report")
TAB = "\t"
TAKES = "\n".join([
    TAB.join(["status", "country", "venue", "rawTitle", "film", "title", "basis", "ids", "was", "explanation"]),
    TAB.join(["UNJUDGED", "de", "CineStar Augsburg", "Herr der Ringe (ext.)", "tmdb:1434460", "Herr der Ringe (2025)",
              "Filled", "tmdb:1434460", "", "filled by venues.current ¶ guards passed: bill.several"]),
    TAB.join(["WRONG", "pl", "Janosik", "Binti", "tmdb:855156", "Binti (2021)", "Agreed", "tmdb:855156", "", ""]),
    TAB.join(["RIGHT", "uk", "Act One", "Carmen", "tmdb:1", "Carmen (2026)", "Broadcast", "tmdb:1", "", ""]),
]) + "\n"
PREFIX = "pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z "
ESC = "\x1b"
OLD_LOG = "".join(PREFIX + line + "\n" for line in [
    f"{ESC}[0m[{ESC}[0minfo{ESC}[0m] {ESC}[31m  WRONG takes:{ESC}[0m",
    "^[[0m[info] ^[[31m  pl\tJanosik\tedukacja MH: Binti\ttmdb:855156\tBinti (2021)^[[0m",
    '[info]   List(Take("pl", ...))',
    "[info]   UNJUDGED takes:",
    "[info]   uk\tAct One\tTHE COMIC STRIP\ttmdb:396598\tThe Comic Strip (1981)\tFilled\ttmdb:396598,imdb:tt0478854",
    "[info] - should keep every right take *** FAILED ***",
    "[info]   right takes LOST:",
    "[info]   (de,Brotfabrik,Jeder Acker, jede Fabrik,wikidata:Q141180912)",
    '[info]   HashSet(("uk", ...))',
])
GH = r'''case "$1 $2" in
  "run download") run=$3; while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; esac; shift; done
                  [ -z "${GH_DOWN:-}" ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
                  [ -z "${GH_HANGS:-}" ] || { echo $$ >> "$GH_HANGS"; exec sleep 86400; }
                  [ -d "$STUB/artifacts/$run" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
                  cp "$STUB/artifacts/$run/"* "$dir/" ;;
  "pr view")      echo '{"body":"The weekly recapture (identity-capture.yml, run 111111).\n\nTo land: judge"}' ;;
  "run view")     case " $* " in
                    *" --log "*) cat "$STUB/old.log" ;;
                    *) echo '{"jobs":[{"name":"capture (de)","databaseId":1},{"name":"pull-request","databaseId":42}]}' ;;
                  esac ;;
  *) exit 1 ;;
esac'''


class IdentityRatchetReportTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.write("artifacts/111111/ratchet-takes.tsv", TAKES)
        self.write("old.log", OLD_LOG)

    def report(self, *args: str, **env: str) -> cli_harness.Done:
        return self.run_tool(*args, env={"STUB": self.work, "KINOWO_IDENTITY_CACHE": self.path("cache"), **env})

    def gh_calls(self, verb: str) -> List[List[str]]:
        return [argv for argv in self.fakes.calls("gh") if " ".join(argv[:2]) == verb]

    def test_a_runs_artifact_is_printed_counting_every_status_listing_the_default_ones(self) -> None:
        done = self.report("111111")
        self.assertEqual(0, done.code, done.output)
        lines = done.output.splitlines()
        self.assertEqual(1, lines.count("findings: WRONG 1, UNJUDGED 1, RIGHT 1"), done.output)
        self.assertNotIn("Carmen", done.output)
        self.assertEqual(1, sum(1 for line in lines if re.search(r"CineStar Augsburg .*tmdb:1434460 .*Filled", line)),
                         done.output)

    def test_the_table_lists_each_status_in_order_with_its_count_and_takes_cut_to_their_columns(self) -> None:
        self.write("long.tsv", TAKES.splitlines()[0] + "\n" + TAB.join(
            ["MOVED", "uk", "V" * 40, "T" * 50, "tmdb:2", "Title", "Agreed", "tmdb:2", "tmdb:3", ""]) + "\n")
        done = self.report(self.path("long.tsv"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["findings: MOVED 1", "", "MOVED (1)",
                          f"  uk  {'V' * 33}…  {'T' * 45}…  {'tmdb:2 (was tmdb:3)':<30}  {'Title':<40}  Agreed"],
                         done.stdout.splitlines())

    def test_explain_prints_the_explanation_under_its_take(self) -> None:
        done = self.report("111111", "--explain")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.output.splitlines().count("        guards passed: bill.several"), done.output)
        self.assertEqual(1, done.output.splitlines().count("        filled by venues.current"), done.output)

    def test_tsv_with_status_all_prints_every_finding_as_the_artifacts_tsv(self) -> None:
        done = self.report("111111", "--status", "ALL", "--tsv")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(4, len(done.output.splitlines()), done.output)
        self.assertEqual(TAKES.splitlines()[0], done.stdout.splitlines()[0])

    def test_country_and_limit_narrow_the_findings(self) -> None:
        done = self.report("111111", "--status", "ALL", "--country", "pl", "--tsv")
        self.assertEqual(["Janosik"], [line.split(TAB)[2] for line in done.stdout.splitlines()[1:]], done.output)
        done = self.report("111111", "--status", "wrong,unjudged", "--limit", "1", "--tsv")
        self.assertEqual(["CineStar Augsburg"], [line.split(TAB)[2] for line in done.stdout.splitlines()[1:]],
                         done.output)

    def test_a_second_read_is_the_cached_downloads(self) -> None:
        self.assertEqual(0, self.report("111111").code)
        self.fakes.clear()
        done = self.report("111111")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.gh_calls("run download"))
        self.assertIn("Herr der Ringe", done.output)

    def test_a_pr_is_read_through_the_run_its_body_names(self) -> None:
        done = self.report("230", "--status", "ALL")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("Herr der Ringe", done.output)
        self.assertEqual("111111", self.gh_calls("run download")[0][2])

    def test_a_run_without_the_artifact_is_read_from_its_pull_request_jobs_log(self) -> None:
        done = self.report("222222", "--status", "ALL", "--tsv")
        self.assertEqual(0, done.code, done.output)
        lines = done.output.splitlines()
        self.assertTrue(any(line.startswith(TAB.join(
            ["WRONG", "pl", "Janosik", "edukacja MH: Binti", "tmdb:855156", "Binti (2021)"])) for line in lines),
            done.output)
        self.assertTrue(any(line.startswith(TAB.join(
            ["UNJUDGED", "uk", "Act One", "THE COMIC STRIP", "tmdb:396598", "The Comic Strip (1981)", "Filled",
             "tmdb:396598,imdb:tt0478854"])) for line in lines), done.output)
        self.assertTrue(any(line.startswith(TAB.join(
            ["LOST", "de", "Brotfabrik", "Jeder Acker, jede Fabrik", "", "", "", "", "wikidata:Q141180912"]))
            for line in lines), done.output)
        self.assertEqual(4, sum(1 for line in lines if not line.startswith("[identity-ratchet-report]")), done.output)
        self.assertEqual(["run", "view", "-R", "pawelkrupinski/movies", "--job", "42", "--log"],
                         self.gh_calls("run view")[-1])

    def test_a_download_that_fails_is_not_read_as_an_absent_artifact(self) -> None:
        done = self.report("333333", GH_DOWN="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.gh_calls("run view"))
        self.assertIn("could not download run 333333's identity-ratchet artifact: HTTP 502", done.output)
        self.assertFalse(os.path.exists(self.path("cache/ratchet/333333/ratchet-takes.tsv")))

    def test_a_gh_that_hangs_is_cut_off_and_fails_leaving_no_gh_running(self) -> None:
        hung = self.write("hung", "")
        done = self.report("444444", GH_HANGS=hung, KINOWO_IDENTITY_DOWNLOAD_TIMEOUT="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("timed out after 1s", done.output)
        self.assertEqual(0, cli_harness.still_running(hung))

    def test_a_findings_file_and_local_are_read_as_they_are(self) -> None:
        self.write("target/identity-unmatched/ratchet-takes.tsv", TAKES)
        local = self.report("--local", "--status", "ALL", "--tsv")
        self.assertEqual(0, local.code, local.output)
        self.assertEqual(TAKES, local.stdout)
        named = self.report(self.path("artifacts/111111/ratchet-takes.tsv"), "--status", "ALL", "--tsv")
        self.assertEqual(TAKES, named.stdout)
        self.assertEqual([], self.fakes.calls("gh"))

    def test_help_prints_the_usage_naming_the_sources(self) -> None:
        done = self.report("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("run id | PR number", done.output)

    def test_no_source_an_unknown_source_or_an_unknown_argument_is_a_usage_error(self) -> None:
        none = self.report()
        self.assertEqual(2, none.code)
        self.assertIn("run id | PR number", none.output)
        self.assertEqual(2, self.report("nonsense").code)
        unknown = self.report("111111", "--bogus")
        self.assertEqual(2, unknown.code)
        self.assertIn("unknown argument --bogus", unknown.output)


if __name__ == "__main__":
    unittest.main()
