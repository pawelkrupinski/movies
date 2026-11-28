#!/usr/bin/env python3
"""scripts/ci/recording-preflight.sh, black-box: a refused key fails the recording before its legs start; OMDb's spent
daily quota, no answer at all, or a 5xx only warns; a key's value is never printed. curl is a fake that answers each
host as the case says, OMDb with the responses recorded from omdbapi.com (test/resources/fixtures/omdb/) where there is
one. Ported from recording-preflight-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/recording_preflight.py", "scripts/ci/recording-preflight.sh")
RECORDED = os.path.join(cli_harness.ROOT, "test/resources/fixtures/omdb")
# The body to `-o`, the status for `-w`, like the real curl; status 000 exits non-zero, as curl does with no answer.
CURL = r"""
sink_path = argv[argv.index("-o") + 1]
url = argv[-1]
host = "omdb" if "omdbapi.com" in url else "tmdb"
with open(os.path.join(env["STUB"], host + ".body")) as body, open(sink_path, "w") as sink:
    sink.write(body.read())
status = open(os.path.join(env["STUB"], host + ".status")).read()
out(status, end="")
sys.exit(1 if status == "000" else 0)
"""


def recorded(name: str) -> str:
    with open(os.path.join(RECORDED, name)) as f:
        return f.read()


class RecordingPreflightTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("curl", CURL)
        self.out = ""

    def answers(self, omdb_status: str, omdb_body: str, tmdb_status: str, tmdb_body: str) -> None:
        self.write("stub/omdb.status", omdb_status)
        self.write("stub/omdb.body", omdb_body)
        self.write("stub/tmdb.status", tmdb_status)
        self.write("stub/tmdb.body", tmdb_body)

    def run_preflight(self, omdb: Optional[str] = "k", tmdb: Optional[str] = "t") -> int:
        done = self.run_tool(env={"STUB": self.path("stub"), "OMDB_API_KEY": omdb, "TMDB_API_KEY": tmdb})
        self.out = done.output
        return done.code

    def said(self, text: str) -> int:
        return sum(1 for line in self.out.split("\n") if text in line)

    def test_both_keys_accepted_passes_printing_no_key(self) -> None:
        # OMDb's whole record puts "Response":"True" last, past its first 400 bytes (1,126 bytes here).
        self.answers("200", recorded("title_aftersun_2022.json"), "200", '{"images":{}}')
        self.assertEqual(0, self.run_preflight("omdb-secret", "tmdb-secret"), self.out)
        self.assertEqual(1, self.said("OMDb: key accepted"))
        self.assertEqual(1, self.said("TMDB: key accepted"))
        self.assertEqual(1, self.said("recording preflight: no key refused"))
        self.assertEqual(0, self.said("::warning::"))
        self.assertNotIn("omdb-secret", self.out)
        self.assertNotIn("tmdb-secret", self.out)

    def test_the_probes_ask_each_host_once_with_the_key(self) -> None:
        self.answers("200", recorded("title_aftersun_2022.json"), "200", "{}")
        self.run_preflight("omdb-secret", "tmdb-secret")
        omdb, tmdb = self.fakes.calls("curl")
        self.assertEqual("https://www.omdbapi.com/?i=tt0113277&apikey=omdb-secret", omdb[-1])
        self.assertEqual(["-H", "Authorization: Bearer tmdb-secret",
                          "https://api.themoviedb.org/3/configuration?api_key=tmdb-secret"], tmdb[-3:])
        self.assertEqual(["-sS", "--max-time", "20", "--retry", "2", "--retry-all-errors", "-o"], omdb[:7])

    # OMDb's spent daily quota as the nightly #2426 met it (HTTP 401 and its "Request limit reached!" document).
    def test_omdbs_spent_daily_quota_only_warns_naming_it_and_when_its_day_turns(self) -> None:
        self.answers("401", '{"Response":"False","Error":"Request limit reached!"}', "200", "{}")
        self.assertEqual(0, self.run_preflight("omdb-secret"), self.out)
        self.assertEqual(1, self.said("::warning::OMDb's daily quota is spent (HTTP 401: Request limit reached!)"))
        self.assertEqual(1, self.said("midnight US Central"))
        self.assertEqual(0, self.said("::error::"))
        self.assertNotIn("omdb-secret", self.out)

    def test_omdbs_spent_quota_as_a_200_document_only_warns(self) -> None:
        self.answers("200", '{"Response":"False","Error":"Request limit reached!"}', "200", "{}")
        self.assertEqual(0, self.run_preflight(), self.out)

    def test_omdbs_invalid_api_key_fails_on_a_401_or_a_200(self) -> None:
        self.answers("401", recorded("invalid_api_key.json"), "200", "{}")
        self.assertEqual(1, self.run_preflight())
        self.assertEqual(1, self.said("::error::OMDb refuses the key (HTTP 401: Invalid API key!)"), self.out)
        self.assertEqual(1, self.said("recording preflight: 1 refusal(s)"), self.out)
        self.answers("200", recorded("invalid_api_key.json"), "200", "{}")
        self.assertEqual(1, self.run_preflight())

    def test_an_omdb_401_that_names_no_reason_fails(self) -> None:
        self.answers("401", "", "200", "{}")
        self.assertEqual(1, self.run_preflight())
        self.assertEqual(1, self.said("(HTTP 401: no reason given)"), self.out)

    def test_a_refused_tmdb_key_fails_even_beside_omdbs_spent_quota(self) -> None:
        self.answers("200", recorded("title_aftersun_2022.json"), "401", '{"status_code":7}')
        self.assertEqual(1, self.run_preflight())
        self.assertEqual(1, self.said("::error::TMDB refuses the key (HTTP 401)"), self.out)
        self.answers("401", '{"Response":"False","Error":"Request limit reached!"}', "401", '{"status_code":7}')
        self.assertEqual(1, self.run_preflight())

    def test_an_unset_or_empty_key_fails(self) -> None:
        self.answers("200", "{}", "200", "{}")
        self.assertEqual(1, self.run_preflight(omdb=None))
        self.assertEqual(1, self.said("::error::secret OMDB_API_KEY is unset or empty"), self.out)
        self.assertEqual(1, self.run_preflight(tmdb=""))
        self.assertEqual(1, self.said("::error::secret TMDB_API_KEY is unset or empty"), self.out)

    def test_both_unset_counts_two_refusals(self) -> None:
        self.answers("200", "{}", "200", "{}")
        self.assertEqual(1, self.run_preflight(omdb="", tmdb=None))
        self.assertEqual(1, self.said("recording preflight: 2 refusal(s)"), self.out)

    def test_no_answer_or_a_5xx_only_warns_saying_so(self) -> None:
        self.answers("000", "", "503", "down")
        self.assertEqual(0, self.run_preflight(), self.out)
        self.assertEqual(2, self.said("::warning::"))
        self.assertEqual(1, self.said("::warning::OMDb did not answer the probe (HTTP 000)"))
        self.assertEqual(1, self.said("::warning::TMDB did not answer the probe (HTTP 503)"))

    def test_a_200_without_the_record_is_no_refusal_and_names_omdbs_reason(self) -> None:
        self.answers("200", recorded("title_not_found.json"), "200", "{}")
        self.assertEqual(0, self.run_preflight(), self.out)
        self.assertEqual(1, self.said("(Movie not found!)"))


if __name__ == "__main__":
    unittest.main()
