#!/usr/bin/env python3
"""scripts/identity-slice-coverage, black-box, against a stand-in sbt (IDENTITY_SLICE_COVERAGE_SBT): every slice covered
passes (0), its tally shown; a slice lacking answers fails (1), naming the slice, each request and the --repin line, the
checkout's paths short; a coverage that never ran (a compile failure) is no verdict (2), sbt's log shown; --repin reads
which recordings the lacking slices need, finds them in the shared cache, and re-pins from there; a slice given on the
command line is the one replayed, made absolute; none given: both fixture roots. Ported from
identity-slice-coverage-test.sh case for case; the same cases hold the shell tool and its Python rewrite (the
extensionless name, whichever it execs)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
TALLY_LACKING = ("identity-slice-coverage: 3 slice(s) replayed in 9.1s; 1 lack answers or could not be decided "
                 "(1 request(s) lacking)")
# sbt as `worker/Test/runMain scripts.IdentitySliceCoverage ... --report FILE [--needs FILE] [--repin ...]` leaves it
SBT = r'''cmd=$1
echo "$cmd" >> "$FAKE_LOG"
report=$(printf '%s\n' "$cmd" | grep -oE -- '--report [^ ]+' | cut -d' ' -f2)
needs=$(printf '%s\n' "$cmd" | grep -oE -- '--needs [^ ]+' | cut -d' ' -f2)
echo "[info] compiling 3 Scala sources"
case ${FAKE_MODE:-covered} in
  broken) echo "[error] IdentitySliceCoverage.scala:1: not found"; exit 1 ;;
  covered) echo "identity-slice-coverage: 3 slice(s) replayed in 9.1s; 0 lack answers or could not be decided (0 request(s) lacking)" > "$report"; exit 0 ;;
esac
slice="$FAKE_REPO/test/resources/fixtures/identity-slices/uk-halstead-nutcracker"
if printf '%s\n' "$cmd" | grep -q -- '--repin'; then
  { echo "TALLY"
    echo "  $slice (recording 573): lacks 1 answer(s)"
    echo "  $slice: re-pinned from 573, 1 request(s) were lacking"
    echo "identity-slice-coverage: 1 slice(s) re-pinned; commit their http.jsonl.gz"; } > "$report"
  exit 0
fi
[ -z "$needs" ] || printf '573\tuk\n' > "$needs"
{ echo "TALLY"
  echo "  $slice (recording 573): lacks 1 answer(s)"
  echo "      GET https://api.themoviedb.org/3/search/movie?query=NATIONAL%20THEATRE%20LIVE"
  echo "SLICE ANSWERS MISSING: the slices above ask requests their fixtures do not hold. Re-pin them all from their recordings: scripts/identity-slice-coverage --repin"; } > "$report"
exit 1'''.replace("TALLY", TALLY_LACKING)


class IdentitySliceCoverageTest(cli_harness.ToolCase):
    TOOL = os.path.join(ROOT, "scripts", "identity-slice-coverage")

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("sbt", SBT)
        self.cache = self.path("cache")
        self.write("cache/recordings/573/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz", "")
        os.makedirs(self.path("cache/recordings/573/pair/test/resources/fixtures/enrichment-uk"))

    def coverage(self, *args: str, mode: str = "") -> cli_harness.Done:
        if os.path.exists(self.path("calls")):
            os.remove(self.path("calls"))
        env = {"FAKE_LOG": self.path("calls"), "FAKE_REPO": ROOT, "KINOWO_IDENTITY_CACHE": self.cache,
               "IDENTITY_SLICE_COVERAGE_SBT": os.path.join(self.fakes.bin, "sbt"), "FAKE_MODE": mode or None}
        return self.run_tool(*args, env=env)

    def calls(self) -> List[str]:
        return self.read("calls").splitlines() if os.path.exists(self.path("calls")) else []

    def test_every_slice_covered_passes_both_fixture_roots_replayed_absolute(self) -> None:
        done = self.coverage()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.output.splitlines().count(
            "identity-slice-coverage: 3 slice(s) replayed in 9.1s; 0 lack answers or could not be decided "
            "(0 request(s) lacking)"), done.output)
        roots = (f"--slices {ROOT}/test/resources/fixtures/identity-slices "
                 f"--slices {ROOT}/test/resources/fixtures/drift-slices --report ")
        self.assertEqual(1, sum(1 for call in self.calls() if roots in call), self.calls())

    def test_a_slice_lacking_answers_fails_naming_it_short_each_request_and_the_repin_line(self) -> None:
        done = self.coverage(mode="lacking")
        self.assertEqual(1, done.code, done.output)
        lines = done.output.splitlines()
        self.assertIn("  test/resources/fixtures/identity-slices/uk-halstead-nutcracker (recording 573): lacks 1 answer(s)",
                      lines)
        self.assertIn("GET https://api.themoviedb.org/3/search/movie?query=NATIONAL%20THEATRE%20LIVE", done.output)
        self.assertTrue(any(line.endswith("scripts/identity-slice-coverage --repin") for line in lines), done.output)
        self.assertFalse(any(line.startswith("[info]") for line in lines), done.output)

    def test_a_coverage_that_never_ran_is_no_verdict_sbts_log_shown(self) -> None:
        done = self.coverage(mode="broken")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("not found", done.output)

    def test_repin_asks_which_recordings_then_repins_from_the_shared_cache(self) -> None:
        done = self.coverage("--repin", mode="lacking")
        self.assertEqual(0, done.code, done.output)
        calls = self.calls()
        self.assertIn("--needs ", calls[0])
        self.assertIn(f"--repin --recordings {self.cache}/recordings", calls[1])
        self.assertIn("re-pinned from 573", done.output)

    def test_repin_with_nothing_lacking_repins_nothing(self) -> None:
        done = self.coverage("--repin")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("nothing to re-pin", done.output)

    def test_a_slice_named_is_the_one_replayed_absolute(self) -> None:
        self.coverage("test/resources/fixtures/drift-slices/de/x/")
        wanted = f"--slices {self.work}/test/resources/fixtures/drift-slices/de/x --report"
        self.assertEqual(1, sum(1 for call in self.calls() if wanted in call), self.calls())

    def test_an_unknown_option_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.coverage("--bogus").code)


if __name__ == "__main__":
    unittest.main()
