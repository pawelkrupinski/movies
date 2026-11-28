#!/usr/bin/env python3
"""scripts/identity-slice-refresh, black-box, against a stand-in identity-slice (IDENTITY_SLICE): a case is re-emitted
with every listing its expected.tsv names (once each, a last line without its newline too), into a scratch root; its
slice files land in its own directory, its hand expected.tsv survives, and no other case is touched. A re-emit that
fails -- even after writing --, that identity-slice refuses, or that pins the listings as several slices leaves the case
as it was. Ported from identity-slice-refresh-test.sh case for case; the same cases hold the shell tool and its Python
rewrite (the extensionless name, whichever it execs)."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
# It pins under the emit root it is handed (KINOWO_IDENTITY_SLICE_EMIT_ROOT, as scripts/identity-slice reads it), saying
# where. PIN_AS names the directory (default "uk-new-name-0000"; it may name an existing case: the stub must not reach
# it); TWO_SLICES pins a second one; FAIL_AFTER_WRITE writes, then fails; REFUSE answers as emitCase does when the
# judgements judge none of the listings: it pins nothing, and exits 0.
SLICE = r'''root=${KINOWO_IDENTITY_SLICE_EMIT_ROOT:?the refresh must hand identity-slice an emit root}
pin() {
  dir="$root/$1"; mkdir -p "$dir"
  for f in slice.json.gz rows.json.gz http.jsonl.gz filed.json.gz; do echo fresh > "$dir/$f"; done
  printf 'venue\trawTitle\tfilm\tlabel\nX\tY\ttmdb:1\tfrom the judgements\n' > "$dir/expected.tsv"
  echo "--emit-case: 1 listing(s) pinned in $dir (IdentitySliceCasesSpec replays it):"
}
[ -z "${REFUSE:-}" ] || { echo "--emit-case REFUSED for key X | Y: the judgements judge none of its 1 listing(s); judge them first"; exit 0; }
pin "${PIN_AS:-uk-new-name-0000}"
[ -z "${TWO_SLICES:-}" ] || pin uk-second-cluster-9999
[ -z "${FAIL_AFTER_WRITE:-}" ] || { echo "SLICE INCOMPLETE" >&2; exit 3; }'''
# the same listing twice (a `not:` case's rows), the last line without its newline
EXPECTED = ("venue\trawTitle\tfilm\tlabel\n"
            "Filmhouse Newlyn\tFallen Angels By Noël Coward\tnot:tmdb:7\tby hand\n"
            "Filmhouse Newlyn\tFallen Angels By Noël Coward\tnot:tmdb:8\tby hand\n"
            "Regent Christchurch\tFallen Angels\ttmdb:9\tjudged")
STALE = "stale\n not:tmdb:7"


class IdentitySliceRefreshTest(cli_harness.ToolCase):
    TOOL = os.path.join(ROOT, "scripts", "identity-slice-refresh")

    def setUp(self) -> None:
        super().setUp()
        self.slice = self.fakes.script("identity-slice", SLICE, shell="/usr/bin/env bash")

    def make_case(self, name: str) -> str:
        for f in ("slice.json.gz", "rows.json.gz", "http.jsonl.gz"):
            self.write(f"identity-slices/{name}/{f}", "stale\n")
        self.write(f"identity-slices/{name}/expected.tsv", EXPECTED)
        return self.path("identity-slices", name)

    def refresh(self, *args: str, **modes: str) -> cli_harness.Done:
        return self.run_tool(*args, env={"IDENTITY_SLICE": self.slice, **modes})

    def untouched(self, name: str) -> str:
        """Its slice file and its first hand expectation, as make_case left them: "stale\\n not:tmdb:7"."""
        film = self.read(f"identity-slices/{name}/expected.tsv").splitlines()[1].split("\t")[2]
        return f"{self.read(f'identity-slices/{name}/slice.json.gz')} {film}"

    def test_a_case_repins_in_place_its_hand_expectation_kept_no_other_case_touched(self) -> None:
        case = self.make_case("uk-fallen-angels-1111")
        self.make_case("uk-new-name-0000")  # another case the emitted directory's name matches
        done = self.refresh("--recording", "38713274617", case)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["uk", "--recording", "38713274617", "--key", "Filmhouse Newlyn|Fallen Angels By Noël Coward",
                           "--key", "Regent Christchurch|Fallen Angels", "--emit-case"]],
                         self.fakes.calls("identity-slice"))
        self.assertEqual("fresh\n", self.read("identity-slices/uk-fallen-angels-1111/slice.json.gz"))
        self.assertEqual("fresh\n", self.read("identity-slices/uk-fallen-angels-1111/filed.json.gz"))
        self.assertEqual(EXPECTED, self.read("identity-slices/uk-fallen-angels-1111/expected.tsv"))
        self.assertEqual(STALE, self.untouched("uk-new-name-0000"))
        self.assertEqual(["uk-fallen-angels-1111", "uk-new-name-0000"], sorted(os.listdir(self.path("identity-slices"))))
        self.assertIn(f"identity-slice-refresh: {case}: re-pinned (2 listing(s))", done.stderr.splitlines())

    def test_a_case_named_with_a_trailing_slash_repins(self) -> None:
        case = self.make_case("uk-fallen-angels-1111")
        done = self.refresh(case + "/")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("uk", self.fakes.calls("identity-slice")[0][0])
        self.assertEqual("fresh\n", self.read("identity-slices/uk-fallen-angels-1111/slice.json.gz"))

    def test_a_reemit_that_writes_then_fails_leaves_the_case_as_it_was(self) -> None:
        done = self.refresh(self.make_case("uk-unrecorded-3333"), FAIL_AFTER_WRITE="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(STALE, self.untouched("uk-unrecorded-3333"))
        self.assertIn("SLICE INCOMPLETE", done.stderr)

    def test_listings_pinned_as_two_slices_leave_the_case_and_add_none(self) -> None:
        done = self.refresh(self.make_case("uk-two-clusters-4444"), TWO_SLICES="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(STALE, self.untouched("uk-two-clusters-4444"))
        self.assertFalse(os.path.exists(self.path("identity-slices", "uk-second-cluster-9999")))
        self.assertIn("were pinned as 2 slices (they fall in several clusters), left as it was", done.stderr)

    def test_a_reemit_identity_slice_refuses_is_reported_as_the_refusal(self) -> None:
        case = self.make_case("uk-unjudged-6666")
        done = self.refresh(case, REFUSE="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(STALE, self.untouched("uk-unjudged-6666"))
        said = [line for line in done.stderr.splitlines() if "uk-unjudged-6666:" in line]
        self.assertEqual([f"identity-slice-refresh: {case}: identity-slice refused to pin it (the judgements judge "
                          "none of its listings), left as it was"], said)

    def test_an_expected_tsv_naming_no_listing_is_a_failure(self) -> None:
        self.write("identity-slices/uk-empty-5555/expected.tsv", "venue\trawTitle\tfilm\tlabel\n")
        self.assertEqual(1, self.refresh(self.path("identity-slices", "uk-empty-5555")).code)
        self.assertEqual([], self.fakes.calls("identity-slice"))

    def test_a_directory_without_expected_tsv_is_no_case(self) -> None:
        done = self.refresh(self.work)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no expected.tsv -- not a slice case", done.stderr)

    def test_no_case_at_all_is_a_usage_error(self) -> None:
        done = self.refresh()
        self.assertEqual(2, done.code, done.output)
        self.assertIn("scripts/identity-slice-refresh [--recording RUN] <case dir>...", done.stderr)

    def test_an_unknown_option_is_a_usage_error(self) -> None:
        done = self.refresh("--bogus")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("identity-slice-refresh: unknown option --bogus\n", done.stderr)

    def test_help_prints_the_header(self) -> None:
        done = self.refresh("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(done.stdout.startswith("Purpose: Re-pin identity slice cases on this checkout's code"),
                        done.stdout)
        self.assertIn("Exit 1 when\nany case was not re-pinned, 2 on a usage error.", done.stdout)


if __name__ == "__main__":
    unittest.main()
