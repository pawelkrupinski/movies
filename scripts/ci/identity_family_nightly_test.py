#!/usr/bin/env python3
"""scripts/ci/identity-family-nightly.sh end to end against a fake `sbt` and `gh`: a night with no stored cache walks
every family and stores nothing without a store; with a store it stores the cache and its commit; a stored cache of the
same commit walks with a reach file; a walk that wrote no verdict fails (exit 2) and stores nothing. Ported from
identity-family-nightly-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/identity_family_nightly.py", "scripts/ci/identity-family-nightly.sh")
SHA = "0123456789abcdef0123456789abcdef01234567"
# sbt: logs its last argument and, unless $NO_VERDICT, writes the walk's two tables and the verdict line.
SBT = r"""
echo "${@: -1}" >> "$WORK/sbt-calls"
[ -z "${NO_VERDICT:-}" ] || exit 1
out=$(sed -E 's/.* --out ([^ ]+).*/\1/' <<< "${@: -1}")
printf 'kind\tcountry\n' > "$out/family-flips-de.tsv"; printf 'family\tseconds\n' > "$out/family-walks-de.tsv"
echo '[]' > "$(sed -E 's/.* --cache ([^ ]+).*/\1/' <<< "${@: -1}")"
echo "## de family check: 3 families, shard 1/2, 3 walked, 0 reused, 0 flip(s)"
"""
# gh: `release view --json` answers $ASSETS, then more of the store's assets than a pipe holds (a reader that stops at its
# first match kills it of SIGPIPE, as it does the real gh); `release download` writes a gzip of one line.
GH = r"""
echo "$*" >> "$WORK/calls"
case "$1 $2" in
  "release view") if [[ "$*" == *--json* ]]; then printf '%s\n' $ASSETS; yes family-cache-xx-1of1-L1.json.gz | head -n 20000; else [ -n "${RELEASE_EXISTS:-}" ] || [ -f "$WORK/exists" ]; fi ;;
  "release create") touch "$WORK/exists" ;;
  "release download")
    for ((i = 1; i <= $#; i++)); do [ "${!i}" = -O ] && { j=$((i + 1)); dest="${!j}"; }; done
    printf '%s\n' "${CACHED_SHA:-x}" | gzip -c > "$dest" ;;
  *) exit 0 ;;
esac
"""
# python3: the reach (scripts/ci/family_reach.py, tested on its own) is a file; every other script runs on the real one.
PYTHON3 = r"""
if any("family_reach.py" in word for word in argv):
    reach_file = argv[argv.index("--out") + 1]
    open(reach_file, "w").write("*\n")
    out("reach: ALL (stub)")
else:
    os.execv(%r, [%r] + argv)
""" % (sys.executable, sys.executable)


class IdentityFamilyNightlyTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("sbt", SBT, shell="/usr/bin/env bash")
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.fakes.script("git", "exit 0")  # the cached commit is never fetched
        self.fakes.program("python3", PYTHON3)
        self.fakes.clear()
        for name in ("sbt-calls", "calls"):
            self.write(name, "")

    def night(self, **extra: str) -> int:
        """One night's walk of de into out/; the exit code. sbt's last argument lands in sbt-calls, gh's in calls."""
        for name in ("sbt-calls", "calls", "exists"):
            if os.path.exists(self.path(name)):
                os.remove(self.path(name))
        for name in ("sbt-calls", "calls"):
            self.write(name, "")
        env: Dict[str, Optional[str]] = {"WORK": self.work, "RUNNER_TEMP": self.work, "GH_RELEASE_RETRY_DELAY": "0",
                                         "GITHUB_SHA": SHA}
        env.update(extra)
        done = self.run_tool("de", self.path("out"), env=env)
        self.write("log", done.output)
        return done.code

    def lines(self, name: str) -> List[str]:
        return self.read(name).splitlines()

    def count(self, name: str, pattern: str) -> int:
        return sum(1 for line in self.lines(name) if re.search(pattern, line))

    def test_no_store_walks_every_family_and_stores_nothing(self) -> None:
        self.assertEqual(0, self.night(FAMILY_SHARD="1/2"))
        self.assertEqual(1, self.count("sbt-calls", r"--shard 1/2 --levels 1 "))
        self.assertEqual(1, self.count("sbt-calls", r"^(?!.*--reach)"))
        self.assertEqual(0, self.count("sbt-calls", r"--reach"))
        self.assertEqual(0, self.count("calls", r"^release"))

    def test_the_flips_and_walks_tables_are_left_for_the_artifact_the_cache_not(self) -> None:
        self.night(FAMILY_SHARD="1/2")
        self.assertTrue(os.path.isfile(self.path("out/family-flips-de.tsv")))
        self.assertTrue(os.path.isfile(self.path("out/family-walks-de.tsv")))
        self.assertFalse(os.path.exists(self.path("out/cache.json")))

    def test_a_store_with_no_cache_yet_creates_the_release_and_stores_the_cache_and_its_commit(self) -> None:
        self.assertEqual(0, self.night(FAMILY_STORE="s", FAMILY_SHARD="1/2", ASSETS=""))
        self.assertEqual(1, self.count("calls", r"^release create s "))
        self.assertEqual(2, self.count("calls", r"^release upload s .*family-cache-de-1of2-L1"))

    def test_a_stored_cache_with_its_commit_walks_with_the_reach_file(self) -> None:
        code = self.night(FAMILY_STORE="s", FAMILY_SHARD="1/2", RELEASE_EXISTS="1", CACHED_SHA=SHA,
                          ASSETS="family-cache-de-1of2-L1.json.gz family-cache-de-1of2-L1.sha.gz")
        self.assertEqual(0, code)
        self.assertEqual(1, self.count("sbt-calls", r"--reach "))

    def test_a_walk_that_wrote_no_verdict_fails_said_and_stores_nothing(self) -> None:
        self.assertEqual(2, self.night(FAMILY_STORE="s", FAMILY_SHARD="1/2", NO_VERDICT="1"))
        self.assertEqual(0, self.count("calls", r"^release upload"))
        self.assertEqual(1, self.count("log", r"^family explorer \(de, shard 1/2\): no verdict"))

    def test_the_sampled_tiers_orders_their_weekly_salt_and_the_recorded_costs_reach_the_walk(self) -> None:
        self.night(FAMILY_SHARD="1/2", FAMILY_ORDERS="2", FAMILY_ORDER_SALT="45")
        self.assertEqual(1, self.count("sbt-calls", r"--orders 2 --order-salt 45 --costs .*/family-costs-de\.tsv "))
        self.night(FAMILY_SHARD="1/2")
        self.assertEqual(1, self.count("sbt-calls", r"--orders 8 --order-salt 0 "))

    def test_a_shard_that_is_not_i_over_n_fails_before_the_walk(self) -> None:
        self.assertEqual(2, self.night(FAMILY_SHARD="3"))
        self.assertEqual(0, len(self.lines("sbt-calls")))


if __name__ == "__main__":
    unittest.main()
