#!/usr/bin/env python3
"""scripts/identity-judge-cards, black-box: the capture it is pointed at (this checkout's, a directory, a run's
artifacts) handed to scripts.IdentityJudgeCards with the judgements that judge it, the options passed through, and a
capture or compile that fails stopping it before the JVM. sbt, java and gh are fakes; the repository is a scratch copy
of the scripts. Ported from identity-judge-cards-test.sh case for case; the same cases hold the shell tool and its
Python rewrite (the extensionless name, whichever it execs)."""
from __future__ import annotations

import glob
import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

SCRIPTS = ("identity-judge-cards", "identity_judge_cards.py", "identity-tools.sh", "tool-conventions.sh", "jvm-gate.sh",
           "vault-env.sh")
SBT = '''[ -z "${SBT_FAILS:-}" ] || { echo "[error] compile failed"; exit 1; }
echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"'''
JAVA = '''echo "$PWD" >> "$WORK/java-dirs"
echo "cards written"'''
# `run download` copies the named artifact's tree into --dir
GH = '''while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --name) name="$2" ;; esac; shift; done
[ -d "$WORK/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
mkdir -p "$dir"; cp -R "$WORK/artifacts/$name/." "$dir/"'''
CARDS = "scripts.IdentityJudgeCards"


class IdentityJudgeCardsTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.path("repo")
        os.makedirs(os.path.join(self.checkout, "scripts", "lib"))
        for name in SCRIPTS:
            source = os.path.join(cli_harness.ROOT, "scripts", name)
            if os.path.exists(source):
                shutil.copy2(source, os.path.join(self.checkout, "scripts", name))
        for module in glob.glob(os.path.join(cli_harness.ROOT, "scripts", "lib", "**", "*.py"), recursive=True):
            target = os.path.join(self.checkout, os.path.relpath(module, cli_harness.ROOT))
            os.makedirs(os.path.dirname(target), exist_ok=True)
            shutil.copy2(module, target)
        for tree in ("worker/src", "common/src", "identity/src", "testkit/src"):
            os.makedirs(os.path.join(self.checkout, tree))
        self.write("repo/test/resources/fixtures/identity-unmatched/pl.json.gz", "pl\n")
        self.write("repo/test/resources/fixtures/identity-judgements.tsv", "labels-here\n")
        self.write("repo/build.sbt", "build\n")
        self.write("artifacts/identity-capture-de/x/de.json.gz", "de\n")
        self.fakes.script("sbt", SBT, shell="/usr/bin/env bash")
        self.fakes.script("java", JAVA, shell="/usr/bin/env bash")
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")

    def cards(self, *args: str, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        extra = {"TMDB_API_KEY": None, "WORK": self.work, "KINOWO_IDENTITY_CACHE": self.path("cache"), **(env or {})}
        return self.run_tool(*args, tool=os.path.join(self.checkout, "scripts", "identity-judge-cards"), env=extra)

    def java_calls(self) -> List[List[str]]:
        return self.fakes.calls("java")

    def java_after_cards(self) -> str:
        call = self.java_calls()[-1]
        return " ".join(call[call.index(CARDS) + 1:])

    def test_this_checkouts_capture_judged_by_this_checkouts_judgements_run_from_the_repository(self) -> None:
        done = self.cards("--country", "pl")
        self.assertEqual(0, done.code, done.output)
        c = self.checkout
        self.assertEqual(
            f"-Xmx6g -Duser.language=en -Duser.country=US -Djava.awt.headless=true --sun-misc-unsafe-memory-access=allow "
            f"-cp {c}/worker/target/scala-3.x/test-classes:/lib/a.jar {CARDS} --fixtures "
            f"{c}/test/resources/fixtures/identity-unmatched --labels {c}/test/resources/fixtures/identity-judgements.tsv "
            f"--country pl", " ".join(self.java_calls()[-1]))
        self.assertEqual(c, self.read("java-dirs").splitlines()[-1])
        self.assertEqual(1, done.stderr.count("no TMDB_API_KEY"), done.output)

    def test_a_capture_directory_is_judged_by_its_own_judgements_options_passed(self) -> None:
        self.write("capture/pl.json.gz", "c\n")
        self.write("capture/identity-judgements.tsv", "labels-there\n")
        done = self.cards("--fixtures", self.path("capture"), "--country", "pl", "--out", "out-dir", "--all", "--offline")
        self.assertEqual(0, done.code, done.output)
        w = self.work
        self.assertEqual(f"--fixtures {w}/capture --labels {w}/capture/identity-judgements.tsv --country pl "
                         f"--out {w}/out-dir --all --offline", self.java_after_cards())
        self.assertTrue(os.path.isdir(self.path("out-dir")))
        self.assertNotIn("no TMDB_API_KEY", done.stderr)

    def test_a_runs_capture_is_downloaded_into_the_cache_judged_by_this_checkouts_judgements(self) -> None:
        done = self.cards("--fixtures", "987654", "--country", "de")
        self.assertEqual(0, done.code, done.output)
        run = self.path("cache", "captures", "run-987654")
        self.assertEqual(f"--fixtures {run} --labels {run}/identity-judgements.tsv --country de", self.java_after_cards())
        self.assertEqual("labels-here\n", self.read("cache/captures/run-987654/identity-judgements.tsv"))

    def test_a_run_lacking_a_countrys_capture_stops_before_the_jvm(self) -> None:
        done = self.cards("--fixtures", "987654", "--country", "de,uk")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.java_calls())

    def test_a_compile_that_fails_stops_before_the_jvm(self) -> None:
        done = self.cards("--country", "pl", env={"SBT_FAILS": "1"})
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.java_calls())

    def test_countries_the_other_tools_spelling_is_the_same_flag(self) -> None:
        done = self.cards("--countries", "PL", "--country=de")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(self.java_after_cards().endswith("identity-judgements.tsv --country pl,de"),
                        self.java_after_cards())

    def test_a_country_no_tool_knows_is_a_usage_error_before_any_download(self) -> None:
        done = self.cards("--countries", "pl,xx")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, done.stderr.count("unknown country xx"), done.output)

    def test_an_unknown_argument_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.cards("--bogus").code)

    def test_help_prints_the_usage(self) -> None:
        done = self.cards("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("--fixtures SRC", done.stdout)


if __name__ == "__main__":
    unittest.main()
