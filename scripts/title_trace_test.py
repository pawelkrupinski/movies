#!/usr/bin/env python3
"""scripts/title-trace, black-box: the raw title handed to ONE JVM whole, each ref laid out once with the tracer's files
dropped in, two refs' traces printed and diffed to the steps that moved, a batch diffed to the billings that moved, a call
naming no country refused. java and sbt are fakes; the repository is a scratch one holding a copy of the tool and the
scripts it runs. Ported from title-trace-test.sh case for case; the same cases hold the shell tool and its Python rewrite
(the extensionless name, whichever it execs)."""
from __future__ import annotations

import glob
import os
import shutil
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
RAW = "Pokaz przyjazny osobom z ADHD: Kandydaci śmierci"
TRACED = ("worker/src/test/scala/services/identity/TitleTrace.scala", "worker/src/fixtures/scala/tools/SearchQueryMarkers.scala",
          "worker/src/fixtures/scala/tools/CinemaSlotInvariants.scala")
# java prints a trace whose search step is the tree's build.sbt: the code a ref holds; a batch two billings, the second's
# search step the tree's build.sbt. Each call's directory noted beside the harness's argv log.
JAVA = '''pwd >> "$WORK/dirs"
case " $* " in *" --batch "*)
  printf '### pl\\tKino Muza\\tAmator\\n3. search: same\\n'
  printf '### pl\\tKino Muza\\tObsession (18)\\n3. search: %s\\n' "$(cat build.sbt)"; exit 0 ;;
esac
echo "1. ingest"
echo "3. search: $(cat build.sbt)"'''
SBT = 'echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"'


class TitleTraceTest(cli_harness.ToolCase):
    TOOL = ""

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.path("repo")
        for name in ["title-trace", "title_trace.py", "title-trace-tool-files.txt", "identity-tools.sh", "tool-conventions.sh", "jvm-gate.sh"]:
            if os.path.exists(os.path.join(ROOT, "scripts", name)):
                os.makedirs(os.path.join(self.checkout, "scripts"), exist_ok=True)
                shutil.copy2(os.path.join(ROOT, "scripts", name), os.path.join(self.checkout, "scripts", name))
        os.makedirs(os.path.join(self.checkout, "scripts", "lib"))
        for module in glob.glob(os.path.join(ROOT, "scripts", "lib", "*.py")):
            shutil.copy2(module, os.path.join(self.checkout, "scripts", "lib"))
        for relative in TRACED:
            self.write(f"repo/{relative}", f"// {relative}\n")
        for tree in ("common/src", "identity/src", "testkit/src"):
            os.makedirs(os.path.join(self.checkout, tree))
        self.write("repo/build.sbt", "rule a\n")
        self.repo("repo")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "base")
        self.write("repo/build.sbt", "rule b\n")
        self.git(self.checkout, "commit", "-qam", "head")
        self.base = self.git(self.checkout, "rev-parse", "HEAD~1").strip()
        self.fakes.script("java", JAVA)
        self.fakes.script("sbt", SBT)

    def trace(self, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, tool=os.path.join(self.checkout, "scripts", "title-trace"),
                             env={"KINOWO_IDENTITY_CACHE": self.path("cache"), "WORK": self.work})

    def java_calls(self) -> List[List[str]]:
        return self.fakes.calls("java")

    def traced(self, *argv: str) -> int:
        """The java calls running TitleTrace with exactly these arguments after the class."""
        def after(call: List[str]) -> List[str]:
            return call[call.index("services.identity.TitleTrace") + 1:] if "services.identity.TitleTrace" in call else []
        return sum(1 for call in self.java_calls() if after(call) == list(argv))

    def refs(self) -> List[str]:
        return [d for d in glob.glob(self.path("cache", "refs", "*")) if os.path.isdir(d)]

    def test_without_a_ref_this_checkouts_code_traces_the_title_whole(self) -> None:
        done = self.trace(RAW, "--country", "pl", "--venue", "Kino Muza")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.traced(RAW, "--country", "pl", "--venue", "Kino Muza"), self.java_calls())
        self.assertEqual(os.path.realpath(self.checkout), os.path.realpath(self.read("dirs").strip()))
        self.assertIn("3. search: rule b\n", done.stdout)

    def test_two_refs_each_laid_out_once_traced_in_its_own_jvm_and_diffed(self) -> None:
        done = self.trace("Obsession (18)", "--country", "uk", "--ref", "HEAD~1", "--ref", "HEAD")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(2, len(self.refs()), "each ref is laid out once in the cache")
        with open(self.path("cache", "refs", self.base, TRACED[0])) as f:
            self.assertEqual(f"// {TRACED[0]}\n", f.read(), "...with the tracer dropped in")
        self.assertEqual(2, self.traced("Obsession (18)", "--country", "uk"))
        moved = [line for line in done.stdout.splitlines() if line.startswith(("-3.", "+3."))]
        self.assertEqual(["-3. search: rule a", "+3. search: rule b"], moved)

    def test_two_refs_of_one_code_say_so(self) -> None:
        done = self.trace("Obsession (18)", "--country", "uk", "--ref", "HEAD", "--ref", "HEAD")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.stdout.count("every step the same"))

    def test_a_batch_one_jvm_a_ref_names_only_the_billing_that_moved_and_counts_it(self) -> None:
        batch = self.write("batch.tsv", "pl\tKino Muza\tAmator\npl\tKino Muza\tObsession (18)\n")
        done = self.trace("--batch", batch, "--ref", "HEAD~1", "--ref", "HEAD")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(2, self.traced("--batch", batch))
        moved = [line for line in done.stdout.splitlines() if line.startswith(("###", "-3", "+3"))]
        self.assertEqual(["### pl\tKino Muza\tObsession (18)", "-3. search: rule a", "+3. search: rule b"], moved)
        self.assertEqual(1, done.stdout.count("1 of 2 billings moved"))

    def test_compiles_lays_a_ref_out_with_the_tracer_and_compiles_it_tracing_nothing(self) -> None:
        done = self.trace("--compiles", "HEAD~1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, len(glob.glob(self.path("cache", "refs", "*", TRACED[0]))))
        self.assertEqual([], self.java_calls())

    def test_usage_errors(self) -> None:
        self.assertEqual(2, self.trace("Obsession (18)").code, "a call naming no country")
        self.assertEqual(2, self.trace("Obsession", "Lalka", "--country", "pl").code, "two raw titles")
        self.assertEqual(2, self.trace("Obsession", "--country").code, "a flag given last, with no value")
        missing = self.path("nope", "batch.tsv")
        done = self.trace("--batch", missing)
        self.assertEqual(2, done.code, "a --batch in a directory that is not there, never read from / instead")
        self.assertIn(f"no directory {self.path('nope')}", done.stderr)


if __name__ == "__main__":
    unittest.main()
