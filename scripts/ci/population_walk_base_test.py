#!/usr/bin/env python3
"""scripts/ci/population-walk-diff, black-box: the population walk's base side for the pre-push hook's identity check.
The tool runs in a scratch checkout (the tool, the judge beside it, a stub scripts/identity-tools.sh whose
identity_tree / identity_classpath / IDENTITY_JAVA answer from the test, and a fake `java` standing in for
PopulationMonotonicitySpec): nothing to do when no slice is a candidate, a slice the change brings is never walked, a
walk's rows are kept per base and slice and a failed walk is not, and a base that cannot be laid out, compiled or walked
is judged on the known rows alone. The same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/population_walk_base.py", "scripts/ci/population-walk-diff")
SLICE = "drift-slices/pl/a"
FIXTURE = f"test/resources/fixtures/{SLICE}"
CANDIDATE = f"slice\t{SLICE}\nhead\tVenue\tTitle\tflip: A > B > A\n"
WALK = f"{SLICE}\tVenue\tTitle\tA > B > A\n"
STUB = '''
IDENTITY_CACHE=$KINOWO_IDENTITY_CACHE
IDENTITY_JAVA=(java)
identity_tree() { echo "identity_tree $*" >> "$STUB_LOG"; [ -z "${TREE_FAILS:-}" ] || return 1; echo "$STUB_TREE"; }
identity_classpath() { echo "identity_classpath $*" >> "$STUB_LOG"; [ -z "${CLASSPATH_FAILS:-}" ] || return 1; echo the-classpath; }
'''
JAVA = '''
if env.get("JAVA_FAILS"):
    err("boom"); exit(1)
out(open(env["WALK"]).read(), end="")
'''


class PopulationWalkBaseTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.repo_dir = self.repo("checkout")
        self.copy_scripts("checkout")
        self.tool = self.path("checkout", "scripts", "ci", os.path.basename(TOOL))
        shutil.copy2(TOOL, self.tool)
        self.write("checkout/scripts/identity-tools.sh", STUB)
        self.write(f"checkout/{FIXTURE}/slice.json", "{}\n")
        self.sha = self.commit_tree(self.repo_dir, "base")
        self.fakes.program("java", JAVA)
        self.candidates = self.path("candidates")
        self.write("candidates/c.tsv", CANDIDATE)
        self.write("walk", WALK)
        self.stub_log = self.path("stub-log")
        self.tree = self.path("a-tree")

    def walk(self, base: Optional[str] = None, **extra: str) -> cli_harness.Done:
        env: Dict[str, Optional[str]] = {"KINOWO_IDENTITY_CACHE": self.path("cache"), "STUB_LOG": self.stub_log,
                                         "STUB_TREE": self.tree, "WALK": self.path("walk")}
        env.update(extra)
        return self.run_tool(self.candidates, base or self.sha, env=env, cwd=self.path("elsewhere"))

    def run_tool(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None, cwd: Optional[str] = None,
                 stdin: Optional[str] = None, tool: Optional[str] = None,
                 bound: float = cli_harness.HANG_BOUND) -> cli_harness.Done:
        os.makedirs(cwd or self.work, exist_ok=True)
        return super().run_tool(*args, env=env, cwd=cwd, stdin=stdin, tool=tool or self.tool, bound=bound)

    def stub_calls(self) -> List[str]:
        return self.read("stub-log").splitlines() if os.path.exists(self.stub_log) else []

    def java_runs(self) -> List[dict]:
        with open(self.fakes.log) as f:
            return [json.loads(line) for line in f if line.strip() and json.loads(line)["name"] == "java"]

    def test_no_candidate_slice_is_nothing_to_do(self) -> None:
        os.remove(self.path("candidates", "c.tsv"))
        done = self.walk()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(([], []), (self.stub_calls(), self.java_runs()))

    def test_a_base_that_is_no_commit_is_judged_on_the_known_rows_alone(self) -> None:
        done = self.walk("nosuch")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("population walk: no base commit nosuch: judged on the known rows alone", done.stderr)
        self.assertEqual(([], []), (self.stub_calls(), self.java_runs()))

    def test_the_slice_is_walked_with_the_bases_tree_and_classpath_from_the_checkout_and_judged_against_it(self) -> None:
        done = self.walk()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([f"identity_tree {self.sha}", "identity_classpath " + self.tree], self.stub_calls())
        (run,) = self.java_runs()
        self.assertEqual(["-cp", "the-classpath", "services.identity.PopulationMonotonicitySpec", FIXTURE], run["argv"])
        self.assertEqual(self.repo_dir, run["cwd"])
        self.assertIn("population walk: 1 candidate slice(s), 0 the change brings, 0 cached for base", done.stderr)
        self.assertIn("pre-existing (the base flips it too, main's)", done.stdout)

    def test_a_second_push_on_the_same_base_walks_nothing_again(self) -> None:
        self.walk()
        done = self.walk()
        self.assertEqual((0, 1), (done.code, len(self.java_runs())))
        self.assertIn("1 candidate slice(s), 0 the change brings, 1 cached for base", done.stderr)
        self.assertIn("pre-existing", done.stdout)

    def test_a_failed_walk_is_judged_unwalked_and_not_kept(self) -> None:
        self.write("walk", f"# {SLICE}: FAILED boom\n")
        first = self.walk()
        self.assertEqual(1, first.code, first.output)
        self.assertIn("base unwalked", first.stdout)
        self.walk()
        self.assertEqual(2, len(self.java_runs()))

    def test_a_java_that_fails_is_judged_on_the_known_rows_alone(self) -> None:
        done = self.walk(JAVA_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("boom", done.stderr)
        self.assertIn("population walk: the base's walk failed: judged on the known rows alone", done.stderr)

    def test_a_base_that_cannot_be_compiled_is_judged_on_the_known_rows_alone(self) -> None:
        done = self.walk(CLASSPATH_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"population walk: the base {self.sha[:9]} could not be compiled: judged on the known rows alone",
                      done.stderr)
        self.assertEqual([], self.java_runs())

    def test_a_base_whose_tree_cannot_be_laid_out_is_judged_on_the_known_rows_alone(self) -> None:
        done = self.walk(TREE_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("could not be compiled: judged on the known rows alone", done.stderr)

    def test_a_slice_the_change_brings_is_never_walked_at_the_base(self) -> None:
        self.write(f"checkout/{FIXTURE}/slice.json", '{"edited": true}\n')
        done = self.walk()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("1 candidate slice(s), 1 the change brings, 0 cached for base", done.stderr)
        self.assertEqual([], self.java_runs())
        self.assertIn("the change brings the slice or its known rows", done.stdout)


if __name__ == "__main__":
    unittest.main()
