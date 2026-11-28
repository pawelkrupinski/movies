#!/usr/bin/env python3
"""scripts/ratchet-diff, black-box (cli_harness), over a scratch checkout holding a copy of the tool beside stub
measures (quality-report, coupling-report, js-quality-report, mobile-quality-report) and a stub scripts/ci/ratchet_diff.py that logs its
argv: the range read into --base/--head, --tsv passed on, the usage errors, and --measure -- which measures the
checkout it lives in, so run from another checkout (a worktree calling the root checkout's copy) it refuses, exit 2,
naming both; run inside its own, it measures and hands the table the rows each measure wrote, or says which wrote none.
Run: python3 scripts/ratchet_diff_cli_test.py"""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
TOOL = cli_harness.tool_path("scripts/ratchet_diff_cli.py", "scripts/ratchet-diff")
COPIED = ("scripts/ratchet-diff", "scripts/ratchet_diff_cli.py", "scripts/own-checkout")
# A stub measure: logs that it ran, writes its rows into --out unless told not to, and fails as the real ones may
MEASURE = r'''#!/usr/bin/env python3
import os, sys
here = os.path.dirname(os.path.abspath(__file__))
name = os.path.basename(sys.argv[0])
with open(os.path.join(here, "..", "measured"), "a") as f:
    f.write(name + " " + " ".join(sys.argv[1:]) + "\n")
out = sys.argv[sys.argv.index("--out") + 1]
os.makedirs(out, exist_ok=True)
if name not in os.environ.get("STUB_NO_ROWS", "").split():
    rows_file = {"js-quality-report": "over-limit.tsv", "mobile-quality-report":
                 sys.argv[sys.argv.index("--language") + 1] + "-over-limit.tsv" if "--language" in sys.argv else ""}
    with open(os.path.join(out, rows_file.get(name, "rows.tsv")), "w") as f:
        f.write(name + " rows\n")
print(name + " said this")
sys.exit(2)
'''
TABLE = r'''#!/usr/bin/env python3
import json, os, sys
here = os.path.dirname(os.path.abspath(__file__))
rows = {}
for flag in ("--quality-rows", "--coupling-rows", "--js-rows", "--swift-rows", "--kotlin-rows"):
    if flag in sys.argv:
        with open(sys.argv[sys.argv.index(flag) + 1]) as f:
            rows[flag] = f.read()
with open(os.path.join(here, "..", "..", "table.log"), "a") as f:
    f.write(json.dumps({"argv": sys.argv[1:], "rows": rows}) + "\n")
print("the table")
'''


class RatchetDiffTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.root = self.checkout("root")
        self.other = self.checkout("other")
        for relative in COPIED:
            if os.path.exists(os.path.join(ROOT, relative)):
                shutil.copy(os.path.join(ROOT, relative), os.path.join(self.root, relative))
        for name in ("quality-report", "coupling-report", "js-quality-report", "mobile-quality-report"):
            self.executable(f"root/scripts/{name}", MEASURE)
        self.executable("root/scripts/ci/ratchet_diff.py", TABLE)
        self.TOOL = os.path.join(self.root, os.path.relpath(TOOL, ROOT))

    def checkout(self, name: str) -> str:
        repo = self.repo(name, "-q")
        self.write(f"{name}/a.txt", "x\n")
        os.makedirs(os.path.join(repo, "scripts", "ci"))
        self.git(repo, "add", "-A")
        self.git(repo, "commit", "-qm", "base")
        return repo

    def executable(self, relative: str, text: str) -> None:
        os.chmod(self.write(relative, text), 0o755)

    def diff_from(self, where: str, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, cwd=where)

    def tables(self) -> List[List[str]]:
        if not os.path.exists(os.path.join(self.root, "table.log")):
            return []
        with open(os.path.join(self.root, "table.log")) as f:
            return [json.loads(line)["argv"] for line in f]

    def measured(self) -> List[str]:
        path = os.path.join(self.root, "measured")
        if not os.path.exists(path):
            return []
        with open(path) as f:
            return [line.split()[0] for line in f]

    def test_a_range_is_the_table_s_base_and_head(self) -> None:
        done = self.diff_from(self.root, "abc..def")
        self.assertEqual((0, "the table\n"), (done.code, done.output))
        self.assertEqual([["--base", "abc", "--head", "def"]], self.tables())

    def test_a_base_alone_leaves_the_head_to_the_table_and_tsv_is_passed_on(self) -> None:
        self.assertEqual(0, self.diff_from(self.root, "--tsv", "abc").code)
        self.assertEqual([["--base", "abc", "--tsv"]], self.tables())

    def test_the_usage_errors_exit_2(self) -> None:
        for args, said in ((["abc", "def"], "ratchet-diff: one range\n"),
                           (["--bogus"], "ratchet-diff: unknown argument --bogus\n"),
                           ([], "ratchet-diff: <base>..<head> (or <base>) is required\n")):
            done = self.diff_from(self.root, *args)
            self.assertEqual((2, said), (done.code, done.output), args)
        self.assertEqual([], self.tables())

    def test_help_prints_the_header(self) -> None:
        done = self.diff_from(self.root, "--help")
        lines = done.stdout.splitlines()
        self.assertEqual(0, done.code)
        self.assertTrue(lines[0].startswith("Purpose: The ratchet diff (CLAUDE.md"), lines[0])
        self.assertEqual("The table and its marks: scripts/ci/ratchet_diff.py.", lines[-1])
        self.assertIn("  scripts/ratchet-diff --tsv <base>..<head>    the same rows as TSV", lines)

    def test_measure_from_another_checkout_refuses_naming_the_one_it_measures(self) -> None:
        done = self.diff_from(self.other, "--measure", "HEAD")
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"measures {self.root}", done.output)
        self.assertEqual([], self.measured())
        self.assertEqual([], self.tables())

    def test_measure_inside_its_own_checkout_hands_the_table_every_measure_s_rows(self) -> None:
        done = self.diff_from(self.root, "--measure", "HEAD")
        self.assertEqual((0, "the table\n"), (done.code, done.output))
        self.assertEqual(["quality-report", "coupling-report", "js-quality-report", "mobile-quality-report",
                          "mobile-quality-report"], self.measured())
        with open(os.path.join(self.root, "table.log")) as f:
            [table] = [json.loads(line) for line in f]
        self.assertEqual(["--base", "HEAD", "--quality-rows", "--coupling-rows", "--js-rows", "--swift-rows", "--kotlin-rows"],
                         [arg for arg in table["argv"] if not arg.startswith("/")])
        self.assertEqual({"--quality-rows": "quality-report rows\n", "--coupling-rows": "coupling-report rows\n",
                          "--js-rows": "js-quality-report rows\n", "--swift-rows": "mobile-quality-report rows\n",
                          "--kotlin-rows": "mobile-quality-report rows\n"}, table["rows"])
        with open(os.path.join(self.root, "measured")) as f:
            js = f.read().splitlines()[2].split()
        self.assertEqual(["--areas", os.path.join(self.root, "scripts/js-quality/ratchet-areas.tsv"), "--out"], js[1:4])

    def test_measure_takes_a_head_only_when_it_is_head(self) -> None:
        self.assertEqual(0, self.diff_from(self.root, "--measure", "HEAD~0..HEAD").code)
        self.write("root/a.txt", "y\n")
        self.git(self.root, "commit", "-qam", "next")
        done = self.diff_from(self.root, "--measure", "HEAD~1..HEAD~1")
        self.assertEqual((2, "ratchet-diff: --measure measures this working tree: give <base> or <base>..HEAD\n"),
                         (done.code, done.output))

    def test_a_measure_that_wrote_no_rows_is_named_with_its_log(self) -> None:
        for name, said in (("coupling-report", "the coupling measure"), ("js-quality-report", "the js measure")):
            done = self.run_tool("--measure", "HEAD", cwd=self.root, env={"STUB_NO_ROWS": name})
            self.assertEqual(2, done.code, done.output)
            self.assertIn(f"{name} said this\n", done.stderr)
            self.assertTrue(done.stderr.endswith(f"ratchet-diff: {said} wrote no rows\n"), done.stderr)
        self.assertEqual([], self.tables())


if __name__ == "__main__":
    unittest.main()
