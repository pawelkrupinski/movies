#!/usr/bin/env python3
"""scripts/class-cycles, black-box, over a stub scripts/sbt-warm (the real one runs `sbt couplingClassCycles`,
ClassCyclesSpec holds its numbers): the package and the order reach the task as one sbt command, an order table's path
absolute, the report reaches stdout and --out; a usage error, an order with a space, a failed or silent task are exit 2,
and a copy run from inside another checkout refuses. Ported from class-cycles-test.sh; the same cases hold the shell
tool and its Python rewrite (the copy is the tool plus whatever it execs, scripts/class_cycles.py once there is one)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

STUB = """#!/bin/sh
printf '%s\\n' "$*" > "$(dirname "$0")/../called"
[ -z "${STUB_FAIL:-}" ] || { echo "[error] boom"; exit 1; }
set -- $1
[ -n "${STUB_SILENT:-}" ] || printf '# Class cycles in `%s`\\n' "$2" > "$3"
"""


class ClassCyclesTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        scripts = self.path("root", "scripts")
        os.makedirs(scripts)
        for name in ("class-cycles", "class_cycles.py", "own-checkout"):
            source = os.path.join(cli_harness.ROOT, "scripts", name)
            if os.path.exists(source):
                shutil.copy2(source, scripts)
        self.write("root/scripts/sbt-warm", STUB)
        os.chmod(self.path("root", "scripts", "sbt-warm"), 0o755)
        self.TOOL = self.path("root", "scripts", "class-cycles")

    def called(self) -> str:
        return self.read("root/called").strip()

    def run_cc(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        return self.run_tool(*args, env=env)

    def test_the_report_reaches_stdout_from_one_task_naming_the_package_and_the_report(self) -> None:
        done = self.run_cc("services.identity")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("# Class cycles in `services.identity`", done.stdout.strip())
        self.assertEqual(["couplingClassCycles", "services.identity"], self.called().split()[:2])
        self.assertTrue(self.called().split()[2].endswith("/cycles.md"), self.called())

    def test_an_inline_order_is_the_tasks_third_word(self) -> None:
        self.run_cc("services.identity", "--order", "vocabulary.*<*Measures,Listing<*")
        self.assertEqual("vocabulary.*<*Measures,Listing<*", self.called().split()[3])

    def test_an_order_tables_path_is_handed_on_absolute_and_out_keeps_the_report(self) -> None:
        table = self.write("tiers.tsv", "Listing\t0\nIdentityMeasures\t1\n")
        done = self.run_cc("services.identity", "--order", "tiers.tsv", "--out", "kept.md")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(table, self.called().split()[3])
        self.assertEqual("# Class cycles in `services.identity`\n", self.read("kept.md"))

    def test_usage_errors_are_exit_2(self) -> None:
        for args in ([], ["a", "b"], ["p", "--order"], ["p", "--bogus"], ["p", "--order", "a < b"]):
            with self.subTest(args=args):
                self.assertEqual(2, self.run_cc(*args).code)

    def test_help_prints_the_header_and_exits_0(self) -> None:
        done = self.run_cc("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("scripts/class-cycles <package> --order a<b<c", done.stdout)

    def test_a_failed_task_is_exit_2_with_its_logs_tail_and_a_silent_one_is_too(self) -> None:
        failed = self.run_cc("p", env={"STUB_FAIL": "1"})
        self.assertEqual(2, failed.code)
        self.assertIn("boom", failed.output)
        self.assertIn("sbt couplingClassCycles failed", failed.output)
        silent = self.run_cc("p", env={"STUB_SILENT": "1"})
        self.assertEqual(2, silent.code)
        self.assertIn("wrote no report", silent.output)

    def test_a_copy_run_from_inside_another_checkout_refuses(self) -> None:
        self.git(self.path("root"), "init", "-q")
        other = self.repo("other")
        done = self.run_tool("p", cwd=other)
        self.assertEqual(2, done.code, done.output)
        self.assertIn("its own checkout", done.output)


if __name__ == "__main__":
    unittest.main()
