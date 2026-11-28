#!/usr/bin/env python3
"""scripts/coupling-report, black-box, from a scratch copy of its checkout whose scripts/sbt-warm and
scripts/ci/coupling_ratchet.py are fakes (the real ones run `sbt couplingReport` and the ratchet; CouplingSimulationSpec,
ExemptionSlackSpec and coupling_ratchet_test.py hold their numbers): the report is printed, then the ratchet's check
over the measured files decides the exit; --update records the rows and the reference sets and exits 0; --check-split
hands its pieces to the ratchet's split-check alone; --new-exemptions runs no sbt; --simulate and --exemption-slack
each run their own task and print its markdown, the moves file's path handed on absolute; a failed or silent task, a
missing moves file, a bad argument and another checkout are exit 2. Ported from coupling-report-test.sh case for case,
plus the report, ratchet and usage paths; the same cases hold the shell tool and its Python rewrite (the copy takes
coupling_report.py and scripts/lib when they exist)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
# the tool's own files, copied into the scratch checkout (whichever exist: the shell tool, or its Python rewrite)
OWN = ("coupling-report", "coupling_report.py", "own-checkout")
# sbt-warm: notes its command; each file the task names gets a line (`# <task>` for the markdown) unless $SBT_SILENT;
# fails on $SBT_FAILS
SBT_WARM = '''
root = os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0])))
with open(os.path.join(root, "called"), "a") as f:
    f.write(json.dumps(argv) + "\\n")
if env.get("SBT_FAILS"):
    out("[error] boom")
    exit(1)
task, *files = argv[0].split(" ")
if not env.get("SBT_SILENT"):
    for name in files:
        if name.endswith(".md") or task == "couplingReport":
            with open(name, "w") as f:
                f.write(f"# {task} {os.path.basename(name)}\\n")
'''
# the ratchet: notes its argv, says its subcommand, exits $RATCHET_EXIT (`refs` prints the reference rows)
RATCHET = '''
root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0]))))
with open(os.path.join(root, "ratchet.jsonl"), "a") as f:
    f.write(json.dumps(argv) + "\\n")
if argv[0] == "refs":
    out("refs\\trows")
else:
    out(f"ratchet {argv[0]}")
exit(int(env.get("RATCHET_EXIT", "0")))
'''


class CouplingReportTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.root = self.path("root")
        os.makedirs(os.path.join(self.root, "scripts", "ci"))
        os.makedirs(os.path.join(self.root, "test", "resources"))
        for name in OWN:
            if os.path.exists(os.path.join(HERE, name)):
                shutil.copy2(os.path.join(HERE, name), os.path.join(self.root, "scripts", name))
        if os.path.exists(os.path.join(HERE, "coupling_report.py")):
            shutil.copytree(os.path.join(HERE, "lib"), os.path.join(self.root, "scripts", "lib"),
                            ignore=shutil.ignore_patterns("__pycache__", "*_test.py"))
        self.fake_at("scripts/sbt-warm", SBT_WARM)
        self.fake_at("scripts/ci/coupling_ratchet.py", RATCHET)

    def fake_at(self, relative: str, body: str) -> None:
        path = os.path.join(self.root, relative)
        shutil.copy2(self.fakes.program(os.path.basename(relative), body), path)

    def report(self, *args: str, **extra: str) -> cli_harness.Done:
        return self.run_tool(*args, env=dict(extra), tool=os.path.join(self.root, "scripts", "coupling-report"))

    def sbt_called(self) -> List[List[str]]:
        called = os.path.join(self.root, "called")
        if not os.path.exists(called):
            return []
        with open(called) as f:
            return [json.loads(line) for line in f]

    def ratchet_called(self) -> List[List[str]]:
        log = os.path.join(self.root, "ratchet.jsonl")
        if not os.path.exists(log):
            return []
        with open(log) as f:
            return [json.loads(line) for line in f]

    def test_the_report_is_printed_then_the_ratchet_decides(self) -> None:
        out = self.path("kept")
        done = self.report("--out", out, RATCHET_EXIT="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("# couplingReport report.md\n\nratchet check\n", done.stdout)
        files = [f"{out}/{name}" for name in ("rows.tsv", "report.md", "edges.tsv", "fanouts.tsv", "collaborators.tsv", "logic.tsv")]
        self.assertEqual([["couplingReport " + " ".join(files)]], self.sbt_called())
        self.assertEqual([["check", "--current", f"{out}/rows.tsv", "--edges", f"{out}/edges.tsv",
                           "--fanouts", f"{out}/fanouts.tsv", "--collaborators", f"{out}/collaborators.tsv"]],
                         self.ratchet_called())
        self.assertEqual(["collaborators.tsv", "edges.tsv", "fanouts.tsv", "logic.tsv", "report.md", "rows.tsv", "sbt.log"],
                         sorted(os.listdir(out)), "--out keeps every file")

    def test_a_relative_out_is_under_the_checkout(self) -> None:
        done = self.report("--out", "kept")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(os.path.isfile(os.path.join(self.root, "kept", "rows.tsv")))

    def test_without_out_the_files_are_removed(self) -> None:
        done = self.report()
        self.assertEqual(0, done.code, done.output)
        rows = self.ratchet_called()[0][2]
        self.assertFalse(os.path.exists(os.path.dirname(rows)), rows)

    def test_update_records_the_rows_and_the_reference_sets_whatever_the_check_says(self) -> None:
        done = self.report("--update", "--out", self.path("kept"), RATCHET_EXIT="1")
        self.assertEqual(1, done.code, "the update's own exit: the fake ratchet's update exits 1 too")
        done = self.report("--update", "--out", self.path("kept"))
        self.assertEqual(0, done.code, done.output)
        kept = self.path("kept")
        self.assertEqual(["update", "--current", f"{kept}/rows.tsv", "--baseline", "test/resources/coupling-baseline.tsv",
                          "--fanouts", f"{kept}/fanouts.tsv", "--collaborators", f"{kept}/collaborators.tsv"],
                         self.ratchet_called()[-2])
        self.assertEqual(["refs", "--current", f"{kept}/rows.tsv", "--fanouts", f"{kept}/fanouts.tsv"],
                         self.ratchet_called()[-1])
        with open(os.path.join(self.root, "test", "resources", "coupling-fanout-refs.tsv")) as f:
            self.assertEqual("refs\trows\n", f.read())
        self.assertTrue(done.stdout.endswith(
            "ratchet check\nratchet update\ncoupling-report: wrote test/resources/coupling-baseline.tsv, "
            "coupling-fanout-refs.tsv and coupling-collaborators.tsv -- a rise is committed with a `Coupling: <why>` trailer\n"), done.stdout)

    def test_update_exits_0_past_a_refusing_check(self) -> None:
        self.fake_at("scripts/ci/coupling_ratchet.py", RATCHET.replace(
            'exit(int(env.get("RATCHET_EXIT", "0")))', 'exit(1 if argv[0] == "check" else 0)'))
        done = self.report("--update")
        self.assertEqual(0, done.code, done.output)

    def test_check_split_hands_its_pieces_to_the_split_check_alone(self) -> None:
        kept = self.path("kept")
        done = self.report("--check-split", "a.X", "a.A", "a.B", "--base", "main~2", "--out", kept, RATCHET_EXIT="1")
        self.assertEqual((1, "ratchet split-check\n"), (done.code, done.stdout), done.output)
        self.assertEqual([["split-check", "--current", f"{kept}/rows.tsv", "--fanouts", f"{kept}/fanouts.tsv",
                           "--split", "a.X", "--into", "a.A", "--into", "a.B", "--base", "main~2"]],
                         self.ratchet_called())

    def test_check_split_needs_a_class_and_a_piece(self) -> None:
        for args, why in ((("--check-split", "a.X"), "--check-split <X> <piece>..."),
                          (("--check-split", "a.X", "--update"), "--check-split <X> needs at least one piece")):
            done = self.report(*args)
            self.assertEqual((2, f"coupling-report: {why}\n"), (done.code, done.stderr), args)
        self.assertEqual([], self.sbt_called())

    def test_new_exemptions_runs_no_sbt(self) -> None:
        done = self.report("--new-exemptions", "--since", "3d", RATCHET_EXIT="1")
        self.assertEqual((1, "ratchet new-exemptions\n"), (done.code, done.stdout))
        self.assertEqual([["new-exemptions", "--since", "3d"]], self.ratchet_called())
        self.report("--new-exemptions")
        self.assertEqual(["new-exemptions", "--since", "7d"], self.ratchet_called()[-1])
        self.assertEqual([], self.sbt_called())

    def test_simulate_prints_the_simulation_from_its_own_task(self) -> None:
        self.write("moves.tsv", "services.movies.Reading\tservices.evidence\n")
        done = self.report("--simulate", "moves.tsv", "--out", self.path("kept"))
        self.assertEqual((0, "# couplingSimulate simulation.md\n"), (done.code, done.stdout), done.output)
        self.assertEqual([[f"couplingSimulate {self.path('moves.tsv')} {self.path('kept')}/simulation.md"]],
                         self.sbt_called(), "the moves file's path absolute")
        self.assertEqual([], self.ratchet_called())

    def test_exemption_slack_prints_the_slack_from_its_own_task(self) -> None:
        done = self.report("--exemption-slack", "--out", self.path("kept"))
        self.assertEqual((0, "# couplingExemptionSlack slack.md\n"), (done.code, done.stdout), done.output)
        self.assertEqual([[f"couplingExemptionSlack {self.path('kept')}/slack.md"]], self.sbt_called())

    def test_simulate_without_an_existing_moves_file_is_exit_2(self) -> None:
        for args in (("--simulate",), ("--simulate", "nothing.tsv")):
            done = self.report(*args)
            self.assertEqual((2, "coupling-report: --simulate <moves.tsv> (an existing file)\n"),
                             (done.code, done.stderr), args)

    def test_a_failed_task_is_exit_2_its_log_shown(self) -> None:
        kept = self.path("kept")
        done = self.report("--exemption-slack", "--out", kept, SBT_FAILS="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(f"[error] boom\ncoupling-report: sbt couplingExemptionSlack failed (log: {kept}/sbt.log)\n",
                         done.stderr)
        done = self.report("--out", kept, SBT_FAILS="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(f"[error] boom\ncoupling-report: sbt couplingReport failed (log: {kept}/sbt.log)\n",
                         done.stderr)
        self.assertEqual([], self.ratchet_called())

    def test_a_task_writing_nothing_is_exit_2(self) -> None:
        self.write("moves.tsv", "a\tb\n")
        done = self.report("--simulate", "moves.tsv", SBT_SILENT="1")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("coupling-report: sbt couplingSimulate failed (log: ", done.stderr)

    def test_an_unknown_argument_is_exit_2(self) -> None:
        done = self.report("--fast")
        self.assertEqual((2, "coupling-report: unknown argument --fast\n"), (done.code, done.stderr))

    def test_another_checkout_is_refused(self) -> None:
        self.git(self.repo("root"), "add", "-A")
        other = self.repo("other")
        done = self.run_tool(tool=os.path.join(self.root, "scripts", "coupling-report"), cwd=other)
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(f"coupling-report: this copy measures {self.root}, its own checkout, but this is {other}: "
                         f"run {other}/scripts/coupling-report\n", done.stderr)
        self.assertEqual([], self.sbt_called())

    def test_help_prints_the_header(self) -> None:
        done = self.report("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("THE COUPLING REPORT (CLAUDE.md, Design rules)", done.stdout)
        self.assertIn("\n  scripts/coupling-report --simulate <moves.tsv>\n", done.stdout)


if __name__ == "__main__":
    unittest.main()
