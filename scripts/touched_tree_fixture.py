"""The fixture scripts/coverage-touched's and scripts/mutation-check's black-box tests share (coverage_touched_test.py,
mutation_check_test.py): a scratch repository whose main~1 edits a scraper source's methods and main a doc only, the
stub sbt (scripts/touched-tree-stub.sh, KINOWO_TOUCHED_SBT) answering from fixtures, and -- for the INT/TERM cases --
a scratch copy of the tools whose own scripts/sbt-warm is a fake that logs `--stop` and can hang on a command. Ported
from the two shell tests' preambles; the same cases hold the shell tools and their Python rewrites
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
from typing import Dict, List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
SOURCE = "scrapers/src/main/scala/services/cinemas/Genres.scala"
GENRES = """package services.cinemas

object Genres {
  def parse(text: String): Seq[String] =
    text.split(',').map(_.trim).toSeq

  def label(genre: String): String = genre.capitalize
}
"""
SPANS = f"{SOURCE}\t{SOURCE}:Genres.parse\t4\t5\n{SOURCE}\t{SOURCE}:Genres.label\t7\t7\n"
# What a scratch copy of the tools holds (each file there, of either version): the tools, their libraries, the
# overlay laid over the worktree, and the stub sbt
COPIED = ("scripts/coverage-touched", "scripts/mutation-check", "scripts/coverage_touched.py",
          "scripts/mutation_check.py", "scripts/touched-tree.sh", "scripts/tool-conventions.sh",
          "scripts/touched_methods.py", "scripts/touched-tree-stub.sh", "project/refactor-tools.sbt",
          "refactor-tools.sbt", "project/AnalysisUnits.scala", "project/TouchedSpecs.scala",
          "project/UsedNamesAccess.scala")
# The copy's sbt-warm: `--stop` logged, a command holding $HANG_ON hangs (its pid in $HANG_PID), the rest the stub's
SBT_WARM = """#!/usr/bin/env bash
echo "$PWD|$*" >> "$WARM_CALLS"
[ "${1:-}" = --stop ] && exit 0
case "$*" in *"${HANG_ON:-<none>}"*) echo $$ > "$HANG_PID"; sleep 600; exit 0 ;; esac
exec "$(dirname "$0")/touched-tree-stub.sh" "$@"
"""
GOV_RUN = """#!/bin/sh
while [ "$1" != -- ]; do shift; done
shift
exec "$@"
"""


class TouchedTreeCase(cli_harness.ToolCase):
    """A case over the scratch repository `self.scratch` (main~2 base, main~1 the methods' edit, main docs only)."""
    NAME = ""  # the tool's name: scripts/<NAME>
    PYTHON = ""  # its Python rewrite, scripts/<PYTHON>
    EDITS: Tuple[Tuple[str, str], ...] = ()  # what main~1 replaces in the source

    def setUp(self) -> None:
        super().setUp()
        self.TOOL = cli_harness.tool_path(f"scripts/{self.PYTHON}", f"scripts/{self.NAME}")
        self.scratch = self.repo("repo", "-b", "main")
        self.write(f"repo/{SOURCE}", GENRES)
        self.write("repo/docs/notes.md", "# notes\n")
        os.makedirs(self.path("repo/project"), exist_ok=True)
        self.git(self.scratch, "add", "-A")
        self.git(self.scratch, "commit", "-qm", "base")
        edited = GENRES
        for before, after in self.EDITS:
            edited = edited.replace(before, after)
        self.write(f"repo/{SOURCE}", edited)
        self.git(self.scratch, "commit", "-qam", "edit the genres")
        self.write("repo/docs/notes.md", "# notes\nmore\n")
        self.git(self.scratch, "commit", "-qam", "docs only")
        self.write("spans.tsv", SPANS)
        self.write("calls", "")
        os.makedirs(self.path("tmp"))

    def stub_env(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, Optional[str]]:
        env: Dict[str, Optional[str]] = {
            "KINOWO_TOUCHED_REPO": self.scratch, "KINOWO_TOUCHED_TREE_DIR": self.path("trees"),
            "KINOWO_TOUCHED_SBT": os.path.join(ROOT, "scripts/touched-tree-stub.sh"), "TMPDIR": self.path("tmp"),
            "STUB_CALLS": self.path("calls"), "STUB_SPANS": self.path("spans.tsv"),
            "STUB_SPECS": self.path("specs.txt"), "STUB_SOURCE": SOURCE}
        env.update(extra or {})
        return env

    def run_in_repo(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None) -> cli_harness.Done:
        return self.run_tool(*args, env=self.stub_env(env), cwd=self.scratch)

    def stub_calls(self) -> List[str]:
        return [line for line in self.read("calls").splitlines() if line]

    def trees_left(self) -> List[str]:
        return os.listdir(self.path("trees")) if os.path.isdir(self.path("trees")) else []

    def assert_help(self, heading: str) -> None:
        done = self.run_tool("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn(heading, done.stdout.splitlines())

    # ── INT / TERM: the scratch copy of the tools, its sbt-warm hanging on a command ───────────────────────────

    def tools_copy(self) -> str:
        tools = self.path("tools")
        for relative in COPIED:
            if os.path.exists(os.path.join(ROOT, relative)):
                os.makedirs(os.path.dirname(os.path.join(tools, relative)), exist_ok=True)
                shutil.copy(os.path.join(ROOT, relative), os.path.join(tools, relative))
        shutil.copytree(os.path.join(ROOT, "scripts/lib"), os.path.join(tools, "scripts/lib"),
                        ignore=shutil.ignore_patterns("__pycache__"))
        for name, body in (("sbt-warm", SBT_WARM), ("gov-run", GOV_RUN)):
            with open(os.path.join(tools, "scripts", name), "w") as f:
                f.write(body)
            os.chmod(os.path.join(tools, "scripts", name), 0o755)
        return tools

    def assert_signal_cleans_up(self, sig: signal.Signals, hang_on: str, *args: str) -> None:
        """The tool, hung in `hang_on`'s sbt command, signalled with `sig` (its whole group, as a terminal's Ctrl-C
        is): it exits 130 with its worktree removed, its sbt server stopped and its temporary directory gone."""
        tools = self.tools_copy()
        env = self.env(self.stub_env({"KINOWO_TOUCHED_SBT": None, "WARM_CALLS": self.path("warm"),
                                      "HANG_ON": hang_on, "HANG_PID": self.path("hang.pid")}))
        tool = os.path.join(tools, os.path.relpath(self.TOOL, ROOT))
        proc = subprocess.Popen([tool, *args], env=env, cwd=self.scratch, start_new_session=True,
                                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        try:
            self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("hang.pid"))
                                                   and os.path.getsize(self.path("hang.pid")) > 0))
            os.killpg(proc.pid, sig)
            out, _ = proc.communicate(timeout=cli_harness.WAIT_BOUND)
        finally:
            if proc.poll() is None:
                os.killpg(proc.pid, signal.SIGKILL)
                proc.communicate()
        output = out.decode(errors="replace")
        self.assertEqual(130, proc.returncode, output)
        self.assertEqual([], self.trees_left(), output)
        self.assertEqual(1, len(self.git(self.scratch, "worktree", "list").splitlines()))
        self.assertEqual([], os.listdir(self.path("tmp")), output)
        self.assertTrue(any(line.endswith("|--stop") for line in self.read("warm").splitlines()), self.read("warm"))
        self.assertEqual(0, cli_harness.still_running(self.path("hang.pid")))
