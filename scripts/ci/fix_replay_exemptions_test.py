#!/usr/bin/env python3
"""fix_replay_exemptions.py: the week's `Fix-Replay: none — <why>` escape hatches (a gate red's fix agentctl ran no
replay of) are listed for the owner with their date, sha, the reds they fix and the reason; a `Fix-Replay: <command>`
is not one, nor a `Fix-Replay:` line outside the final trailer block (git reads it as prose)."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.path.join(HERE, "fix_replay_exemptions.py")

CO = "Co-Authored-By: A <a@b.c>"
REASON = "the hosted runner lost its disk mid-job (infra); no code path replays it"


class ExemptionsTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.commit("base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, message):
        with open(os.path.join(self.repo, "a.txt"), "a") as f:
            f.write("x\n")
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def report(self):
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, "--since", "7d"], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        return done.stdout

    def test_a_none_exemption_is_listed_with_its_reds_and_reason(self):
        sha = self.commit(f"web: the fix\n\nFixes-Red: 2600\nFix-Replay: none — {REASON}\n{CO}")
        out = self.report()
        self.assertIn(f" {sha[:9]} Fixes-Red: 2600: {REASON}\n", out)
        self.assertIn("  commit: web: the fix", out)

    def test_an_ascii_dash_counts_too(self):
        self.commit(f"web: the fix\n\nFixes-Red: 2600\nFix-Replay: none -- {REASON}\n{CO}")
        self.assertIn(f"Fixes-Red: 2600: {REASON}", self.report())

    def test_a_reason_folded_onto_a_continuation_line_is_listed_whole(self):
        head, tail = REASON.split(" ", 1)
        self.commit(f"web: the fix\n\nFixes-Red: 2600\nFix-Replay: none — {head}\n {tail}\n{CO}")
        self.assertIn(f"Fixes-Red: 2600: {REASON}\n", self.report())

    def test_a_replayed_fix_and_a_misplaced_line_are_not_exemptions(self):
        self.commit(f"web: replayed\n\nFixes-Red: 2601\nFix-Replay: sbt 'web/testOnly X'\n{CO}")
        self.commit(f"web: prose\n\nFix-Replay: none — {REASON}\n\nFixes-Red: 2602\n{CO}")
        out = self.report()
        self.assertIn("No `Fix-Replay: none` exemptions.", out)


if __name__ == "__main__":
    unittest.main()
