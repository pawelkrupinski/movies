#!/usr/bin/env python3
"""THE SPLIT CREDIT, end to end (design_check.py check over a repository git makes; splits.py's rule): a module split
into a leaf piece raises an importer's fan-out past the limit -- the commit carrying `Coupling: split of <X> into <A>`
passes, re-freezing the row; the same commit without the trailer fails; an importer that also gained an unrelated
collaborator fails although the commit carries the split trailer."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from typing import Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
CORE = os.environ.get("DESIGN_RULES_UNDER_TEST") or HERE
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
import cli_harness  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

CO = "Co-Authored-By: A <a@b.c>"
HELPERS = [f"h{n}" for n in range(8)]   # eight collaborators every importer uses: with core, at the limit of 9
CORE_BEFORE = "import subprocess\n\n\ndef run_git(a):\n    return subprocess.run(a)\n\n\ndef keep(a):\n    return subprocess.run(a)\n"
CORE_AFTER = "import subprocess\n\n\ndef keep(a):\n    return subprocess.run(a)\n"
GITREPO = "import subprocess\n\n\ndef run_git(a):\n    return subprocess.run(a)\n"
NET = "import subprocess\n\n\ndef get(u):\n    return subprocess.run(u)\n"


def user(*modules: str, calls: str) -> str:
    return "".join(f"import {m}\n" for m in [*HELPERS, *modules]) + "\n\ndef go():\n" + "".join(
        f"    {h}.f()\n" for h in HELPERS) + calls


class SplitCreditTest(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.root = os.path.join(self.dir.name, "repo")
        os.makedirs(os.path.join(self.root, "test", "resources"))
        shutil.copy(os.path.join(ROOT, "test/resources/design-rules.tsv"), os.path.join(self.root, "test/resources"))
        with open(os.path.join(self.root, "test/resources/design-rules.tsv"), "a") as f:
            f.write("adapter\tpy\tscripts/lib/gitrepo.py\tprocess\n")  # the new piece: the git leaf adapter
        self.env = cli_harness.isolated_env(os.environ)
        self.git = tool_git.Git(tool_process.SubprocessRunner(), self.env, self.root)
        self.git.text("init", "-q", "-b", "main")
        files = {f"scripts/lib/{h}.py": "import subprocess\n\n\ndef f():\n    return subprocess.run(['x'])\n" for h in HELPERS}
        files.update({"scripts/lib/core.py": CORE_BEFORE, "scripts/lib/net.py": NET,
                      "scripts/user.py": user("core", calls="    core.run_git(1)\n    core.keep(2)\n")})
        self.base = self.commit(files, "base")

    def design(self, *args: str) -> "subprocess.CompletedProcess[str]":
        return subprocess.run([sys.executable, "-B", os.path.join(CORE, "design_check.py"), *args, "--root", self.root],
                              env=self.env, capture_output=True, text=True, timeout=300)

    def commit(self, files: Mapping[str, str], message: str) -> str:
        for path, text in files.items():
            full = os.path.join(self.root, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w", encoding="utf-8") as f:
                f.write(text)
        done = self.design("update")
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)
        self.git.text("add", "-A")
        self.git.text("commit", "-q", "-F", "-", input=message)
        return self.git.text("rev-parse", "HEAD").strip()

    def check(self, head: str) -> "subprocess.CompletedProcess[str]":
        return self.design("check", "--languages", "py", "--range", f"{self.base}..{head}")

    def split(self, message: str, calls: str = "    gitrepo.run_git(1)\n    core.keep(2)\n", *extra: str) -> str:
        return self.commit({"scripts/lib/core.py": CORE_AFTER, "scripts/lib/gitrepo.py": GITREPO,
                            "scripts/user.py": user("core", "gitrepo", *extra, calls=calls)}, message)

    def test_the_split_trailer_admits_and_refreezes_the_importers_rise(self) -> None:
        head = self.split(f"split core\n\nCoupling: split of scripts/lib/core.py into scripts/lib/gitrepo.py\n{CO}\n")
        done = self.check(head)
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)
        with open(os.path.join(self.root, "test/resources/design-baseline.tsv")) as f:
            self.assertIn("fanout\tscripts/user.py\t10\n", f.read())

    def test_the_same_split_without_the_trailer_fails(self) -> None:
        done = self.check(self.split(f"split core\n\n{CO}\n"))
        self.assertEqual(1, done.returncode, done.stdout + done.stderr)
        self.assertIn("fanout scripts/user.py raised <=9 -> 10", done.stdout)

    def test_an_importer_gaining_an_unrelated_collaborator_fails_under_the_split_trailer(self) -> None:
        head = self.split(f"split core\n\nCoupling: split of scripts/lib/core.py into scripts/lib/gitrepo.py\n{CO}\n",
                          "    gitrepo.run_git(1)\n    core.keep(2)\n    net.get(3)\n", "net")
        done = self.check(head)
        self.assertEqual(1, done.returncode, done.stdout + done.stderr)
        self.assertIn("scripts/user.py gained scripts/lib/net.py, no piece of scripts/lib/core.py", done.stdout)


if __name__ == "__main__":
    unittest.main()
