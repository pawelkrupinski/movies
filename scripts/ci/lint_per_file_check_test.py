#!/usr/bin/env python3
"""lint_per_file_check.py: a lint judging each file alone is PER-FILE however a file is changed, twinned or emptied; one
reading across files -- a name registered twice, an index naming the scripts that must exist -- is CROSS-FILE, and so
is one failing with no file named; every file it changed is put back. LINT_PER_FILE_CHECK_UNDER_TEST names another
copy of the tool."""

import importlib.util
import os
import subprocess
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("LINT_PER_FILE_CHECK_UNDER_TEST") or os.path.join(HERE, "lint_per_file_check.py")
spec = importlib.util.spec_from_file_location("lint_per_file_check", TOOL)
T = importlib.util.module_from_spec(spec)
spec.loader.exec_module(T)

FILES = {f"scripts/tool{i}.sh": f"echo tool{i}\n" for i in range(6)}
FILES.update({"scripts/index.txt": "tool0.sh\ntool1.sh\ntool2.sh\ntool3.sh\ntool4.sh\ntool5.sh\n"})
# each script alone: no line says `sleep`
PER_FILE = 'for f in scripts/*.sh; do grep -l sleep "$f"; done | grep . && exit 1; exit 0'
# no two scripts echo the same thing
UNIQUE = 'cat scripts/*.sh | sort | uniq -d | grep . && { ls scripts/*.sh; exit 1; }; exit 0'
# every script the index names exists and is non-empty
INDEX = 'while read -r s; do [ -s "scripts/$s" ] || { echo "scripts/$s is missing"; exit 1; }; done < scripts/index.txt; ls scripts > /dev/null'
SILENT = 'ls scripts > /dev/null; cat scripts/*.sh > /dev/null; [ "$(cat scripts/*.sh | wc -l)" -ge 6 ]'


# the check sees reads by atime (lint_reads_trace.atime_moves): where they do not move it refuses, and these skip
FILE_READS, DIRECTORY_LISTINGS = T.trace.atime_moves(tempfile.gettempdir())


@unittest.skipUnless(FILE_READS and DIRECTORY_LISTINGS,
                     "atimes do not move on this filesystem (noatime / a bind mount): the check refuses to run here")
class LintPerFileCheckTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        subprocess.run(["bash", "-c", '. "$1"; scratch_repo "$2" -q', "_", SCRATCH_GIT, self.repo], check=True)
        for path, text in FILES.items():
            os.makedirs(os.path.join(self.repo, os.path.dirname(path)), exist_ok=True)
            with open(os.path.join(self.repo, path), "w") as f:
                f.write(text)
        subprocess.run(["bash", "-c", '. "$1"; scratch_git "$2" add -A && scratch_git "$2" commit -qm base', "_",
                        SCRATCH_GIT, self.repo], check=True)
        self.cwd = os.getcwd()
        os.chdir(self.repo)

    def tearDown(self):
        os.chdir(self.cwd)
        self.dir.cleanup()
        os.environ.update(self.saved)

    def verdict(self, script):
        files, dirs = T.trace.tracked()
        return T.check_command(["sh", "-c", script], None, files + dirs, {}, 3)[0]

    def test_a_lint_judging_each_file_alone_is_per_file_and_every_file_is_put_back(self):
        self.assertEqual(self.verdict(PER_FILE), "PER-FILE")
        status = subprocess.run(["bash", "-c", '. "$1"; scratch_git "$2" status --porcelain', "_", SCRATCH_GIT, self.repo],
                                capture_output=True, text=True, check=True).stdout
        self.assertEqual(status, "", "every changed file put back, every twin removed")

    def test_a_lint_comparing_files_is_cross_file(self):
        self.assertEqual(self.verdict(UNIQUE), "CROSS-FILE")

    def test_a_lint_whose_index_names_other_files_is_cross_file(self):
        self.assertEqual(self.verdict(INDEX), "CROSS-FILE")

    def test_a_lint_failing_with_no_file_named_is_cross_file(self):
        self.assertEqual(self.verdict(SILENT), "CROSS-FILE")


if __name__ == "__main__":
    unittest.main()
