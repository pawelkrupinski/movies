#!/usr/bin/env python3
"""linux-tree-sync.py: a checkout's files copied into one stable tree, the copy kept in step with it.

    python3 scripts/linux_tree_sync_test.py
"""
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT = pathlib.Path(__file__).with_name("linux-tree-sync.py")


def git(repo, *args):
    subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True,
                   env={**os.environ, "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"})


def sync(root, dest):
    return subprocess.run([sys.executable, str(SCRIPT), str(root), str(dest)], capture_output=True, text=True, timeout=60)


def files(tree):
    return sorted(str(p.relative_to(tree)) for p in tree.rglob("*") if p.is_file() or p.is_symlink())


class LinuxTreeSyncTest(unittest.TestCase):
    def setUp(self):
        self.tmp = pathlib.Path(tempfile.mkdtemp())
        self.repo, self.dest = self.tmp / "repo", self.tmp / "slot"
        self.repo.mkdir()
        git(self.repo, "init", "-q")
        git(self.repo, "config", "user.email", "t@t"), git(self.repo, "config", "user.name", "t")
        (self.repo / "scripts").mkdir()
        (self.repo / "scripts/a.sh").write_text("echo a\n")
        (self.repo / "scripts/a.sh").chmod(0o755)
        (self.repo / "test/resources/fixtures").mkdir(parents=True)
        (self.repo / "test/resources/fixtures/f.json").write_text("{}\n")
        (self.repo / "gone.txt").write_text("gone\n")
        os.symlink("scripts/a.sh", self.repo / "link")
        (self.repo / ".gitignore").write_text("target/\n")
        git(self.repo, "add", "-A"), git(self.repo, "commit", "-qm", "base")

    def tearDown(self):
        subprocess.run(["rm", "-rf", str(self.tmp)], check=True)

    def test_copies_tracked_and_untracked_files_but_not_ignored_ones_or_the_git_dir(self):
        (self.repo / "new.txt").write_text("untracked\n")
        (self.repo / "target").mkdir()
        (self.repo / "target/big.class").write_text("ignored\n")
        result = sync(self.repo, self.dest)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual([".gitignore", "gone.txt", "link", "new.txt", "scripts/a.sh", "test/resources/fixtures/f.json"],
                         files(self.dest))
        self.assertTrue((self.dest / ".git").is_dir(), "the git dir's mount point")
        self.assertEqual([], list((self.dest / ".git").iterdir()), "nothing of the git dir itself")
        self.assertEqual("scripts/a.sh", os.readlink(self.dest / "link"))
        self.assertTrue(os.access(self.dest / "scripts/a.sh", os.X_OK), "the mode kept")

    def test_a_second_sync_keeps_unchanged_files_and_follows_edits_and_deletions(self):
        sync(self.repo, self.dest)
        (self.repo / "scripts/a.sh").write_text("echo changed, longer\n")
        (self.repo / "gone.txt").unlink()                       # deleted in the working tree, still tracked
        git(self.repo, "rm", "-q", "-r", "test/resources/fixtures/f.json")
        (self.repo / "test/resources").mkdir(parents=True, exist_ok=True)   # git rm took the emptied directories
        (self.repo / "test/resources/other.json").write_text("[]\n")
        result = sync(self.repo, self.dest)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual([".gitignore", "link", "scripts/a.sh", "test/resources/other.json"], files(self.dest))
        self.assertEqual("echo changed, longer\n", (self.dest / "scripts/a.sh").read_text())
        self.assertFalse((self.dest / "test/resources/fixtures").exists(), "an emptied directory removed")
        git(self.repo, "checkout", "-q", "HEAD", "--", "test/resources/fixtures/f.json")
        os.utime(self.repo / "test/resources/fixtures/f.json", ns=(1, 1))
        sync(self.repo, self.dest)
        again = (self.dest / "test/resources/fixtures/f.json").stat()
        self.assertEqual(1, again.st_mtime_ns, "the source's mtime kept")
        sync(self.repo, self.dest)
        self.assertEqual(again.st_ino, (self.dest / "test/resources/fixtures/f.json").stat().st_ino,
                         "an unchanged file is not written again")

    def test_a_worktree_brings_its_git_file(self):
        wt = self.tmp / "wt"
        git(self.repo, "worktree", "add", "-q", "-b", "wt", str(wt))
        result = sync(wt, self.dest)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue((self.dest / ".git").is_file())
        self.assertEqual((wt / ".git").read_text(), (self.dest / ".git").read_text())
        sync(self.repo, self.dest)                              # the main checkout after it: a directory again
        self.assertTrue((self.dest / ".git").is_dir())

    def test_a_path_that_turned_from_a_file_into_a_directory(self):
        (self.dest / "scripts").mkdir(parents=True)
        (self.dest / "gone.txt").mkdir()
        (self.dest / "gone.txt/stale").write_text("x")
        (self.dest / "scripts/a.sh").mkdir()
        result = sync(self.repo, self.dest)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual("gone\n", (self.dest / "gone.txt").read_text())
        self.assertEqual("echo a\n", (self.dest / "scripts/a.sh").read_text())

    def test_not_a_checkout_fails(self):
        result = sync(self.tmp, self.dest)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("not a git checkout", result.stderr)


if __name__ == "__main__":
    unittest.main()
