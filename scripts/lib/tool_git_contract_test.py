#!/usr/bin/env python3
"""THE GitRepo CONTRACT (tool_git.py's seam; CLAUDE.md, "Side effects live at the leaves"): one suite of assertions held
by both implementations -- SubprocessGitRepo against a temporary repository git itself makes (the integration leg),
InMemoryGitRepo over its commit graph (what every tool's unit tests then run on). A case only the fake passes would
let a tool's unit tests pass while the tool is broken against git; one only git passes, the fake is wrong."""
from __future__ import annotations

import os
import sys
import tempfile
import unittest
from typing import Dict, List, Mapping, Optional, Protocol

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from harness import cli_harness  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

CO = "Co-Authored-By: A <a@b.c>"


class Builder(Protocol):
    @property
    def root(self) -> str: ...

    @property
    def repo(self) -> tool_git.GitRepo: ...

    def commit(self, changes: Mapping[str, Optional[str]], message: str) -> str: ...

    def branch(self, name: str, at: str) -> None: ...

    def checkout(self, name: str) -> None: ...

    def add_worktree(self, name: str, branch: str) -> None: ...


class RealBuilder:
    """Commits to a temporary repository through git (an isolated environment: no developer config, no hook's GIT_DIR)."""

    def __init__(self, root: str) -> None:
        self.root = root
        self.git = tool_git.Git(tool_process.SubprocessRunner(), cli_harness.isolated_env(os.environ), root)
        self.git.text("init", "-q", "-b", "main")
        self.repo = tool_git.SubprocessGitRepo(self.git)

    def commit(self, changes: Mapping[str, Optional[str]], message: str) -> str:
        for path, text in changes.items():
            full = os.path.join(self.root, path)
            if text is None:
                os.remove(full)
                continue
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w", encoding="utf-8") as f:
                f.write(text)
        self.git.text("add", "-A")
        self.git.text("commit", "-q", "--allow-empty", "--cleanup=strip", "-F", "-", input=message)
        return self.git.text("rev-parse", "HEAD").strip()

    def branch(self, name: str, at: str) -> None:
        self.git.text("branch", name, at)

    def checkout(self, name: str) -> None:
        self.git.text("checkout", "-q", name)

    def add_worktree(self, name: str, branch: str) -> None:
        self.git.text("worktree", "add", "-q", os.path.join(os.path.dirname(self.root), name), branch)


class MemoryBuilder:
    def __init__(self, root: str) -> None:
        self.root = root
        self.repo = tool_git.InMemoryGitRepo(root)

    def commit(self, changes: Mapping[str, Optional[str]], message: str) -> str:
        return self.repo.commit(changes, message)

    def branch(self, name: str, at: str) -> None:
        self.repo.branch(name, at)

    def checkout(self, name: str) -> None:
        self.repo.checkout(name)

    def add_worktree(self, name: str, branch: str) -> None:
        self.repo.add_worktree(os.path.join(os.path.dirname(self.root), name), branch)


class Contract(unittest.TestCase):
    """The assertions; a subclass names the builder. A linear history of four commits, then a side branch."""

    def builder(self, root: str) -> Builder:
        raise unittest.SkipTest("the contract itself: its implementations' subclasses run it")

    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.b = self.builder(os.path.join(os.path.realpath(self.dir.name), "repo"))
        os.makedirs(self.b.root, exist_ok=True)
        self.repo = self.b.repo
        self.c: Dict[str, str] = {}
        self.c["root"] = self.b.commit({"a.txt": "a\n", "lib/x.py": "x\n"}, "root\n")
        self.c["two"] = self.b.commit({"a.txt": "b\n", "b.txt": "b\n"}, "two: change a, add b\n\nWhy it is.\n")
        self.c["three"] = self.b.commit({"lib/x.py": None, "lib/y.py": "y\n"}, f"three\n\nCoupling: split of X\n{CO}\n")
        self.c["four"] = self.b.commit({"a.txt": "c\n"}, f"four\n\nLength: why\n\n{CO}\n")

    def test_resolve_reads_head_a_branch_a_prefix_and_parent_steps(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual(c["four"], r.resolve("HEAD"))
        self.assertEqual(c["four"], r.resolve("main"))
        self.assertEqual(c["two"], r.resolve(c["two"][:9]))
        self.assertEqual(c["three"], r.resolve("HEAD^"))
        self.assertEqual(c["two"], r.resolve(f"{c['four']}~2"))
        self.assertIsNone(r.resolve(f"{c['root']}^"))
        self.assertIsNone(r.resolve("no-such-branch"))

    def test_commits_of_a_range_newest_first_reversed_and_by_path(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual([c["four"], c["three"], c["two"]], r.commits(f"{c['root']}..{c['four']}"))
        self.assertEqual([c["two"], c["three"], c["four"]], r.commits(f"{c['root']}..", reverse=True))
        self.assertEqual([c["four"], c["two"]], r.commits(f"{c['root']}..HEAD", paths=["a.txt"]))
        self.assertEqual([c["three"], c["root"]], r.commits("HEAD", paths=["lib"]))
        self.assertEqual([], r.commits(f"{c['four']}..{c['two']}"))

    def test_a_message_its_subject_and_the_trailers_git_reads_from_its_final_block(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual("two: change a, add b\n\nWhy it is.\n", r.message(c["two"]))
        self.assertEqual("two: change a, add b", r.subject(c["two"]))
        self.assertEqual([("Coupling", "split of X"), ("Co-Authored-By", "A <a@b.c>")], r.trailers(c["three"]))
        self.assertEqual([("Co-Authored-By", "A <a@b.c>")], r.trailers(c["four"]))  # Length: is a paragraph of its own
        self.assertEqual(["split of X"], r.trailer_values(c["three"], "coupling"))
        self.assertEqual([], r.trailers(c["root"]))

    def test_trailers_parsed_from_any_message_as_git_reads_them(self) -> None:
        parse = self.repo.parse_trailers
        self.assertEqual([("Coupling", "exempt X — a reason continued here"), ("Co-Authored-By", "A <a@b.c>")],
                         parse(f"s\n\nCoupling: exempt X — a reason\n continued here\n{CO}\n"))
        self.assertEqual([], parse("s\n\nThis is prose, long.\nMore prose here.\nAnd more prose.\nLength: why\n"))
        self.assertEqual([("Signed-off-by", "A"), ("Length", "why")],
                         parse("s\n\nprose one\nprose two\nSigned-off-by: A\nLength: why\n"))
        self.assertEqual([], parse(f"Coupling: in the subject\n{CO}\n"))
        self.assertEqual([("Co-Authored-By", "A <a@b.c> Complexity: why")], parse(f"s\n\n{CO}\n  Complexity: why\n"))

    def test_changed_between_two_commits_sorted_and_filtered(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual(["a.txt", "b.txt", "lib/x.py", "lib/y.py"], r.changed(c["root"], c["four"]))
        self.assertEqual(["b.txt", "lib/y.py"], r.changed(c["root"], c["four"], "A"))
        self.assertEqual(["a.txt", "b.txt", "lib/y.py"], r.changed(c["root"], "HEAD", "AMCR"))
        self.assertEqual(["lib/x.py"], r.changed(c["root"], "HEAD", "D"))
        self.assertEqual([], r.changed("HEAD", c["four"]))

    def test_a_commits_own_change_and_a_root_commits_every_file(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual(["a.txt", "lib/x.py"], r.changed_in(c["root"]))
        self.assertEqual(["lib/x.py", "lib/y.py"], r.changed_in(c["three"]))
        self.assertEqual(["lib/y.py"], r.changed_in(c["three"], "AM"))

    def test_merge_base_and_ancestry_across_a_side_branch(self) -> None:
        r, c = self.repo, self.c
        self.b.branch("side", c["two"])
        self.b.checkout("side")
        side = self.b.commit({"s.txt": "s\n"}, "side\n")
        self.assertEqual(c["two"], r.merge_base(side, c["four"]))
        self.assertEqual(c["two"], r.merge_base("side", "main"))
        self.assertTrue(r.is_ancestor(c["two"], side))
        self.assertFalse(r.is_ancestor(c["three"], side))
        self.assertFalse(r.is_ancestor("no-such-branch", side))
        self.assertEqual([side], r.commits("main..side"))

    def test_show_and_exists_read_a_file_at_a_revision(self) -> None:
        r, c = self.repo, self.c
        self.assertEqual("b\n", r.show(c["two"], "a.txt"))
        self.assertEqual("c\n", r.show("HEAD", "a.txt"))
        self.assertIsNone(r.show(c["root"], "b.txt"))
        self.assertIsNone(r.show("no-such-branch", "a.txt"))
        self.assertTrue(r.exists("HEAD", "lib/y.py"))
        self.assertTrue(r.exists("HEAD", "lib"))
        self.assertFalse(r.exists("HEAD", "lib/x.py"))
        self.assertFalse(r.exists(f"{c['root']}^", "a.txt"))

    def test_worktrees_the_main_one_and_one_added_on_a_branch(self) -> None:
        self.b.branch("side", self.c["two"])
        self.b.add_worktree("other", "side")
        found: List[tool_git.Worktree] = self.repo.worktrees()
        self.assertEqual([("repo", self.c["four"], "main"), ("other", self.c["two"], "side")],
                         [(os.path.basename(w.path), w.head, w.branch) for w in found])


class SubprocessGitRepoContractTest(Contract):
    def builder(self, root: str) -> Builder:
        os.makedirs(root, exist_ok=True)
        return RealBuilder(root)


class InMemoryGitRepoContractTest(Contract):
    def builder(self, root: str) -> Builder:
        return MemoryBuilder(root)


class SharedReadingsTest(unittest.TestCase):
    def test_a_range_reads_as_git_does(self) -> None:
        self.assertEqual(("a", "b"), tool_git.split_range("a..b"))
        self.assertEqual(("a", "HEAD"), tool_git.split_range("a.."))
        self.assertEqual((None, "b"), tool_git.split_range("b"))

    def test_the_worktree_porcelain_parses_a_branch_and_a_detached_head(self) -> None:
        porcelain = "worktree /r\nHEAD abc\nbranch refs/heads/main\n\nworktree /w\nHEAD def\ndetached\n"
        self.assertEqual([tool_git.Worktree("/r", "abc", "main"), tool_git.Worktree("/w", "def", None)],
                         tool_git.parse_worktrees(porcelain))


if __name__ == "__main__":
    unittest.main()
