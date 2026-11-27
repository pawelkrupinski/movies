"""GIT, the one way the Python tools run it (scripts/lib): in one repository (`git -C <root>`), each call through the
injected runner, optionally bounded, its stderr passed on unless asked to be quiet.

Every call acts on `root`, whatever repository a calling hook's GIT_DIR / GIT_WORK_TREE point at (a hook exports them
for ITS repository, so a bare `git -C <other>` reads the hook's), and prints a path as itself (`core.quotePath=false`:
`drift-slices/pl/...przemyśl...` is not a quoted octal escape that no prefix test matches).

  Git(runner, env, root).out("rev-parse", "HEAD") -> stdout, or None when git failed
  .line("rev-parse", "--verify", x)               -> stdout stripped, or None when git failed (its stderr unprinted)
  .lines("ls-files", "--others")                  -> its stdout lines, [] when git failed (its stderr unprinted)
  .ok("diff", "--quiet", a, b)                    -> whether git exited 0
  .text("show", "HEAD:x", ok=(0, 128))           -> stdout, or raises Failed outside `ok` (git's stderr unprinted)
  Git.here(root)                                  -> over this process's one subprocess runner and its environment

THE GIT REPOSITORY SEAM (CLAUDE.md, "Side effects live at the leaves"): a tool's decisions over a repository take a
`GitRepo` -- the operations the tools ask of git -- never a runner: `SubprocessGitRepo(git)` asks git itself,
`InMemoryGitRepo()` holds a tiny commit graph (files, trees, messages, branches, worktrees) for unit tests. Both share
what is no git's to decide (a range's reading, a message's normal form, the trailers of a commit read from its message,
a subject) and are held alike by tool_git_contract_test.py: the real one against temporary repositories, the fake in
memory, the same assertions.

  repo.resolve(rev) -> sha or None                 `rev^{commit}` (`HEAD`, a branch, `x^`, `x~2`, a prefix)
  repo.commits(rng, paths=(), reverse=False, first_parent=False) -> [sha]   `rev-list`, newest first
  repo.message(sha) / .subject(sha) / .trailers(sha) / .parse_trailers(message) -> git's reading of the final block
  repo.changed(base, head, diff_filter="") -> [path]  `diff --name-only --no-renames`, sorted
  repo.changed_in(sha, diff_filter="") -> [path]      its own change against its first parent (`diff-tree --root`)
  repo.merge_base(a, b) -> sha or None / repo.is_ancestor(a, b) -> bool
  repo.show(rev, path) -> text or None / repo.exists(rev, path) -> bool (a file or a directory)
  repo.worktrees() -> [Worktree(path, head, branch)]"""
from __future__ import annotations

import dataclasses
import functools
import hashlib
import os
import re
import subprocess
import sys
from typing import BinaryIO, Dict, List, Mapping, Optional, Protocol, Sequence, Set, Tuple, cast

import tool_process

HOOK_REPOSITORY = ("GIT_DIR", "GIT_WORK_TREE")


class Failed(subprocess.CalledProcessError):
    """A git that exited outside the codes its caller expects, said `git <args>: <its stderr, or its exit code>`."""

    def __str__(self) -> str:
        return f"{' '.join(self.cmd)}: {(self.stderr or '').strip() or self.returncode}"


@functools.lru_cache(maxsize=None)
def shared_runner() -> tool_process.SubprocessRunner:
    """One runner a process (each installs its signal handlers): git only captures, so the stdout it would tee to is
    the process's own whatever a caller swapped sys.stdout for."""
    out = sys.__stdout__
    return tool_process.SubprocessRunner(stdout=cast(BinaryIO, out.buffer) if out is not None else None)


class Git:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], root: str) -> None:
        self.runner = runner
        self.env = {k: v for k, v in env.items() if k not in HOOK_REPOSITORY}
        self.root = root

    @classmethod
    def here(cls, root: str = ".") -> "Git":
        return cls(shared_runner(), os.environ, root)

    def call(self, *args: str, timeout: Optional[float] = None, quiet: bool = False,
             options: Sequence[str] = (), input: Optional[str] = None) -> tool_process.Captured:
        """`git [options] -C <root> args`: `options` (`-c key=value`) before the repository."""
        argv = ["git", "-c", "core.quotePath=false", *options, "-C", self.root, *args]
        done = self.runner.capture(tool_process.Command(argv, self.env, timeout=timeout, input=input))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def out(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> Optional[str]:
        done = self.call(*args, timeout=timeout, quiet=quiet)
        return done.stdout if done.code == 0 else None

    def line(self, *args: str, timeout: Optional[float] = None) -> Optional[str]:
        """The stripped stdout of a git that succeeded (`rev-parse --verify`), None when it failed; its stderr unprinted."""
        out = self.out(*args, timeout=timeout, quiet=True)
        return None if out is None else out.strip()

    def lines(self, *args: str, timeout: Optional[float] = None) -> List[str]:
        """The lines of a git that succeeded (`ls-files`, `diff --name-only`); none when it failed -- its stderr unprinted."""
        return (self.out(*args, timeout=timeout, quiet=True) or "").splitlines()

    def ok(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> bool:
        return self.call(*args, timeout=timeout, quiet=quiet).code == 0

    def text(self, *args: str, ok: Sequence[int] = (0,), input: Optional[str] = None) -> str:
        done = self.call(*args, quiet=True, input=input)
        if done.code not in ok:
            raise Failed(done.code, ["git", *args], done.stdout, done.stderr)
        return done.stdout


# ---------------------------------------------------------------------------------------------------------------- GitRepo

Trailer = Tuple[str, str]


@dataclasses.dataclass(frozen=True)
class Worktree:
    path: str
    head: Optional[str]          # None for a worktree on an unborn branch
    branch: Optional[str]        # its branch's short name, None when detached


def split_range(rng: str) -> Tuple[Optional[str], str]:
    """`A..B` -> (A, B), `A..` -> (A, HEAD), `B` -> (None, B): the commits B reaches that A does not."""
    if ".." not in rng:
        return None, rng
    base, head = rng.split("..", 1)
    return base or "HEAD", head or "HEAD"


def normal_message(message: str) -> str:
    """A commit message as git stores and `%B` prints it: trailing blank lines dropped, one newline at its end."""
    return message.rstrip() + "\n"


def trailer_pairs(lines: Sequence[str]) -> List[Trailer]:
    """`Key: value` lines (interpret-trailers --parse's output) as (key, value) pairs, both stripped."""
    out: List[Trailer] = []
    for line in lines:
        key, sep, value = line.partition(":")
        if sep:
            out.append((key.strip(), value.strip()))
    return out


class GitRepo(Protocol):
    """The git operations a tool's decisions use; the shared readings are the methods with a body."""

    def resolve(self, rev: str) -> Optional[str]: ...

    def commits(self, rng: str, paths: Sequence[str] = (), reverse: bool = False,
                first_parent: bool = False) -> List[str]: ...

    def message(self, sha: str) -> str: ...

    def parse_trailers(self, message: str) -> List[Trailer]: ...

    def changed(self, base: str, head: str, diff_filter: str = "") -> List[str]: ...

    def changed_in(self, sha: str, diff_filter: str = "") -> List[str]: ...

    def merge_base(self, a: str, b: str) -> Optional[str]: ...

    def is_ancestor(self, a: str, b: str) -> bool: ...

    def show(self, rev: str, path: str) -> Optional[str]: ...

    def exists(self, rev: str, path: str) -> bool: ...

    def worktrees(self) -> List[Worktree]: ...

    def subject(self, sha: str) -> str:
        return self.message(sha).split("\n", 1)[0].strip()

    def trailers(self, sha: str) -> List[Trailer]:
        """The trailers git reads from the commit's message: its final trailer block only."""
        return self.parse_trailers(self.message(sha))

    def trailer_values(self, sha: str, key: str) -> List[str]:
        return [v for k, v in self.trailers(sha) if k.lower() == key.lower()]


def parse_worktrees(porcelain: str) -> List[Worktree]:
    """`git worktree list --porcelain`'s stanzas."""
    out: List[Worktree] = []
    for stanza in porcelain.strip().split("\n\n"):
        fields = dict((line.split(" ", 1) + [""])[:2] for line in stanza.splitlines() if line)
        if "worktree" in fields:
            branch = fields.get("branch", "")
            out.append(Worktree(fields["worktree"], fields.get("HEAD") or None,
                                branch[len("refs/heads/"):] if branch.startswith("refs/heads/") else None))
    return out


class SubprocessGitRepo(GitRepo):
    """GitRepo asking git itself, through one `Git` (its runner, environment and repository)."""

    def __init__(self, git: Git) -> None:
        self.git = git

    @classmethod
    def here(cls, root: str = ".") -> "SubprocessGitRepo":
        return cls(Git.here(root))

    def resolve(self, rev: str) -> Optional[str]:
        return self.git.line("rev-parse", "--verify", "-q", f"{rev}^{{commit}}")

    def commits(self, rng: str, paths: Sequence[str] = (), reverse: bool = False,
                first_parent: bool = False) -> List[str]:
        flags = [*(["--reverse"] if reverse else []), *(["--first-parent"] if first_parent else [])]
        return self.git.text("rev-list", *flags, rng, *(["--", *paths] if paths else [])).split()

    def message(self, sha: str) -> str:
        return normal_message(self.git.text("log", "-1", "--format=%B", sha))

    def parse_trailers(self, message: str) -> List[Trailer]:
        return trailer_pairs(self.git.text("interpret-trailers", "--parse", input=message).splitlines())

    def changed(self, base: str, head: str, diff_filter: str = "") -> List[str]:
        flags = [f"--diff-filter={diff_filter}"] if diff_filter else []
        return self.git.text("diff", "--name-only", "--no-renames", *flags, base, head).splitlines()

    def changed_in(self, sha: str, diff_filter: str = "") -> List[str]:
        flags = [f"--diff-filter={diff_filter}"] if diff_filter else []
        return self.git.text("diff-tree", "--no-commit-id", "--name-only", "--no-renames", "-r", "--root", *flags,
                             sha).splitlines()

    def merge_base(self, a: str, b: str) -> Optional[str]:
        return self.git.line("merge-base", a, b) or None

    def is_ancestor(self, a: str, b: str) -> bool:
        return self.git.ok("merge-base", "--is-ancestor", a, b, quiet=True)

    def show(self, rev: str, path: str) -> Optional[str]:
        return self.git.out("show", f"{rev}:{path}", quiet=True)

    def exists(self, rev: str, path: str) -> bool:
        return self.git.ok("cat-file", "-e", f"{rev}:{path}", quiet=True)

    def worktrees(self) -> List[Worktree]:
        return parse_worktrees(self.git.text("worktree", "list", "--porcelain"))


# git's trailer block (trailer.c, find_trailer_block_start): the last paragraph after the title, all of whose lines are
# trailers or continuations -- or, holding a git-generated line, at least a quarter of them trailers
TRAILER_LINE = re.compile(r"^([A-Za-z0-9-]+)\s*:(.*)$")
GENERATED = ("Signed-off-by: ", "(cherry picked from commit ")


def line_kind(line: str) -> str:
    """One message line as git's trailer reading sees it: blank, generated, trailer, continuation or prose."""
    if not line.strip():
        return "blank"
    if line.startswith(GENERATED):
        return "generated"
    if TRAILER_LINE.match(line):
        return "trailer"
    return "continuation" if line[0].isspace() else "prose"


@dataclasses.dataclass
class BlockCount:
    """The lines counted from the message's end up to a blank one: whether they are a trailer block."""
    trailers: int = 0
    prose: int = 0
    continuations: int = 0
    recognized: bool = False

    def add(self, kind: str) -> None:
        if kind in ("generated", "trailer"):
            self.trailers, self.continuations = self.trailers + 1, 0
            self.recognized = self.recognized or kind == "generated"
        elif kind == "continuation":
            self.continuations += 1
        else:
            self.prose, self.continuations = self.prose + 1 + self.continuations, 0

    def is_block(self) -> bool:
        prose = self.prose + self.continuations
        return (self.recognized and self.trailers * 3 >= prose) or (self.trailers > 0 and prose == 0)


def block_start(lines: List[str]) -> int:
    """The index of the final trailer block's first line in `lines` (the title paragraph's aside), len(lines) if none."""
    title_end = next((i for i, line in enumerate(lines) if not line.strip()), len(lines))
    count = BlockCount()
    seen = False
    for i in range(len(lines) - 1, title_end - 1, -1):
        kind = line_kind(lines[i])
        if kind == "blank" and seen:
            return i + 1 if count.is_block() else len(lines)
        if kind != "blank":
            seen = True
            count.add(kind)
    return len(lines)


def read_trailers(message: str) -> List[Trailer]:
    """What `git interpret-trailers --parse` reads from `message`: the final block's trailers, continuations unfolded."""
    lines = message.split("\n")
    out: List[str] = []
    for line in lines[block_start(lines):]:
        if out and line[:1].isspace() and line.strip():
            out[-1] += " " + line.strip()
        elif TRAILER_LINE.match(line):
            out.append(line)
    return trailer_pairs(out)


@dataclasses.dataclass(frozen=True)
class Commit:
    sha: str
    parents: Tuple[str, ...]
    tree: Mapping[str, str]       # path -> its text
    message: str
    order: int                    # when it was made: the newest first is the order rev-list walks a line in


REV_SUFFIX = re.compile(r"(\^\{commit\}|\^\d*|~\d*)$")


class InMemoryGitRepo(GitRepo):
    """GitRepo over a commit graph in memory: `commit(changes, message)` on the checked-out branch, `branch`,
    `checkout`, `add_worktree`. The boundary only: what git would answer, read from the graph."""

    def __init__(self, root: str = "/repo") -> None:
        self.root = root
        self.objects: Dict[str, Commit] = {}
        self.branches: Dict[str, Optional[str]] = {"main": None}
        self.current = "main"
        self.extra_worktrees: List[Worktree] = []

    # building the graph
    def commit(self, changes: Mapping[str, Optional[str]], message: str, parents: Optional[Sequence[str]] = None) -> str:
        """A commit of `changes` (path -> its new text, None deletes it) over the checked-out branch's head."""
        head = self.branches[self.current]
        parents = tuple(parents) if parents is not None else ((head,) if head else ())
        tree = dict(self.objects[parents[0]].tree) if parents else {}
        for path, text in changes.items():
            if text is None:
                tree.pop(path, None)
            else:
                tree[path] = text
        order = len(self.objects)
        sha = hashlib.sha1(repr((parents, sorted(tree.items()), message, order)).encode()).hexdigest()
        self.objects[sha] = Commit(sha, parents, tree, normal_message(message), order)
        self.branches[self.current] = sha
        return sha

    def branch(self, name: str, at: Optional[str] = None) -> None:
        self.branches[name] = self.resolve(at or "HEAD")

    def checkout(self, name: str) -> None:
        self.current = name

    def add_worktree(self, path: str, branch: str) -> None:
        self.extra_worktrees.append(Worktree(path, self.branches[branch], branch))

    # reading it
    def resolve(self, rev: str) -> Optional[str]:
        suffix = REV_SUFFIX.search(rev)
        if suffix and suffix.group(0) != rev:
            base = self.resolve(rev[:suffix.start()])
            return self.step(base, suffix.group(0)) if base else None
        if rev == "HEAD":
            return self.branches[self.current]
        if rev in self.branches:
            return self.branches[rev]
        found = [sha for sha in self.objects if len(rev) >= 4 and sha.startswith(rev)]
        return found[0] if len(found) == 1 else None

    def step(self, sha: str, suffix: str) -> Optional[str]:
        if suffix == "^{commit}":
            return sha
        count = int(suffix[1:] or 1)
        if suffix[0] == "^":
            parents = self.objects[sha].parents
            return parents[count - 1] if count <= len(parents) else None
        for _ in range(count):
            parents = self.objects[sha].parents
            if not parents:
                return None
            sha = parents[0]
        return sha

    def must(self, rev: str) -> str:
        sha = self.resolve(rev)
        if sha is None:
            raise Failed(128, ["git", "rev-parse", rev], "", f"fatal: bad revision '{rev}'")
        return sha

    def reachable(self, sha: Optional[str], first_parent: bool = False) -> Set[str]:
        seen: Set[str] = set()
        todo = [sha] if sha else []
        while todo:
            at = todo.pop()
            if at not in seen:
                seen.add(at)
                parents = self.objects[at].parents
                todo.extend(parents[:1] if first_parent else parents)
        return seen

    def commits(self, rng: str, paths: Sequence[str] = (), reverse: bool = False,
                first_parent: bool = False) -> List[str]:
        base, head = split_range(rng)
        wanted = self.reachable(self.must(head), first_parent) - (self.reachable(self.must(base)) if base else set())
        if paths:
            wanted = {sha for sha in wanted if any(self.touches(sha, p) for p in paths)}
        ordered = sorted(wanted, key=lambda sha: self.objects[sha].order, reverse=not reverse)
        return ordered

    def touches(self, sha: str, path: str) -> bool:
        commit = self.objects[sha]
        before: Mapping[str, str] = self.objects[commit.parents[0]].tree if commit.parents else {}
        return any(p == path or p.startswith(path.rstrip("/") + "/") for p in diff_trees(before, commit.tree, ""))

    def message(self, sha: str) -> str:
        return self.objects[self.must(sha)].message

    def parse_trailers(self, message: str) -> List[Trailer]:
        return read_trailers(message)

    def changed(self, base: str, head: str, diff_filter: str = "") -> List[str]:
        return diff_trees(self.objects[self.must(base)].tree, self.objects[self.must(head)].tree, diff_filter)

    def changed_in(self, sha: str, diff_filter: str = "") -> List[str]:
        commit = self.objects[self.must(sha)]
        if len(commit.parents) > 1:
            return []  # diff-tree shows a merge nothing without -m / -c
        before: Mapping[str, str] = self.objects[commit.parents[0]].tree if commit.parents else {}
        return diff_trees(before, commit.tree, diff_filter)

    def merge_base(self, a: str, b: str) -> Optional[str]:
        common = self.reachable(self.must(a)) & self.reachable(self.must(b))
        best = [c for c in common if not any(c != o and c in self.reachable(o) for o in common)]
        return max(best, key=lambda sha: self.objects[sha].order) if best else None

    def is_ancestor(self, a: str, b: str) -> bool:
        sha_a, sha_b = self.resolve(a), self.resolve(b)
        return bool(sha_a and sha_b and sha_a in self.reachable(sha_b))

    def show(self, rev: str, path: str) -> Optional[str]:
        sha = self.resolve(rev)
        return self.objects[sha].tree.get(path) if sha else None

    def exists(self, rev: str, path: str) -> bool:
        sha = self.resolve(rev)
        tree: Mapping[str, str] = self.objects[sha].tree if sha else {}
        return path in tree or any(p.startswith(path.rstrip("/") + "/") for p in tree)

    def worktrees(self) -> List[Worktree]:
        return [Worktree(self.root, self.branches[self.current], self.current), *self.extra_worktrees]


def diff_trees(before: Mapping[str, str], after: Mapping[str, str], diff_filter: str) -> List[str]:
    """The paths two trees differ at, sorted: A added, D deleted, M modified (`diff_filter` keeps only its letters)."""
    status = {p: "A" for p in after.keys() - before.keys()}
    status.update({p: "D" for p in before.keys() - after.keys()})
    status.update({p: "M" for p in before.keys() & after.keys() if before[p] != after[p]})
    return sorted(p for p, s in status.items() if not diff_filter or s in diff_filter.upper())
