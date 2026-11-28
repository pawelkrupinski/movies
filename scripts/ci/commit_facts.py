#!/usr/bin/env python3
# Purpose: The commit the drift gate's rules read: its sha, subject, trailers and changed paths, and a file at a revision
"""THE COMMIT A RULE READS (split out of commit_rules.py, which names the rules): one commit's facts against its first
parent, `at` (a path at a revision) and the `Rule` shape every rule has. Imported by the rule modules beside it."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable

import tool_git
import trailers

SLICES = "test/resources/fixtures/drift-slices/"


def at(rev: str, path: str) -> str:
    """`path` at `rev`, "" when `rev` lacks it; a revision git cannot read at all (a shallow clone's parent) raises: exit 2."""
    here = tool_git.Git.here()
    return here.text("show", f"{rev}:{path}") if here.text("ls-tree", "--name-only", rev, "--", path).strip() else ""


@dataclass
class Commit:
    sha: str
    subject: str
    trailers: dict[str, list[str]]
    changed: set[str]  # against the first parent: a merge's resolution counts
    message: str = ""
    parent: str = field(init=False)

    def __post_init__(self):
        self.parent = f"{self.sha}^"

    @staticmethod
    def read(sha: str) -> "Commit":
        subject = tool_git.Git.here().text("log", "-1", "--format=%s", sha).strip()
        trailers: dict[str, list[str]] = {}
        for line in tool_git.Git.here().text("log", "-1", "--format=%(trailers:only,unfold)", sha).splitlines():
            key, sep, value = line.partition(":")
            if sep and value.strip():
                trailers.setdefault(key.strip().lower(), []).append(value.strip())
        has_parent = tool_git.Git.here().ok("rev-parse", "-q", "--verify", f"{sha}^", quiet=True)
        names = tool_git.Git.here().text("diff", "--name-only", "--no-renames", f"{sha}^", sha) if has_parent \
            else tool_git.Git.here().text("diff-tree", "--no-commit-id", "--name-only", "-r", "--root", sha)
        return Commit(sha, subject, trailers, {n for n in names.splitlines() if n}, tool_git.Git.here().text("log", "-1", "--format=%B", sha))

    def has(self, key: str) -> bool:
        return bool(self.trailers.get(key.lower()))

    def misplaced(self, key: str) -> str:
        """Why the commit has no `key` trailer although it wrote one: each such line git reads as prose, or ""."""
        return "".join(f"\n  {trailers.explain(m)}" for m in trailers.misplaced(self.message, (key,)))

    def claims_fix(self) -> bool:
        """`Fixes-Red:` as a claim: a `Red-Unrelated:` commit names the red only to land on it under the owner's call
        and claims no fix (as the pre-push hook reads it), so the red-fix guards do not judge it."""
        return self.has("Fixes-Red") and not self.has("Red-Unrelated")

    def label(self) -> str:
        return f"{self.sha[:9]} {self.subject}"


Opts = dict[str, Any]  # the walker's facts about a push: registry, pushed, in_push, declared
Rule = Callable[[Commit, Opts], list[str]]
