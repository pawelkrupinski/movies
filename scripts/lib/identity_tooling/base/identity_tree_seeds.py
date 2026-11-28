"""THE NEAREST COMPILED TREE (scripts/lib; identity-tools.sh's identity_seed_distance and identity_nearest_compiled): a
fresh worktree has nothing compiled, and seeded from the nearest compiled code (APFS clones, no copy) a new ref compiled
only what it changed instead of all 1,766 sources (108 s idle and ~10 min with five agents compiling beside it,
2026-10-15).

  seeds.distance(tree, sha) -> the source files the compiled <tree> differs from <sha> by (this checkout: its HEAD's);
                               None for a commit this clone lacks (another clone's tree): never a failed diff read as none
  seeds.nearest(sha)        -> this checkout, or one of the cache's most recently compiled refs, whichever differs from
                               <sha> by the fewest source files; "" when none is compiled
  classpath_lock(tree)      -> the name of the lock a tree's compile holds"""
from __future__ import annotations

import glob
import hashlib
import os
from typing import List, Optional

from identity_tooling.base.identity_context import Context
from governor.jvm_gate import lock_busy

COMPILED_PATHS = ("geodata/src", "common/src", "identity/src", "scrapers/src", "worker/src", "testkit/src", "build.sbt",
                  "project")
CLASSPATH = os.path.join("target", "identity-tools", "classpath")


def classpath_lock(tree: str) -> str:
    return "classpath-" + hashlib.sha1(tree.encode()).hexdigest()[:12]


class Seeds:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx

    def distance(self, tree: str, sha: str) -> Optional[int]:
        git = self.ctx.git()
        origin = git.line("rev-parse", "HEAD") if tree == self.ctx.repo else os.path.basename(tree)
        if not origin or not git.ok("cat-file", "-e", f"{origin}^{{commit}}", quiet=True):
            return None
        out = git.out("diff", "--name-only", origin, sha, "--", *COMPILED_PATHS, quiet=True)
        return None if out is None else len(out.strip("\n").splitlines()) if out.strip("\n") else 0

    def candidates(self) -> List[str]:
        found = [self.ctx.repo] if os.path.isdir(os.path.join(self.ctx.repo, "worker", "target")) else []
        kept = glob.glob(os.path.join(self.ctx.cache, "refs", "*", CLASSPATH))
        kept.sort(key=os.path.getmtime, reverse=True)
        limit = int(self.ctx.env.get("KINOWO_IDENTITY_SEED_CANDIDATES") or 40)
        return found + [os.path.dirname(os.path.dirname(os.path.dirname(path))) for path in kept[:limit]]

    def compiling(self, tree: str) -> bool:
        """A tree compiling now (its classpath lock is held) may hold half-written classes; a lock given back holds no
        pid, and only one with a pid in is asked."""
        lock = os.path.join(self.ctx.cache, "locks", classpath_lock(tree) + ".flock")
        return os.path.isfile(lock) and os.path.getsize(lock) > 0 and lock_busy(lock)

    def nearest(self, sha: str) -> str:
        best, best_n = "", None
        for tree in self.candidates():
            if os.path.basename(tree) == sha or self.compiling(tree):
                continue
            n = self.distance(tree, sha)
            if n is not None and (best_n is None or n < best_n):
                best, best_n = tree, n
        return best
