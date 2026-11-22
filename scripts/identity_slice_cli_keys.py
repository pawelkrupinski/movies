"""The keys identity-slice keeps what it made under (scripts/identity_slice_cli.py), byte for byte the shell tool's, so a
cache another checkout or the shell tool filled is read back:

  code_key(ref)     what a whole-corpus index and a slice's result are a function of besides the recording and the filed
                    answers: the code a ref runs -- its main code, the fixtures package and the build -- with THIS
                    checkout's tool files dropped in. Keyed by content, not commit: a commit that changes no code (a
                    judgement, a spec, a doc) resolves nothing again (2026-10-15: recording 38547714686 held 252 indexes
                    under 252 commits and worktree edits -- 12 distinct ones). `.` is this checkout's code, edits and all.
  worktree_key()    this checkout's code, keyed by WHICH checkout, its commit and its uncommitted code (tracked edits and
                    new files): the index cached under it is never another worktree's, an older commit's or an earlier
                    edit's
  sha_of / ref_of   a ref's key (its commit, or the worktree key for `.`) and back to what code_key reads"""
from __future__ import annotations

import hashlib
import os
import subprocess
import sys
from typing import List

from identity_slice_cli_context import TOOL_FILES, Context

CODE_PATHS = ("geodata/src/main", "base/src/main", "http/src/main", "common/src/main", "identity/src/main",
              "scrapers/src/main", "worker/src/main", "worker/src/fixtures", "testkit/src/main", "build.sbt", "project")
# every module the tool's classpath compiles (the scrapers and geodata reach the resolver too: a venue's details)
MODULES = ("geodata", "common", "identity", "scrapers", "worker", "testkit")


def sha1(data: bytes) -> str:
    return hashlib.sha1(data).hexdigest()


class Keys:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx

    def git(self, *args: str) -> bytes:
        """git's stdout as bytes: a key hashes what git printed, whatever its encoding."""
        return subprocess.run(["git", "-C", self.ctx.repo, *args], env=self.ctx.env, stdout=subprocess.PIPE).stdout

    def hash_each(self, nul_separated: bytes) -> bytes:
        """`shasum <path>` of each NUL-separated path (relative to the checkout), nothing for none or an unreadable
        one -- never `xargs -0 shasum`: GNU xargs runs shasum once on no input, hashing its empty stdin."""
        out = b""
        for path in nul_separated.split(b"\0"):
            if not path:
                continue
            try:
                with open(os.path.join(self.ctx.repo.encode(), path), "rb") as f:
                    out += sha1(f.read()).encode() + b"  " + path + b"\n"
            except OSError:
                pass
        return out

    def untracked(self, *paths: str) -> bytes:
        return self.hash_each(self.git("ls-files", "-z", "--others", "--exclude-standard", "--", *paths))

    def tool_files_hashed(self) -> bytes:
        """`(cd repo && shasum TOOL_FILES)`: a missing one said on stderr, as shasum says it."""
        out = b""
        for name in TOOL_FILES:
            try:
                with open(os.path.join(self.ctx.repo, name), "rb") as f:
                    out += f"{sha1(f.read())}  {name}\n".encode()
            except OSError:
                print(f"shasum: {name}: No such file or directory", file=sys.stderr, flush=True)
        return out

    def code_key(self, ref: str) -> str:
        if ref == ".":
            listing = (self.git("ls-tree", "-r", "HEAD", "--", *CODE_PATHS) + self.git("diff", "HEAD", "--", *CODE_PATHS)
                       + self.untracked(*CODE_PATHS))
        else:
            sha = self.git("rev-parse", "--verify", f"{ref}^{{commit}}").decode().strip()
            listing = self.git("ls-tree", "-r", sha, "--", *CODE_PATHS)
        # the ref's own tool files are replaced by this checkout's: their lines go, these files' contents stand in
        lines: List[bytes] = (listing.rstrip(b"\n") + b"\n").split(b"\n")[:-1]
        dropped = [b"\t" + name.encode() for name in TOOL_FILES]
        kept = b"".join(line + b"\n" for line in lines if not any(d in line for d in dropped))
        return "code-" + sha1(kept + self.tool_files_hashed())[:24]

    def worktree_key(self) -> str:
        where = sha1(self.ctx.repo.encode())[:8]
        head = self.git("rev-parse", "--short=12", "HEAD").decode().strip()
        edits = self.git("diff", "HEAD", "--", *MODULES, "build.sbt", "project") + self.untracked(*MODULES, "project")
        return f"worktree-{where}-{head}-{sha1(edits)[:8]}"

    def sha_of(self, ref: str) -> str:
        if ref == ".":
            return self.worktree_key()
        return self.git("rev-parse", "--verify", f"{ref}^{{commit}}").decode().strip()

    @staticmethod
    def ref_of(key: str) -> str:
        """sha_of's key back to what code_key reads: a worktree key is this checkout's code."""
        return "." if key.startswith("worktree-") else key

    def results_of(self, code: str, run: str) -> str:
        """Where each slice's result at <code> is kept, shared by every checkout: per recording, code and filed answers.
        Nothing with --fresh, nor beside prod's moment."""
        o = self.ctx.o
        if o.fresh or o.mirror:
            return ""
        fam = "none"
        if os.path.isfile(o.families):
            with open(o.families, "rb") as f:
                fam = sha1(f.read())[:8]
        return f"{self.ctx.cache}/slice/{run}/results/{code}-{fam}"

    @staticmethod
    def serve_key(tree: str, ref: str, cc: str, recording: str, families: str) -> str:
        return sha1(f"{tree}\n{ref}\n{cc}\n{recording}\n{families}\n".encode())[:16]
