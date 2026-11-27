#!/usr/bin/env python3
"""THE LAST GREEN BASE OF A MAIN ROW, KEPT IN GIT: refs/verified/<name> records the newest commit on main the row held
on, so the row judges every commit since -- a superseded (replaced) or red run's commits stay in its range until a run
passes with them. A row's base was the push's own `before` or "the newest green Main run": a replaced queued run's
commits fell between two pushes and were never judged (the old-ref compile, 2026-11-21), and a red Main held every
other row's range open (commit rules, simplification-2026-10-31 item 5). Shared by scripts/ci/commit-rules-ci.sh
(refs/verified/commit-rules) and main.yml's old-ref compile (refs/verified/old-ref-compile), each through the name
scripts/ci/verified-ref.sh.

  verified-ref.sh base <name>      the commit the ref records, on stdout; nothing (exit 0) when the ref does not exist
                                   yet (the row's first run: its caller falls back to the push's `before`); exit 2 when
                                   origin cannot be read
  verified-ref.sh advance <name>   moves the ref to a marker of HEAD -- only forward: the push leases the ref's current
                                   value and goes only when the commit it records is HEAD's ancestor; a newer run past
                                   HEAD already is said and passes; exit 2 when it cannot (the next run would judge from
                                   a stale base, never silently)

THE REF HOLDS A MARKER, NOT MAIN'S COMMIT (Main #2301): GitHub refuses github.token, an App without the `workflows`
permission, any ref update whose new commits (old..new) change a workflow file, on refs/verified/* as on a branch, so
pointing the ref at main's own commit failed on every range that touched .github/workflows/. The ref points at a
parentless commit whose one file, `verified`, names the commit the row held on: it adds no workflow change.
Tested by verified_ref_test.py beside it."""
from __future__ import annotations

import os
import sys
import tempfile
from typing import Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import ci_machine  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

USAGE = "usage: verified-ref.sh base|advance <name>"
NO_SUCH_REF = 2  # git ls-remote --exit-code: the ref does not exist


class VerifiedRef:
    def __init__(self, git: tool_git.Git, name: str) -> None:
        self.git = git
        self.ref = f"refs/verified/{name}"

    def recorded(self, oid: str) -> str:
        """The commit the ref's `oid` records: a marker's `verified` file, or the ref's first form, the commit itself."""
        if self.git.ok("cat-file", "-e", f"{oid}:verified", quiet=True):
            return "".join((self.git.out("cat-file", "blob", f"{oid}:verified") or "").split())
        return oid

    def marker(self, commit: str) -> Optional[str]:
        """A parentless commit recording `commit` as the base the row held on (its tree built in an index of its own)."""
        with tempfile.TemporaryDirectory() as work:
            named = os.path.join(work, "verified")
            with open(named, "w") as f:
                f.write(commit + "\n")
            blob = self.git.out("hash-object", "-w", named)
            alone = tool_git.Git(self.git.runner, {**self.git.env, "GIT_INDEX_FILE": os.path.join(work, "index")},
                                 self.git.root)
            if blob is None or not alone.ok("update-index", "--add", "--cacheinfo", f"100644,{blob.strip()},verified"):
                return None
            tree = alone.out("write-tree")
        if tree is None:
            return None
        done = self.git.call("commit-tree", tree.strip(), "-m", f"{self.ref} held on main at {commit}",
                             options=["-c", "user.name=verified ref", "-c", "user.email=verified-ref@ci.invalid"])
        return done.stdout.strip() if done.code == 0 else None

    def fetch(self) -> bool:
        return self.git.ok("fetch", "--quiet", "--no-tags", "origin", f"+{self.ref}:{self.ref}", timeout=120)

    def ancestor(self, older: str, newer: str) -> bool:
        return self.git.ok("merge-base", "--is-ancestor", older, newer, quiet=True)

    def base(self) -> int:
        listed = self.git.call("ls-remote", "--exit-code", "origin", self.ref, timeout=60).code
        if listed == NO_SUCH_REF:
            return 0
        if listed != 0:
            print(f"::error title={self.ref}::could not read {self.ref} (git ls-remote exit {listed})", file=sys.stderr)
            return 2
        if not self.fetch():
            print(f"::error title={self.ref}::could not fetch {self.ref}", file=sys.stderr)
            return 2
        print(self.recorded(self.rev(self.ref)))
        return 0

    def past(self, head: str) -> bool:
        """Whether the ref, fetched again, records a commit HEAD is an ancestor of (a newer run advanced it)."""
        return self.fetch() and self.ancestor(head, self.recorded(self.rev(self.ref)))

    def advance(self) -> int:
        head, short = self.rev("HEAD"), self.rev("--short=9", "HEAD")
        current = (self.git.out("ls-remote", "origin", self.ref, timeout=60) or "").split("\t")[0].strip()
        readable = not current or self.fetch()
        if readable and self.behind(head, current):
            print(f"  {self.ref} is past {short} already (a newer run advanced it)")
        elif readable and self.pushed(head, current):
            print(f"  {self.ref} -> {short}")
        elif self.past(head):
            print(f"  {self.ref} is past {short} already (a newer run advanced it)")
        else:
            print(f"::error title={self.ref}::could not advance {self.ref} to {short}: the next run would judge from a "
                  "stale base")
            return 2
        return 0

    def behind(self, head: str, current: str) -> bool:
        """Whether HEAD lies strictly behind the commit the ref's `current` value records (a newer run advanced it)."""
        recorded = self.recorded(current) if current else ""
        return bool(recorded) and not self.ancestor(recorded, head) and self.ancestor(head, recorded)

    def rev(self, *args: str) -> str:
        return (self.git.out("rev-parse", *args) or "").strip()

    def pushed(self, head: str, current: str) -> bool:
        """A marker of HEAD pushed to the ref, leased on the value it had (none: it must not exist yet)."""
        marker = self.marker(head)
        return marker is not None and self.git.ok("push", "--quiet", f"--force-with-lease={self.ref}:{current}",
                                                   "origin", f"{marker}:{self.ref}", timeout=120)


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if len(argv) != 2 or argv[0] not in ("base", "advance"):
        print(USAGE, file=sys.stderr)
        return 2
    ref = VerifiedRef(tool_git.Git(tool_process.SubprocessRunner(), env, "."), argv[1])
    return ref.base() if argv[0] == "base" else ref.advance()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(ci_machine.environment())))
