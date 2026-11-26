"""THE PURE MOVE CHECK'S GIT (scripts/pure_move_check.py): one `git` call that refuses with `Undecidable`, and `rev:path`
blob contents through a single `git cat-file --batch`. Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import os
import subprocess
import sys
from typing import Dict, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_git  # noqa: E402


class Undecidable(Exception):
    pass


def git(*args: str, ok: Tuple[int, ...] = (0,)) -> str:
    try:
        return tool_git.Git.here().text(*args, ok=ok)
    except tool_git.Failed as failed:
        raise Undecidable(str(failed)) from failed


class Blobs:
    """`rev:path` contents through one `git cat-file --batch` (hundreds of files in a second)."""

    def __init__(self) -> None:
        self.proc: Optional["subprocess.Popen[bytes]"] = None
        self.cache: Dict[str, Optional[bytes]] = {}

    def _process(self) -> "subprocess.Popen[bytes]":
        if self.proc is None:
            self.proc = subprocess.Popen(["git", "cat-file", "--batch"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        return self.proc

    def get(self, rev: str, path: str) -> Optional[bytes]:
        key = f"{rev}:{path}"
        if key in self.cache:
            return self.cache[key]
        proc = self._process()
        assert proc.stdin is not None and proc.stdout is not None
        proc.stdin.write(key.encode() + b"\n")
        proc.stdin.flush()
        header = proc.stdout.readline().decode().split()
        if len(header) < 3 or header[1] == "missing":
            self.cache[key] = None
            return None
        body = proc.stdout.read(int(header[2]))
        proc.stdout.read(1)
        self.cache[key] = body
        return body

    def text(self, rev: str, path: str) -> Optional[str]:
        b = self.get(rev, path)
        if b is None:
            return None
        try:
            return b.decode("utf-8")
        except UnicodeDecodeError:
            return None
