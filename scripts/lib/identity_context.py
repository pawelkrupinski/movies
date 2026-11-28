"""WHAT THE LOCAL IDENTITY TOOLS' CACHE FUNCTIONS SHARE (scripts/lib; identity-tools.sh's variables, identity_log,
identity_gh and identity_put): the repository, the shared cache, the gh and git a call reads through, and a file
written whole or not at all. Every function of scripts/identity-tools.sh that moved to Python takes one `Context`.

  Context.from_environment(env, repo)  the repository and the settings the shim passes (KINOWO_IDENTITY_CACHE,
                                       KINOWO_GH, KINOWO_GH_REPO, IDENTITY_TOOL, IDENTITY_FILLS, the bounds)
  ctx.log(text)                        `[<tool>] text` on stderr
  ctx.gh(seconds, *args)               (status, stdout) of gh, ended once it outlasts <seconds>
  ctx.git()                            tool_git.Git over the repository
  ctx.put(path, fill)                  `fill` writes <path> whole or not at all: a reader never sees it half written
  Failed(code)                         a function that failed: its status (the reason already said on stderr)"""
from __future__ import annotations

import os
import sys
import tempfile
from typing import BinaryIO, Callable, MutableMapping, Optional, Tuple

import gh_download
import tool_git
from tool_files import Files

FIXTURES = "test/resources/fixtures/identity-unmatched"
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"


class Failed(Exception):
    def __init__(self, code: int = 1) -> None:
        super().__init__(code)
        self.code = code


class Context:
    def __init__(self, env: MutableMapping[str, str], repo: str, files: Optional[Files] = None) -> None:
        self.env = env
        self.repo = repo
        self.files = files or Files.here()
        self.cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(os.path.expanduser("~"), ".cache/kinowo-identity")
        self.tool = env.get("IDENTITY_TOOL") or "identity"
        self.fills = env.get("IDENTITY_FILLS") or ""
        self.repository = env.get("KINOWO_GH_REPO") or "pawelkrupinski/movies"
        self.gh_binary = env.get("KINOWO_GH") or "gh"
        self.gh_read = int(env.get("KINOWO_IDENTITY_GH_TIMEOUT") or 60)
        self.gh_download = int(env.get("KINOWO_IDENTITY_DOWNLOAD_TIMEOUT") or 1800)
        self.lock_wait = int(env.get("KINOWO_IDENTITY_LOCK_WAIT") or 3600)

    @classmethod
    def from_environment(cls, env: MutableMapping[str, str], repo: str) -> "Context":
        return cls(env, repo)

    def log(self, text: str) -> None:
        sys.stderr.write(f"[{self.tool}] {text}\n")
        sys.stderr.flush()

    def say(self, text: str) -> None:
        sys.stderr.write(text + "\n")
        sys.stderr.flush()

    def gh(self, seconds: int, *args: str) -> Tuple[int, str]:
        return gh_download.captured(self.env, seconds, [self.gh_binary, *args], quiet=False)

    def git(self) -> tool_git.Git:
        return tool_git.Git(tool_git.shared_runner(), self.env, self.repo)

    def put(self, path: str, fill: Callable[[BinaryIO], bool]) -> bool:
        """`fill` writes <path>: whole, or not at all (a failed fill leaves nothing; two writers never interleave)."""
        directory = os.path.dirname(path)
        self.files.make_directories(directory)
        handle, part = tempfile.mkstemp(prefix=os.path.basename(path) + ".part.", dir=directory)
        with os.fdopen(handle, "wb") as out:
            done = fill(out)
        if done:
            os.replace(part, path)
        else:
            self.files.remove(part)
        return done
