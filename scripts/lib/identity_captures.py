"""A CAPTURE FETCHED ONCE INTO THE SHARED CACHE (scripts/lib; identity-tools.sh's identity_capture and
identity_judgements): the identity capture of a country, from a run's artifact or a git ref, with the judgements that
judge it.

  Captures(ctx, locks).capture(src, ccs) -> the directory holding <cc>.json.gz for each cc, and its judgements
        src: "" (this checkout's), a directory, an "Identity capture" run id, or a git ref
  Captures.judgements(directory)         the judgements that judge the capture in <directory>: its own, else this
                                         checkout's

A run's capture is judged by this checkout's judgements; a ref's by that ref's -- or, from before they were one file, its
labels.tsv, which tools.IdentityJudgements reads too."""
from __future__ import annotations

import os
import re
import shutil
import subprocess
from typing import BinaryIO, Callable, Sequence

import gh_download
import machine
from identity_context import FIXTURES, JUDGEMENTS, Context, Failed
from identity_lock import Locks

JUDGEMENT_FILE = "identity-judgements.tsv"


class Captures:
    def __init__(self, ctx: Context, locks: Locks) -> None:
        self.ctx = ctx
        self.locks = locks

    def capture(self, src: str, ccs: Sequence[str]) -> str:
        if not src:
            return os.path.join(self.ctx.repo, FIXTURES)
        if os.path.isdir(src):
            return os.path.abspath(src)
        if re.fullmatch(r"[0-9]{6,}", src):
            return self.of_run(src, ccs)
        return self.of_ref(src, ccs)

    def judgements(self, directory: str) -> str:
        own = os.path.join(directory, JUDGEMENT_FILE)
        return own if os.path.isfile(own) else os.path.join(self.ctx.repo, JUDGEMENTS)

    def of_run(self, run: str, ccs: Sequence[str]) -> str:
        directory = os.path.join(self.ctx.cache, "captures", f"run-{run}")
        for cc in ccs:
            if not os.path.isfile(os.path.join(directory, f"{cc}.json.gz")):
                with self.locks.held(f"capture-{run}-{cc}"):
                    self.download(run, directory, cc)
        if not self.ctx.put(os.path.join(directory, JUDGEMENT_FILE), self.copy_of_checkouts_judgements):
            raise Failed(1)
        return directory

    def copy_of_checkouts_judgements(self, out: BinaryIO) -> bool:
        try:
            with open(os.path.join(self.ctx.repo, JUDGEMENTS), "rb") as source:
                shutil.copyfileobj(source, out)
        except OSError:
            return False
        return True

    def of_ref(self, ref: str, ccs: Sequence[str]) -> str:
        sha = self.ctx.git().line("rev-parse", "--verify", f"{ref}^{{commit}}")
        if not sha:
            self.ctx.say(f"no directory, run id or ref {ref}")
            raise Failed(2)
        directory = os.path.join(self.ctx.cache, "captures", f"ref-{sha}")
        for cc in ccs:
            target = os.path.join(directory, f"{cc}.json.gz")
            if not os.path.isfile(target) and not self.ctx.put(target, self.shown(f"{sha}:{FIXTURES}/{cc}.json.gz")):
                raise Failed(1)
        judged = os.path.join(directory, JUDGEMENT_FILE)
        if not (os.path.isfile(judged) and os.path.getsize(judged) > 0):
            if not (self.ctx.put(judged, self.shown(f"{sha}:{JUDGEMENTS}", quiet=True))
                    or self.ctx.put(judged, self.shown(f"{sha}:{FIXTURES}/labels.tsv"))):
                raise Failed(1)
        return directory

    def shown(self, spec: str, quiet: bool = False) -> Callable[[BinaryIO], bool]:
        def fill(out: BinaryIO) -> bool:
            return machine.status(["git", "-C", self.ctx.repo, "show", spec], out=out,
                                  err=subprocess.DEVNULL if quiet else None) == 0
        return fill

    def download(self, run: str, directory: str, cc: str) -> None:
        """The run's identity-capture-<cc> artifact's <cc>.json.gz, as <directory>/<cc>.json.gz."""
        target = os.path.join(directory, f"{cc}.json.gz")
        if os.path.isfile(target):
            return
        self.ctx.log(f"downloading identity-capture-{cc} of run {run}")
        self.ctx.files.make_directories(directory)
        with self.ctx.files.scratch(f"{directory}/dl-{cc}.") as fetched:
            if gh_download.download(self.ctx.env, self.ctx.gh_download, [
                    "run", "download", run, "-R", self.ctx.repository, "--name", f"identity-capture-{cc}",
                    "--dir", fetched]) != 0:
                self.ctx.log(f"could not download identity-capture-{cc} of run {run} (gh failed or timed out)")
                raise Failed(1)
            found = [p for p in self.ctx.files.walk(fetched) if os.path.basename(p) == f"{cc}.json.gz"]
            if not found:
                self.ctx.log(f"run {run}'s identity-capture-{cc} holds no {cc}.json.gz")
                raise Failed(1)
            os.replace(found[0], target)
