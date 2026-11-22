"""THE PATHS THAT CAN MOVE A TAKE, and what identity-measure (scripts/identity_measure_cli.py) reads off them: REUSE (an
earlier measurement of the same change, main moved only outside them) and THE FOOTPRINT (the countries a change can
move a take in). The patterns are the convergence paths less the judgements and the CI plumbing a leg runs (.github/,
scripts/): a change there makes a run fail or report differently, never resolve differently -- 2026-10-15 a landing
re-measured a083 for 13 minutes because main had moved only convergence-setup.sh and its siblings. The takes check's own
scripts stay: they write the takes the measurement reads. And what the takes check reads that no convergence path names
(identity_measure_cli_reuse_test.py holds this list to the takes check's): the resolve dump and its shadow
(worker/src/it), the ratchet spec the final takes run, and the captures they replay (identity-unmatched/;
expected-matches.tsv is the ratchet's output, regenerated from the judgements)."""
from __future__ import annotations

import os
import re
import tempfile
from typing import Dict, List, Optional, Tuple

import tool_countries
import tool_process
from identity_measure_cli_context import HERE, Context

MATCHES = os.path.join(os.path.dirname(HERE), ".github", "actions", "changed-paths", "matches.sh")
PLUMBING = re.compile(r"^\s*(\.github/|scripts/)")
TAKES_CHECK = re.compile(r"^scripts/ci/(identity-takes-check\.sh|identity_takes[a-z_]*\.py|corpus_shape_diff\.py)$")
READ_BY_THE_CHECK = ("worker/src/it/**", "worker/src/test/scala/services/identity/UnmatchedClustersRatchetSpec.scala",
                     "test/resources/fixtures/identity-unmatched/**",
                     "!test/resources/fixtures/identity-unmatched/expected-matches.tsv",
                     "!test/resources/fixtures/identity-judgements.tsv")


class TakePaths:
    def __init__(self, c: Context) -> None:
        self.c = c
        self.files: Dict[str, Optional[str]] = {}

    def patterns(self, ref: str) -> Optional[str]:
        """The take-moving patterns at `ref`, as a file (matches.sh's dialect); None when it has no convergence paths."""
        if ref not in self.files:
            self.files[ref] = self._write(ref)
        return self.files[ref]

    def _write(self, ref: str) -> Optional[str]:
        listed = self.c.git.out("show", f"{ref}:.github/convergence-paths.txt", quiet=True)
        lines = (listed or "").splitlines()
        kept = [line for line in lines if not PLUMBING.match(line)]
        if listed is None or not kept:
            return None
        kept += [line for line in lines if TAKES_CHECK.match(line)] + list(READ_BY_THE_CHECK)
        handle, path = tempfile.mkstemp(dir=self.c.scratch)
        with os.fdopen(handle, "w") as f:
            f.write("".join(line + "\n" for line in kept))
        return path

    def matches(self, patterns: str, paths: List[str]) -> Optional[bool]:
        """Whether any of `paths` is a take-moving path; None when matches.sh failed."""
        done = self.c.runner.capture(tool_process.Command([MATCHES, patterns], self.c.env,
                                                          input="".join(p + "\n" for p in paths)))
        return None if done.code != 0 else done.stdout.strip() == "true"

    def changed(self, a: str, b: str) -> Optional[List[str]]:
        out = self.c.git.out("diff", "--name-only", "--no-renames", a, b, quiet=True)
        return None if out is None else [line for line in out.splitlines() if line]

    def moving(self, a: str, b: str, patterns: str) -> List[str]:
        """The files a..b changed that can move a take."""
        return [f for f in self.changed(a, b) or [] if self.matches(patterns, [f])]

    def change_id(self, base: str, head: str, patterns: str) -> str:
        """base..head as one patch-id over its take-moving files; "empty" when it touches none; "" when none is made."""
        files = self.moving(base, head, patterns)
        if not files:
            return "empty"
        diff = self.c.git.out("diff", "--no-renames", base, head, "--", *files, quiet=True) or ""
        done = self.c.runner.capture(tool_process.Command(["git", "patch-id", "--stable"], self.c.env, input=diff))
        return done.stdout.split(" ", 1)[0].strip() if done.code == 0 else ""

    def reuse_root(self) -> str:
        return os.path.join(self.c.cache, "measure", "reuse")

    def store(self, base: str, head: str, run: str, dispatched: int) -> None:
        """The run answers this change from this base, written whole."""
        patterns = self.patterns(base)
        if not os.path.isfile(self.c.measured(run, "downloaded")) or patterns is None:
            return
        change = self.change_id(base, head, patterns)
        if not change:
            return
        directory = os.path.join(self.reuse_root(), change)
        os.makedirs(directory, exist_ok=True)
        handle, part = tempfile.mkstemp(prefix=f".{base}.", dir=directory)
        with os.fdopen(handle, "w") as f:
            f.write(f"{run}\t{head}\t{dispatched}\n")
        os.replace(part, os.path.join(directory, base))

    def find(self, base: str, head: str) -> Optional[Tuple[str, str]]:
        """(run, stored base) of a stored measurement answering base..head; None when none does."""
        patterns = self.patterns(base)
        change = self.change_id(base, head, patterns) if patterns else ""
        directory = os.path.join(self.reuse_root(), change)
        if not patterns or not change or not os.path.isdir(directory):
            return None
        entries = sorted((e for e in os.listdir(directory) if not e.startswith(".")),
                         key=lambda e: os.path.getmtime(os.path.join(directory, e)), reverse=True)
        for b0 in entries:
            stored = read_entry(os.path.join(directory, b0))
            if stored and self.answers(b0, stored, base, head, patterns):
                return stored[0], b0
        return None

    def answers(self, b0: str, stored: Tuple[str, str, int], base: str, head: str, patterns: str) -> bool:
        run, h0, at = stored
        max_age = int(self.c.env.get("IDENTITY_MEASURE_REUSE_HOURS") or 24) * 3600
        if int(self.c.now()) - at > max_age or not self.holds_every_country(run):
            return False
        if not all(self.c.git.ok("cat-file", "-e", f"{sha}^{{commit}}", quiet=True) for sha in (b0, h0)):
            return False
        return self.unmoved(b0, base, patterns) and self.unmoved(h0, head, patterns)

    def holds_every_country(self, run: str) -> bool:
        """The run measured every country asked, over the pinned pair (a named recording answers nothing else)."""
        artifacts = self.c.measured(run, "artifacts")
        if not os.path.isfile(self.c.measured(run, "downloaded")):
            return False
        if not all(os.path.isdir(os.path.join(artifacts, f"identity-takes-{cc}"))
                   for cc in self.c.state.countries.split(",")):
            return False
        return not any(named_recording(os.path.join(artifacts, d, "recording.txt")) for d in sorted(os.listdir(artifacts)))

    def unmoved(self, a: str, b: str, patterns: str) -> bool:
        files = self.changed(a, b)
        return files is not None and self.matches(patterns, files) is False

    def footprint(self, base: str, head: str) -> str:
        """The countries the change can move a take in, in the tools' order; empty when none."""
        patterns = self.patterns(base)
        if patterns is None:
            return ",".join(tool_countries.CODES)
        top = (self.c.git.out("rev-parse", "--show-toplevel", quiet=True) or "").strip()
        done = self.c.runner.capture(tool_process.Command(
            ["python3", os.path.join(HERE, "ci", "identity_impact.py"), "countries", base, head, "--tree", top],
            self.c.env, input="".join(f + "\n" for f in self.moving(base, head, patterns))))
        reach = done.stdout.strip().split(",") if done.code == 0 else ["all"]
        return ",".join(cc for cc in tool_countries.CODES if "all" in reach or cc in reach)


def read_entry(path: str) -> Optional[Tuple[str, str, int]]:
    with open(path) as f:
        fields = f.readline().rstrip("\n").split("\t")
    if len(fields) < 3 or not fields[2].lstrip("-").isdigit():
        return None
    return fields[0], fields[1], int(fields[2])


def named_recording(path: str) -> bool:
    try:
        with open(path) as f:
            return any(line.startswith("named") for line in f)
    except OSError:
        return False
