"""THE PIN GUARD (convergence_publish.py `verified-pin`): a country is pinned only on proof. A verify that did not
succeed, diagnostics that do not show the served-corpus check ran, any finding in them, no pinned copy of the
recording's tree, or a corpus that could not be archived is a refusal, and that country's pair stays as it was.

A PINNED PAIR OUTLIVES ITS ARTIFACT: the corpus half is the recording run's scrape-fixtures-<code> artifact, kept 14
days, while the pin names it for weeks (Identity takes #76, run 43317254260). So the pin archives the corpus itself as
scrapes-<code>-<run>.tar.gz on the permanent RECORDING_ARCHIVE_TAG release (default identity-recordings), where
convergence-setup, convergence-preflight.sh and identity-fetch-recording.sh read it once the artifact is gone.

ONE DEADLINE, VERIFIED_PIN_SECONDS (default 1200) from the start, kept under the pin step's `timeout-minutes`: each
corpus download gets what is left of it (600 s at most), and a country with none left is refused."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
import time
from typing import Callable, List, Mapping, Optional, Protocol

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "scripts/lib"))
import convergence_release  # noqa: E402
import tool_process  # noqa: E402

DOWNLOAD_MOST = 600


class Releases(Protocol):
    """What a pin needs of convergence_publish.py's Publish: the release, its writes and its pruning."""
    env: Mapping[str, str]
    runner: tool_process.Runner
    github: convergence_release.GitHub

    def tag(self) -> str: ...

    def say(self, line: str) -> None: ...

    def release(self, *args: str) -> int: ...

    def asset_names(self, quiet: bool = False) -> Optional[List[str]]: ...

    def prune_pairs(self, code: str) -> None: ...


class Refused(Exception):
    """This country is not pinned; the message says why (an `::error::` line)."""


class Pin:
    def __init__(self, publish: Releases, run: str, verdict: str, directory: str,
                 now: Callable[[], float] = time.time) -> None:
        self.publish = publish
        self.run = run
        self.verdict = verdict
        self.directory = directory
        self.now = now
        self.seconds = publish.env.get("VERIFIED_PIN_SECONDS") or "1200"
        self.deadline = now() + int(self.seconds)
        self.names: List[str] = []

    def countries(self, countries: str) -> int:
        tag = self.publish.tag()
        names = self.publish.asset_names()
        if names is None:
            self.publish.say(f"::error::could not list release {tag} -- no pair pinned")
            return 1
        self.names = names
        refused = 0
        for code in countries.replace(",", " ").split():
            try:
                self.country(code)
            except Refused as e:
                if str(e):
                    self.publish.say(f"::error::{code}: {e} -- {code}'s pair stays as it was")
                refused = 1
            except convergence_release.Exit:
                refused = 1
        return refused

    def country(self, code: str) -> None:
        if self.verdict != "success":
            raise Refused(f"recording {self.run}'s verify ended {self.verdict}, not success")
        self.no_finding(code)
        tree = next((n for n in self.names if re.match(rf"^enrichment-{code}-{self.run}\.tar\.(zst|gz)$", n)), "")
        if not tree:
            raise Refused(f"no pinned copy enrichment-{code}-{self.run}.tar.* of recording {self.run} on "
                          f"{self.publish.tag()}")
        self.archive_corpus(code)
        marker = f"hermetic-{code}.txt"
        with open(marker, "w") as f:
            f.write(f"{self.run} {self.recorded(code) or 'unknown'} {tree}\n")
        if self.publish.release("upload", self.publish.tag(), marker, "--clobber") != 0:
            raise convergence_release.Exit(1)
        self.publish.say(f"pinned recording {self.run} ({tree}) as {code}'s hermetic pair: its verify on main's code was"
                         " green with no finding")
        self.publish.prune_pairs(code)

    def findings_files(self, code: str) -> List[str]:
        return sorted(os.path.join(top, f) for top, _, files in os.walk(self.directory) for f in files
                      if f.startswith(f"{code}-served-") and f.endswith(".txt"))

    def no_finding(self, code: str) -> None:
        files = self.findings_files(code)
        if not any(f.endswith(f"/{code}-served-served.txt") for f in files):
            raise Refused(f"no served-corpus findings in recording {self.run}'s verify diagnostics ({self.directory}) --"
                          " nothing proves the gate's legs would pass on it")
        lines: List[str] = []
        for path in files:
            with open(path) as f:
                lines += [line for line in f.read().splitlines() if not line.startswith("# corpus-run ")]
        found = sum(1 for line in lines if line)
        if found > 0:
            self.publish.say(f"::error::{code}: recording {self.run}'s verify found {found} served-corpus finding(s) (the"
                             " data's, standing at the recording's own suite too, or the code's) -- the gate's strict"
                             f" legs would fail every push on them; {code}'s pair stays as it was:")
            for line in lines[:12]:
                self.publish.say("  " + third_field(line))
            raise Refused("")

    def archive_corpus(self, code: str) -> None:
        """The corpus half outlives the recording's 14-day artifact: archived for good first, or no pin."""
        left = int(self.deadline - self.now())
        if left <= 0:
            raise Refused(f"the pin's deadline (VERIFIED_PIN_SECONDS={self.seconds}) passed before its corpus was"
                          " archived")
        left = min(left, DOWNLOAD_MOST)
        with tempfile.TemporaryDirectory() as corpus:
            # bounded as a whole: gh-optional's own bound is per attempt, and it asks again on a transient answer
            try:
                present = "present" if self.publish.github.optional(
                    "run", "download", self.run, "--name", f"scrape-fixtures-{code}", "--dir", corpus,
                    timeout=str(left), bound=float(left)) else "absent"
            except convergence_release.ReadFailed:
                present = "failed or past the pin's deadline"
            if present != "present" or not self.archived(code, os.path.join(corpus, f"scrapes-{code}.tar.gz")):
                raise Refused(f"recording {self.run}'s corpus (artifact scrape-fixtures-{code}: {present}) could not be"
                              " archived")

    def archived(self, code: str, corpus: str) -> bool:
        archive = self.publish.env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        name = f"scrapes-{code}-{self.run}.tar.gz"
        if not os.path.isfile(corpus):
            self.publish.say(f"::error::no {corpus} to archive as {name} -- the pinned pair stays as it was")
            return False
        with tempfile.TemporaryDirectory() as staged:
            try:
                os.link(corpus, os.path.join(staged, name))
            except OSError:
                shutil.copy(corpus, os.path.join(staged, name))
            if self.publish.release("upload", archive, os.path.join(staged, name), "--clobber") != 0:
                self.publish.say(f"::error::could not archive {name} to release {archive} -- the pinned pair stays as it"
                                 " was")
                return False
        self.publish.say(f"archived {name} to release {archive}")
        return True

    def recorded(self, code: str) -> str:
        """The recording's own time as its leg wrote it into prod-latest, else the run's start."""
        with tempfile.TemporaryDirectory() as latest:
            self.publish.runner.capture(tool_process.Command(
                ["gh", "release", "download", self.publish.tag(), "--pattern", f"prod-latest-{code}.txt", "--dir", latest,
                 "--clobber"], self.publish.env))
            try:
                with open(os.path.join(latest, f"prod-latest-{code}.txt")) as f:
                    rows = [line.split() for line in f]
            except OSError:
                rows = []
        recorded = next((r[1] for r in rows if len(r) > 1 and r[0] == self.run), "")
        return recorded or self.publish.github.created_at(self.run)


def third_field(line: str) -> str:
    """`cut -f3`: a line with no tab whole, else its third tab-separated field (none: empty)."""
    fields = line.split("\t")
    return line if len(fields) == 1 else (fields[2] if len(fields) > 2 else "")
