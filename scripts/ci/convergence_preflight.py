#!/usr/bin/env python3
"""THE PAIR EVERY LEG WILL REPLAY EXISTS, checked in seconds before any leg takes a runner (identity-model-convergence.yml's
`preflight`). Each of these used to fail every leg of a run separately, minutes into its setup, after the JDK, sbt and
the build cache: 2026-10-20..22, 50 takes rows on a recording this forge never held (38713274617, "the pinned pair has
expired"), 30 rows on a pinned tree the release no longer kept ("it keeps the newest five") and 15 on a country with no
pin -- ~5 of the 13.4 failed job-hours those two days.

  convergence-preflight.sh <codes, comma-separated> [recording run]

With a recording (the workflow's `recording` input, identity-measure --recording): that run's scrape corpus
(`scrape-fixtures-<code>`) and its tree (`enrichment-<code>-<run>.tar.*` in $FIXTURE_RELEASE_TAG), per country. Without:
each country's pin (`hermetic-<code>.txt`, convergence-setup's `pinned`), the tree it names, and the corpus of the run it
names. Fills are not checked (convergence-fill lists only what the release holds). The ARCHIVE release
($RECORDING_ARCHIVE_TAG, identity-recordings) keeps the landing recordings past the rolling release's five and their
artifacts: trees under the same name, corpora as scrapes-<code>-<run>.tar.* -- legs read both back from it.

ONLY AN ANSWERED "NOT THERE" FAILS: a gh read that fails otherwise is said as a warning and passes, exactly as
convergence-setup tells a failed read from an expired pair (a throttled storage is no evidence of a missing pair).
Reads GH_TOKEN, GH_REPO (gh's own) and FIXTURE_RELEASE_TAG. The workflows run it by the name
scripts/ci/convergence-preflight.sh; tested by convergence_preflight_test.py beside it."""
from __future__ import annotations

import fnmatch
import os
import sys
import tempfile
from typing import Dict, List, Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from vcs import tool_github  # noqa: E402
import tool_process  # noqa: E402

USAGE = "usage: convergence-preflight.sh <codes> [recording run]"
READ, ABSENT, FAILED = 0, 3, 1  # an artifacts listing: read, the forge answered the run does not exist, any other failure


def said(stderr: str) -> str:
    """What a failed gh call said, as the error line quotes it (its first 200 characters)."""
    return stderr[:200].rstrip("\n")


class Preflight:
    def __init__(self, gh: tool_github.GitHub, env: Mapping[str, str], work: str) -> None:
        self.gh = gh
        self.env = env
        self.work = work
        self.tag = env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"
        self.problems = 0
        self.assets: List[str] = []
        self.archived: List[str] = []
        self.assets_known = True
        self.archive_known = True
        self.listings: Dict[str, List[str]] = {}
        self.status: Dict[str, int] = {}
        self.warned: Dict[str, bool] = {}
        self.last_error = ""

    def error(self, text: str) -> None:
        print(f"::error::{text}", flush=True)
        self.problems += 1

    @staticmethod
    def warn(text: str) -> None:
        print(f"::warning::{text}", flush=True)

    def release(self, tag: str) -> Optional[List[str]]:
        done = self.gh.call(["release", "view", tag, "--json", "assets", "--jq", ".assets[].name"], 120, quiet=True)
        self.last_error = done.stderr
        return [n for n in done.stdout.split("\n") if n] if done.code == 0 else None

    def read_releases(self) -> None:
        assets = self.release(self.tag)
        if assets is None:
            self.warn(f"could not read release {self.tag} ({said(self.last_error)}) -- its pins and trees are not "
                      "checked here")
            self.assets_known = False
        self.assets = assets or []
        # An unreadable archive is no evidence a recording is gone: a run the forge's API does not know (the archive
        # keeps the github.com-era ones) is then not checked (Identity model convergence #1505).
        archive = self.env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        archived = self.release(archive)
        if archived is None:
            self.warn(f"could not read release {archive} ({said(self.last_error)}) -- the recordings it keeps are not "
                      "checked")
            self.archive_known = False
        self.archived = archived or []

    def has_asset(self, pattern: str) -> bool:
        return any(fnmatch.fnmatchcase(n, pattern) for n in self.assets + self.archived)

    def artifacts(self, run: str) -> int:
        """The run's unexpired artifact names into self.listings[run], once: READ, ABSENT or FAILED."""
        if run in self.listings or run in self.status:
            return self.status.get(run, READ)
        repo = self.env.get("GH_REPO") or sys.exit("convergence-preflight.sh: GH_REPO: GH_REPO names the repository")
        done = self.gh.call(["api", f"repos/{repo}/actions/runs/{run}/artifacts?per_page=100", "--jq",
                             ".artifacts[] | select(.expired != true) | .name"], 120, quiet=True)
        self.last_error = done.stderr
        if done.code == 0:
            self.listings[run] = done.stdout.split("\n")
            return READ
        self.status[run] = ABSENT if "HTTP 404" in done.stderr else FAILED
        return self.status[run]

    def corpus_present(self, run: str, code: str, what: str) -> None:
        if any(fnmatch.fnmatchcase(n, f"scrapes-{code}-{run}.tar.*") for n in self.archived):
            return
        status = self.artifacts(run)
        if status == READ:
            if f"scrape-fixtures-{code}" not in self.listings[run]:
                self.error(f"{what}: recording run {run} holds no scrape-fixtures-{code} (its artifacts expired or were "
                           "deleted) -- re-run Record scrape fixtures, or measure a recording this forge holds")
        elif status == ABSENT and not self.archive_known:
            if not self.warned.get(run):
                self.warn(f"recording run {run} is no run of this forge's and the archive was not read -- not checked")
                self.warned[run] = True
        elif status == ABSENT:
            self.error(f"{what}: recording run {run} is not on this forge (a github.com-era run?) -- measure the pinned "
                       "pair or a recording this forge holds")
        elif not self.warned.get(run):
            self.warn(f"could not list recording run {run}'s artifacts ({said(self.last_error)}) -- not checked")
            self.warned[run] = True

    def download_pins(self) -> None:
        done = self.gh.call(["release", "download", self.tag, "--pattern", "hermetic-*.txt", "--dir",
                             os.path.join(self.work, "pins"), "--clobber"], 300, quiet=True)
        if done.code != 0:
            self.warn(f"could not download the pins ({said(done.stderr)}) -- not checked")
            self.assets_known = False

    def recorded(self, code: str, recording: str) -> None:
        if self.assets_known and not self.has_asset(f"enrichment-{code}-{recording}.tar.*"):
            self.error(f"{code}: release {self.tag} holds no enrichment-{code}-{recording}.tar.* (recording "
                       f"{recording}'s tree; it keeps the newest five recordings)")
        self.corpus_present(recording, code, code)

    def pinned(self, code: str) -> None:
        if not self.has_asset(f"hermetic-{code}.txt"):
            self.error(f"No recorded pair pinned for {code} (release asset hermetic-{code}.txt) -- a hermetic leg "
                       "replays nothing else. Record one: gh workflow run \"Record scrape fixtures\" --ref main")
            return
        try:
            with open(os.path.join(self.work, "pins", f"hermetic-{code}.txt")) as f:
                first = f.read().split("\n")[0]
        except OSError:
            first = ""
        fields = first.split()
        if len(fields) < 3:
            self.error(f"hermetic-{code}.txt is malformed: '{first}'")
            return
        corpus, tree = fields[0], fields[2]
        if not self.has_asset(tree):
            self.error(f"{code}: the pinned tree {tree} is not in release {self.tag} (it keeps the newest five) -- a "
                       "hermetic leg replays nothing else")
        self.corpus_present(corpus, code, f"{code}'s pinned pair")

    def check(self, codes: str, recording: str) -> int:
        self.read_releases()
        if not recording and self.assets_known:
            self.download_pins()
        for code in (c for c in codes.split(",") if c):
            if recording:
                self.recorded(code, recording)
            elif self.assets_known:
                self.pinned(code)
        if self.problems:
            print(f"{self.problems} problem(s): no leg would replay its pair, so none starts")
            return 1
        print(f"every leg's pair is there ({f'recording {recording}, ' if recording else ''}countries {codes})")
        return 0


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if not argv or not argv[0]:
        print(f"convergence-preflight.sh: 1: {USAGE}", file=sys.stderr)
        return 1
    with tempfile.TemporaryDirectory() as work:
        gh = tool_github.GitHub(tool_process.SubprocessRunner(), env)
        return Preflight(gh, env, work).check(argv[0], argv[1] if len(argv) > 1 else "")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
