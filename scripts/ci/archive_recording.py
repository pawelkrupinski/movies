#!/usr/bin/env python3
"""Copy "Record scrape fixtures" recordings into the permanent archive release (.github/workflows/archive-recording.yml).

  scripts/ci/archive-recording.sh <run> [<run> ...]

Per run and country (tool_countries.CODES): enrichment-<cc>-<run>.tar.* from $FIXTURE_RELEASE_TAG (default
convergence-fixtures) and the run's scrape-fixtures-<cc> artifact's scrapes-<cc>.tar.* as scrapes-<cc>-<run>.tar.*, both
uploaded (scripts/ci/gh-release.sh, its retries) to $RECORDING_ARCHIVE_TAG (default identity-recordings). An asset the
archive holds at the source's size is skipped (a partial upload, at another size, is replaced); a country the recording
lacks is said and skipped. Exit 1 when any copy failed or a run left nothing archived, 64 for no run. Needs `gh` with
release write. The workflow runs it by the name scripts/ci/archive-recording.sh; tested by archive_recording_test.py."""
from __future__ import annotations

import glob
import os
import re
import shutil
import sys
import tempfile
from typing import Callable, Dict, Mapping, Optional, Sequence, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import machine  # noqa: E402
import tool_countries  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402


class Failed(Exception):
    """A copy that failed: said, and the run goes on."""


class Archiver:
    def __init__(self, gh: tool_github.GitHub, runner: tool_process.Runner, env: Mapping[str, str], work: str) -> None:
        self.gh = gh
        self.runner = runner
        self.env = env
        self.work = work
        self.source = env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"
        self.archive = env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        self.source_assets: Dict[str, str] = {}
        self.archived: Dict[str, str] = {}

    def sizes(self, release: str) -> Optional[Dict[str, str]]:
        """name -> size of every asset of a release; None when it cannot be read."""
        listed = self.gh.read(["release", "view", release, "--json", "assets", "--jq",
                               '.assets[] | "\\(.name)\\t\\(.size)"'], timeout=120)
        if listed is None:
            return None
        return dict(line.split("\t", 1) for line in listed.split("\n") if "\t" in line)

    def upload(self, path: str, name: str) -> None:
        """Into the archive unless it holds `name` at this size already."""
        want = str(os.path.getsize(path))
        if self.archived.get(name) == want:
            print(f"  {name}: archived already ({want} bytes)", flush=True)
            return
        named = os.path.join(self.work, name)
        shutil.copyfile(path, named)
        uploaded = self.runner.run(tool_process.Command(
            [os.path.join(HERE, "gh-release.sh"), "upload", self.archive, named, "--clobber"], self.env))
        os.remove(named)
        if uploaded != 0:
            raise Failed(f"could not upload {name} to {self.archive}")
        print(f"  {name}: archived ({want} bytes)", flush=True)

    def tree(self, cc: str, run: str) -> bool:
        """The country's tree archived (or already there): True; the recording holds none: False."""
        pattern = re.compile(rf"enrichment-{cc}-{run}\.tar\.(zst|gz)$")
        tree = next((n for n in self.source_assets if pattern.match(n)), "")
        if not tree and any(pattern.match(n) for n in self.archived):
            print(f"  enrichment-{cc}-{run}: only in {self.archive} already", flush=True)
            return True
        if not tree:
            print(f"  {cc}: {self.source} holds no enrichment-{cc}-{run} -- not a recording of {cc} (or already "
                  "pruned): skipped", flush=True)
            return False
        if self.archived.get(tree) == self.source_assets[tree]:
            print(f"  {tree}: archived already", flush=True)
            return True
        downloads = tempfile.mkdtemp(dir=self.work)
        if not self.gh.succeeds(["release", "download", self.source, "--pattern", tree, "--dir", downloads,
                                 "--clobber"], timeout=1800, quiet=False):
            raise Failed(f"could not download {tree} from {self.source}")
        self.upload(os.path.join(downloads, tree), tree)
        shutil.rmtree(downloads)
        return True

    def corpus(self, cc: str, run: str) -> None:
        if any(re.match(rf"scrapes-{cc}-{run}\.tar\.(zst|gz)$", n) for n in self.archived):
            print(f"  scrapes-{cc}-{run}: archived already", flush=True)
            return
        artifact = tempfile.mkdtemp(dir=self.work)
        if not self.gh.succeeds(["run", "download", run, "--name", f"scrape-fixtures-{cc}", "--dir", artifact],
                                timeout=1800, quiet=False):
            raise Failed(f"recording {run} holds a {cc} tree but its scrape-fixtures-{cc} artifact could not be read")
        found = sorted(glob.glob(os.path.join(artifact, "**", f"scrapes-{cc}.tar.*"), recursive=True))
        if not found:
            raise Failed(f"{run}'s scrape-fixtures-{cc} holds no scrapes-{cc}.tar.*")
        suffix = os.path.basename(found[0])[len(f"scrapes-{cc}."):]
        self.upload(found[0], f"scrapes-{cc}-{run}.{suffix}")
        shutil.rmtree(artifact)

    @staticmethod
    def attempt(copy: Callable[[], Optional[bool]]) -> Tuple[bool, bool]:
        """(whether the recording holds the asset, whether its copy held): a failed copy is said, never fatal."""
        try:
            return copy() is not False, True
        except Failed as why:
            print(f"::error::{why}", flush=True)
            return True, False

    def recording(self, run: str) -> bool:
        """Whether every copy of the run held and something was archived."""
        print(f"recording {run}", flush=True)
        ok, copied = True, 0
        for cc in tool_countries.CODES:
            present, held = self.attempt(lambda: self.tree(cc, run))
            ok = ok and held
            copied += 1 if present and held else 0
            if present:
                ok = self.attempt(lambda: self.corpus(cc, run))[1] and ok
        if not copied:
            print(f"::error::recording {run}: nothing archived (no tree in {self.source} or {self.archive})", flush=True)
        return ok and copied > 0

    def run(self, runs: Sequence[str]) -> int:
        for release, into in ((self.source, "source_assets"), (self.archive, "archived")):
            sizes = self.sizes(release)
            if sizes is None:
                print(f"::error::could not read release {release}")
                return 1
            setattr(self, into, sizes)
        failed = False
        for run in runs:
            if not re.fullmatch(r"[0-9]+", run):
                print(f"::error::not a run id: {run}", flush=True)
                failed = True
            elif not self.recording(run):
                failed = True
        return 1 if failed else 0


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if not argv:
        print("usage: archive-recording.sh <run> [<run> ...]", file=sys.stderr)
        return 64
    runner = tool_process.SubprocessRunner()
    with tempfile.TemporaryDirectory() as work:
        return Archiver(tool_github.GitHub(runner, env), runner, env, work).run(argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
