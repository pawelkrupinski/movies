#!/usr/bin/env python3
"""The rolling-release writes a recording convergence leg makes (.github/actions/convergence-publish), and the pin its
verify makes (verify-scrape-recording.yml). convergence-publish.sh, beside it, is the name its callers run.

  convergence-publish.sh tree <code> <complete> <log>
      Upload enrichment-upload/enrichment-<code>.tar.zst over the country's working asset -- and, when the leg's boot
      completed (`[<code>] boot complete` in <log>, <complete> = true, a corpus run in KINOWO_CONVERGENCE_CORPUS_RUN),
      its PINNED copy enrichment-<code>-<corpus run>.tar.zst in the same call, named to the next step in $GITHUB_ENV as
      KINOWO_PINNED_TREE. Retires the gzip working asset.
  convergence-publish.sh verified-pin <recording run> <codes> <verdict> <diagnostics dir>
      THE PIN GUARD (Verify scrape recording's `pin` job; convergence_publish_pin.py): make the recording each country's
      hermetic pair ONLY when its hermetic verify on main's code came out <verdict> = success AND that country's verify
      diagnostics hold no served-corpus finding at all. Archives the pair's corpus for good to the permanent
      RECORDING_ARCHIVE_TAG release first -- a corpus it cannot archive is no pin -- then writes hermetic-<code>.txt
      naming the pair, LAST, then prunes all but the newest five pairs and the fills / refetch / refused lists of the
      pairs it pruned. Refused per country (exit 1, naming why): that country's pair stays as it was.
  convergence-publish.sh latest <code>
      Write prod-latest-<code>.txt naming this complete recording's pinned copy (the hermetic marker's format) WITHOUT
      making it the hermetic pair, then prune as `verified-pin` does. What `--recording prod-latest` and
      scripts/identity-repro replay.
  convergence-publish.sh dead <code>
      Delete the country's assets nothing reads any more: an identity overlay (`identity-overlay-<code>-<run>`, no
      longer published) whose recording's tree is not on the release, and the takes check's results
      (`takes-<kind>-<code>-`), stored on a release of their own since (TAKES_RELEASE_TAG). Run before a leg's uploads:
      run 41351315090 found the release at GitHub's 1000-asset cap, and a release at the cap refuses every upload.

Pruning never removes a KEPT recording: the hermetic pair, prod-latest, and the identity measure's landing recordings
($KINOWO_KEPT_RECORDINGS, default "528 573"). Every write goes through scripts/ci/gh-release.sh, which retries GitHub's
transient answers. Reads FIXTURE_RELEASE_TAG from the environment. Run from the repository root.
(convergence_publish_test.py, against a fake `gh`)"""
from __future__ import annotations

import os
import re
import sys
import tempfile
from typing import List, Optional, Set

# (its own directory is sys.path's first, run as a script; convergence_release puts scripts/lib after it)
import convergence_publish_pin  # noqa: E402
import convergence_release  # noqa: E402
import tool_process  # noqa: E402

TOOL = "convergence-publish.sh"
NOTES = """Recorded enrichment responses and remembered answers per country.
Replaced in place by every convergence leg — one copy each, no history.
Not a version of anything; do not treat as a release."""


class Publish(convergence_release.Step):
    def tag(self) -> str:
        return self.env.get("FIXTURE_RELEASE_TAG", "")

    def ensure_release(self) -> None:
        """The country legs race to create the release on its very first run and exactly one wins; the others find it
        there, the outcome they wanted anyway."""
        if self.release_quietly("view", self.tag()) != 0:
            self.release("create", self.tag(), "--prerelease", "--title", "Convergence fixture trees (rolling)",
                         "--notes", NOTES)

    def boot_completed(self, code: str, log: str) -> bool:
        try:
            with open(log) as f:
                return any(line.startswith(f"[{code}] boot complete") for line in f)
        except OSError:
            return False

    def tree(self, code: str, complete: str, log: str) -> int:
        """Replaces the country's asset IN PLACE: one copy per country, never a history."""
        tag = self.tag()
        archive = f"enrichment-upload/enrichment-{code}.tar.zst"
        if not os.path.isfile(archive):
            self.say(f"nothing to publish — no {archive}")
            return 0
        self.ensure_release()
        # The PINNED copy rides the same upload: `gh release upload` sends its files side by side, and a second,
        # sequential upload of the same bytes was 18 s of the UK recording's critical path (run 37071880312).
        pin: List[str] = []
        run = self.env.get("KINOWO_CONVERGENCE_CORPUS_RUN", "")
        if complete == "true" and run and self.boot_completed(code, log):
            pinned = f"enrichment-{code}-{run}.tar.zst"
            link(archive, f"enrichment-upload/{pinned}")
            pin = [f"enrichment-upload/{pinned}"]
            self.actions.export("KINOWO_PINNED_TREE", pinned)
        if self.release("upload", tag, archive, *pin, "--clobber") != 0:
            return 1
        self.say(f"published {archive} {' '.join(pin)} to release {tag}")
        # The gzip working tree this zstd one replaces: left beside it, readers' glob would download both.
        legacy = f"enrichment-{code}.tar.gz"
        if legacy in (self.asset_names() or []):
            self.release("delete-asset", tag, legacy, "-y")
        return 0

    def latest(self, code: str) -> int:
        """prod-latest: the marker only, LAST (a reader sees the old recording or the new one), then the pruning."""
        pinned = self.env.get("KINOWO_PINNED_TREE", "")
        if not pinned:
            self.say("the boot did not finish, or nothing was packed — prod-latest stays as it was")
            return 0
        marker = f"prod-latest-{code}.txt"
        with open(marker, "w") as f:
            f.write(f"{self.env.get('KINOWO_CONVERGENCE_CORPUS_RUN', '')} "
                    f"{self.env.get('KINOWO_CONVERGENCE_CORPUS_RECORDED_AT', '')} {pinned}\n")
        if self.release("upload", self.tag(), marker, "--clobber") != 0:
            return 1
        self.say(f"marked {pinned} as {code}'s prod-latest recording")
        self.prune_pairs(code)
        return 0

    def verified_pin(self, run: str, countries: str, verdict: str, directory: str) -> int:
        return convergence_publish_pin.Pin(self, run, verdict, directory).countries(countries)

    # ── what the writes share ─────────────────────────────────────────────────────────────────

    def release(self, *args: str) -> int:
        """scripts/ci/gh-release.sh, its messages where they came."""
        return self.runner.run(tool_process.Command([convergence_release.GH_RELEASE, *args], self.env))

    def release_quietly(self, *args: str) -> int:
        return self.runner.capture(tool_process.Command([convergence_release.GH_RELEASE, *args], self.env)).code

    def asset_names(self, quiet: bool = False) -> Optional[List[str]]:
        """The release's asset names, None when the listing failed (or hung past 120 s)."""
        return self.github.asset_names(self.tag(), 120, quiet)

    def kept_runs(self, code: str) -> Optional[List[str]]:
        """The runs pruning must keep: the hermetic pair's, prod-latest's and the kept landing recordings. None when a
        marker could not be READ: a keep-list missing the hermetic pair would delete the tree every hermetic leg replays."""
        kept: List[str] = []
        with tempfile.TemporaryDirectory() as directory:
            for marker in (f"hermetic-{code}.txt", f"prod-latest-{code}.txt"):
                try:
                    if self.github.optional("release", "download", self.tag(), "--pattern", marker, "--dir", directory,
                                            "--clobber", timeout="120"):
                        with open(os.path.join(directory, marker)) as f:
                            kept += [line.split(" ")[0] for line in f.read().splitlines()]
                except convergence_release.ReadFailed:
                    return None
        return kept + self.env.get("KINOWO_KEPT_RECORDINGS", "528 573").split(" ")

    def prune_pairs(self, code: str) -> None:
        """Keep the newest FIVE pairs of either compressor (keeping two evicted a pair before an identity measure
        dispatched on it could restore it, measure 37114004953), and only their fills, refetch and refused lists."""
        protected = self.kept_runs(code)
        if protected is None:
            self.say(f"::warning::could not read {code}'s hermetic or prod-latest marker -- pruning nothing this time"
                     " (the next pin prunes)")
            return
        for stale in self.stale_pairs(code):  # as `| sort -r | tail -n +6 | cut -f2` took them
            if stale.split("-")[2].split(".")[0] not in protected:
                self.release("delete-asset", self.tag(), stale, "-y")
        names = self.asset_names() or []
        kept = tree_runs(code, names)
        listed = rf"^(fill|refetch|refused)-{code}-[0-9]+-[0-9]+(-[0-9]+)?(-[a-z][a-z-]*)?\.(tar\.zst|tsv)$"
        for asset in names:
            if re.match(listed, asset) and asset.split("-")[2] not in kept:
                self.release("delete-asset", self.tag(), asset, "-y")

    def stale_pairs(self, code: str) -> List[str]:
        done = self.github.captured(["gh", "release", "view", self.tag(), "--json", "assets", "--jq",
                                     ".assets[] | [.createdAt, .name] | @tsv"])
        rows = [line.split("\t") for line in done.stdout.splitlines() if "\t" in line]
        pairs = sorted((r for r in rows if re.match(rf"^enrichment-{code}-[0-9]+\.tar\.(gz|zst)$", r[1])), reverse=True)
        return [r[1] for r in pairs[5:]]

    def dead(self, code: str) -> int:
        names = self.asset_names(quiet=True)
        if names is None:
            return 0  # pruning is housekeeping: a listing that fails prunes nothing now, the next leg's does
        trees = tree_runs(code, names)
        for overlay in names:
            if re.match(rf"^identity-overlay-{code}-[0-9]+\.tar\.(gz|zst)$", overlay) and \
                    re.findall("[0-9]+", overlay)[0] not in trees:
                self.release("delete-asset", self.tag(), overlay, "-y")
        if self.tag() == self.env.get("TAKES_RELEASE_TAG", ""):
            return 0
        for takes in names:
            if re.match(rf"^takes-[a-z]+-{code}[-.]", takes):
                self.release("delete-asset", self.tag(), takes, "-y")
        return 0


def tree_runs(code: str, names: List[str]) -> Set[str]:
    """The runs of the country's pinned trees on the release (`grep -oE '[0-9]+'` of each enrichment-<code>-<run>)."""
    return {d for n in names if re.match(rf"^enrichment-{code}-[0-9]+\.tar\.(gz|zst)$", n) for d in re.findall("[0-9]+", n)}


def link(source: str, target: str) -> None:
    """`ln -f`."""
    if os.path.lexists(target):
        os.remove(target)
    os.link(source, target)


USAGE = {"dead": convergence_release.Usage(1, 1, "dead <code>"),
         "tree": convergence_release.Usage(3, 3, "tree <code> <complete> <log>"),
         "latest": convergence_release.Usage(1, 1, "latest <code>"),
         "verified-pin": convergence_release.Usage(4, 4, "verified-pin <run> <codes> <verdict> <diagnostics dir>")}


def main(argv: List[str]) -> int:
    return convergence_release.main(Publish, TOOL, USAGE, "<tree <code> <complete> <log> | latest <code> | verified-pin"
                                    " <run> <codes> <verdict> <dir>>", argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
