#!/usr/bin/env python3
"""Fetch a convergence leg's enrichment tree and unpack it into a STAGING directory, for convergence-setup to move into
the workspace once its build-cache steps are done.

  restore-enrichment-tree.sh <code> <mode> <stage dir>

Reads FIXTURE_RELEASE_TAG, KINOWO_CONVERGENCE_TREE_ASSET ("Resolve the recorded pair") and GH_TOKEN from the
environment.

WHY A STAGE, AND WHY IN THE BACKGROUND. The tree is 200-300 MB of zstd and ~130k files: a download and an unpack that
were 13-19 s of every leg's setup (run 37105119296), queued behind the JDK, sbt and build-cache restores they share
nothing with. Those restores have to run BEFORE the tree is in the workspace, because their cache keys glob the
workspace and a restored tree is 130k files every glob would walk (~50 s of the US setup once). Unpacked outside it --
into $RUNNER_TEMP, on the same filesystem, so moving it in afterwards is a rename -- the two overlap: convergence-setup
starts this first (scripts/ci/in-background.sh) and waits for it where it used to unpack.

Exit status: 0 with the tree staged, or with nothing to stage on a RECORDING (which fetches live and publishes what it
learns); 3 when a replay leg's pinned tree is missing (a hermetic leg replays nothing else, so its setup fails); 4 when
the release could not be read at all; anything else is an unpack that failed -- the tree's, or a fill's the pair names.

convergence-setup runs it by the name .github/scripts/restore-enrichment-tree.sh; tested by
restore_enrichment_tree_test.py beside it."""
from __future__ import annotations

import glob
import os
import subprocess
import sys
from typing import List, Mapping, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "..", "scripts", "lib"))
import machine  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402
from tool_shell import shell_function  # noqa: E402

USAGE = "usage: restore-enrichment-tree.sh <code> <mode> <stage dir>"
ABSENT = 3


class Restore:
    def __init__(self, code: str, mode: str, stage: str, env: Mapping[str, str], runner: Runner) -> None:
        self.code = code
        self.mode = mode
        self.stage = stage
        self.env = env
        self.runner = runner
        # The pinned pair's tree for a hermetic leg, the working tree for a recording one. Unpacked by whatever name
        # it downloads under -- the tarball's paths are what place it, and its magic what inflates it.
        self.asset = env.get("KINOWO_CONVERGENCE_TREE_ASSET") or f"enrichment-{code}.tar.*"
        self.archives = stage + ".archive"
        self.timeout = env.get("RESTORE_TREE_DOWNLOAD_TIMEOUT") or "1200"

    def download(self, tag: str, env: Mapping[str, str]) -> int:
        """Only gh's own "no assets match" means the release lacks the tree (gh_download's 3). Any other failure -- a
        network blip, a 5xx, an expired token -- is a read that did not happen, not an empty release: a recording
        that took it for one would fetch every lookup live and publish that over the tree it never read. gh_download
        bounds each attempt (RESTORE_TREE_DOWNLOAD_TIMEOUT seconds, 1200) and asks a failed read again: one GitHub 5xx
        used to fail the leg's setup."""
        args = [self.timeout, "release", "download", tag, "--pattern", self.asset, "--dir", self.archives, "--clobber"]
        done = shell_function(self.runner, env, "scripts/tool-conventions.sh", "gh_download", args)
        sys.stdout.write(done.stdout)
        sys.stdout.flush()
        return done.code

    def located(self, tag: str) -> Tuple[int, str]:
        """gh_download's status for the tree, and the release it came from."""
        status = self.download(tag, self.env)
        # A replay's pinned tree the rolling release no longer holds (it keeps the newest five): the ARCHIVE release
        # (RECORDING_ARCHIVE_TAG, default identity-recordings) keeps the landing recordings' trees (528, 573) under the
        # same name. The archive release is permanent: its "release not found" is GitHub's hiccup, asked again (run
        # 43317254260).
        if status != ABSENT or self.mode == "record":
            return status, tag
        archive = self.env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        status = self.download(archive, {**self.env, "GH_DOWNLOAD_RELEASE_EXISTS": "1"})
        return status, archive if status == 0 else tag

    def fetched(self, tag: str) -> Optional[int]:
        """None with the tree in the archives, or none to fetch (a recording's); else the exit status to end with."""
        status, tag = self.located(tag)
        if status == 0:
            # Both spellings of the working tree only in the moment between a first zstd publish and its deleting the
            # gzip it replaced: the zstd one is the newer.
            if glob.glob(os.path.join(self.archives, "*.tar.zst")):
                for gzip in glob.glob(os.path.join(self.archives, "*.tar.gz")):
                    os.remove(gzip)
            print(f"enrichment tree {self.asset} from release {tag}", flush=True)
            return None
        if status != ABSENT:
            print(f"::error::could not read release {tag} for {self.asset} (gh's error above)", flush=True)
            return 4
        if self.mode != "record":
            print(f"::error::the pinned tree {self.asset} is not in release {tag} (it keeps the newest five) — a "
                  "hermetic leg replays nothing else", flush=True)
            return 3
        return None

    def unpacked(self, tree: str) -> int:
        self.runner.run(Command(["ls", "-l", self.archives + "/"], self.env))
        status = self.runner.run(Command([os.path.join(HERE, "unpack-fixture-archive.sh"), tree, self.stage],
                                         self.env))
        if status != 0:
            return status
        size = subprocess.run(["du", "-h", tree], stdout=subprocess.PIPE, text=True).stdout.split("\t", 1)[0]
        print(f"restored {tree} ({size})", flush=True)
        # A HERMETIC leg's pair is the pinned tree AND the fills earlier legs published over it, oldest first
        # ("Resolve the recorded pair" names them in KINOWO_CONVERGENCE_FILL_ASSETS; convergence-fill.sh). One it
        # cannot restore fails the restore, and so the leg's setup: the pair names it, so its verdict and its bisect
        # would not agree.
        fills = self.env.get("KINOWO_CONVERGENCE_FILL_ASSETS", "").split()
        if self.mode == "record" or not fills:
            return 0
        return self.runner.run(Command([os.path.join(HERE, "convergence-fill.sh"), "unpack", self.stage, *fills],
                                       self.env))

    def run(self, tag: str) -> int:
        os.makedirs(self.archives, exist_ok=True)
        os.makedirs(self.stage, exist_ok=True)
        ended = self.fetched(tag)
        if ended is not None:
            return ended
        trees = sorted(glob.glob(os.path.join(self.archives, "*")))
        if not trees:
            print(f"no enrichment capture found for {self.code} — this leg fetches live and publishes the result",
                  flush=True)
            return 0
        return self.unpacked(trees[0])


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if len(argv) < 3 or not all(argv[:3]):
        print(USAGE, file=sys.stderr)
        return 1
    tag = env.get("FIXTURE_RELEASE_TAG", "")
    if not tag:
        print("restore-enrichment-tree.sh: FIXTURE_RELEASE_TAG names the rolling release", file=sys.stderr)
        return 1
    return Restore(argv[0], argv[1], argv[2], env, SubprocessRunner()).run(tag)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
