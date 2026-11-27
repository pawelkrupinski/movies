#!/usr/bin/env python3
"""The release side of a convergence leg's FILL: what a hermetic leg's tree lacked, fetched by the next leg's
convergence row before its suite and published beside the pinned pair, so the gap closes run over run instead of
waiting for the next recording (docs/design/convergence-fixture-fill.md). convergence-fill.sh, beside it, is the name
its callers run.

  convergence-fill.sh fills  <code> <corpus run>               # the pair's fill assets, oldest first, one line
  convergence-fill.sh unpack <stage dir> [fill asset...]       # each laid over a staged tree
  convergence-fill.sh gaps   <code> <corpus run> <out file>    # the newest refetch list, less what was refused lately;
                                                               # prints its line count
  convergence-fill.sh pack   <fill dir> <archive>              # what a fill fetched; no archive when nothing
  convergence-fill.sh fold   <code> <corpus run>               # fold the pair's fills into one, past CONVERGENCE_FILL_FOLD_AT

Reads FIXTURE_RELEASE_TAG and GH_TOKEN from the environment.

NAMES, NEVER A REPLACEMENT. A fill is `fill-<code>-<corpus run>-<run id>-<run attempt>[-<row>].tar.zst` -- a leg's
pre-suite fill carries its row (`-convergence`), the manual `Convergence fill` workflow's none -- a refetch list
`refetch-<code>-<corpus run>-<run id>-<run attempt>.tsv`, and a fill's refused gaps `refused-<...>.tsv`: each publish adds
an asset under a name nobody else writes, a re-run of a failed job included, so no reader ever meets the moment
`--clobber` deletes an asset before re-uploading it. (Names from before the attempt was part of them still read, as
attempt 0.) Readers take every fill of the pair (`fills`), the NEWEST list (`gaps`) and the newest few refused lists; the
pin step prunes them all with the pair they belong to.

FOLDED, SO A PINNED PAIR'S FILLS STAY BOUNDED (run 41351315090: the release at GitHub's 1000-asset cap, 278 of them fills
of pair 573). Past CONVERGENCE_FILL_FOLD_AT fills since the newest fold, `fold` lays them over one another as `unpack`
does and publishes the result as `fill-<code>-<corpus>-<run id>-<attempt>-folded.tar.zst`, under the newest one's run id
and attempt, so it sorts after every fill it holds. `fills` reads from the newest fold on. A fold deletes only what the
fold BEFORE it holds: a leg that listed the fills before this fold existed still finds every asset it named.

A release it cannot LIST, asked three times, fails `fills` and `gaps` loudly: a failed listing is not an empty release.
A fill it cannot DOWNLOAD or unpack, asked three times, fails `unpack` the same way: the leg's pair names it, so a leg
replaying without it would decide its verdict on a pair it never replayed. (convergence_fill_folds.py: gaps and fold)"""
from __future__ import annotations

import os
import re
import sys
import time
from typing import Callable, List, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
import convergence_fill_folds  # noqa: E402  (its own directory is sys.path's first, run as a script)
import machine  # noqa: E402
import convergence_release  # noqa: E402
import tool_process  # noqa: E402

TOOL = "convergence-fill.sh"
ATTEMPTS = 3
UNPACK = os.path.join(HERE, "unpack-fixture-archive.sh")


def run_key(name: str) -> "tuple[int, int, str]":
    """`LC_ALL=C sort -t- -k4,4n -k5,5n`: by run id, then attempt (a row or extension reads as 0), then the name."""
    fields = name.split("-")

    return leading_number(fields, 3), leading_number(fields, 4), name


def leading_number(fields: List[str], i: int) -> int:
    """`sort -n`'s reading of a field: its leading digits, 0 for none (or no such field)."""
    digits = re.match("[0-9]*", fields[i]) if i < len(fields) else None
    return int(digits.group(0)) if digits and digits.group(0) else 0


class Fill(convergence_release.Step):
    def tag(self) -> str:
        return self.env["FIXTURE_RELEASE_TAG"]

    def quietly(self) -> "Fill":
        """This tool with its own lines set aside, as `"$0" <subcommand> > /dev/null` ran it."""
        return Fill(self.env, self.runner, open(os.devnull, "w"), self.clock)

    def seconds(self, variable: str, default: int) -> int:
        return int(self.env.get(variable) or default)

    def backoff(self, attempt: int) -> None:
        """The wait doubling from CONVERGENCE_FILL_LIST_BACKOFF seconds (5): one GitHub 5xx is not a release that cannot
        be read, and failing the leg on it re-runs a whole convergence row."""
        time.sleep(self.seconds("CONVERGENCE_FILL_LIST_BACKOFF", 5) << (attempt - 1))

    def download_env(self) -> "dict[str, str]":
        """Every download goes through gh_download, on the listing's back-off."""
        return dict(self.env, GH_DOWNLOAD_BACKOFF=str(self.seconds("CONVERGENCE_FILL_LIST_BACKOFF", 5)))

    def assets(self) -> Optional[List[str]]:
        """Every asset name in the release; None (said) when it cannot be listed, asked ATTEMPTS times, each bounded
        (CONVERGENCE_FILL_GH_TIMEOUT, 120 s): a hung read is a failed attempt, never a leg waiting out its job."""
        for attempt in range(1, ATTEMPTS + 1):
            names = self.github.asset_names(self.tag(), self.seconds("CONVERGENCE_FILL_GH_TIMEOUT", 120))
            if names is not None:
                return names
            if attempt == ATTEMPTS:
                sys.stderr.write(f"::error::could not list release {self.tag()} after {ATTEMPTS} attempts — its fills are"
                                 " unknown, not none\n")
                return None
            sys.stderr.write(f"::warning::listing release {self.tag()} failed (attempt {attempt} of {ATTEMPTS}); asking"
                             " again\n")
            self.backoff(attempt)
        return None

    def named(self, prefix: str, code: str, corpus: str, ext: str) -> List[str]:
        """^<prefix>-<code>-<corpus>-<run id>[-<attempt>][-<row>].<ext>$, by run id then attempt; Exit(1) unlisted."""
        names = self.assets()
        if names is None:
            raise convergence_release.Exit(1)
        pattern = rf"^{prefix}-{code}-{corpus}-[0-9]+(-[0-9]+)?(-[a-z][a-z-]*)?\.{ext}$"
        return sorted((n for n in names if re.match(pattern, n)), key=run_key)

    def fills(self, code: str, corpus: str) -> int:
        self.say(" ".join(convergence_fill_folds.from_fold(self.named("fill", code, corpus, r"tar\.zst"))))
        return 0

    def download(self, *args: str, quiet: bool = False, attempts: Optional[str] = None) -> bool:
        env = self.download_env()
        if attempts:
            env["GH_DOWNLOAD_ATTEMPTS"] = attempts
        return self.github.gh_download(self.seconds("CONVERGENCE_FILL_DOWNLOAD_TIMEOUT", 1200), args, env,
                                       quiet=quiet) == 0

    def unpack(self, stage: str, *fills: str) -> int:
        if not fills:
            return 0
        downloads = f"{stage}.fills"
        os.makedirs(downloads, exist_ok=True)
        os.makedirs(stage, exist_ok=True)
        status = 0
        for fill in fills:
            if self.lay(fill, downloads, stage):
                self.say(f"laid fill {fill} over the tree")
            else:
                self.say(f"::error::fill {fill} could not be restored after {ATTEMPTS} attempts — the pair names it, so"
                         " this leg will not replay without it")
                status = 1
        return status

    def lay(self, fill: str, downloads: str, stage: str) -> bool:
        """One attempt a turn, the download and its unpack together, their messages SAID (run 41228572378's de-2 spent
        its whole tree-restore bound on one fill's retries, silenced, its log saying nothing of why)."""
        for attempt in range(1, ATTEMPTS + 1):
            if self.download("release", "download", self.tag(), "--pattern", fill, "--dir", downloads, "--clobber",
                             attempts="1") and \
                    self.runner.run(tool_process.Command([UNPACK, os.path.join(downloads, fill), stage], self.env)) == 0:
                return True
            if attempt < ATTEMPTS:
                self.backoff(attempt)
        return False

    def pack(self, directory: str, archive: str) -> int:
        files = convergence_release.regular_files(directory)
        if files == 0:
            self.say("the fill fetched nothing — no archive")
            return 0
        os.makedirs(os.path.dirname(archive) or ".", exist_ok=True)
        script = 'set -o pipefail; tar -C "$1" -cf - . | zstd -q -T0 -3 -c > "$2"'
        if self.runner.run(tool_process.Command(["bash", "-c", script, "_", directory, archive], self.env)) != 0:
            return 1
        size = self.runner.capture(tool_process.Command(["du", "-h", archive], self.env)).stdout.split("\t")[0]
        self.say(f"packed {files} fetched file(s) into {archive} ({size})")
        return 0

    def gaps(self, code: str, corpus: str, out: str) -> int:
        return convergence_fill_folds.gaps(self, code, corpus, out)

    def fold(self, code: str, corpus: str) -> int:
        return convergence_fill_folds.fold(self, code, corpus)


USAGE = {"fills": convergence_release.Usage(2, 2, "fills <code> <corpus>", more=True),
         "unpack": convergence_release.Usage(1, 1 << 20, "unpack <stage> [asset...]"),
         "gaps": convergence_release.Usage(3, 3, "gaps <code> <corpus> <out>", more=True),
         "pack": convergence_release.Usage(2, 2, "pack <dir> <archive>", more=True),
         "fold": convergence_release.Usage(2, 2, "fold <code> <corpus>", more=True)}
OVERALL = "fills <code> <corpus> | unpack <stage> [asset...] | gaps <code> <corpus> <out> | pack <dir> <archive> | fold" \
          " <code> <corpus>"


def main(argv: List[str]) -> int:
    if not machine.environment().get("FIXTURE_RELEASE_TAG"):
        sys.stderr.write(f"{TOOL}: FIXTURE_RELEASE_TAG: FIXTURE_RELEASE_TAG names the rolling release\n")
        return 1
    known: Callable[[str], bool] = USAGE.__contains__
    if argv and known(argv[0]) and len(argv) - 1 < USAGE[argv[0]].least:
        sys.stderr.write(f"{TOOL}: {USAGE[argv[0]].line}: an argument is missing\n")
        return 1
    return convergence_release.main(Fill, TOOL, USAGE, OVERALL, argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
