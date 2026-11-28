#!/usr/bin/env python3
"""Lay out ONE "Record scrape fixtures" run's recording beside the checkout, not in it — each
country's scrape corpus, the enrichment tree recorded with it, and its identity overlay when the
release holds one — under <dest>/pair as the repo's own tree (test/resources/fixtures/{corpus,
enrichment-<cc>}), to replay or diff an older recording against today's. scripts/identity-capture.sh
and scripts/convergence-local.sh fetch only the newest (or pinned) recording, into the checkout.

  scripts/identity-fetch-recording.sh [--corpus-only | --no-overlay] [--fills "<asset> ..."] <recording run id> <dest dir> [cc ...]
                                                                        (default: pl de uk us es)
  --corpus-only  the scrape corpus alone: no enrichment tree, no overlay
  --no-overlay   the corpus and enrichment tree, without the identity overlay (what the decorations relearn reads)
  --fills        the fill assets a hermetic leg laid over its tree, as its log names them ("fills: fill-uk-573-655-1-
                 convergence.tar.zst ..."): each of a laid-out country's (fill-<cc>-...) is laid over its tree in order,
                 a later file winning, as .github/scripts/convergence-fill.sh unpack lays them -- so a replay reads the
                 answers that leg read. A named fill the release does not hold fails the fetch.

Each country's corpus is laid out LAST, its cinema-scrapes-<cc>.json.gz moved in by a rename: that file
there means the country is laid out whole (identity_recorded in scripts/identity-tools.sh, which
calls this one download at a time). Every gh download goes through tool-conventions.sh's gh_download: bounded
(KINOWO_IDENTITY_DOWNLOAD_TIMEOUT seconds, default 1800) and a failed read asked again (GH_DOWNLOAD_ATTEMPTS).

Needs `gh` signed in. KINOWO_REPO (default pawelkrupinski/movies) and FIXTURE_RELEASE_TAG (default
convergence-fixtures) name where the recording lives. A recording the convergence release no longer holds (it keeps
the newest five pairs), or one recorded on a forge that no longer holds it (no run, so no artifact, by that id: 38713274617),
is read from the ARCHIVE release instead (RECORDING_ARCHIVE_TAG, default identity-recordings): its tree under the
same name, and its corpus as scrapes-<cc>-<run>.tar.*, which is asked first (absent, the run's artifact is read). Archives are zstd or gzip; each is unpacked by
its magic bytes (.github/scripts/unpack-fixture-archive.sh). Downloads are kept in <dest>/archive.
Exit: 0 laid out, 1 a piece missing or unreadable (named), 2 usage."""
from __future__ import annotations

import glob
import os
import shutil
import sys
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Union

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from processes import machine  # noqa: E402
from cli import tool_countries  # noqa: E402
import tool_process  # noqa: E402
from processes import tool_shell  # noqa: E402

UNPACK = os.path.join(os.path.dirname(HERE), ".github", "scripts", "unpack-fixture-archive.sh")
FIXTURES = os.path.join("test", "resources", "fixtures")
GOT, ABSENT = 0, 3  # gh_download's answers; anything else is a read that failed
UNREADABLE = -1     # fetch(): the release could not be read


class Failed(Exception):
    """A piece of the recording missing or unreadable: the line said, exit 1."""


@dataclass
class Options:
    corpus_only: bool = False
    overlay: bool = True
    fills: List[str] = field(default_factory=lambda: [])
    run: str = ""
    dest: str = ""
    countries: List[str] = field(default_factory=lambda: [])


def leading_flags(o: Options, argv: List[str]) -> Optional[int]:
    """The index of the first argument after the flags, each read into `o`; None when --fills has no value."""
    i = 0
    while i < len(argv) and argv[i] in ("--corpus-only", "--no-overlay", "--fills"):
        if argv[i] == "--fills":
            if i + 1 >= len(argv):
                return None
            i += 1
            o.fills = argv[i].replace(",", " ").split()
        elif argv[i] == "--corpus-only":
            o.corpus_only = True
        else:
            o.overlay = False
        i += 1
    return i


def options(argv: List[str]) -> Optional[Options]:
    """The options, or None when asked wrongly (too few arguments, --fills with no value)."""
    o = Options()
    i = leading_flags(o, argv)
    rest = argv[i:] if i is not None else []
    if len(rest) < 2:
        return None
    o.run, o.dest, o.countries = rest[0], rest[1], rest[2:] or list(tool_countries.CODES)
    return o


class Fetch:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner) -> None:
        self.o, self.env, self.runner = o, env, runner
        self.repo = env.get("KINOWO_REPO") or "pawelkrupinski/movies"
        self.release = env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"
        self.archive_tag = env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        self.limit = env.get("KINOWO_IDENTITY_DOWNLOAD_TIMEOUT") or "1800"
        self.pair = os.path.join(o.dest, "pair")
        self.archive = os.path.join(o.dest, "archive")

    def download(self, *args: str, to_stdout: bool = False) -> int:
        """gh_download's status; what gh says on stdout goes to stderr (`>&2`, a release read's) unless `to_stdout`."""
        done = tool_shell.shell_function(self.runner, self.env, "scripts/tool-conventions.sh", "gh_download",
                                         [self.limit, *args])
        (sys.stdout if to_stdout else sys.stderr).write(done.stdout)
        return done.code

    def release_download(self, tag: str, pattern: str) -> int:
        return self.download("release", "download", tag, "-R", self.repo, "--pattern", pattern, "--dir", self.archive,
                             "--clobber")

    def fetch(self, prefix: str, tag: str) -> Union[str, int]:
        """The downloaded asset's path, whichever compressor packed it; ABSENT when the release lacks it, UNREADABLE
        when the release could not be read (gh_download tells the two apart, a timeout is a failed read)."""
        for ext in ("tar.zst", "tar.gz"):
            status = self.release_download(tag, f"{prefix}.{ext}")
            if status == GOT:
                return os.path.join(self.archive, f"{prefix}.{ext}")
            if status != ABSENT:
                return UNREADABLE
        return ABSENT

    def unpack(self, archive: str, into: str) -> None:
        if self.runner.run(tool_process.Command(["bash", UNPACK, archive, into], self.env)) != 0:
            raise Failed(f"could not unpack {archive}")

    def tree(self, cc: str) -> None:
        """The enrichment tree (the convergence release's, else the archive's), its overlay, the fills named."""
        prefix = f"enrichment-{cc}-{self.o.run}"
        found = self.fetch(prefix, self.release)
        if found == UNREADABLE:
            raise Failed(f"{cc}: could not read release {self.release} (see gh's error above)")
        if found == ABSENT:
            found = self.fetch(prefix, self.archive_tag)
            if found == UNREADABLE:
                raise Failed(f"{cc}: could not read release {self.archive_tag} (see gh's error above)")
            if found == ABSENT:
                raise Failed(f"{cc}: neither release {self.release} nor {self.archive_tag} holds {prefix}")
            print(f"{cc}: tree from the archive release {self.archive_tag}", file=sys.stderr)
        self.unpack(str(found), self.pair)
        self.overlay(cc)
        for fill in self.o.fills:
            if fill.startswith(f"fill-{cc}-"):
                self.fill(cc, fill)

    def overlay(self, cc: str) -> None:
        """The overlay is optional: absent is fine, a failed read is not."""
        if not self.o.overlay:
            return
        found = self.fetch(f"identity-overlay-{cc}-{self.o.run}", self.release)
        if found == UNREADABLE:
            raise Failed(f"{cc}: could not read release {self.release} (see gh's error above)")
        if isinstance(found, str):
            self.unpack(found, self.pair)

    def fill(self, cc: str, fill: str) -> None:
        status = self.release_download(self.release, fill)
        if status == ABSENT:
            raise Failed(f"{cc}: release {self.release} holds no fill {fill}")
        if status != GOT:
            raise Failed(f"{cc}: could not read release {self.release} for fill {fill} (see gh's error above)")
        self.unpack(os.path.join(self.archive, fill), self.pair)
        print(f"{cc}: laid fill {fill} over the tree", file=sys.stderr)

    def corpus_archive(self, cc: str) -> str:
        """The scrape corpus's archive: the archive release's, else the run's artifact."""
        # a download cut short earlier leaves files gh will not overwrite
        scrape = os.path.join(self.archive, f"scrape-{cc}")
        shutil.rmtree(scrape, ignore_errors=True)
        os.makedirs(scrape)
        found = self.fetch(f"scrapes-{cc}-{self.o.run}", self.archive_tag)
        if found == UNREADABLE:
            raise Failed(f"{cc}: could not read release {self.archive_tag} (see gh's error above)")
        if isinstance(found, str):
            print(f"{cc}: corpus from the archive release {self.archive_tag}", file=sys.stderr)
            return found
        if self.download("run", "download", self.o.run, "-R", self.repo, "--name", f"scrape-fixtures-{cc}",
                         "--dir", scrape, to_stdout=True) != 0:
            raise Failed(f"{cc}: could not download run {self.o.run}'s scrape-fixtures-{cc} (gh failed or timed out)")
        packed = sorted(glob.glob(os.path.join(scrape, f"scrapes-{cc}.tar.*")))
        if not packed:
            raise Failed(f"{cc}: run {self.o.run}'s scrape-fixtures-{cc} holds no scrapes-{cc}.tar.*")
        return packed[0]

    def corpus(self, cc: str) -> None:
        """Unpacked beside the pair, then moved in: cinema-scrapes-<cc>.json.gz is renamed in last."""
        archive = self.corpus_archive(cc)
        beside = os.path.join(self.archive, f"corpus-{cc}")
        shutil.rmtree(beside, ignore_errors=True)
        self.unpack(archive, beside)
        unpacked = os.path.join(beside, FIXTURES, "corpus")
        last = f"cinema-scrapes-{cc}.json.gz"
        if not os.path.isfile(os.path.join(unpacked, last)):
            raise Failed(f"{cc}: run {self.o.run}'s scrapes-{cc} archive holds no {last}")
        corpus = os.path.join(self.pair, FIXTURES, "corpus")
        os.makedirs(corpus, exist_ok=True)
        for name in sorted(os.listdir(unpacked)):
            if name != last:
                move(os.path.join(unpacked, name), os.path.join(corpus, name))
        move(os.path.join(unpacked, last), os.path.join(corpus, last))
        shutil.rmtree(beside, ignore_errors=True)

    def size(self) -> str:
        done = self.runner.capture(tool_process.Command(["du", "-sh", self.pair], self.env))
        return done.stdout.split("\t", 1)[0].strip()

    def main(self) -> int:
        os.makedirs(self.pair, exist_ok=True)
        os.makedirs(self.archive, exist_ok=True)
        try:
            for cc in self.o.countries:
                if not self.o.corpus_only:
                    self.tree(cc)
                self.corpus(cc)
                print(f"{cc}: {self.size()} laid out so far", flush=True)
        except Failed as failed:
            print(failed, file=sys.stderr)
            return 1
        return 0


def move(source: str, target: str) -> None:
    """`mv -f`: a directory already there replaced, as a rename."""
    if os.path.isdir(target) and not os.path.islink(target):
        shutil.rmtree(target)
    os.replace(source, target)


def main(argv: List[str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        print(__doc__)
        return 0
    o = options(argv)
    if o is None:
        print(__doc__, file=sys.stderr)
        return 2
    return Fetch(o, dict(machine.environment()), tool_process.SubprocessRunner()).main()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
