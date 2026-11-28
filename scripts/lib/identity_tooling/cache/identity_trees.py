"""A REF'S SOURCES LAID OUT ONCE UNDER THE CACHE (scripts/lib; identity-tools.sh's identity_tree and the layout it
calls): a compiled-once copy of the ref's sources, each named tool file (a path relative to the repository) dropped in
from this checkout -- the tool's own code, so a ref from before the tool works.

The tree is shared by every checkout, so two checkouts whose tool files differ must not drop theirs into one tree (the
compile would be a mix, and each would run the other's code): the first set of tool files to reach refs/<sha> claims it
(target/identity-tools/tools-hash); a checkout with other tool files gets refs/<sha>-t<hash of its tool files>.

  Trees(ctx, locks, seeds).tree(ref, files) -> the tree holding exactly this checkout's tool files
  Trees.tree_judgements(sha, tree)          the ref's judgements in <tree> (a tree laid out before they were, too): the
                                            worker's build bundles them as a resource (build.sbt) and fails to compile
                                            without them
  tools_hash(repo, files)                   the hash of this checkout's files (names and contents), 12 hex"""
from __future__ import annotations

import filecmp
import hashlib
import os
import shutil
import subprocess
from typing import Sequence

from identity_tooling.base import identity_older_refs
from processes import machine
from identity_tooling.base.identity_context import JUDGEMENTS, Context, Failed
from identity_tooling.base.identity_lock import Locks
from identity_tooling.base.identity_tree_seeds import Seeds

MARK = ".identity-tree-whole"
TOOLS_DIR = os.path.join("target", "identity-tools")
HASH_MARK = os.path.join(TOOLS_DIR, "tools-hash")
EXCLUDED = (".", ":(exclude)test/resources", ":(exclude)ios", ":(exclude)android")


def tools_hash(repo: str, files: Sequence[str]) -> str:
    digest = hashlib.sha1()
    for name in files:
        digest.update(f"{name}\n".encode())
        try:
            with open(os.path.join(repo, name), "rb") as f:
                digest.update(f.read())
        except OSError:
            pass
    return digest.hexdigest()[:12]


class Trees:
    def __init__(self, ctx: Context, locks: Locks, seeds: Seeds) -> None:
        self.ctx, self.locks, self.seeds = ctx, locks, seeds

    @staticmethod
    def whole(directory: str) -> bool:
        return os.path.isfile(os.path.join(directory, MARK))

    def tree(self, ref: str, files: Sequence[str]) -> str:
        sha = self.ctx.git().line("rev-parse", "--verify", f"{ref}^{{commit}}")
        if not sha:
            self.ctx.say(f"no ref {ref}")
            raise Failed(2)
        directory = os.path.join(self.ctx.cache, "refs", sha)
        self.laid_out(ref, sha, directory, f"tree-{sha}")
        # one agent a tree drops the files in: a second, comparing while the first copies, would copy over it again and
        # move the time the first's compile reads -- or read (and remove) the seed marker the first still needs
        with self.locks.held(f"tools-{sha}"):
            return self.claimed(ref, sha, directory, files)

    def laid_out(self, ref: str, sha: str, directory: str, lock: str) -> None:
        if not self.whole(directory):
            with self.locks.held(lock):
                self.layout(ref, sha, directory)
        self.tree_judgements(sha, directory)

    def claimed(self, ref: str, sha: str, directory: str, files: Sequence[str]) -> str:
        """The tree holding exactly this checkout's tool files: <directory> when unclaimed or claimed by the same tool
        files, else the variant tree for their hash."""
        digest = tools_hash(self.ctx.repo, files)
        held = self.ctx.files.read_text_or(os.path.join(directory, HASH_MARK), "").strip()
        if held and held != digest:
            directory = f"{directory}-t{digest}"
            self.laid_out(ref, sha, directory, f"tree-{sha}-t{digest}")
        self.drop_tools(directory, files)
        self.ctx.files.make_directories(os.path.join(directory, TOOLS_DIR))
        self.ctx.files.write_text(os.path.join(directory, HASH_MARK), digest + "\n")
        return directory

    def drop_tools(self, directory: str, files: Sequence[str]) -> None:
        """Each named file dropped into the laid-out tree from this checkout, whole or not at all."""
        identity_older_refs.older_titles(self.ctx.files, directory)
        identity_older_refs.older_models(self.ctx.files, directory)
        for name in files:  # copied only when it changed: an unchanged copy keeps its time, and the ref is not compiled again
            source, target = os.path.join(self.ctx.repo, name), os.path.join(directory, name)
            if not os.path.isfile(source):
                self.ctx.say(f"cp: {source}: No such file or directory")  # the shell's cp said so, and went on
                continue
            self.ctx.files.make_directories(os.path.dirname(target))
            if not (os.path.isfile(target) and filecmp.cmp(source, target, shallow=False)):
                shutil.copyfile(source, target + ".part")
                os.replace(target + ".part", target)
        self.touch_recompiled(directory, files)

    def touch_recompiled(self, directory: str, files: Sequence[str]) -> None:
        """A classpath kept from a seed (layout) holds the classes of the SEED's tool files: one differing from those now
        dropped in is made newer than it, and compiles."""
        marker = os.path.join(directory, TOOLS_DIR, "seed")
        seed = self.ctx.files.read_text_or(marker, "").strip()
        if not self.ctx.files.exists(marker):
            return
        for name in files:
            target = os.path.join(directory, name)
            if os.path.isfile(target) and not same_file(os.path.join(seed, name), target):
                os.utime(target, None)
        self.ctx.files.remove(marker)

    def tree_judgements(self, sha: str, directory: str) -> None:
        if os.path.isfile(os.path.join(directory, JUDGEMENTS)):
            return
        if not self.ctx.git().ok("cat-file", "-e", f"{sha}:{JUDGEMENTS}", quiet=True):
            return
        self.archive_into(directory, sha, [JUDGEMENTS], ["--"])

    def archive_into(self, directory: str, sha: str, paths: Sequence[str], first: Sequence[str] = ("--",)) -> None:
        """The paths of <sha> extracted into <directory>. Through a file, never a pipe: tar may stop reading at the
        archive's end while git archive still writes its last record's padding, and git's SIGPIPE then failed the layout
        with nothing said (Order independence #90, #97). Each side's failure is its own, and named."""
        tarball = f"{directory}.tar-{machine.own_pid()}"
        try:
            if not self.ctx.git().ok("archive", "-o", tarball, sha, *first, *paths):
                self.ctx.log(f"git archive of {sha} failed")
                raise Failed(1)
            if machine.status(["tar", "-x", "-C", directory, "-f", tarball]) != 0:
                self.ctx.log(f"extracting {sha} into {directory} failed")
                raise Failed(1)
        finally:
            self.ctx.files.remove(tarball)

    def layout(self, ref: str, sha: str, directory: str) -> None:
        """The ref's sources in <directory>, laid out beside it and moved in whole (the mark, written last into
        <directory>.part before the move, says whole: build.sbt was the mark until 2026-11-20, and a tree holding none
        was removed and laid out again by every call, under the steps still reading it: Order independence #90)."""
        if self.whole(directory):
            return
        self.ctx.log(f"laying out {ref} ({sha}) in {directory}")
        part = directory + ".part"
        shutil.rmtree(part, ignore_errors=True)
        shutil.rmtree(directory, ignore_errors=True)
        self.ctx.files.make_directories(part)
        self.archive_into(part, sha, EXCLUDED)
        self.tree_judgements(sha, part)
        self.seeded(part, directory, sha)
        self.ctx.files.write_text(os.path.join(part, MARK), "")
        os.rename(part, directory)

    def seeded(self, part: str, directory: str, sha: str) -> None:
        """Seed the compile from the nearest compiled code (APFS clones, no copy): sbt recompiles only what the ref changed."""
        seed = self.seeds.nearest(sha)
        if not seed:
            shutil.rmtree(os.path.join(part, TOOLS_DIR), ignore_errors=True)
            return
        apart = self.seeds.distance(seed, sha)
        self.ctx.log(f"seeding its compile from {seed} ({'' if apart is None else apart} source file(s) apart)")
        self.copy_builds(seed, part)
        # Zinc never deletes a class file it did not produce: one another build wrote into the seed's classes/ (IntelliJ's
        # JPS, 2026-10-18) would shadow the real class here for good
        machine.status(["python3", os.path.join(self.ctx.repo, "scripts", "zinc_orphans.py"), part])
        shutil.rmtree(os.path.join(part, TOOLS_DIR), ignore_errors=True)
        self.keep_classpath(seed, part, directory, apart)

    def copy_builds(self, seed: str, part: str) -> None:
        """Every module the ref has (each */src), as sbt-warm's seed reads them: a fixed list missed the scrapers and geodata."""
        modules = [os.path.basename(os.path.dirname(m)) + "/target" for m in sorted(self.sources(part))]
        for target in ["project/target", *modules, "target"]:
            if os.path.isdir(os.path.join(seed, target)):
                self.ctx.files.make_directories(os.path.join(part, os.path.dirname(target)))
                clone = ["cp", "-cR", os.path.join(seed, target), os.path.join(part, target)]
                if machine.status(clone, err=subprocess.DEVNULL) != 0:
                    machine.status(["cp", "-R", *clone[2:]])

    def sources(self, part: str) -> Sequence[str]:
        return [os.path.join(part, name, "src") for name in self.ctx.files.list(part)
                if os.path.isdir(os.path.join(part, name, "src"))]

    def keep_classpath(self, seed: str, part: str, directory: str, apart: object) -> None:
        """The very sources of a compiled ref (a commit changing no source: a judgement, a doc): its classes ARE this
        ref's, so its classpath is kept, pointed here -- no sbt start to find nothing to compile (~15 s). The tool files
        dropped in afterwards (tree) are newer than it when they differ, and recompile as ever."""
        kept = os.path.join(seed, TOOLS_DIR, "classpath")
        if apart != 0 or seed == self.ctx.repo or not (os.path.isfile(kept) and os.path.getsize(kept) > 0):
            return
        self.ctx.files.make_directories(os.path.join(part, TOOLS_DIR))
        with open(kept, encoding="utf-8") as source:
            text = source.read().replace(f"{seed}/", f"{directory}/")
        self.ctx.files.write_text(os.path.join(part, TOOLS_DIR, "classpath"), text)
        self.ctx.files.write_text(os.path.join(part, TOOLS_DIR, "seed"), seed + "\n")


def same_file(a: str, b: str) -> bool:
    return os.path.isfile(a) and filecmp.cmp(a, b, shallow=False)
