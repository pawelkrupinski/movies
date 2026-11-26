"""scripts/sbt-warm's SEEDING and THE SHARED BUILD CACHE (its header says what each copies, from where, and when): a
checkout with no compiled output gets the newest local build of a main commit its HEAD contains, as APFS clones; a
passing call in a clean checkout of a main commit publishes its build as that commit's cache entry."""
from __future__ import annotations

import glob
import os
import shutil
import subprocess
import time
from dataclasses import dataclass
from typing import Callable, List, Mapping, Optional, Tuple

from tool_git import Git

ZINC_ORPHANS = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "zinc_orphans.py")


def clone(source: str, target: str) -> None:
    """`source` copied to `target` as APFS clones (`cp -c`), or plainly where the volume cannot clone."""
    os.makedirs(os.path.dirname(target), exist_ok=True)
    if subprocess.run(["cp", "-cR", source, target], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode != 0:
        subprocess.run(["cp", "-R", source, target])


def line(git: Git, *args: str) -> Optional[str]:
    """git's one-line answer, stripped; None when it failed or said nothing."""
    out = git.out(*args, quiet=True)
    return out.strip() or None if out is not None else None


def has(pattern: str) -> bool:
    return bool(glob.glob(pattern))


def build_paths(directory: str) -> List[str]:
    """The paths a seed copies, of `directory`."""
    paths = [d for d in ("project/target", "project/project/target") if os.path.isdir(os.path.join(directory, d))]
    paths += [os.path.relpath(d, directory) for d in sorted(glob.glob(os.path.join(glob.escape(directory), "*/target/scala-*")))
              if os.path.isdir(d)]
    return paths


def built_zincs(directory: str) -> int:
    """How many configurations `directory` has built (Zinc state directories)."""
    return len([d for d in glob.glob(os.path.join(glob.escape(directory), "*/target/scala-*/*zinc")) if os.path.isdir(d)])


@dataclass(frozen=True)
class Places:
    root: str
    refs: str
    cache: str


def wanted(name: str, cached: bool) -> bool:
    """A seed source's directory name: a cache entry's has no dot (no .part, .retired), a ref tree's is no .part."""
    return "." not in name if cached else not name.endswith(".part")


class Builds:
    def __init__(self, places: Places, git: Callable[[str], Git], env: Mapping[str, str],
                 say: Callable[[str], None], pid: int) -> None:
        self.places, self.git, self.env, self.say, self.pid = places, git, env, say, pid
        self.here = git(places.root)

    def candidates(self) -> List[str]:
        out = self.git(os.getcwd()).out("worktree", "list", "--porcelain", quiet=True) or ""
        found = [line[len("worktree "):] for line in out.splitlines() if line.startswith("worktree ")]
        for parent, cached in ((self.places.refs, False), (self.places.cache, True)):
            if os.path.isdir(parent):
                found += [os.path.join(parent, n) for n in sorted(os.listdir(parent))
                          if wanted(n, cached) and os.path.isdir(os.path.join(parent, n))]
        return found

    def commit_of(self, directory: str) -> Tuple[Optional[str], int]:
        """The commit a build stands on, and 1 for a cache entry."""
        if directory.startswith(self.places.refs + os.sep):
            return os.path.basename(directory), 0
        if directory.startswith(self.places.cache + os.sep):
            return os.path.basename(directory), 1
        return line(self.git(directory), "rev-parse", "-q", "--verify", "HEAD"), 0

    def source(self) -> Optional[str]:
        """The newest local build of a commit this checkout's HEAD contains (the nearest; a cache entry wins a tie)."""
        head = line(self.here, "rev-parse", "-q", "--verify", "HEAD") or "origin/main"
        ranked = [r for r in (self.rank(d, head) for d in self.candidates()) if r is not None]
        return max(ranked)[2] if ranked else None

    def rank(self, directory: str, head: str) -> Optional[Tuple[int, int, str]]:
        """(commits behind it, 1 for a cache entry, the directory) of a build of an ancestor of `head`; else None."""
        if os.path.realpath(directory) == self.places.root or not self.built(directory):
            return None
        sha, cached = self.commit_of(directory)
        if not sha or not self.here.ok("merge-base", "--is-ancestor", sha, head, quiet=True):
            return None
        count = line(self.here, "rev-list", "--count", sha)
        return (int(count), cached, directory) if count and count.isdigit() else None

    @staticmethod
    def built(directory: str) -> bool:
        pattern = os.path.join(glob.escape(directory), "worker/target/scala-*/")
        return has(pattern + "test-classes") and has(pattern + "zinc")

    def unbuilt(self) -> List[str]:
        """project/target, and each module (a directory with src/) without Zinc state of any configuration."""
        root = self.places.root
        missing = [] if os.path.isdir(os.path.join(root, "project/target")) else ["project/target", "project/project/target"]
        for src in sorted(glob.glob(os.path.join(glob.escape(root), "*/src"))):
            module = os.path.basename(os.path.dirname(src))
            if not has(os.path.join(glob.escape(root), module, "target/scala-*/*zinc")):
                missing.append(module)
        return missing

    def seed(self) -> None:
        missing = self.unbuilt()
        if not missing:
            return
        started = time.monotonic()
        src = self.env.get("KINOWO_SBT_WARM_SEED") or self.source()
        if not src:
            self.say("no local build of main to seed from: the first compile is cold")
            return
        root = self.places.root
        self.say(f"seeding {' '.join(missing)} in {root} from {src}'s build")
        for item in missing:
            self.copy(src, item)
        if "project/target" in missing:
            remove(os.path.join(root, "project/target/active.json"))
        subprocess.run(["python3", ZINC_ORPHANS, root])
        self.say(f"seeded in {int(time.monotonic() - started)}s")

    def copy(self, src: str, item: str) -> None:
        """One unbuilt item seeded from `src`: project/target as it is, a module its target/scala-* directories."""
        parts = [item] if item.startswith("project/") else \
            [os.path.relpath(d, src) for d in sorted(glob.glob(os.path.join(glob.escape(src), item, "target/scala-*")))]
        for part in parts:
            if os.path.isdir(os.path.join(src, part)):
                shutil.rmtree(os.path.join(self.places.root, part), ignore_errors=True)
                clone(os.path.join(src, part), os.path.join(self.places.root, part))

    def publishable(self, sha: str) -> bool:
        """Whether `sha` is on a remote's main, with a clean checkout (no tracked change, no untracked source)."""
        refs = (self.here.out("for-each-ref", "--format=%(refname)", "refs/remotes/*/main", quiet=True) or "").split()
        if not any(self.here.ok("merge-base", "--is-ancestor", sha, ref, quiet=True) for ref in refs):
            return False
        if not self.here.ok("diff", "--quiet", "HEAD", "--", quiet=True):
            return False
        untracked = self.here.out("ls-files", "--others", "--exclude-standard", "--", "build.sbt", ":(glob)project/*.s*",
                                  "project/build.properties", ":(glob)*/src/**", ":(glob)*/conf/**", quiet=True)
        return untracked is not None and untracked.strip() == ""

    def publish(self) -> None:
        """This checkout's build as its HEAD's cache entry (the caller holds $key.busy exclusively)."""
        if self.env.get("KINOWO_SBT_CACHE_PUBLISH", "1") == "0":
            return
        head = line(self.here, "rev-parse", "-q", "--verify", "HEAD")
        if not head or not self.publishable(head):
            return
        zincs, entry = built_zincs(self.places.root), os.path.join(self.places.cache, head)
        have = read_int(os.path.join(entry, ".zincs"))
        if zincs <= 0 or zincs <= have:
            return
        os.makedirs(self.places.cache, exist_ok=True)
        part = f"{entry}.part-{self.pid}"
        shutil.rmtree(part, ignore_errors=True)
        for path in build_paths(self.places.root):
            clone(os.path.join(self.places.root, path), os.path.join(part, path))
        remove(os.path.join(part, "project/target/active.json"))
        subprocess.run(["python3", ZINC_ORPHANS, part], stdout=subprocess.DEVNULL)
        with open(os.path.join(part, ".zincs"), "w") as f:
            f.write(f"{zincs}\n")
        if os.path.isdir(entry):
            os.rename(entry, f"{entry}.retired-{int(time.time())}")
        try:
            os.rename(part, entry)
            self.say(f"published this build of {head[:10]} ({zincs} configurations) to the shared build cache "
                     f"{self.places.cache}")
        except OSError:
            shutil.rmtree(part, ignore_errors=True)
        self.prune()

    def prune(self) -> None:
        """The newest KINOWO_SBT_CACHE_KEEP (8) entries stay; retired ones go after 10 min, abandoned copies after an hour."""
        cache = self.places.cache
        names = os.listdir(cache) if os.path.isdir(cache) else []
        for name in names:
            drop_aged(os.path.join(cache, name))
        entries = sorted((n for n in names if "." not in n and os.path.exists(os.path.join(cache, n))),
                         key=lambda n: os.stat(os.path.join(cache, n)).st_mtime, reverse=True)
        for name in entries[int(self.env.get("KINOWO_SBT_CACHE_KEEP", "8")):]:
            shutil.rmtree(os.path.join(cache, name), ignore_errors=True)


def drop_aged(path: str) -> None:
    """A retired entry older than 10 minutes, or an abandoned .part copy older than an hour, deleted."""
    name = os.path.basename(path)
    minutes = 10 if ".retired-" in name else 60 if ".part-" in name else None
    if minutes is not None and os.path.isdir(path) and time.time() - os.stat(path).st_mtime > minutes * 60:
        shutil.rmtree(path, ignore_errors=True)


def read_int(path: str) -> int:
    try:
        with open(path) as f:
            text = f.read().strip()
    except OSError:
        return 0
    return int(text) if text.isdigit() else 0


def remove(path: str) -> None:
    try:
        os.remove(path)
    except OSError:
        pass
