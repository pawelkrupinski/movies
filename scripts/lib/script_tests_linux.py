"""--linux (scripts/run-script-tests): the selected tests in a container like CI's ubuntu runner. The image is the
runner's recipe (scripts/run-script-tests' LINUX_IMAGE_RECIPE, from mirror.gcr.io, Docker Hub when the mirror lost a
blob), built once and kept, tagged by the recipe's hash. The checkout is mounted READ-ONLY at its own path -- not the
checkout itself but its copy in one of a few fixed slots (Rancher's VM keeps a host descriptor for every file a
container saw: 25 worktrees mounted in a day filled the Mac's file table on 2026-11-20), held by an flock for the run's
life. Its git metadata (config, HEAD, refs, the worktree's own dir) is a snapshot unpacked into a tmpfs at the git dir's
own path, only the objects (and those it borrows, objects/info/alternates) mounted live: a host git rewriting the
shared .git/config read as missing in there for most of the next second. The tests run as this user, not root."""
from __future__ import annotations

import fcntl
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from typing import IO, Callable, List, Optional, Sequence, Tuple

import machine
import tool_python

GIT_ENTRIES = ("config", "config.worktree", "HEAD", "ORIG_HEAD", "FETCH_HEAD", "commondir", "gitdir", "index",
               "packed-refs", "refs", "shallow", "info")
MIRROR_LOST = ("could not fetch content descriptor", "unknown blob")
Say = Callable[[str], None]


@dataclass(frozen=True)
class Container:
    """What the container run needs: the checkout, the image's recipe, where failing logs land, the runner's argv."""
    root: str
    recipe: str
    log_dir: str
    inner: Sequence[str]
    home: str
    sync: str  # linux-tree-sync.py
    scratch: str  # the run's own directory: the build's log
    slots: int = 4
    wait: int = 1800


def docker_binary(home: str) -> Optional[str]:
    found = shutil.which("docker")
    if found:
        return found
    rancher = os.path.join(home, ".rd/bin/docker")
    return rancher if os.access(rancher, os.X_OK) else None


def alternate_object_dirs(objects: str, depth: int = 0) -> List[str]:
    """Every object directory `objects` borrows from, transitively (git follows five levels), absolute and resolved."""
    out: List[str] = []
    path = os.path.join(objects, "info/alternates")
    if depth >= 5 or not os.path.isfile(path):
        return out
    with open(path) as f:
        for line in f.read().splitlines():
            if not line or line.startswith("#"):
                continue
            target = line if line.startswith("/") else os.path.join(objects, line)
            if os.path.isdir(target):
                resolved = os.path.realpath(target)
                out += [resolved, *alternate_object_dirs(resolved, depth + 1)]
    return out


def metadata_entries(common: str, git_dir: str) -> List[str]:
    """The git metadata a checkout reads, all but the objects, relative to the common dir."""
    prefixes = [""] + ([] if git_dir == common else [os.path.relpath(git_dir, common) + "/"])
    return [p + e for p in prefixes for e in GIT_ENTRIES if os.path.lexists(os.path.join(common, p + e))]


def image_name(recipe: str) -> str:
    code, out = machine.output(["git", "hash-object", "--stdin"], text_in=recipe)
    return "kinowo-script-tests:" + (out.strip()[:12] if code == 0 else "unknown")


def build(c: Container, docker: str, image: str, say: Say) -> bool:
    """The image built (once): from Docker Hub when the mirror answered "not found" for a blob it served before."""
    if machine.status([docker, "image", "inspect", image], out=subprocess.DEVNULL, err=subprocess.DEVNULL) == 0:
        return True
    say(f"run-script-tests: building {image} (once)")
    code, err = build_once(c, docker, image, c.recipe)
    if code == 0:
        return True
    sys.stderr.write(err)
    if not any(m in err for m in MIRROR_LOST):
        say(f"run-script-tests: could not build {image}")
        return False
    say(f"run-script-tests: mirror.gcr.io could not fetch a blob: building {image} from Docker Hub")
    hub = c.recipe.replace("FROM mirror.gcr.io/library/", "FROM docker.io/library/", 1)
    if build_once(c, docker, image, hub)[0] != 0:
        say(f"run-script-tests: could not build {image}")
        return False
    return True


def build_once(c: Container, docker: str, image: str, recipe: str) -> Tuple[int, str]:
    """(status, stderr) of one bounded `docker build` of the recipe."""
    log = os.path.join(c.scratch, "build.log")
    with open(log, "w") as err:
        argv = tool_python.isolated(bounded_tool(), "900", docker, "build", "-q", "-t", image, "-",
                                    python=sys.executable)
        code = machine.run(argv,
                           machine.Ran(out=subprocess.DEVNULL, err=err), recipe + "\n")[0]
    with open(log) as f:
        return code, f.read()


def bounded_tool() -> str:
    return os.path.join(os.path.dirname(os.path.realpath(__file__)), "tool_bounded.py")


class Slot:
    """THE TREE SLOT: the first of $RUN_SCRIPT_TESTS_LINUX_SLOTS (4) copies under ~/.cache/movies-linux-tree no other
    run holds (its flock), synced to the checkout; all held, waited for (said each half minute) up to
    $RUN_SCRIPT_TESTS_LINUX_WAIT seconds (1800). The lock is let go when the run ends, however it ends."""

    def __init__(self, c: Container, say: Say, sleep: Callable[[float], None]) -> None:
        self.c, self.say, self.sleep = c, say, sleep
        self.held: Optional[IO[str]] = None
        self.tree = ""

    def take(self) -> Optional[str]:
        base = os.path.join(self.c.home, ".cache/movies-linux-tree")
        slots, wait = self.c.slots, self.c.wait
        os.makedirs(base, exist_ok=True)
        deadline, said = machine.monotonic() + wait, 0
        while True:
            tree = self.first_free(base, slots)
            if tree is not None:
                return tree or None
            if machine.monotonic() >= deadline:
                self.say(f"run-script-tests: every Linux tree slot is busy ({slots} under {base}, "
                         "RUN_SCRIPT_TESTS_LINUX_SLOTS) -- another run holds each")
                return None
            if said % 15 == 0:
                self.say(f"run-script-tests: all {slots} Linux tree slots busy, waiting")
            said += 1
            self.sleep(2)

    def first_free(self, base: str, slots: int) -> Optional[str]:
        return next((t for t in (self.try_slot(base, n) for n in range(slots)) if t is not None), None)

    def try_slot(self, base: str, n: int) -> Optional[str]:
        """The slot's tree once it is held and synced ("" when the sync failed), None when another run holds it."""
        lock = open(os.path.join(base, f"slot-{n}.lock"), "w")
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            lock.close()
            return None
        self.held = lock
        tree = self.tree = os.path.join(base, f"slot-{n}")
        synced = machine.status([sys.executable, self.c.sync, self.c.root, tree], out=sys.stderr)
        return tree if synced == 0 else ""


def git_dirs(root: str) -> Tuple[str, str]:
    common = machine.output(["git", "rev-parse", "--path-format=absolute", "--git-common-dir"], cwd=root)
    own = machine.output(["git", "rev-parse", "--path-format=absolute", "--git-dir"], cwd=root)
    return (common[1].strip() if common[0] == 0 else "", own[1].strip() if own[0] == 0 else "")


def mounts(c: Container, tree: str, common: str) -> List[str]:
    out = ["-v", f"{tree}:{c.root}:ro"]
    if common:
        out += ["--mount", f"type=tmpfs,destination={common},tmpfs-mode=1777",
                "-v", f"{common}/objects:{common}/objects:ro"]
        for alternate in alternate_object_dirs(os.path.join(common, "objects")):
            out += ["-v", f"{alternate}:{alternate}:ro"]
    os.makedirs(c.log_dir, exist_ok=True)
    return out + ["-v", f"{c.log_dir}:{c.log_dir}"]


def ready(c: Container, say: Say, sleep: Callable[[float], None]) -> Optional[Tuple[str, str, Slot]]:
    """(docker, image, the slot holding the tree) once Docker answers, the image is built and a slot is held."""
    docker = docker_binary(c.home)
    info = tool_python.isolated(bounded_tool(), "20", docker or "docker", "info", python=sys.executable)
    if docker is None or machine.status(info,
                                        out=subprocess.DEVNULL, err=subprocess.DEVNULL) != 0:
        say("run-script-tests: --linux needs a running Docker (docker info failed) -- start Docker / Rancher Desktop")
        return None
    image = image_name(c.recipe)
    if not build(c, docker, image, say):
        return None
    slot = Slot(c, say, sleep)
    return (docker, image, slot) if slot.take() is not None else None


def run(c: Container, tests: Sequence[str], say: Say, sleep: Callable[[float], None]) -> int:
    """The tests in the container: its status, or 2 when no Docker, image or slot."""
    got = ready(c, say, sleep)
    if got is None:
        return 2
    docker, image, slot = got
    common, own = git_dirs(c.root)
    say(f"run-script-tests: {len(tests)} test(s) on Linux ({image})")
    argv = [docker, "run", "--rm", "-i", "--user", f"{os.getuid()}:{os.getgid()}", "-e", "HOME=/tmp", "-e",
            "TERM=dumb", "-e", f"RUN_SCRIPT_TESTS_LOG_DIR={c.log_dir}", *mounts(c, slot.tree, common), "-w", c.root,
            image, "bash", "-c", '[ -z "$0" ] || tar -xf - -C "$0" || exit 2; exec "$@" < /dev/null', common, *c.inner,
            *tests]
    tar = ["tar", "-C", common, "--no-xattrs", "-cf", "-", *metadata_entries(common, own)] if common else ["true"]
    env = {**machine.environment(), "COPYFILE_DISABLE": "1"}
    return machine.piped(tar, argv, machine.Ran(env=env))
