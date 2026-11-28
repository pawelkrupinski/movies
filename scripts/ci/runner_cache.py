#!/usr/bin/env python3
"""A runner-local mirror of actions/cache entries, for the self-hosted runners on ci-runner.

WHY: on ci-runner every landing check restored the same ~580 MB sbt dependency cache and ~260 MB build cache from
GitHub's cache service at 12-19 MB/s -- 57 of the ~67 s `setup-sbt-build` took (precommit-check run 44045907609,
2026-11-17). The host keeps a directory across jobs (`MOVIES_RUNNER_CACHE`, movies-runners.nix in bitcashier infra);
this mirrors there each entry a job restored, so the next job reads it off the local disk.

WHY IT CANNOT MAKE A CHECK PASS WRONGLY: it mirrors ENTRIES, not live directories. GitHub still decides which entry a
job gets (.github/actions/cache-restore asks it with `lookup-only` first), and an entry's key names an immutable
snapshot (only main saves, CacheSavePolicySpec; GitHub never overwrites a key). So a job extracts, into its own fresh
workspace, the very files the network restore would have given it: nothing is shared live between jobs, target/ stays
per job, and zinc sees exactly what it sees today -- the stale-zinc and stale-TASTy guards (sbt-invalidate-exporters,
the sbt guard) run after either restore alike.

WHY CONCURRENT JOBS CANNOT CORRUPT IT: an entry is written to a temporary name in the same directory and renamed into
place (atomic; two writers of one key write the same files, the later rename wins), and is never modified after.
Readers only read. A missing, unreadable or corrupt entry (zstd's frame checksum) is a miss: the job falls back to
the network restore, after removing whatever a failed extraction left.

  runner_cache.py dir                                     # prints the usable mirror directory, or nothing
  runner_cache.py restore --key K --paths P [--root /]    # exit 0 restored, 1 miss
  runner_cache.py save    --key K --paths P [--root /]    # mirrors what P holds now under K (exit 0 always)

P is actions/cache's `path` input: one pattern a line, `~` for $HOME, `!` to exclude, relative to the workspace (the
current directory). The host ages entries out (systemd-tmpfiles); a hit refreshes the entry's mtime.
"""
import argparse
import glob
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
from fnmatch import fnmatchcase
from typing import Callable, List, Optional, Set, Tuple
import ci_machine  # noqa: E402

ENTRY_SUFFIX = ".tar.zst"


def mirror_dir():
    """The mirror directory from MOVIES_RUNNER_CACHE when it exists and this job may write it, else None."""
    directory = ci_machine.environment().get("MOVIES_RUNNER_CACHE", "")
    if not directory or not os.path.isdir(directory) or not os.access(directory, os.W_OK | os.X_OK):
        return None
    if shutil.which("zstd") is None or shutil.which("tar") is None:
        return None
    return directory


def parse_patterns(paths_text):
    """(include patterns, exclude patterns), absolute, from actions/cache's multi-line `path` input."""
    includes, excludes = [], []
    for raw in paths_text.splitlines():
        line = raw.strip()
        if not line:
            continue
        negated = line.startswith("!")
        if negated:
            line = line[1:]
        if line.startswith("~"):
            line = os.path.expanduser("~") + line[1:]
        line = os.path.normpath(os.path.join(os.getcwd(), line))
        (excludes if negated else includes).append(line)
    return includes, excludes


def entry_path(directory, key, paths_text):
    """The entry's file: the key (readable) plus a digest of the key, the resolved patterns and the workspace, since an
    entry restores to absolute paths and actions/cache's own version also covers the path list."""
    includes, excludes = parse_patterns(paths_text)
    digest = hashlib.sha256("\n".join([key, os.getcwd()] + includes + ["!"] + excludes).encode()).hexdigest()[:16]
    readable = "".join(c if c.isalnum() or c in "-_." else "_" for c in key)[:150]
    return os.path.join(directory, f"{readable}-{digest}{ENTRY_SUFFIX}")


def _excluded(path, excludes):
    # actions/cache's globs: `*` never crosses a `/`; fnmatch's does, so compare component counts as well.
    depth = path.count("/")
    return any(pattern.count("/") == depth and fnmatchcase(path, pattern) for pattern in excludes)


def _scan(directory: str) -> List["os.DirEntry[str]"]:
    """The entries of `directory`; none where it cannot be listed (os.walk's own way with an unreadable directory)."""
    try:
        return list(os.scandir(directory))
    except OSError:
        return []


def _children(directory: str) -> List[Tuple[str, bool]]:
    """(path, whether to descend: a directory, not a symlink to one) of every entry of `directory`."""
    return [(entry.path, entry.is_dir(follow_symlinks=False)) for entry in _scan(directory)]


def included_under(top: str, children: Callable[[str], List[Tuple[str, bool]]], excludes: List[str]) -> Set[str]:
    """Every entry under directory `top` that is not excluded, an excluded directory not entered; `children` lists one."""
    found: Set[str] = set()
    todo = [top]
    while todo:
        for path, descend in children(todo.pop()):
            if _excluded(path, excludes):
                continue
            found.add(path)
            if descend:
                todo.append(path)
    return found


def _glob(pattern: str) -> List[str]:
    return glob.glob(pattern)


def _real_directory(path: str) -> bool:
    return os.path.isdir(path) and not os.path.islink(path)


def collect(paths_text):
    """Every directory, file and symlink the patterns cover, excluded entries dropped, sorted."""
    includes, excludes = parse_patterns(paths_text)
    found = set()
    for top in (t for pattern in includes for t in _glob(pattern) if not _excluded(t, excludes)):
        found.add(top)
        if _real_directory(top):
            found |= included_under(top, _children, excludes)
    return sorted(found), includes


def save(directory, key, paths_text, root):
    target = entry_path(directory, key, paths_text)
    if os.path.exists(target):
        return "present"
    members, _ = collect(paths_text)
    if not members:
        return "empty"
    temporary = tempfile.NamedTemporaryFile(dir=directory, prefix=".writing-", suffix=ENTRY_SUFFIX, delete=False)
    temporary.close()
    listing = tempfile.NamedTemporaryFile("wb", delete=False)
    try:
        listing.write(b"\0".join(os.path.relpath(m, root).encode() for m in members) + b"\0")
        listing.close()
        with open(temporary.name, "wb") as out:
            tar = subprocess.Popen(["tar", "-C", root, "--null", "--no-recursion", "-T", listing.name, "-cf", "-"],
                                   stdout=subprocess.PIPE)
            zstd = subprocess.run(["zstd", "-q", "-T0", "-3", "-c"], stdin=tar.stdout, stdout=out)
            if tar.stdout is not None:
                tar.stdout.close()
            if tar.wait() != 0 or zstd.returncode != 0:
                os.unlink(temporary.name)
                return "failed"
        os.chmod(temporary.name, 0o644)
        os.replace(temporary.name, target)
        return "saved"
    finally:
        os.unlink(listing.name)
        if os.path.exists(temporary.name):
            os.unlink(temporary.name)


def _remove_path(path: str) -> None:
    """A restored path gone, whatever it is: a directory tree, or a file or link (one already gone is no matter)."""
    if os.path.isdir(path) and not os.path.islink(path):
        shutil.rmtree(path, ignore_errors=True)
        return
    try:
        os.unlink(path)
    except OSError:
        pass


def _remove_restored(paths_text):
    includes, _ = parse_patterns(paths_text)
    for pattern in includes:
        for path in glob.glob(pattern):
            _remove_path(path)


def restore(directory, key, paths_text, root):
    source = entry_path(directory, key, paths_text)
    try:
        entry = open(source, "rb")
    except OSError:
        return False
    with entry:
        zstd = subprocess.Popen(["zstd", "-q", "-d", "-c"], stdin=entry, stdout=subprocess.PIPE)
        tar = subprocess.run(["tar", "-C", root, "-xf", "-"], stdin=zstd.stdout)
        if zstd.stdout is not None:
            zstd.stdout.close()
        if zstd.wait() != 0 or tar.returncode != 0:
            _remove_restored(paths_text)
            return False
    try:
        os.utime(source)  # the host ages entries out by mtime: a used one stays
    except OSError:
        pass
    return True


DOCSTRING = __doc__ or ""  # None under python -OO


def _parse_args(argv: Optional[List[str]]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    parser.add_argument("command", choices=["dir", "restore", "save"])
    parser.add_argument("--key", default="")
    parser.add_argument("--paths", default="")
    parser.add_argument("--root", default="/")
    return parser.parse_args(argv)


def _run(args: argparse.Namespace, directory: str) -> int:
    """`restore` or `save` against the mirror at `directory`: the entry's state said, restore's exit 1 on a miss."""
    if args.command == "restore":
        restored = restore(directory, args.key, args.paths, args.root)
        print(f"runner-cache {'hit' if restored else 'miss'} {args.key}")
        return 0 if restored else 1
    print(f"runner-cache {save(directory, args.key, args.paths, args.root)} {args.key}")
    return 0


def main(argv=None):
    args = _parse_args(argv)
    directory = mirror_dir()
    if args.command == "dir":
        print(directory or "")
        return 0
    if directory is None or not args.key or not args.paths.strip():
        return 1 if args.command == "restore" else 0
    return _run(args, directory)


if __name__ == "__main__":
    sys.exit(main())
