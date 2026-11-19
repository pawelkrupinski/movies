#!/usr/bin/env python3
"""Copy a checkout's files (tracked and untracked, never ignored) into DEST, and keep DEST in step with it.

    python3 scripts/linux-tree-sync.py <checkout> <dest>

WHY. `run-script-tests --linux` mounted each checkout into its container. Rancher Desktop's VM shares the Mac's home
over virtiofs, and the VM process holds a host descriptor for every file and directory the guest kernel has cached --
which it keeps until memory pressure or `drop_caches`, even after the directory is deleted. A worktree's tree is ~25k
files and directories: 25 agent worktrees running --linux in a day held ~200k descriptors, and on 2026-11-20 the Mac's
system-wide table (kern.maxfiles 491,520) overflowed, ENFILE for every process. The container now mounts one of a few
fixed copies under ~/.cache/movies-linux-tree instead, so the VM only ever sees those trees, however many worktrees run.

A file whose size, mtime and mode match is left alone (the same inode, nothing new for the VM to cache); any other is
copied beside itself and renamed over. A path DEST has and the checkout does not is removed, then every directory left
empty. The checkout's `.git` comes across as it is in a worktree (the file naming the git dir) and as an empty directory
in a main checkout: the container mounts the git dir's snapshot there.
"""
import os
import shutil
import stat
import subprocess
import sys


def checkout_paths(root):
    listed = subprocess.run(["git", "-C", root, "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
                            capture_output=True)
    if listed.returncode != 0:
        sys.exit(f"linux-tree-sync: {root} is not a git checkout: {listed.stderr.decode(errors='replace').strip()}")
    return {p for p in listed.stdout.decode().split("\0") if p and os.path.lexists(os.path.join(root, p))}


def remove(path):
    if os.path.isdir(path) and not os.path.islink(path):
        shutil.rmtree(path)
    elif os.path.lexists(path):
        os.unlink(path)


def same(source, target):
    try:
        t = os.lstat(target)
    except FileNotFoundError:
        return False
    if stat.S_ISLNK(source.st_mode):
        return stat.S_ISLNK(t.st_mode)  # its target compared by the caller
    return (stat.S_ISREG(t.st_mode) and t.st_size == source.st_size and t.st_mtime_ns == source.st_mtime_ns
            and stat.S_IMODE(t.st_mode) == stat.S_IMODE(source.st_mode))


def ensure_parent(dest, relative):
    """The directories above DEST/relative, any file standing where one belongs removed."""
    parent = dest
    for part in relative.split("/")[:-1]:
        parent = os.path.join(parent, part)
        if os.path.lexists(parent) and not (os.path.isdir(parent) and not os.path.islink(parent)):
            os.unlink(parent)
        if not os.path.lexists(parent):
            os.mkdir(parent)


def copy_one(root, dest, relative):
    source_path, target = os.path.join(root, relative), os.path.join(dest, relative)
    source = os.lstat(source_path)
    if stat.S_ISLNK(source.st_mode):
        link = os.readlink(source_path)
        if same(source, target) and os.readlink(target) == link:
            return False
        ensure_parent(dest, relative)
        remove(target)
        os.symlink(link, target)
        return True
    if same(source, target):
        return False
    ensure_parent(dest, relative)
    if os.path.isdir(target) and not os.path.islink(target):
        shutil.rmtree(target)
    temporary = os.path.join(os.path.dirname(target), f".{os.path.basename(target)}.linux-tree-sync")
    shutil.copy2(source_path, temporary, follow_symlinks=False)
    os.replace(temporary, target)
    return True


def prune(dest, keep):
    """Every path under DEST the checkout lacks removed, then the directories left empty (DEST/.git kept)."""
    removed = 0
    for directory, subdirectories, names in os.walk(dest, topdown=False):
        relative_dir = os.path.relpath(directory, dest)
        relative_dir = "" if relative_dir == "." else relative_dir + "/"
        for name in names + [d for d in subdirectories if os.path.islink(os.path.join(directory, d))]:
            if relative_dir + name not in keep:
                os.unlink(os.path.join(directory, name))
                removed += 1
        if directory != dest and relative_dir != ".git/" and not os.listdir(directory):
            os.rmdir(directory)
    return removed


def main(argv):
    if len(argv) != 3:
        sys.exit("usage: linux-tree-sync.py <checkout> <dest>")
    root, dest = os.path.abspath(argv[1]), os.path.abspath(argv[2])
    paths = checkout_paths(root)
    os.makedirs(dest, exist_ok=True)
    git_entry, git_target = os.path.join(root, ".git"), os.path.join(dest, ".git")
    if os.path.isdir(git_entry):
        if not os.path.isdir(git_target) or os.path.islink(git_target):
            remove(git_target)
            os.mkdir(git_target)
        for name in os.listdir(git_target):
            remove(os.path.join(git_target, name))
    else:
        if os.path.isdir(git_target) and not os.path.islink(git_target):
            shutil.rmtree(git_target)
        paths.add(".git")
    copied = sum(copy_one(root, dest, relative) for relative in sorted(paths))
    removed = prune(dest, paths)
    print(f"linux-tree-sync: {dest}: {len(paths)} files, {copied} copied, {removed} removed", file=sys.stderr)


if __name__ == "__main__":
    main(sys.argv)
