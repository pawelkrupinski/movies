"""THE PURE MOVE CHECK'S MOVES (scripts/pure_move_check.py): `git diff -M -C --name-status` over every first-parent hop of
a range, composed hop by hop (a file renamed twice is one move), with the direct base..head pairs filling what the hops
saw as a delete and an add; from the moved Scala files the class map and the package map. Stdlib only."""
from __future__ import annotations

from typing import Dict, List, Optional, Set, Tuple

from pure_move_git import Blobs, Undecidable, git
from pure_move_source import is_scala, packages_of, top_level_names


def directly_under(paths: List[str], rel: str) -> List[str]:
    """The paths holding `rel` (a `/scala/<package>/` fragment) with no further directory after it."""
    return [p for p in paths if rel in p and "/" not in p.split(rel, 1)[1]]


class Moves:
    def __init__(self, base: str = "", head: str = "", blobs: Optional[Blobs] = None) -> None:
        self.files: Dict[str, str] = {}       # old path -> new path
        self.added: Set[str] = set()          # head paths no base path became
        self.deleted: Set[str] = set()        # base paths no head path is
        self.modified: Set[str] = set()       # base paths changed in place
        self.classes: Dict[str, str] = {}     # old FQN -> new FQN (changed only)
        self.simple: Dict[str, str] = {}      # old simple name -> new (class renames)
        self.packages: Dict[str, str] = {}    # old package -> new (wholly moved)
        self.dirs: Dict[str, str] = {}        # old directory/ -> new directory/ (wholly moved)
        self.base = base
        self.head = head
        self.blobs = blobs
        self.trees: Dict[str, List[str]] = {}
        self.members: Dict[Tuple[str, str], Set[str]] = {}

    def path_pairs(self) -> List[Tuple[str, str]]:
        """(old, new) text forms to map, longest first: file paths, directories, FQNs dotted and slashed, packages."""
        pairs = [(old, new) for old, new in self.files.items() if old != new] + list(self.dirs.items())
        for old, new in self.classes.items():
            pairs += [(old, new), (old.replace(".", "/"), new.replace(".", "/"))]
        for old, new in self.packages.items():
            pairs += [(old, new), (old.replace(".", "/"), new.replace(".", "/"))]
        return sorted(set(pairs), key=lambda p: -len(p[0]))

    def mentions(self) -> Set[str]:
        """Every form a moved thing is named by, old and new: paths, directories, FQNs and their packages."""
        names: Set[str] = set()
        for old, new in list(self.files.items()) + list(self.dirs.items()):
            names |= {old, new}
        names |= self.added
        for old, new in self.classes.items():
            for fqn in (old, new):
                names |= {fqn, fqn.replace(".", "/")}
                if "." in fqn:
                    package_name = fqn.rsplit(".", 1)[0]
                    names |= {package_name, package_name.replace(".", "/")}
        return {m for m in names if len(m) > 3}

    def tree(self, rev: str) -> List[str]:
        if rev not in self.trees:
            self.trees[rev] = git("ls-tree", "-r", "--name-only", rev).splitlines()
        return self.trees[rev]

    def _defined_names(self, rev: str, path: str, package_name: str) -> Set[str]:
        """The top-level names of the Scala file `path` at `rev`, when it is in package `package_name`."""
        src = self.blobs.text(rev, path) if self.blobs and path.endswith(".scala") else None
        return set(top_level_names(src)) if src and ".".join(packages_of(src)) == package_name else set()

    def package_members(self, rev: str, package_name: str) -> Set[str]:
        """The top-level names the Scala files of package `package_name` define at `rev` (its directories, by convention)."""
        key = (rev, package_name)
        if key not in self.members:
            rel = "/scala/" + package_name.replace(".", "/") + "/"
            names: Set[str] = set()
            for path in directly_under(self.tree(rev), rel):
                names |= self._defined_names(rev, path, package_name)
            self.members[key] = names
        return self.members[key]


def name_status(before: str, after: str) -> List[List[str]]:
    return [line.split("\t") for line in git("diff", "-M", "-C", "--name-status", "--no-color", before, after).splitlines() if line]


class _Composed:
    """The hops' name statuses composed: where each base path is now, what was added since."""

    def __init__(self) -> None:
        self.current: Dict[str, Optional[str]] = {}  # base path -> its name now (None: deleted)
        self.added: Set[str] = set()
        self.origin: Dict[str, str] = {}             # a path now -> the base path it is

    def apply(self, row: List[str]) -> None:
        status, paths = row[0][0], row[1:]
        if status == "R":
            self._rename(paths[0], paths[1])
        elif status in "CA":
            self.added.add(paths[-1])
        elif status == "D":
            self._delete(paths[0])
        elif status in "MT":
            if paths[0] not in self.added and paths[0] not in self.origin:
                self.current.setdefault(paths[0], paths[0])

    def _rename(self, old: str, new: str) -> None:
        if old in self.added:
            self.added.discard(old)
            self.added.add(new)
            return
        src = self.origin.get(old, old)
        self.current[src] = new
        self.origin.pop(old, None)
        self.origin[new] = src

    def _delete(self, path: str) -> None:
        if path in self.added:
            self.added.discard(path)
        else:
            self.current[self.origin.pop(path, path)] = None


def _pair_direct_renames(base: str, head: str, composed: _Composed, deleted: Set[str]) -> None:
    """What the hops saw as a delete and an add, the direct diff may pair."""
    for row in name_status(base, head):
        if row[0][0] == "R" and row[1] in deleted and row[2] in composed.added:
            composed.current[row[1]] = row[2]
            deleted.discard(row[1])
            composed.added.discard(row[2])


def _record_files(moves: Moves, current: Dict[str, Optional[str]], blobs: Blobs) -> None:
    for old, now in current.items():
        if now is None:
            continue
        if now != old:
            moves.files[old] = now
        elif blobs.get(moves.base, old) != blobs.get(moves.head, now):
            moves.modified.add(old)
    for old, now in moves.files.items():
        if blobs.get(moves.base, old) is None or blobs.get(moves.head, now) is None:
            raise Undecidable(f"{old} -> {now}: a side git cannot read")


def _stem(path: str) -> str:
    return path.rsplit("/", 1)[-1][:-len(".scala")]


def _qualified(package_name: str, name: str) -> str:
    return f"{package_name}.{name}" if package_name else name


def _new_name(name: str, new_names: Set[str], old_stem: str, new_stem: str) -> Optional[str]:
    """What the top-level `name` of a moved file is called after it: itself, or the file's new name when the file's own
    class was renamed with it."""
    if name in new_names:
        return name
    return new_stem if name == old_stem and new_stem in new_names else None


def _record_file_classes(moves: Moves, old: str, new: str, old_src: str, new_src: str) -> Tuple[str, str]:
    """The class and simple-name maps taken from one moved Scala file; its (old package, new package)."""
    old_package, new_package = ".".join(packages_of(old_src)), ".".join(packages_of(new_src))
    new_names = set(top_level_names(new_src))
    for name in top_level_names(old_src):
        new_name = _new_name(name, new_names, _stem(old), _stem(new))
        if new_name is None:
            continue
        old_fqn, new_fqn = _qualified(old_package, name), _qualified(new_package, new_name)
        if old_fqn != new_fqn:
            moves.classes[old_fqn] = new_fqn
        if new_name != name:
            moves.simple[name] = new_name
    return old_package, new_package


def _record_classes(moves: Moves, blobs: Blobs) -> Dict[str, Set[str]]:
    """The class map from the moved Scala files; each package they left -> the packages they went to."""
    moved_from: Dict[str, Set[str]] = {}
    for old, new in moves.files.items():
        if not is_scala(old) or not is_scala(new):
            continue
        old_package, new_package = _record_file_classes(
            moves, old, new, blobs.text(moves.base, old) or "", blobs.text(moves.head, new) or "")
        if old_package != new_package:
            moved_from.setdefault(old_package, set()).add(new_package)
    return moved_from


def _record_dirs(moves: Moves, base_files: List[str]) -> None:
    """A directory moved whole: every file directly in it at the base moved, all into one directory."""
    by_dir: Dict[str, Set[str]] = {}
    for old, new in moves.files.items():
        by_dir.setdefault(old.rsplit("/", 1)[0] + "/", set()).add(new.rsplit("/", 1)[0] + "/")
    for old_dir, targets in by_dir.items():
        children = [p for p in base_files if p.startswith(old_dir) and "/" not in p[len(old_dir):]]
        target = next(iter(targets))
        if len(targets) == 1 and all(p in moves.files for p in children) and target != old_dir:
            moves.dirs[old_dir] = target


def _package_moved_whole(moves: Moves, old_package: str, base_files: List[str], head_files: Set[str]) -> bool:
    rel = "/scala/" + old_package.replace(".", "/") + "/"
    members = [p for p in directly_under(base_files, rel) if is_scala(p)]
    return bool(members) and all(p in moves.files and moves.files[p] != p for p in members) \
        and not any(is_scala(p) for p in directly_under(list(head_files), rel))


def _record_packages(moves: Moves, moved_from: Dict[str, Set[str]], base_files: List[str], head_files: Set[str]) -> None:
    """A package moved whole: every file under its directories at the base moved, all to one package."""
    for old_package, targets in moved_from.items():
        if len(targets) == 1 and old_package and _package_moved_whole(moves, old_package, base_files, head_files):
            moves.packages[old_package] = next(iter(targets))


def find_moves(base: str, head: str, blobs: Blobs) -> Moves:
    hops = [base] + git("rev-list", "--reverse", "--first-parent", f"{base}..{head}").split()
    composed = _Composed()
    for before, after in zip(hops, hops[1:]):
        for row in name_status(before, after):
            composed.apply(row)
    moves = Moves(base=base, head=head, blobs=blobs)
    deleted = {p for p, now in composed.current.items() if now is None}
    _pair_direct_renames(base, head, composed, deleted)
    _record_files(moves, composed.current, blobs)
    moves.added, moves.deleted = composed.added, deleted
    moved_from = _record_classes(moves, blobs)
    base_files, head_files = moves.tree(base), set(moves.tree(head))
    _record_dirs(moves, base_files)
    _record_packages(moves, moved_from, base_files, head_files)
    return moves
