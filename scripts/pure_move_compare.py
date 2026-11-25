"""THE PURE MOVE CHECK'S COMPARISON (scripts/pure_move_check.py): one changed file, old side against new, as findings --
the imports held to what a move needs, the code identical once normalised, a visibility change told apart, a text file
edited only in the moved names. Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import difflib
import re
from dataclasses import dataclass
from typing import List, Optional, Set, Tuple

from pure_move_git import Undecidable
from pure_move_lexer import Unlexable
from pure_move_moves import Moves
from pure_move_normalise import Normalised, map_name, map_text, normalise_scala
from pure_move_source import REFERENCE_LISTS, is_production, is_twirl

QUALIFIER = re.compile(r"\b(private|protected)\[\w+\]")
IMPLICIT_LINE = re.compile(r"\b(implicit|given|using|extension)\b")
DECLARED = re.compile(r"\b(?:def|val|var|lazy val|class|object|trait|type)\s+([A-Za-z_]\w*)")


class Verdict:
    def __init__(self, moved: Optional[List[Tuple[str, str]]] = None) -> None:
        self.moved: List[Tuple[str, str]] = moved or []
        self.references: List[str] = []
        self.visibility: List[str] = []
        self.hunks: List[str] = []
        self.risks: List[str] = []


@dataclass
class ImportScope:
    """What the imports of a file are judged against: its new package, the packages it saw before, the names a move
    brings, the wildcards it had, and the code it now holds."""
    moves: Moves
    new_package: str
    visible_before: List[str]
    old_wildcards: Set[str]
    new_targets: Set[str]
    used: str


def diff_lines(a: List[str], b: List[str], old: str, new: str, limit: int = 12) -> str:
    out = [line for line in difflib.unified_diff(a, b, old, new, n=0, lineterm="") if not line.startswith("@@")]
    shown = out[2:2 + limit]
    more = len(out) - 2 - len(shown)
    return "\n".join("      " + line for line in shown) + (f"\n      ... {more} more line(s)" if more > 0 else "")


def _uses(name: str, used: str) -> bool:
    return re.search(rf"\b{re.escape(name)}\b", used) is not None


def _shadowing_risk(package_name: str, scope: ImportScope) -> str:
    """An added wildcard of the file's old package, which it moved out of: none of its names may shadow a name of the
    file's new package that the file uses, as the inner package did."""
    moves, new_package = scope.moves, scope.new_package
    clash: Set[str] = set()
    if new_package:
        clash = moves.package_members(moves.head, package_name) & moves.package_members(moves.head, new_package)
    used_clash = sorted(n for n in clash if _uses(n, scope.used))
    return f"{package_name}.* now shadows {', '.join(used_clash)} of {new_package}" if used_clash else ""


def wildcard_risk(package_name: str, scope: ImportScope) -> str:
    """Why an added `import package_name.*` is no move, or "". Two are: the file's old package, which it moved out of (its names
    were in scope; none of them may shadow a name of the file's new package it uses, as the inner package did), and a
    package holding only moved classes, each from a package the file saw or wildcard-imported before."""
    if package_name in scope.visible_before:
        return _shadowing_risk(package_name, scope)
    return _moved_wildcard_risk(package_name, scope)


def _moved_wildcard_risk(package_name: str, scope: ImportScope) -> str:
    """An added wildcard of a package other than the file's old one: it may bring only moved classes, each from a package
    the file saw or wildcard-imported before."""
    moves = scope.moves
    members = moves.package_members(moves.head, package_name)
    moved_here = {new.rsplit(".", 1)[1]: old.rsplit(".", 1)[0] for old, new in moves.classes.items() if new.rsplit(".", 1)[0] == package_name}
    stayed = sorted(set(members) - set(moved_here))
    if not members or stayed:
        return f"a wildcard bringing names that did not move: {', '.join(stayed[:3]) or 'an empty package'}"
    unseen = sorted({p for p in moved_here.values() if p not in scope.visible_before and p not in scope.old_wildcards})
    return f"a wildcard of names the file did not see before (from {', '.join(unseen)})" if unseen else ""


def _added_import_finding(entry: str, scope: ImportScope) -> str:
    target = entry.split(" ")[0]
    package_name, _, name = target.rpartition(".")
    if "=>" in entry or " as " in entry or name == "given" or entry.endswith(" given"):
        return f"import added: {entry} (a given or renaming import)"
    if name in ("_", "*"):
        why = wildcard_risk(package_name, scope)
        return f"import added: {entry} ({why})" if why else ""
    if package_name in scope.visible_before or target in scope.new_targets:
        return ""
    return f"import added: {entry} (neither from the file's old package nor a moved name)"


def _now_in_scope(entry: str, package_name: str, scope: ImportScope, new_wildcards: Set[str]) -> bool:
    """A removed import of a package the file now sits in, or one a wildcard still brings."""
    return bool(package_name == scope.new_package and scope.new_package) \
        or (package_name in new_wildcards and "=>" not in entry)


def _removed_import_finding(entry: str, scope: ImportScope, new_wildcards: Set[str]) -> str:
    target = entry.split(" ")[0]
    package_name, _, name = target.rpartition(".")
    if _now_in_scope(entry, package_name, scope, new_wildcards):
        return ""
    if name not in ("_", "*", "given") and "=>" not in entry and not _uses(name, scope.used):
        return ""
    # a wildcard the move left with nothing to bring: the file names none of the package's members at the head (what
    # it named of it moved, and is imported by its new name above), so dropping it -- as the unused-import lint
    # requires -- resolves no name otherwise
    if name in ("_", "*") and not any(_uses(m, scope.used) for m in scope.moves.package_members(scope.moves.head, package_name)):
        return ""
    return f"import removed: {entry}"


def _wildcards(imports: Set[str]) -> Set[str]:
    return {e.rsplit(".", 1)[0] for e in imports if e.endswith((".*", "._"))}


def import_findings(old: Normalised, new: Normalised, moves: Moves, body: List[str]) -> List[str]:
    old_imports = {map_name(e, moves) for e in old.imports}
    scope = ImportScope(
        moves=moves, new_package=".".join(new.packages),
        visible_before=[".".join(old.packages[:k]) for k in range(1, len(old.packages) + 1)],
        old_wildcards=_wildcards(old_imports), new_targets=set(moves.classes.values()) | set(moves.packages.values()),
        used=" ".join(body))
    found = [_added_import_finding(entry, scope) for entry in sorted(new.imports - old_imports)]
    new_wildcards = _wildcards(new.imports)
    found += [_removed_import_finding(entry, scope, new_wildcards) for entry in sorted(old_imports - new.imports)]
    return [f for f in found if f]


def _widened(lines: List[str]) -> List[str]:
    return [QUALIFIER.sub(r"\1[_]", line) for line in lines]


def _qualifier_text(line: str) -> str:
    m = QUALIFIER.search(line)
    return m.group(0) if m else "?"


def _visibility_entry(a: str, b: str) -> str:
    declared = DECLARED.search(b)
    return f"{_qualifier_text(a)} -> {_qualifier_text(b)}" + (f" ({declared.group(1)})" if declared else "")


def _overloaded(changed: List[str], lines: List[str]) -> List[str]:
    """The names the changed lines declare that the file declares as a `def` more than once."""
    names = [m.group(1) for line in changed for m in [DECLARED.search(line)] if m]
    return [n for n in names if len(re.findall(rf"\bdef\s+{re.escape(n)}\b", "\n".join(lines))) > 1]


def _line_findings(old: Normalised, new: Normalised, old_path: str, new_path: str, label: str, verdict: Verdict) -> List[str]:
    """The finding for code that differs, or (visibility widened or narrowed only, on lines safe to) the note to say so."""
    if old.lines == new.lines:
        return []
    if _widened(old.lines) != _widened(new.lines):
        return ["code differs:\n" + diff_lines(old.lines, new.lines, old_path, new_path)]
    return _visibility_findings(old, new, label, verdict)


def _visibility_findings(old: Normalised, new: Normalised, label: str, verdict: Verdict) -> List[str]:
    """Code identical but for qualifiers (`private[a]` -> `private[b]`): a finding where a line is implicit, given, an
    extension or overloaded (the qualifier may change which is chosen), else a note that the change is pure."""
    changed = [b for a, b in zip(old.lines, new.lines) if a != b]
    overloaded = _overloaded(changed, new.lines)
    unsafe = [line for line in changed if IMPLICIT_LINE.search(line)]
    if unsafe or overloaded:
        why = "an implicit/given/extension line" if unsafe else f"overloaded {', '.join(sorted(set(overloaded)))}"
        return [f"visibility changed on {why}:\n" + diff_lines(old.lines, new.lines, "old", "new")]
    verdict.visibility.append(f"{label}: " + "; ".join(_visibility_entry(a, b) for a, b in zip(old.lines, new.lines) if a != b))
    return []


def compare_scala(old_path: str, new_path: str, old_src: str, new_src: str, moves: Moves, verdict: Verdict) -> None:
    twirl = is_twirl(new_path)
    try:
        old = normalise_scala(old_src, old_path, True, moves, twirl)
        new = normalise_scala(new_src, new_path, False, moves, twirl)
    except Unlexable as e:
        raise Undecidable(f"{new_path}: cannot lex ({e})")
    label = new_path if old_path == new_path else f"{old_path} -> {new_path}"
    findings = import_findings(old, new, moves, new.lines) + _line_findings(old, new, old_path, new_path, label, verdict)
    verdict.hunks += [f"{label}: {f}" for f in findings]


def _decoded(old_bytes: bytes, new_bytes: bytes) -> Optional[Tuple[str, str]]:
    try:
        return old_bytes.decode("utf-8"), new_bytes.decode("utf-8")
    except UnicodeDecodeError:
        return None


def _changed_lines(mapped: str, new_t: str) -> List[str]:
    return [line[1:] for line in difflib.unified_diff(mapped.splitlines(), new_t.splitlines(), n=0, lineterm="")
            if line[:1] in "+-" and not line.startswith(("+++", "---"))]


def compare_text(old_path: str, new_path: str, old_bytes: bytes, new_bytes: bytes, moves: Moves, verdict: Verdict) -> None:
    label = new_path if old_path == new_path else f"{old_path} -> {new_path}"
    if REFERENCE_LISTS.search(new_path):
        verdict.references.append(f"{label} (reference list)")
        return
    decoded = _decoded(old_bytes, new_bytes)
    if decoded is None:
        verdict.hunks.append(f"{label}: binary content differs")
        return
    old_t, new_t = decoded
    mapped = map_text(old_t, moves.path_pairs(), {})
    if mapped == new_t:
        if old_t != new_t:
            verdict.references.append(f"{label} (names the moved paths)")
        return
    if not is_production(new_path):
        # a tool's path list or comment: every line it changes names a moved path, package or class
        changed = _changed_lines(mapped, new_t)
        mentions = moves.mentions()
        if all(any(m in line for m in mentions) for line in changed):
            verdict.references.append(f"{label} (path lists: {len(changed)} line(s) naming the moved paths)")
            return
    verdict.hunks.append(f"{label}: differs beyond the moved names:\n"
                         + diff_lines(mapped.splitlines(), new_t.splitlines(), old_path, new_path))
