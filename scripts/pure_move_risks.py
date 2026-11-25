"""THE PURE MOVE CHECK'S NAME RISK (scripts/pure_move_check.py): a moved class's old name in a production string literal,
a class-name read in a moved file, a renamed class's old simple name in a literal -- data a move cannot carry along.
Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import re
from typing import List, Set, Tuple

from pure_move_git import Blobs, git
from pure_move_lexer import Unlexable, segments
from pure_move_moves import Moves
from pure_move_normalise import boundary
from pure_move_source import REFERENCE_LISTS, is_production, is_scala, is_twirl

CLASS_NAME_READ = re.compile(r"\b(getName|getCanonicalName|getTypeName|Class\.forName)\b")
Hit = Tuple[int, str, str]
Scanned = Set[Tuple[str, ...]]


def _hits_in_piece(src: str, text: str, offset: int, names: Set[str], lines: List[str]) -> List[Hit]:
    hits: List[Hit] = []
    for nm in sorted(names):
        for m in boundary(nm).finditer(text):
            no = src.count("\n", 0, offset + m.start()) + 1
            hits.append((no, nm, lines[no - 1].strip()[:160] if no <= len(lines) else ""))
    return hits


def literal_hits(src: str, names: Set[str]) -> List[Hit]:
    """(line, name, the line) for each of `names` inside a string literal of the Scala source `src`."""
    try:
        pieces = segments(src)
    except Unlexable:
        pieces = [("str", src)]
    hits: List[Hit] = []
    offset, lines = 0, src.splitlines()
    for kind, text in pieces:
        if kind == "str":
            hits += _hits_in_piece(src, text, offset, names, lines)
        offset += len(text)
    return hits


def _grep_hit(hit: str) -> Tuple[str, str, str]:
    """A `git grep -n` line as its path, line number and text."""
    _, path, line_no, text = hit.split(":", 3)
    return path, line_no, text


def _literal_names(moves: Moves) -> Set[str]:
    names: Set[str] = set()
    for old in list(moves.classes) + list(moves.packages):
        if "." in old:
            names |= {old, old.replace(".", "/")}
    return names


def _scala_literal_risks(side: str, rev: str, path: str, names: Set[str], blobs: Blobs, scanned: Scanned) -> List[str]:
    if (rev, path) in scanned:
        return []
    scanned.add((rev, path))
    return [f"{side} {path}:{no}: a string literal names {nm}: {line}"
            for no, nm, line in literal_hits(blobs.text(rev, path) or "", names)]


def _hit_risks(side: str, rev: str, hit: str, names: Set[str], blobs: Blobs, scanned: Scanned) -> List[str]:
    """What one `git grep` hit of a moved name is: a Scala file's literals (once a file), or another production file's line."""
    path, line_no, text = _grep_hit(hit)
    if not is_production(path) or REFERENCE_LISTS.search(path) or is_twirl(path):
        return []
    if is_scala(path):
        return _scala_literal_risks(side, rev, path, names, blobs, scanned)
    hit_names = [nm for nm in names if boundary(nm).search(text)]
    if hit_names:
        return [f"{side} {path}:{line_no}: names {', '.join(sorted(hit_names))}: {text.strip()[:160]}"]
    return []


def _moved_name_risks(side: str, rev: str, names: Set[str], blobs: Blobs, scanned: Scanned) -> List[str]:
    args = ["grep", "-n", "-F"] + [x for nm in sorted(names) for x in ("-e", nm)] + [rev, "--"]
    risks: List[str] = []
    for hit in git(*args, ok=(0, 1)).splitlines():
        risks += _hit_risks(side, rev, hit, names, blobs, scanned)
    return risks


def _name_read_lines(src: str) -> List[Tuple[int, str]]:
    """(line number, line) of each non-comment line of `src` that reads a class name."""
    return [(no, line) for no, line in enumerate(src.splitlines(), 1)
            if CLASS_NAME_READ.search(line) and not line.strip().startswith(("//", "*", "/*"))]


def _class_name_reads(moves: Moves, blobs: Blobs) -> List[str]:
    """A class-name read in a moved production file."""
    risks: List[str] = []
    for new in moves.files.values():
        if not is_production(new) or not is_scala(new):
            continue
        risks += [f"head {new}:{no}: reads a class name in a moved file: {line.strip()[:160]}"
                  for no, line in _name_read_lines(blobs.text(moves.head, new) or "")]
    return risks


def _moved_class_name_reads(moves: Moves) -> List[str]:
    """A moved class's name read anywhere in production."""
    moved_simple = {c.rsplit(".", 1)[-1] for c in moves.classes}
    if not moved_simple:
        return []
    alternation = "|".join(map(re.escape, sorted(moved_simple)))
    pattern = r"(classOf\[(" + alternation + r")\][^\n]*\b(getName|getCanonicalName|getTypeName)" \
              r"|\b(" + alternation + r")\.getClass\.(getName|getCanonicalName|getTypeName))"
    risks: List[str] = []
    for hit in git("grep", "-n", "-E", "-e", pattern, moves.head, "--", "*.scala", ok=(0, 1)).splitlines():
        path, line_no, text = _grep_hit(hit)
        if is_production(path) and path not in moves.files.values():
            risks.append(f"head {path}:{line_no}: reads a moved class's name: {text.strip()[:160]}")
    return risks


def _renamed_class_literals(moves: Moves, base: str, blobs: Blobs, scanned: Scanned) -> List[str]:
    """A renamed class's old simple name in a production literal."""
    risks: List[str] = []
    for old in moves.simple:
        for hit in git("grep", "-new", "-w", "-F", "-e", old, base, "--", "*.scala", ok=(0, 1)).splitlines():
            path = _grep_hit(hit)[0]
            if not is_production(path) or (base, path, old) in scanned:
                continue
            scanned.add((base, path, old))
            risks += [f"base {path}:{no}: a string literal names the renamed class {old}: {line}"
                      for no, _, line in literal_hits(blobs.text(base, path) or "", {old})]
    return risks


def name_risks(base: str, head: str, moves: Moves, blobs: Blobs) -> List[str]:
    names = _literal_names(moves)
    risks: List[str] = []
    scanned: Scanned = set()
    for rev, side in ((base, "base"), (head, "head")):
        if names:
            risks += _moved_name_risks(side, rev, names, blobs, scanned)
    risks += _class_name_reads(moves, blobs) + _moved_class_name_reads(moves)
    risks += _renamed_class_literals(moves, base, blobs, scanned)
    return sorted(set(risks))
