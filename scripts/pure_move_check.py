#!/usr/bin/env python3
"""THE PURE MOVE CHECK: whether <base>..<head> only moves code -- files renamed, packages and classes renamed -- so no
take, and no other behaviour, can move, and scripts/identity-what-if (which cannot compile a base across a package
rename) has nothing to measure. The owner, 2026-11-18: a pure move needs no what-if.

  1. THE MOVES: `git diff -M -C --name-status` over every first-parent hop of the range, composed hop by hop (a file
     renamed twice is one move), with the direct base..head pairs filling what the hops saw as a delete and an add.
     From the moved Scala files: the class map (each top-level name's old fully-qualified name -> its new one, a file
     renamed with its class renaming that class) and the package map (a package whose every file moved to one package).
  2. EACH CHANGED SCALA FILE, normalised on both sides: comments dropped; `package` clauses and imports set aside; every
     moved name on the old side mapped to its new one (dotted and slashed, `_root_.` too) -- in code, and in string
     literals only outside production (`<module>/src/main/`): a production literal naming a class is data, so its edit is
     never a move; whitespace runs collapsed (indentation kept: Scala 3 reads it), blank lines dropped. The two must be
     identical. The imports are held to what a move needs: an import the old side had (mapped); one added from the
     file's old package (it moved out) or of a moved class's new name (or package); one removed of a class now in the
     file's own package, or of a name the file no longer uses. An added wildcard, given or renaming import is no move
     (it can shadow a name). A twirl template (`*.scala.html`) is compared as text, its `@import`s as imports.
     A qualified access widened or narrowed (`private[identity]` -> `private[services]`, both public in bytecode) on a
     line declaring no implicit, given or extension and no overloaded name is reported as VISIBILITY, and is pure.
  3. NON-CODE REFERENCE EDITS are pure, reported apart: any other text file whose edit is the moved paths and names
     (path lists in tool configs and scripts, workflows, the identity tools' path lists) -- the old side, every moved
     path, package and class mapped, equals the new; and the reference lists, whatever their edit -- Markdown, docs/,
     .claude/, the AOT class list (aot-classes.txt) and the coupling, complexity, duplication and length baselines.
  4. NAME RISK: a moved class's old fully-qualified name (or a wholly moved package), dotted or slashed, in a string
     literal of production Scala, or anywhere in a production resource, conf/ or infra/ (logger-name filters, codec
     discriminators, serialized names, Class.forName), at the base or the head; a class-name read in moved production
     code or naming a moved class (getName, getCanonicalName, getTypeName, Class.forName); a renamed class's old
     simple name in a production literal. Each is NOT PURE, with its location.
  Any added or deleted file beyond the moves (a reference list's aside) is NOT PURE. A range moving no file is NOT PURE.

A static check: what it calls pure still has to compile (the pre-push hook compiles every config), and a package's
members seen through a package clause it cannot see are the compiler's to find.

usage: pure_move_check.py <base>..<head> [--quiet]
Exit 0 PURE MOVE, 1 NOT PURE (each non-move hunk named), 2 undecidable (a range git cannot read, base not an ancestor of
head, a Scala file that cannot be lexed). Tested by scripts/pure_move_check_test.py.
"""

from __future__ import annotations

import os
import subprocess
import sys
from typing import List, Tuple

sys.path[:0] = [os.path.dirname(os.path.abspath(__file__))]  # the modules beside it: `python3 -I` adds no script directory
from pure_move_compare import Verdict, compare_scala, compare_text  # noqa: E402
from pure_move_git import Blobs, Undecidable, git  # noqa: E402
from pure_move_lexer import segments  # noqa: E402
from pure_move_moves import Moves, find_moves  # noqa: E402
from pure_move_risks import literal_hits, name_risks  # noqa: E402
from pure_move_source import REFERENCE_LISTS, is_scala, is_twirl  # noqa: E402

__all__ = ["Undecidable", "git", "segments", "literal_hits", "check", "report", "main"]


def _both_scala_or_twirl(old: str, new: str) -> bool:
    return (is_scala(old) and is_scala(new)) or (is_twirl(old) and is_twirl(new))


def _compare_pair(blobs: Blobs, base: str, head: str, old: str, new: str, moves: Moves, verdict: Verdict) -> None:
    old_bytes, new_bytes = blobs.get(base, old), blobs.get(head, new)
    if old_bytes == new_bytes:
        return
    if _both_scala_or_twirl(old, new):
        old_s, new_s = blobs.text(base, old), blobs.text(head, new)
        if old_s is None or new_s is None:
            raise Undecidable(f"{new}: not UTF-8")
        compare_scala(old, new, old_s, new_s, moves, verdict)
    elif old_bytes is None or new_bytes is None:
        raise Undecidable(f"{new}: a side git cannot read")
    else:
        compare_text(old, new, old_bytes, new_bytes, moves, verdict)


def _note_unpaired(paths: List[str], reference_tag: str, hunk_text: str, verdict: Verdict) -> None:
    """A path no move pairs: a reference list's edit is pure, any other is a hunk."""
    for path in sorted(paths):
        if REFERENCE_LISTS.search(path):
            verdict.references.append(f"{path} (reference list, {reference_tag})")
        else:
            verdict.hunks.append(f"{path}: {hunk_text}")


def check(base: str, head: str) -> Tuple[Verdict, Moves]:
    blobs = Blobs()
    for ref in (base, head):
        git("rev-parse", "--verify", f"{ref}^{{commit}}")
    if subprocess.run(["git", "merge-base", "--is-ancestor", base, head]).returncode != 0:
        raise Undecidable(f"{base} is not an ancestor of {head}")
    moves = find_moves(base, head, blobs)
    verdict = Verdict(moved=sorted(moves.files.items()))
    for old, new in sorted(moves.files.items()) + [(p, p) for p in sorted(moves.modified)]:
        _compare_pair(blobs, base, head, old, new, moves, verdict)
    _note_unpaired(list(moves.added), "added", "added (no file it was moved from)", verdict)
    _note_unpaired(list(moves.deleted), "deleted", "deleted (no file it was moved to)", verdict)
    if not moves.files:
        verdict.hunks.append("(the range) moves no file: not a move")
    verdict.risks = name_risks(base, head, moves, blobs)
    return verdict, moves


def _print_section(heading: str, items: List[str]) -> None:
    if items:
        print(heading)
        for item in items:
            print(f"    {item}")


def _print_details(rng: str, verdict: Verdict, moves: Moves) -> None:
    print(f"pure-move-check {rng}: {len(verdict.moved)} file(s) moved, {len(moves.classes)} class name(s), "
          f"{len(moves.packages)} package(s) wholly moved")
    for old, new in sorted(moves.packages.items()):
        print(f"  package {old} -> {new}")
    _print_section(f"  reference edits (pure, {len(verdict.references)}):", verdict.references)
    _print_section(f"  visibility only (pure, {len(verdict.visibility)} file(s)):", verdict.visibility)
    _print_section(f"  NON-MOVE HUNKS ({len(verdict.hunks)}):", verdict.hunks)
    _print_section(f"  NAME RISK ({len(verdict.risks)}):", verdict.risks)


def report(rng: str, verdict: Verdict, moves: Moves, quiet: bool) -> int:
    pure = not verdict.hunks and not verdict.risks
    if not quiet or not pure:
        _print_details(rng, verdict, moves)
    if pure:
        print("PURE MOVE")
        return 0
    print(f"NOT PURE: {len(verdict.hunks)} non-move hunk(s), {len(verdict.risks)} name risk(s)")
    return 1


def main(argv: List[str]) -> int:
    quiet = "--quiet" in argv
    args = [a for a in argv if a != "--quiet"]
    if len(args) != 1 or ".." not in args[0] or args[0].startswith("-"):
        print((__doc__ or "").split("usage: ")[1].splitlines()[0], file=sys.stderr)
        return 2
    base, head = args[0].split("..", 1)
    try:
        verdict, moves = check(base, head or "HEAD")
    except Undecidable as e:
        print(f"pure-move-check: UNDECIDABLE: {e}", file=sys.stderr)
        return 2
    except Exception as e:  # noqa: BLE001 -- a check that broke decides nothing: undecidable, never pure
        print(f"pure-move-check: UNDECIDABLE: the check failed: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    return report(args[0], verdict, moves, quiet)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
