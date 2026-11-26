#!/usr/bin/env python3
"""THE PYTHON QUALITY RATCHET (the pre-push `pyquality` check; CLAUDE.md, "Code quality ratchets"): the repository's
Python tools' measures (scripts/py-quality-report --check: every `.py` and extension-less Python tool under HELD_ROOTS:
scripts/, .github/scripts/, infra/bin/, infra/nix/bin/, infra/nix/files/, android/scripts/, ios/scripts/) may not grow past test/resources/py-quality-baseline.tsv -- the JS/TS ratchet's model and engine
(baseline_ratchet.py), each kind under the Scala ratchet's own trailer, strict pyright's error count under `Typing:`:

  Complexity:   cognitive, cyclomatic, inner-defs     <file>:<function>[#n] (no line: an edit above never re-keys it)
  Length:       method-lines <file>:<function>[#n], file-lines <file>
  Duplication:  copies <file>|<file>... (the copies of every clone group spanning exactly those files), clone-tokens
  Coupling:     fanout <file>, cycle <member>|<member>...
  Typing:       pyright <file> (its strict pyright errors; a new file has none)

Self-tests (`*_test.py`) are reported, never held. THE MIGRATED TOOLS (scripts/py-quality/migrated.py: every tool of
scripts/shell-py-progress/migrate-tools.tsv and its new `.py` files) have NO row: one past a limit is refused whatever
the baseline or a trailer says -- "migrated tools stay under the bar — split it".

  py_quality_ratchet.py check --current <over-limit.tsv> --migrated <files.txt> [--range A..B ...]
                                                                              exit 1 on a violation, 2 on an error
                                                                              (files.txt: the migrated tools' files, one
                                                                              a line; `check` without it is an error, as
                                                                              scripts/py-quality-report names them)
  py_quality_ratchet.py update --current <over-limit.tsv>                     rewrite the baseline from the measure"""
from __future__ import annotations

import argparse
import dataclasses
import os
import sys
from typing import Dict, List, Set, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # the engine beside it, under `python3 -I` too
import baseline_ratchet  # noqa: E402

BASELINE = "test/resources/py-quality-baseline.tsv"
HELD_ROOTS = ("scripts/", ".github/scripts/", "infra/bin/", "infra/nix/bin/", "infra/nix/files/", "android/scripts/", "ios/scripts/")
TRAILERS: Dict[str, Tuple[str, ...]] = {
    "Complexity": ("cognitive", "cyclomatic", "inner-defs"),
    "Length": ("method-lines", "file-lines"),
    "Duplication": ("copies", "clone-tokens"),
    "Coupling": ("fanout", "cycle"),
    "Typing": ("pyright",),
}
HEADER = """# THE PYTHON QUALITY BASELINE (scripts/py-quality-report --update writes it; scripts/ci/py_quality_ratchet.py holds
# every push touching the Python tools to it). Every function, file, clone set and module of the Python under scripts/
# and .github/scripts/ past a limit, frozen, and every file's strict pyright errors:
#   limit         each kind's limit: a key without a row stays at or below it
#   cognitive / cyclomatic / inner-defs   <file>:<function>[#n]                 Complexity:
#   method-lines <file>:<function>[#n], file-lines <file>                       Length:
#   copies <file>|<file>... (clone groups spanning exactly them), clone-tokens  Duplication:
#   fanout <file>, cycle <member>|<member>...                                   Coupling:
#   pyright <file> (strict errors)                                              Typing:
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying its kind's trailer (CLAUDE.md, "Code quality ratchets"). Self-tests are reported, not held."""
HINT = ("Split the function or file, extract the clone, cut the import, type it -- or record it: scripts/py-quality-report"
        " --update, committed with the kind's trailer (`Complexity:`, `Length:`, `Duplication:`, `Coupling:`, `Typing:"
        " <why>`) in the commit message's final trailer block.")


def held(part: str) -> bool:
    """A key's file (`<file>` or `<file>:<function>`) is a tool's under HELD_ROOTS; a self-test's
    is reported, never held."""
    path = part.split(":", 1)[0]
    return path.startswith(HELD_ROOTS) and not path.endswith("_test.py")


SPEC = baseline_ratchet.Spec(
    title="PYTHON QUALITY", tool="py quality", baseline=BASELINE, header=HEADER, hint=HINT,
    update_command="scripts/py-quality-report --update",
    held_rows="held rows past a limit; self-tests reported, not held", trailers=TRAILERS, held=held)


MIGRATED_HINT = ("migrated tools stay under the bar — split it (a tool of scripts/shell-py-progress/migrate-tools.tsv"
                 " and its new files, scripts/py-quality/migrated.py: no baseline row or trailer admits one)")


def spec_for(migrated: Set[str]) -> baseline_ratchet.Spec:
    """SPEC with the migrated tools' files strict: none of their rows past a limit is admitted."""
    def strict(part: str) -> bool:
        return part.split(":", 1)[0] in migrated

    return dataclasses.replace(SPEC, strict=strict, strict_hint=MIGRATED_HINT)


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description="The Python quality ratchet (scripts/py-quality-report --check).")
    parser.add_argument("command", choices=("check", "update"))
    parser.add_argument("--current", required=True)
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--migrated", help="the migrated tools' files, one a line (required by `check`)")
    args = parser.parse_args(argv)
    if args.command == "update":
        return baseline_ratchet.run(SPEC, args.command, args.current, args.range)
    if args.migrated is None:
        print("py quality: `check` needs --migrated <files.txt>: without the migrated tools' files the strict rule"
              " (no row admits one past a limit) would silently not apply", file=sys.stderr)
        return 2
    try:
        with open(args.migrated, encoding="utf-8") as f:
            files = {line.strip() for line in f if line.strip()}
    except OSError as e:
        print(f"py quality: {e}", file=sys.stderr)
        return 2
    return baseline_ratchet.run(spec_for(files), args.command, args.current, args.range)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
