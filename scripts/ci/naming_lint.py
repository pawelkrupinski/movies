#!/usr/bin/env python3
"""THE NAMING LINT (the owner's "names must match behaviour", 2026-11-19): the production Scala's top-level type names,
in the pre-push hook's quality step (a production Scala change).

  RULE 1, BLOCKS: two top-level types of one package whose names are the same words in another order (PruneSweep and
  SweepPrune): a reader cannot tell from the names which is which. Rename one after what makes it distinct. A pair
  being renamed may be allowed for now in ALLOWED_PERMUTATIONS (its owner and why); a row no clash matches any more is
  said, so it is struck.
  RULE 2, WARNS, new files only: a new top-level type whose name ends in a noun of what it does to something else
  (Apart, Stages, Read(s), Reading(s), Builds, Rereads, Drafts, Traces, Denials, Plans) yet has at most one public def
  -- usually a function wearing a type's name: fold it into the class whose data it reads, or name the one thing it is.
  A warning, never a refusal (about 80% precise when measured on 2026-11-19).

Usage:
  naming_lint.py check [--root DIR] [--range A..B ...]   rule 1 over every production source of DIR (default .),
      rule 2 over the sources the ranges add; exit 1 on a rule-1 clash, 0 otherwise (warnings on stdout)
"""

import argparse
import os
import re
import subprocess
import sys
from typing import Dict, List, Set, Tuple

MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")
# (package, names): a clash allowed while it is renamed -- its owner and why. Struck once the rename lands.
ALLOWED_PERMUTATIONS = {
}
DECLARATION = re.compile(r"^(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:(?:final|sealed|abstract|case|implicit)\s+)*"
                         r"(class|object|trait|enum)\s+([A-Z]\w*)", re.M)
PACKAGE = re.compile(r"^package\s+([\w.]+)", re.M)
PUBLIC_DEF = re.compile(r"^  (?:(?:override|final|inline)\s+)*def\s+(\w+)", re.M)
ACTION_SUFFIX = re.compile(r"(Apart|Stages|Reads?|Readings?|Builds|Rereads|Drafts|Traces|Denials|Plans)$")
WORD = re.compile(r"[A-Z][a-z0-9]*|[a-z0-9]+")


def production(path: str) -> bool:
    return path.endswith(".scala") and path.split("/", 1)[0] in MODULES and "/src/main/" in path


def top_level(text: str) -> List[Tuple[str, str]]:
    """(name, body) of each top-level type: its declaration at the start of a line, its body to the next one."""
    found = list(DECLARATION.finditer(text))
    return [(m.group(2), text[m.end():found[i + 1].start() if i + 1 < len(found) else len(text)])
            for i, m in enumerate(found)]


def package_of(text: str, path: str) -> str:
    parts = PACKAGE.findall(text)
    return ".".join(parts) if parts else os.path.dirname(path).split("/src/main/scala/")[-1].replace("/", ".")


def words(name: str) -> Tuple[str, ...]:
    return tuple(sorted(w.lower() for w in WORD.findall(name)))


def public_defs(body: str) -> int:
    return len(PUBLIC_DEF.findall(body))


def sources(root: str) -> List[str]:
    out = []
    for module in MODULES:
        base = os.path.join(root, module, "src", "main")
        for d, _, files in os.walk(base):
            out += [os.path.relpath(os.path.join(d, f), root) for f in files if f.endswith(".scala")]
    return sorted(out)


def read(root: str, path: str) -> str:
    with open(os.path.join(root, path), encoding="utf-8", errors="replace") as fh:
        return fh.read()


def clashes(root: str, paths: List[str]) -> Tuple[List[str], List[str], List[str]]:
    """(refused, allowed, stale allow rows) of rule 1."""
    groups: Dict[Tuple[str, Tuple[str, ...]], Dict[str, str]] = {}
    for path in paths:
        text = read(root, path)
        pkg = package_of(text, path)
        for name, _ in top_level(text):
            groups.setdefault((pkg, words(name)), {}).setdefault(name, path)
    refused, allowed, seen = [], [], set()
    for (pkg, _), names in sorted(groups.items()):
        if len(names) < 2:
            continue
        key = (pkg, frozenset(names))
        said = f"{pkg}: " + ", ".join(f"{n} ({p})" for n, p in sorted(names.items()))
        if key in ALLOWED_PERMUTATIONS:
            seen.add(key)
            allowed.append(f"{said} -- allowed: {ALLOWED_PERMUTATIONS[key]}")
        else:
            refused.append(said)
    stale = [f"{pkg}: {', '.join(sorted(names))}" for pkg, names in ALLOWED_PERMUTATIONS if (pkg, names) not in seen]
    return refused, allowed, stale


def added(root: str, ranges: List[str]) -> Set[str]:
    out: Set[str] = set()
    for rng in ranges:
        found = subprocess.run(["git", "diff", "--name-only", "--diff-filter=A", "--no-renames", rng], cwd=root,
                               capture_output=True, text=True, check=True).stdout
        out |= {p for p in found.split() if production(p)}
    return out


def action_names(root: str, paths) -> List[str]:
    out = []
    for path in sorted(paths):
        if not os.path.exists(os.path.join(root, path)):
            continue
        counts: Dict[str, int] = {}
        for name, body in top_level(read(root, path)):  # a class and its companion: one name, their defs together
            counts[name] = counts.get(name, 0) + public_defs(body)
        out += [f"{name} ({path}): {n} public def" for name, n in counts.items() if ACTION_SUFFIX.search(name) and n <= 1]
    return out


def check(root: str, ranges: List[str]) -> int:
    refused, allowed, stale = clashes(root, sources(root))
    for a in allowed:
        print(f"naming: {a}")
    for s in stale:
        print(f"naming: an ALLOWED_PERMUTATIONS row no clash matches any more ({s}): strike it in scripts/ci/naming_lint.py")
    for w in action_names(root, added(root, ranges)) if ranges else []:
        print(f"naming WARNING (rule 2): a new type named for an action with at most one public def: {w} -- fold it"
              " into the class whose data it reads, or name the one thing it is")
    if refused:
        print("NAMING REFUSED (rule 1): two types of one package named with the same words in another order -- rename one"
              " after what makes it distinct:")
        for r in refused:
            print(f"  - {r}")
        return 1
    print("naming: no two types of a package share their words in another order")
    return 0


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    c = sub.add_parser("check")
    c.add_argument("--root", default=".")
    c.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    try:
        return check(args.root, args.range)
    except (OSError, subprocess.CalledProcessError) as e:
        print(f"naming_lint: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
