#!/usr/bin/env python3
"""THE DESIGN RULES GATE (CLAUDE.md, "Design rules"): the general rule core's rules, ONE implementation over every
language's modules (extractors.py: Scala, Python, JS/TS), held to test/resources/design-baseline.tsv by the ratchet
engine every quality ratchet uses (scripts/ci/baseline_ratchet.py): what is past a rule today is frozen and may only
fall; a new row, or a frozen row rising, fails unless its commit carries the kind's trailer.

  Naming:    suffix, abbreviation <file>:<name>          naming.py (and permutations BLOCK, action names warn)
  Design:    effect, state, import-io <file>; effect <file>:<files|storage|network>   design_lint.py (Python, JS/TS)
             mixed <file>:<function> (an effect and decisions in one function)    design_lint.py (every language)
  Coupling:  fanout <file> (collaborators only), databag <file>:<type>, datafanout <file>   collaborators.py
             distance, unplaced, dircycle <directory>, layer <from>-><to>                    packages.py
  A `Coupling: split of <X> into <A>[, <B>]` commit may raise its importers' fan-out where splits.py proves the use
  moved from X into the pieces (the owner, 2026-11-27: the split credit for importers); nothing else of it rises.
  An exemption (test/resources/design-exemptions.tsv: kind, key, added date, a SPECIFIC why -- coupling_reasons.py's
  lint) admits one NEW row; its row added carries the kind's trailer too.

Scala's own fan-out, distance, layers, cycles and data guard stay project/Coupling.scala's (read from the class files);
Scala is held here for its names and its `mixed` functions (scala_names.py reads both from the text).

  design_check.py check  [--root DIR] [--range A..B]... [--languages scala,py,js] [--js-modules FILE]
      the asked languages' rows held (each pre-push check holds its own); exit 1 on a violation, 2 on an error
  design_check.py update [--root DIR] [--js-modules FILE]                     the baseline rewritten from the measure
The blocking permutation rule and the action-name warning are naming_lint.py's, over the same modules."""
from __future__ import annotations

import argparse
import contextlib
import dataclasses
import os
import sys
import tempfile
from typing import Dict, Generator, Iterable, List, Set, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(1, os.path.join(os.path.dirname(HERE), "ci"))
sys.path.insert(2, os.path.join(os.path.dirname(HERE), "lib"))
import baseline_ratchet  # noqa: E402
import collaborators  # noqa: E402
import design_lint  # noqa: E402
import exemptions  # noqa: E402
import extractors  # noqa: E402
import naming  # noqa: E402
import packages  # noqa: E402
import rules_table  # noqa: E402
import splits  # noqa: E402
import tool_git  # noqa: E402
from model import Module  # noqa: E402

BASELINE = "test/resources/design-baseline.tsv"
TRAILERS: Dict[str, Tuple[str, ...]] = {
    "Naming": ("suffix", "abbreviation"),
    "Design": ("effect", "state", "import-io", "mixed"),
    "Coupling": ("fanout", "distance", "layer", "unplaced", "dircycle", "databag", "datafanout"),
}
ZERO_LIMITS = ("suffix", "abbreviation", "effect", "state", "import-io", "mixed", "layer", "unplaced", "dircycle")
HEADER = """# THE DESIGN BASELINE (scripts/design-rules/design_check.py update writes it; the pre-push pyquality, jsquality and
# quality checks hold every push to it). Every module, name and directory of the Scala, Python and JS/TS past a
# general rule (test/resources/design-rules.tsv), frozen:
#   suffix / abbreviation <file>:<name>                 Naming:
#   effect / state / import-io <file>, effect <file>:<files|storage|network>, mixed <file>:<function>   Design:
#   fanout <file>, databag <file>:<type>, datafanout <file>, distance / unplaced / dircycle <directory>,
#   layer <from>-><to>                                  Coupling:
# A row may only fall (update records it, no trailer); a row added or raised lands only with its kind's trailer."""
HINT = ("Rename it, inject the clock / runner / settings, move the state into an object the root passes on, cut the"
        " dependency -- or record it: scripts/design-rules/design_check.py update, committed with the kind's trailer"
        " (`Naming:`, `Design:`, `Coupling: <why>`), or a test/resources/design-exemptions.tsv row for a new one.")
SPEC = baseline_ratchet.Spec(
    title="DESIGN RULES", tool="design rules", baseline=BASELINE, header=HEADER, hint=HINT,
    update_command="scripts/design-rules/design_check.py update", held_rows="rows past a general rule",
    trailers=TRAILERS, held=lambda _: True)
Row = Tuple[str, str, float]


def language_rows(language: str, modules: Dict[str, Module], rules: rules_table.Rules) -> List[Row]:
    out = naming.rows(modules.values(), rules)
    if language == "scala":  # its coupling, data and other design measures are project/Coupling.scala's
        return out + [r for m in modules.values() for r in design_lint.mixed_rows(m, rules)]
    collaborating = collaborators.collaborators(modules, rules)
    out += design_lint.rows(modules.values(), rules)
    out += [("fanout", p, n) for p, m in sorted(modules.items())
            if (n := collaborators.fanout(m, collaborating)) > rules.limit("fanout")]
    out += collaborators.rows(modules, rules)
    if language == "js":  # its directory cycles are the JS/TS quality ratchet's `cycle` rows already
        return out + [r for r in packages.rows(language, modules, collaborating, rules) if r[0] != "dircycle"]
    out += packages.rows(language, modules, collaborating, rules)
    return out


def limits(rules: rules_table.Rules) -> List[Row]:
    out: List[Row] = [("limit", kind, 0) for kind in ZERO_LIMITS]
    return out + [("limit", kind, rules.limit(kind)) for kind in ("fanout", "distance", "databag", "datafanout")]


def current_rows(git: tool_git.Git, args: argparse.Namespace) -> List[Row]:
    rules = rules_table.load(git.root)
    found = extractors.by_language(git, languages(args), args.range, args.js_modules)
    rows = [r for language, modules in found.items() for r in language_rows(language, modules, rules)]
    admitted = exemptions.keys(git.root)
    return limits(rules) + [r for r in rows if (r[0], r[1]) not in admitted]


def write(rows: Iterable[Row], path: str) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write("".join(f"{k}\t{key}\t{baseline_ratchet.show(v)}\n" for k, key, v in rows))


def languages(args: argparse.Namespace) -> Set[str]:
    asked = set(args.languages.split(","))
    if args.command == "update" and asked != set(extractors.LANGUAGES):
        raise ValueError("update measures every language (the baseline holds them all): drop --languages")
    return asked


@contextlib.contextmanager
def revision_tree(git: tool_git.Git, rev: str) -> Generator[tool_git.Git, None, None]:
    """A detached worktree of `rev` under the repository's own git directory, removed after: the tree the split credit
    measures a side of the commit in."""
    common = os.path.join(git.root, git.text("rev-parse", "--git-common-dir").strip())
    tree = os.path.join(common, "design-split", git.text("rev-parse", rev).strip())
    git.text("worktree", "add", "-q", "--detach", tree, rev)
    try:
        yield tool_git.Git(git.runner, git.env, tree)
    finally:
        git.ok("worktree", "remove", "--force", tree, quiet=True)


def measured_side(git: tool_git.Git, asked: Set[str], rules: rules_table.Rules) -> Tuple[splits.Modules, Set[str], Dict[str, int]]:
    """One side of a split commit: its modules, the collaborators among them and each module's fan-out."""
    modules = {p: m for found in extractors.by_language(git, asked, []).values() for p, m in found.items()}
    collaborating = collaborators.collaborators(modules, rules)
    return modules, collaborating, {p: collaborators.fanout(m, collaborating) for p, m in modules.items()}


def split_check_for(git: tool_git.Git, asked: Set[str]):  # -> baseline_ratchet.Spec's split_check
    """The split credit (splits.py) over both sides of a `split of` commit, measured from its trees."""
    def check(sha: str, value: str, grown: List[baseline_ratchet.Grown]) -> List[str]:
        rules = rules_table.load(git.root)
        held = {extractors.language_of(key) for _kind, key, _old, _new in grown} & asked
        with revision_tree(git, f"{sha}^") as before, revision_tree(git, sha) as after:
            base, head = measured_side(before, held, rules), measured_side(after, held, rules)
        return splits.refusals(value, grown, (base[0], head[0]), (base[1], head[1]), (base[2], head[2]))
    return check


def spec_for(asked: Set[str], git: tool_git.Git) -> baseline_ratchet.Spec:
    """SPEC holding only the asked languages' rows (a Python push measures no JS, and leaves its rows be), with the
    split credit judging a `split of` commit's importers."""
    def held(part: str) -> bool:
        return extractors.language_of(part) in asked

    return dataclasses.replace(SPEC, held=held, split_check=split_check_for(git, asked))


def ratchet(git: tool_git.Git, args: argparse.Namespace) -> int:
    problems = exemptions.problems(args.root, args.range, TRAILERS)
    for p in problems:
        print(f"DESIGN EXEMPTION REFUSED: {p}")
    with tempfile.TemporaryDirectory(prefix="design-") as out:
        current = os.path.join(out, "current.tsv")
        write(current_rows(git, args), current)
        status = baseline_ratchet.run(spec_for(languages(args), git), args.command, current, args.range)
    return 1 if problems and status == 0 else status


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", choices=("check", "update"))
    parser.add_argument("--root", default=os.path.dirname(os.path.dirname(HERE)))
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--js-modules", default="")
    parser.add_argument("--languages", default=",".join(extractors.LANGUAGES), help="check: the languages held")
    args = parser.parse_args(argv)
    args.root = os.path.abspath(args.root)
    os.chdir(args.root)
    git = tool_git.Git.here(args.root)  # the composition root: the one runner and environment every reader is handed
    try:
        return ratchet(git, args)
    except (OSError, ValueError, RuntimeError, tool_git.Failed) as e:
        print(f"design_check: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
