#!/usr/bin/env python3
"""Move a class to another package, its references rewritten compiler-checked (scalafix MoveSymbol).

A CLASS MOVED TO ANOTHER PACKAGE, its references rewritten compiler-checked (scalafix's MoveSymbol rule,
scripts/scalafix-rules/MoveSymbol.scala, reading what every name means from SemanticDB -- never a text search): in the
kept scalafix tree (scripts/lib/scalafix_tree.py), every module's sources rewritten, the source moved to the new
package's directory, then every configuration compiled; only a rewrite that compiles is laid back over the checkout and
its index. Run scripts/scalafix-organize afterwards for the import order the rewrite's added imports left.

  scripts/scalafix-move <from.package.Name> <to.package.Name>

<from...Name> is a top-level class, trait or object (with its companion); <to...Name> its new full name -- another
package, and optionally another name (references renamed too). The source moves when it holds only that class and
companion and is named after it; otherwise the class stays in its file and the file is moved to the new package only
when it holds nothing else -- else the tool stops (split the file first). What the rule cannot see -- the class named
in a string, a comment, a resource -- the compile cannot either: `grep -rn` the old name afterwards, and run the
layers. A pure move then lands with `scripts/pure-move-check` (CLAUDE.md, identity what-if).

Exit 0 moved (the changes staged in the checkout); 1 the rewrite did not compile (its errors shown; the checkout
untouched), or a step failed; 2 a usage error."""
from __future__ import annotations

import fnmatch
import os
import re
import sys
from typing import List, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import scalafix_tree  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402

TOOL = "scalafix-move"
FULL_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)+")
DEFINITION = r"^(final |sealed |abstract |case |private\[[a-z]+\] )*(class|trait|object|enum) "
COMPILES = ("Test/compile", "worker/IntegrationTest/compile", "web/IntegrationTest/compile", "web/PageTest/compile")


def names(argv: List[str]) -> Tuple[str, str]:
    if len(argv) != 2:
        raise tool_cli.UsageError("scalafix-move <from.package.Name> <to.package.Name>")
    for fqn in argv:
        if not FULL_NAME.fullmatch(fqn):
            raise tool_cli.UsageError(f"'{fqn}' is no full name (package.Name)")
    if argv[0] == argv[1]:
        raise tool_cli.UsageError("from and to are the same name")
    return argv[0], argv[1]


def scala_sources(tree: str) -> List[str]:
    """Every .scala file under the tree, relative, outside target/ and .git, in a stable order."""
    found: List[str] = []
    for directory, subdirectories, files in os.walk(tree):
        subdirectories[:] = sorted(d for d in subdirectories if d not in ("target", ".git"))
        relative = os.path.relpath(directory, tree)
        found.extend(os.path.normpath(os.path.join(relative, f)) for f in sorted(files) if f.endswith(".scala"))
    return found


def defines(path: str, name: str) -> int:
    """How many lines of the source at `path` define `name` (any top-level type with name ""); 0 when unreadable."""
    pattern = re.compile(DEFINITION + (re.escape(name) + r"\b" if name else "[A-Za-z]"), re.M)
    try:
        with open(path, errors="replace") as f:
            return len(pattern.findall(f.read()))
    except OSError:
        return 0


def source_of(tree: str, package_dir: str, name: str) -> Optional[str]:
    """The source defining it: <module>/src/<config>/scala/<package path>/<Name>.scala, or a file of the package
    defining it."""
    sources = scala_sources(tree)
    named = [s for s in sources if fnmatch.fnmatchcase(f"./{s}", f"./*/src/*/scala/{package_dir}/{name}.scala")]
    if named:
        return named[0]
    in_package = [s for s in sources if os.path.dirname(f"/{s}").endswith(f"/{package_dir}")]
    return next((s for s in in_package if defines(os.path.join(tree, s), name)), None)


def move(tree: scalafix_tree.ScalafixTree, source: str, target: str, rule: str) -> None:
    """The rewrite, the source moved, the compile, the patch laid over the checkout; Failed at the first that fails."""
    rewrite_log = tree.path("rewrite.log")
    if tree.sbt(rewrite_log, scalafix_tree.commands(rule)) != 0:
        sys.stderr.write("".join(scalafix_tree.log_lines(rewrite_log)[-40:]))
        tree.discard()
        raise scalafix_tree.Failed("the rewrite failed")
    os.makedirs(os.path.join(tree.tree, os.path.dirname(target)), exist_ok=True)
    if not tree.git_tree.ok("mv", source, target):
        raise scalafix_tree.Failed(f"cannot move {source} to {target}")
    compile_log = tree.path("compile.log")
    if tree.sbt(compile_log, COMPILES) != 0:
        sys.stderr.write("".join([l for l in scalafix_tree.log_lines(compile_log) if l.startswith("[error]")][:40]))
        tree.reset_hard()
        raise scalafix_tree.Failed("the moved code does not compile: the checkout is untouched (fix the rule's gap by "
                                   "hand, or move in two steps)")
    if not tree.apply():
        raise scalafix_tree.Failed("the rewrite changed nothing")


def scalafix_move(argv: List[str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        print((__doc__ or "").split("\n\n", 1)[1], end="\n")
        return 0
    try:
        from_name, to_name = names(argv)
    except tool_cli.UsageError as e:
        return tool_cli.usage_failed(TOOL, e)
    tree = scalafix_tree.ScalafixTree.opened(TOOL, tool_process.SubprocessRunner(), machine.environment())
    try:
        started = int(machine.wall())
        from_package, name = from_name.rsplit(".", 1)
        to_package, new_name = to_name.rsplit(".", 1)
        package_dir = from_package.replace(".", "/")
        source = source_of(tree.tree, package_dir, name)
        if source is None:
            raise scalafix_tree.Failed(f"no source of {from_package} defines {name}")
        if defines(os.path.join(tree.tree, source), "") != defines(os.path.join(tree.tree, source), name):
            raise scalafix_tree.Failed(f"{source} defines more than {name} (and its companion): split it first")
        target = f"{source[:source.rfind('/' + package_dir + '/')]}/{to_package.replace('.', '/')}/{new_name}.scala"
        if os.path.lexists(os.path.join(tree.tree, target)):
            raise scalafix_tree.Failed(f"{target} exists already")
        print(f"{TOOL}: {from_name} -> {to_name} ({source} -> {target})", flush=True)
        move(tree, source, target, "--rules=file:scripts/scalafix-rules/MoveSymbol.scala"
                                   f" --settings.MoveSymbol.from={from_name} --settings.MoveSymbol.to={to_name}")
        print(tree.staged_stat())
        print(f"{TOOL}: compiled and staged in the checkout ({int(machine.wall()) - started} s); now grep -rn "
              f"'{name}' for strings and docs")
        return 0
    finally:
        tree.close()


def main() -> int:
    try:
        return scalafix_move(sys.argv[1:])
    except scalafix_tree.Failed as e:
        sys.stdout.flush()
        print(f"{TOOL}: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
