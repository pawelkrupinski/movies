#!/usr/bin/env python3
"""Every Python tool in the repository runs on the Python floor, scripts/run-script-tests' `PythonFloor` (3.9: macOS's
/usr/bin/python3, where agents and the pre-push hook run them; CI's runners are newer, so CI alone never sees a break).

Two ways a file outgrows the floor without any test on a newer Python noticing:
  - SYNTAX newer than the floor (`match`, ...): every file is compiled by an interpreter AT the floor when one is here
    ($PYTHON_FLOOR_INTERPRETER, python3.9 on the PATH, or /usr/bin/python3 -- macOS's, tool-self-tests.yml's macOS job's), which
    is the real answer. Without one, each is parsed with `feature_version` set to the floor, which is best-effort: on
    3.12+ it accepts PEP 701 f-strings (a nested quote of the f-string's own kind, a backslash, a comment or a line
    break in a single-quoted one's expression), so those are found from the tokens instead.
  - an ANNOTATION evaluated at runtime that uses `X | Y` (PEP 604, 3.10): a TypeError the moment the def runs on 3.9,
    unless the module has `from __future__ import annotations`, which leaves every annotation unevaluated.
Library calls new since the floor (`zip(strict=)`, ...) are not detected; the floor's own runs catch those.

usage: python_floor_lint.py [ROOT]   (default the checkout this script is in)
Exit 0 when every tracked Python file -- *.py, and extension-less scripts with a python shebang -- is within the floor,
1 listing each that is not.
"""

import ast
import io
import json
import os
import re
import shutil
import subprocess
import sys
import tokenize
from typing import Any, Iterator, List, Optional, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import ci_machine  # noqa: E402
import tool_git  # noqa: E402

RUNNER = "scripts/run-script-tests"


def floor_of(root):
    """The (major, minor) the runner states as `PythonFloor="M.N"`."""
    with open(os.path.join(root, RUNNER), encoding="utf-8") as f:
        found = re.search(r'^PythonFloor="(\d+)\.(\d+)"', f.read(), re.M)
    if not found:
        raise SystemExit(f"{RUNNER} states no PythonFloor=\"M.N\"")
    return int(found.group(1)), int(found.group(2))


def _is_python_file(root: str, path: str) -> bool:
    """A tracked `.py` file, or an extension-less one whose first line is a python shebang."""
    name = os.path.basename(path)
    if name.endswith(".py"):
        return True
    if "." in name:
        return False
    try:
        with open(os.path.join(root, path), "rb") as f:
            first = f.readline()
    except OSError:
        return False
    return first.startswith(b"#!") and b"python" in first


def python_files(root):
    tracked = tool_git.Git.here(root).text("ls-files", "-z")
    for path in sorted(p for p in tracked.split("\0") if p):
        if _is_python_file(root, path):
            yield path


def _def_annotations(node: Any) -> Iterator[Tuple[int, ast.expr]]:
    """(line, annotation) of a def's parameters and return."""
    a = node.args
    for arg in a.posonlyargs + a.args + a.kwonlyargs + [a.vararg, a.kwarg]:
        if arg is not None and arg.annotation is not None:
            yield node.lineno, arg.annotation
    if node.returns is not None:
        yield node.lineno, node.returns


def annotations(tree, in_function=False):
    """(line, annotation) of every annotation Python evaluates at runtime: a def's (when it runs), and a variable's in a
    module or class body -- a function's local variable annotation is never evaluated (PEP 526)."""
    for node in ast.iter_child_nodes(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            yield from _def_annotations(node)
            yield from annotations(node, True)
        elif isinstance(node, ast.ClassDef):
            yield from annotations(node, False)
        else:
            if isinstance(node, ast.AnnAssign) and not in_function:
                yield node.lineno, node.annotation
            yield from annotations(node, in_function)


def is_floor(interpreter, floor):
    """`interpreter` runs and is Python `floor` (major.minor)."""
    try:
        out = subprocess.run([interpreter, "-c", "import sys; print('%d.%d' % sys.version_info[:2])"],
                             capture_output=True, text=True, timeout=30).stdout.strip()
    except (OSError, subprocess.SubprocessError):
        return False
    return out == f"{floor[0]}.{floor[1]}"


def floor_interpreter(floor):
    """An interpreter at the floor, or None: this one, $PYTHON_FLOOR_INTERPRETER, python<M.N> on the PATH, /usr/bin/python3."""
    if tuple(sys.version_info[:2]) == tuple(floor):
        return sys.executable
    candidates = [ci_machine.environment().get("PYTHON_FLOOR_INTERPRETER"), shutil.which(f"python{floor[0]}.{floor[1]}"), "/usr/bin/python3"]
    return next((c for c in candidates if c and is_floor(c, floor)), None)


# Run by the floor's interpreter: each file compiled as that Python compiles it.
FLOOR_COMPILE = r"""
import json, sys
found = []
for path in json.load(sys.stdin):
    try:
        with open(path, encoding="utf-8") as f:
            source = f.read()
        compile(source, path, "exec", dont_inherit=True)
    except (OSError, UnicodeDecodeError, ValueError):
        pass
    except SyntaxError as e:
        found.append([path, e.lineno or 0, e.msg])
json.dump(found, sys.stdout)
"""


def floor_syntax(interpreter, root, paths):
    """[(path, line, message)] for each of `paths` (relative to `root`) the floor's interpreter cannot compile."""
    done = subprocess.run([interpreter, "-c", FLOOR_COMPILE], input=json.dumps([os.path.join(root, p) for p in paths]),
                          capture_output=True, text=True, timeout=600, check=True)
    return [(os.path.relpath(path, root), line, msg) for path, line, msg in json.loads(done.stdout)]


def _bare_quote(token_string: str) -> str:
    """A string token's quote, its prefix letters off."""
    return token_string.lstrip("rRfFbBuU")


def _expression_problem(tok: tokenize.TokenInfo, quote: str) -> Optional[str]:
    """What inside an f-string's expression (the quote it is open with) only Python 3.12 reads, else None."""
    if tok.type in (tokenize.STRING, getattr(tokenize, "FSTRING_START")) and _bare_quote(tok.string).startswith(quote):
        return f"a quote ({quote}) inside an f-string of the same quote (Python 3.12)"
    if tok.type == tokenize.COMMENT:
        return "a comment inside an f-string's expression (Python 3.12)"
    if "\\" in tok.string:
        return "a backslash inside an f-string's expression (Python 3.12)"
    if tok.type == tokenize.NL and len(quote) == 1:
        return "a single-quoted f-string's expression across lines (Python 3.12)"
    return None


def _fstring_problems(tokens: List[tokenize.TokenInfo]) -> Set[Tuple[int, str]]:
    """{(line, what)} of the f-string expressions in `tokens` that only Python 3.12 reads."""
    fstring_start, fstring_middle, fstring_end = (getattr(tokenize, kind) for kind in ("FSTRING_START", "FSTRING_MIDDLE", "FSTRING_END"))
    found, open_fstrings = set(), []  # each open f-string's quote
    for tok in tokens:
        if tok.type == fstring_end:
            open_fstrings.pop()
            continue
        problem = _expression_problem(tok, open_fstrings[-1]) if open_fstrings and tok.type != fstring_middle else None
        if problem:
            found.add((tok.start[0], problem))
        if tok.type == fstring_start:
            open_fstrings.append(_bare_quote(tok.string))
    return found


def newer_fstrings(source):
    """[(line, what)] for each PEP 701 (3.12) f-string: the tokenizer that reads them is the only one that sees them. An
    older tokenizer ends an f-string at its first own quote and has no FSTRING_* tokens (its parser refuses them)."""
    if not hasattr(tokenize, "FSTRING_START"):
        return []
    try:
        tokens = list(tokenize.generate_tokens(io.StringIO(source).readline))
    except (tokenize.TokenError, SyntaxError):
        return []
    return sorted(_fstring_problems(tokens))


def _defers_annotations(tree: ast.Module) -> bool:
    """Whether the module says `from __future__ import annotations`."""
    return any(isinstance(n, ast.ImportFrom) and n.module == "__future__" and any(a.name == "annotations" for a in n.names)
               for n in tree.body)


def _bitor_annotations(tree: ast.Module) -> Set[Tuple[int, str]]:
    """{(line, what)} of the annotations spelled `X | Y`, which Python 3.10 first reads."""
    return {(line, "an `X | Y` annotation (Python 3.10) without `from __future__ import annotations`")
            for line, ann in annotations(tree)
            if any(isinstance(n, ast.BinOp) and isinstance(n.op, ast.BitOr) for n in ast.walk(ann))}


def findings_in(source, floor, syntax=True):
    """(line, what) for each way `source` outgrows `floor`; with `syntax=False` (an interpreter at the floor compiled it
    already) its annotations alone."""
    try:
        tree = ast.parse(source, feature_version=floor)
    except SyntaxError as e:
        return [(e.lineno or 0, f"syntax newer than Python {floor[0]}.{floor[1]}: {e.msg}")] if syntax else []
    newer = newer_fstrings(source) if syntax else []
    if newer:
        return [(line, f"syntax newer than Python {floor[0]}.{floor[1]}: {what}") for line, what in newer]
    return [] if _defers_annotations(tree) else sorted(_bitor_annotations(tree))


def main(argv):
    root = os.path.abspath(argv[0]) if argv else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
    floor = floor_of(root)
    paths = list(python_files(root))
    bad = []
    interpreter = floor_interpreter(floor)
    if interpreter:
        bad += [f"{path}:{line}: syntax newer than Python {floor[0]}.{floor[1]} (its own compile, {interpreter}): {msg}"
                for path, line, msg in floor_syntax(interpreter, root, paths)]
    for path in paths:
        try:
            with open(os.path.join(root, path), encoding="utf-8") as f:
                source = f.read()
        except (OSError, UnicodeDecodeError):
            continue
        bad += [f"{path}:{line}: {what}" for line, what in findings_in(source, floor, syntax=interpreter is None)]
    for line in bad:
        print(line)
    if bad:
        print(f"{len(bad)} finding(s): the Python floor is {floor[0]}.{floor[1]} ({RUNNER}'s PythonFloor)")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
