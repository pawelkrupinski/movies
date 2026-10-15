#!/usr/bin/env python3
"""Every Python tool in the repository runs on the Python floor, scripts/run-script-tests' `PythonFloor` (3.9: macOS's
/usr/bin/python3, where agents and the pre-push hook run them; CI's runners are newer, so CI alone never sees a break).

Two ways a file outgrows the floor without any test on a newer Python noticing:
  - SYNTAX newer than the floor (`match`, ...): every file is compiled by an interpreter AT the floor when one is here
    ($PYTHON_FLOOR_INTERPRETER, python3.9 on the PATH, or /usr/bin/python3 -- macOS's, the nightly macOS job's), which
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

RUNNER = "scripts/run-script-tests"


def floor_of(root):
    """The (major, minor) the runner states as `PythonFloor="M.N"`."""
    with open(os.path.join(root, RUNNER), encoding="utf-8") as f:
        found = re.search(r'^PythonFloor="(\d+)\.(\d+)"', f.read(), re.M)
    if not found:
        raise SystemExit(f"{RUNNER} states no PythonFloor=\"M.N\"")
    return int(found.group(1)), int(found.group(2))


def git(root, *args):
    """git in `root`, whatever repository a calling hook's GIT_DIR / GIT_WORK_TREE / GIT_INDEX_FILE point at."""
    env = {k: v for k, v in os.environ.items() if k not in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE")}
    return subprocess.run(["git", "-C", root] + list(args), capture_output=True, text=True, check=True, env=env).stdout


def python_files(root):
    tracked = git(root, "ls-files", "-z")
    for path in sorted(p for p in tracked.split("\0") if p):
        name = os.path.basename(path)
        if name.endswith(".py"):
            yield path
        elif "." not in name:
            try:
                with open(os.path.join(root, path), "rb") as f:
                    first = f.readline()
            except OSError:
                continue
            if first.startswith(b"#!") and b"python" in first:
                yield path


def annotations(tree):
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            a = node.args
            for arg in a.posonlyargs + a.args + a.kwonlyargs + [a.vararg, a.kwarg]:
                if arg is not None and arg.annotation is not None:
                    yield node.lineno, arg.annotation
            if node.returns is not None:
                yield node.lineno, node.returns
        elif isinstance(node, ast.AnnAssign):
            yield node.lineno, node.annotation


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
    candidates = [os.environ.get("PYTHON_FLOOR_INTERPRETER"), shutil.which(f"python{floor[0]}.{floor[1]}"), "/usr/bin/python3"]
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


def newer_fstrings(source):
    """[(line, what)] for each PEP 701 (3.12) f-string: the tokenizer that reads them is the only one that sees them. An
    older tokenizer ends an f-string at its first own quote and has no FSTRING_* tokens (its parser refuses them)."""
    if not hasattr(tokenize, "FSTRING_START"):
        return []
    try:
        tokens = list(tokenize.generate_tokens(io.StringIO(source).readline))
    except (tokenize.TokenError, SyntaxError):
        return []
    found, open_fstrings = set(), []  # each open f-string's quote
    for tok in tokens:
        if tok.type == tokenize.FSTRING_END:
            open_fstrings.pop()
            continue
        if not open_fstrings or tok.type == tokenize.FSTRING_MIDDLE:
            if tok.type == tokenize.FSTRING_START:
                open_fstrings.append(tok.string.lstrip("rRfFbBuU"))
            continue
        quote = open_fstrings[-1]
        if tok.type in (tokenize.STRING, tokenize.FSTRING_START) and tok.string.lstrip("rRfFbBuU").startswith(quote):
            found.add((tok.start[0], f"a quote ({quote}) inside an f-string of the same quote (Python 3.12)"))
        elif tok.type == tokenize.COMMENT:
            found.add((tok.start[0], "a comment inside an f-string's expression (Python 3.12)"))
        elif "\\" in tok.string:
            found.add((tok.start[0], "a backslash inside an f-string's expression (Python 3.12)"))
        elif tok.type == tokenize.NL and len(quote) == 1:
            found.add((tok.start[0], "a single-quoted f-string's expression across lines (Python 3.12)"))
        if tok.type == tokenize.FSTRING_START:
            open_fstrings.append(tok.string.lstrip("rRfFbBuU"))
    return sorted(found)


def findings_in(source, floor, syntax=True):
    """(line, what) for each way `source` outgrows `floor`; with `syntax=False` (an interpreter at the floor compiled it
    already) its annotations alone."""
    try:
        tree = ast.parse(source, feature_version=floor)
    except SyntaxError as e:
        return [(e.lineno or 0, f"syntax newer than Python {floor[0]}.{floor[1]}: {e.msg}")] if syntax else []
    if syntax:
        newer = newer_fstrings(source)
        if newer:
            return [(line, f"syntax newer than Python {floor[0]}.{floor[1]}: {what}") for line, what in newer]
    deferred = any(isinstance(n, ast.ImportFrom) and n.module == "__future__" and any(a.name == "annotations" for a in n.names)
                   for n in tree.body)
    if deferred:
        return []
    return sorted({(line, "an `X | Y` annotation (Python 3.10) without `from __future__ import annotations`")
                   for line, ann in annotations(tree)
                   if any(isinstance(n, ast.BinOp) and isinstance(n.op, ast.BitOr) for n in ast.walk(ann))})


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
