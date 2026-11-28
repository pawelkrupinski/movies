#!/usr/bin/env python3
"""The JS/TS quality report: complexity, length, clones and coupling of the production JS/TS
THE JS/TS QUALITY REPORT: the Scala code-quality and coupling measures (CLAUDE.md, "Code quality ratchets" and
"Coupling is reviewed") taken over the JavaScript and TypeScript, per area (scripts/js-quality/areas.tsv). Every area
is REPORTED; the production ones (scripts/js-quality/ratchet-areas.tsv: web/src/main/assets/js and
infra/version-dashboard/src) are also GATED, by THE JS/TS QUALITY RATCHET (scripts/ci/js_quality_ratchet.py, the
pre-push `jsquality` check) against test/resources/js-quality-baseline.tsv:

  scripts/js-quality-report --check [--range A..B]...   the production rows held to the baseline (exit 1 past it)
  scripts/js-quality-report --update                     the baseline rewritten from them (falls recorded; a rise is
                                                         committed with its kind's trailer)

  scripts/js-quality-report                      the markdown report to stdout
  scripts/js-quality-report --out DIR            also keep report.md, functions.tsv, coupling.tsv and over-limit.tsv
                                                 (`kind<TAB>key<TAB>value`, every item over a limit) in DIR
  scripts/js-quality-report --root DIR [--area NAME[:globals]=PATH[,PATH...]]...
                                                 another checkout (e.g. ~/projects/events --area app=packages);
                                                 --areas FILE reads the areas from a file of areas.tsv's shape

  scripts/js-quality-report --tools-dir         where the pinned tools live (installed or not), and nothing else

The measures and their tools (scripts/js-quality/package.json and its lock, pinned; installed by `npm ci` on first run
OUTSIDE the checkout, in ~/.cache/kinowo/js-quality/<lock hash>/node_modules -- $KINOWO_TOOL_CACHE for ~/.cache/kinowo --
shared by every worktree: a node_modules left in the tree was read by the next test's `grep -r`, Order independence #99):
  cognitive complexity  <= 10   eslint-plugin-sonarjs `cognitive-complexity` (Sonar's own implementation)
  cyclomatic complexity <= 10   ESLint core `complexity`
  inner defs            <= 2    named functions (declarations, `const f = () =>`) a function holds; not callbacks
  function length       <= 50   ESLint core `max-lines-per-function` (physical lines; IIFEs not counted)
  file length           <= 500  physical lines
  clone groups (50+ tokens)     jscpd, import/require lines skipped, pairs joined into groups
  fan-out <= 9, cycles, D <= 0.20 (directories of 8+ modules)
                                dependency-cruiser's resolved imports; for classic browser scripts (kind `globals`)
                                the `/* global */` lines resolved to the file whose `/* exported */` offers the name
Each JS function is measured by itself, a callback as a function of its own (Scala folds lambdas into their
method); a module stands for a class and its directory for a package; fan-out counts every module used.
"""
from __future__ import annotations

import hashlib
import os
import shutil
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from typing import List, Mapping, NoReturn

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from processes import machine  # noqa: E402
from code_tools import pinned_npm_tools  # noqa: E402
from web_templates import twirl_scripts  # noqa: E402
from seams import tool_files  # noqa: E402
import tool_process  # noqa: E402

TOOLS = os.path.join(HERE, "js-quality")
MANIFESTS = ("package.json", "package-lock.json")
RATCHET = os.path.join(HERE, "ci", "js_quality_ratchet.py")


class Refused(Exception):
    """What stops the report, said on stderr: the exit status and its line."""

    def __init__(self, status: int, line: str = "") -> None:
        super().__init__(line)
        self.status = status
        self.line = line


@dataclass
class Options:
    root: str = os.path.dirname(HERE)
    out: str = ""
    args: List[str] = field(default_factory=lambda: [])
    ratchet: str = ""
    ranges: List[str] = field(default_factory=lambda: [])
    print_tools: bool = False


def value(argv: List[str], i: int) -> str:
    if i + 1 >= len(argv):
        raise Refused(2, f"js-quality-report: {argv[i]} needs a value")
    return argv[i + 1]


def directory(path: str) -> str:
    if not os.path.isdir(path):
        raise Refused(1, f"js-quality-report: no directory {path}")
    return os.path.abspath(path)


def read_word(o: Options, argv: List[str], i: int) -> int:
    """`argv[i]` (and its value) read into `o`: the index of the last word taken."""
    word = argv[i]
    if word == "--root":
        o.root = directory(value(argv, i))
    elif word == "--out":
        o.out = value(argv, i)
    elif word in ("--area", "--areas"):
        o.args += [word, value(argv, i)]
    elif word == "--range":
        o.ranges += ["--range", value(argv, i)]
    elif word in ("--check", "--update"):
        o.ratchet = word[2:]
        return i
    elif word == "--tools-dir":
        o.print_tools = True
        return i
    else:
        raise Refused(2, f"js-quality-report: unknown argument {word}")
    return i + 1


def lock_key() -> str:
    """`cat package.json package-lock.json | git hash-object --stdin`'s first 16 digits: the shell tool's key, so an
    existing install is found where it is."""
    data = b""
    for name in MANIFESTS:
        with open(os.path.join(TOOLS, name), "rb") as f:
            data += f.read()
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()[:16]


def tools_dir(env: Mapping[str, str]) -> str:
    """The pinned tools' home: keyed by the manifest and its lock, so a bump installs beside the old set, never over
    it."""
    return os.path.join(pinned_npm_tools.cache_root(env), "js-quality", lock_key())


def has_eslint(installed: str) -> bool:
    return os.access(os.path.join(installed, "node_modules", ".bin", "eslint"), os.X_OK)


def ensure_tools(installed: str, env: Mapping[str, str]) -> None:
    if shutil.which("node") is None:
        raise Refused(2, "js-quality-report: node is not installed")
    if has_eslint(installed):
        return
    if shutil.which("npm") is None:
        raise Refused(2, "js-quality-report: npm is not installed (needed once, to install the tools)")
    print(f"js-quality-report: installing the tools (npm ci in {installed})", file=sys.stderr)
    try:
        pinned_npm_tools.npm_ci(TOOLS, MANIFESTS, installed, capture=False, runner=tool_process.SubprocessRunner(), env=env)
    except pinned_npm_tools.NpmFailed:
        raise Refused(2) from None  # npm said why, on stderr
    if not has_eslint(installed):
        raise Refused(2, f"js-quality-report: the tools did not install in {installed}")


def node_env(installed: str) -> Mapping[str, str]:
    """report.js finds the tools here: its binaries by JS_QUALITY_NODE_MODULES, its require()s by NODE_PATH."""
    modules = os.path.join(installed, "node_modules")
    node_path = machine.environment().get("NODE_PATH")
    return {**machine.environment(), "JS_QUALITY_NODE_MODULES": modules,
            "NODE_PATH": modules + (":" + node_path if node_path else "")}


def measure_production(o: Options, out: str, env: Mapping[str, str]) -> int:
    """The production areas measured (their over-limit rows into `out`), then the Twirl templates' inline `<script>`s
    (web_templates/twirl_scripts.py: the production JS they are, as virtual files of a scratch checkout, their rows appended);
    the first pass's node status, else the second's."""
    def measure(root: str, where: str) -> int:
        node = ["node", os.path.join(TOOLS, "report.js"), "--root", root, "--out", where,
                "--areas", os.path.join(TOOLS, "ratchet-areas.tsv")]
        return subprocess.run(node, env=env, stdout=subprocess.DEVNULL).returncode

    return measure(o.root, out) or twirl_scripts.measure(tool_files.Files.here(), o.root, measure,
                                                          os.path.join(os.path.abspath(out), "over-limit.tsv"))


def report(o: Options, out: str, env: Mapping[str, str]) -> int:
    node = ["node", os.path.join(TOOLS, "report.js"), "--root", o.root, "--out", out]
    if not o.ratchet:
        return subprocess.run(node + o.args, env=env).returncode
    measured = measure_production(o, out, env)
    if measured != 0:
        return measured
    current = os.path.join(os.path.abspath(out), "over-limit.tsv")
    return subprocess.run([sys.executable, RATCHET, o.ratchet, "--current", current, *o.ranges],
                          cwd=o.root).returncode


def ended(signum: int, _frame: object) -> NoReturn:
    sys.exit(128 + signum)  # through the finally that removes a scratch --out


def run(argv: List[str]) -> int:
    o = Options()
    i = 0
    while i < len(argv):
        if argv[i] in ("-h", "--help"):
            print("Purpose: " + (__doc__ or ""), end="")
            return 0
        i = read_word(o, argv, i) + 1
    installed = tools_dir(machine.environment())
    if o.print_tools:
        print(installed)
        return 0
    ensure_tools(installed, machine.environment())
    if o.out:
        return report(o, o.out, node_env(installed))
    signal.signal(signal.SIGTERM, ended)
    out = tempfile.mkdtemp()
    try:
        return report(o, out, node_env(installed))
    finally:
        shutil.rmtree(out, ignore_errors=True)


def main() -> int:
    try:
        return run(sys.argv[1:])
    except Refused as e:
        if e.line:
            print(e.line, file=sys.stderr)
        return e.status


if __name__ == "__main__":
    sys.exit(main())
