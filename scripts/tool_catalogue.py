#!/usr/bin/env python3
"""The tool catalogue: docs/tools.md, generated from every tool's own one-line purpose.

A tool is a script under the tool roots (scripts/, .github/scripts/, .github/actions/, infra/bin/, infra/nix/,
infra/test/, data/*/scripts/, the mobile scripts/, tools/), a top-level *.sh, a composite action or a workflow; a
self-test, a test library or a package __init__ is none. Its PURPOSE is one line it carries itself:
  - a `# Purpose: <one line>` comment in its first 40 lines (shell, workflows, and Python without a docstring);
  - a Python module's docstring's first line;
  - a composite action's top-level `description:`;
  - for a NixOS host's file (infra/nix/files/), the first sentence of its leading comment, as it stands.

    python3 scripts/tool_catalogue.py                     # rewrite docs/tools.md
    python3 scripts/tool_catalogue.py --check             # exit 1: a tool without a purpose, or docs/tools.md stale
    python3 scripts/tool_catalogue.py --refresh-agentctl [~/bin]   # rewrite the agentctl callers list from agentctl

The catalogue reads only the repository, so CI and every clone render the same page: agentctl (~/bin, the laptop's)
is read into scripts/tool-catalogue/agentctl-callers.txt by --refresh-agentctl, and each shell tool's migration class
(MIGRATE / KEEP, the 2026-11-22 shell-to-Python analysis) is scripts/tool-catalogue/migration-classes.tsv. Held by
scripts/tool_catalogue_test.py, which runs --check over this repository.
"""

from __future__ import annotations

import argparse
import ast
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Optional, Sequence, Set, Tuple

CATALOGUE = "docs/tools.md"
DATA_DIR = "scripts/tool-catalogue"
AGENTCTL_CALLERS = DATA_DIR + "/agentctl-callers.txt"
MIGRATION_CLASSES = DATA_DIR + "/migration-classes.tsv"
ACRONYMS = frozenset({"CI", "OS", "UK", "US", "ID", "IO", "JVM", "API", "URL", "SSH", "DNS", "TLS", "GC"})
HOST_FILES = "infra/nix/files/"  # deployed into the hosts' closures: read, never edited (header_sentence)
PURPOSE_LINE = re.compile(r"^\s*#\s*Purpose:\s*(\S.*?)\s*$")
TOOL_ROOTS = ("scripts/", ".github/scripts/", ".github/actions/", "infra/bin/", "infra/nix/", "infra/test/",
              "android/scripts/", "ios/scripts/", "tools/")
SKIPPED_PARTS = ("/src/", "/node_modules/", "/fixtures/", "/testdata/", "/__pycache__/")
TOKEN = re.compile(r"[A-Za-z0-9_][A-Za-z0-9_.-]*")

# (area, path pattern) -- the first match wins
AREAS: Tuple[Tuple[str, str], ...] = (
    ("Mobile", r"^(android|ios)/|store-screenshots|store-graphics|mobile-|ios-release|android-release|xcuitest"),
    ("Infra and deploy", r"^infra/|^\.github/actions/(mark-grafana|flux)|deploy|gitops|k8s|nix"),
    ("Identity", r"identity|drift|takes|title-trace|venue-replay|recording|label-lookup|judge|slice|convergence"
                 r"|hard-clusters|family|ablation|calibrat|flip|listing.history|stamp-carry|corpus"),
    ("Mirror and data", r"^data/|local-mirror|mongo|prod-|fleet-db|reset-corpus|enrichment|poster|roster|cinema"),
    ("Landing and CI", r"^scripts/(hooks|ci|claude-hooks)/|^\.github/|land|sbt|jvm-gate|gov|fail-before|ratchet"
                       r"|quality|coupling|run-script-tests|floor|stall|ci-|wait-for|agent"),
)
AREA_ORDER = ("Landing and CI", "Identity", "Mirror and data", "Infra and deploy", "Mobile", "Dev")


@dataclass(frozen=True)
class Tool:
    path: str
    purpose: Optional[str]
    area: str
    run_as: str
    callers: str
    self_test: str
    migration: str


def git_files(root: str) -> List[str]:
    out = subprocess.run(["git", "-C", root, "ls-files"], capture_output=True, text=True, check=True).stdout
    return [p for p in out.split("\n") if p and os.path.isfile(os.path.join(root, p))]


def first_line(root: str, path: str) -> str:
    try:
        with open(os.path.join(root, path), "rb") as handle:
            return handle.readline().decode("utf-8", "replace")
    except OSError:
        return ""


def is_test(path: str) -> bool:
    base = os.path.basename(path)
    return (base.endswith(("-test.sh", "_test.py", "-test-lib.sh")) or base.startswith("test_")
            or base in ("__init__.py", "conftest.py"))


def is_tool(root: str, path: str) -> bool:
    if path.startswith(".github/workflows/"):
        return path.endswith((".yml", ".yaml"))
    if any(part in "/" + path for part in SKIPPED_PARTS) or is_test(path):
        return False
    in_root = path.startswith(TOOL_ROOTS) or re.match(r"^[^/]+\.sh$|^data/[^/]+/scripts/|^[^/]+/scripts/", path)
    if not in_root:
        return False
    base = os.path.basename(path)
    if base in ("action.yml", "action.yaml"):
        return path.startswith(".github/actions/")
    if base.endswith((".py", ".sh")):
        return True
    return "." not in base and first_line(root, path).startswith("#!")


def discover(root: str) -> List[str]:
    return sorted(p for p in git_files(root) if is_tool(root, p))


def read_text(root: str, path: str) -> str:
    with open(os.path.join(root, path), encoding="utf-8", errors="replace") as handle:
        return handle.read()


def is_python(root: str, path: str) -> bool:
    return path.endswith(".py") or ("." not in os.path.basename(path) and "python" in first_line(root, path))


def docstring_line(text: str) -> Optional[str]:
    try:
        doc = ast.get_docstring(ast.parse(text))
    except SyntaxError:
        return None
    line = doc.strip().split("\n")[0].strip() if doc else ""
    return line or None


def first_sentence(text: str, limit: int = 160) -> str:
    sentence = re.split(r"(?<=[a-z0-9)`'\"])\.(?:\s|$)", re.sub(r"\s+", " ", text).strip())[0]
    return sentence if len(sentence) <= limit else sentence[:limit].rsplit(" ", 1)[0]


def block_scalar(lines: List[str]) -> str:
    """The text of a YAML block scalar's indented lines, up to the first line indented less than its first."""
    indent = len(lines[0]) - len(lines[0].lstrip()) if lines else 0
    taken = []
    for line in lines:
        if line.strip() and len(line) - len(line.lstrip()) < indent:
            break
        taken.append(line.strip())
    return " ".join(taken)


def action_description(text: str) -> Optional[str]:
    """A composite action's top-level `description:`; a block scalar (`>`, `>-`, `|`) read to its first sentence."""
    lines = text.split("\n")
    for number, line in enumerate(lines):
        match = re.match(r"^description:\s*(.*?)\s*$", line)
        if not match:
            continue
        value = match.group(1)
        if value and value[0] not in ">|":
            return first_sentence(value.strip("'\"")) or None
        body = [l for l in lines[number + 1:] if l.strip() or l == ""]
        return first_sentence(block_scalar([l for l in body if l.startswith((" ", "\t")) or not l])) or None
    return None


def purpose_of(root: str, path: str) -> Optional[str]:
    text = read_text(root, path)
    for line in text.split("\n")[:40]:
        match = PURPOSE_LINE.match(line)
        if match:
            return match.group(1)
    if os.path.basename(path).startswith("action.y"):
        return action_description(text)
    if is_python(root, path) and docstring_line(text):
        return docstring_line(text)
    return header_sentence(text) if path.startswith(HOST_FILES) else None


def header_sentence(text: str) -> Optional[str]:
    """The first sentence of a script's leading comment: what a NixOS host's file says of itself. Its content is part of
    the host's closure, so a line added for the catalogue alone would restage every host that runs it."""
    taken: List[str] = []
    for line in text.split("\n")[1:40]:
        stripped = line.strip()
        if not stripped.startswith("#") or (taken and stripped == "#"):
            if taken or (stripped and not stripped.startswith("#")):
                break
            continue
        words = stripped.lstrip("#").strip()
        if words and not words.startswith("shellcheck"):
            taken.append(words)
    return calm(first_sentence(" ".join(taken))) if taken else None


def shouted(word: str) -> bool:
    letters = re.sub(r"[^A-Za-z]", "", word)
    return (len(letters) > 1 and letters.isupper() and letters not in ACRONYMS
            and not re.search(r"[0-9$_/`.]", word))


def calm(sentence: str) -> str:
    """A header written in capitals for emphasis, in sentence case: words of capitals alone lowered, the rest kept."""
    letters = [c for c in sentence if c.isalpha()]
    if sum(c.isupper() for c in letters) <= len(letters) // 2:
        return sentence
    words = [w.lower() if shouted(w) else w for w in sentence.split(" ")]
    text = " ".join(words)
    return text[:1].upper() + text[1:]


def area_of(path: str) -> str:
    for area, pattern in AREAS:
        if re.search(pattern, path):
            return area
    return "Dev"


def workflow_triggers(text: str) -> List[str]:
    block = re.search(r"^on:\s*\n((?:[ \t]+.*\n|\s*\n)+)", text, re.M)
    if not block:
        return []
    indents = [len(m.group(1)) for m in re.finditer(r"^([ \t]+)[a-z_]+:", block.group(1), re.M)]
    top = min(indents) if indents else 0
    return [m.group(2) for m in re.finditer(r"^([ \t]+)([a-z_]+):", block.group(1), re.M) if len(m.group(1)) == top]


def run_as(root: str, path: str) -> str:
    if path.startswith(".github/workflows/"):
        return "workflow: " + (", ".join(workflow_triggers(read_text(root, path))) or "-")
    if os.path.basename(path) in ("action.yml", "action.yaml"):
        return "uses: ./" + os.path.dirname(path)
    executable = os.access(os.path.join(root, path), os.X_OK)
    if not first_line(root, path).startswith("#!"):
        return "sourced" if not is_python(root, path) else "imported"
    if executable:
        return path
    return ("python3 " if is_python(root, path) else "bash ") + path


def self_test_of(path: str, files: Set[str]) -> str:
    folder, base = os.path.split(path)
    stem = re.sub(r"\.(sh|py)$", "", base)
    names = (stem + "-test.sh", stem + "_test.py", stem.replace("-", "_") + "_test.py", "test_" + stem + ".py")
    for name in names:
        candidate = os.path.join(folder, name)
        if candidate in files:
            return name
    return "-"


def reference_keys(path: str) -> List[str]:
    base = os.path.basename(path)
    if base in ("action.yml", "action.yaml"):
        return [os.path.basename(os.path.dirname(path))]
    keys = [base]
    if base.endswith(".py"):
        keys.append(base[:-3])
    return keys


def caller_kind(path: str) -> Optional[str]:
    if path.startswith(".github/workflows/"):
        return "wf"
    if path.startswith("scripts/hooks/") and not is_test(path) and not path.endswith((".tsv", ".txt")):
        return "hook"
    if path == "CLAUDE.md" or path.startswith(".claude/skills/"):
        return "skill"
    return None


class CallerIndex:
    """Which repository files name each token, for the caller columns."""

    def __init__(self, root: str, files: Iterable[str], tools: Set[str]) -> None:
        self.tokens: Dict[str, Set[str]] = {}
        self.tools = tools
        for path in files:
            if caller_kind(path) is None and path not in tools:
                continue
            for token in set(TOKEN.findall(read_text(root, path))):
                self.tokens.setdefault(token, set()).add(path)

    def callers(self, path: str) -> Set[str]:
        found: Set[str] = set()
        for key in reference_keys(path):
            found |= self.tokens.get(key, set())
        own = os.path.dirname(path) + "/" if os.path.basename(path) in ("action.yml", "action.yaml") else None
        return {c for c in found if c != path and not is_test(c) and not (own and c.startswith(own))}


def callers_text(callers: Set[str], agentctl: bool) -> str:
    workflows = sorted(os.path.basename(c).rsplit(".", 1)[0] for c in callers if caller_kind(c) == "wf")
    hooks = sorted(os.path.basename(c) for c in callers if caller_kind(c) == "hook")
    skills = sorted({"CLAUDE.md" if c == "CLAUDE.md" else c.split("/")[2] for c in callers if caller_kind(c) == "skill"})
    tools = [c for c in callers if caller_kind(c) is None]
    parts = []
    if workflows:
        parts.append("wf " + ", ".join(workflows[:3]) + (f" +{len(workflows) - 3}" if len(workflows) > 3 else ""))
    if hooks:
        parts.append("hook " + ", ".join(hooks))
    if agentctl:
        parts.append("agentctl")
    if skills:
        parts.append("skill " + ", ".join(skills))
    if tools:
        parts.append(f"{len(tools)} tool{'s' if len(tools) > 1 else ''}")
    return "; ".join(parts) or "by hand"


def read_lines(root: str, path: str) -> List[str]:
    full = os.path.join(root, path)
    if not os.path.exists(full):
        return []
    with open(full, encoding="utf-8") as handle:
        return [line.rstrip("\n") for line in handle if line.strip() and not line.startswith("#")]


def migration_classes(root: str) -> Dict[str, str]:
    pairs = (line.split("\t") for line in read_lines(root, MIGRATION_CLASSES))
    return {fields[0]: fields[1] for fields in pairs if len(fields) >= 2}


def migration_of(root: str, path: str, classes: Dict[str, str]) -> str:
    if path.startswith(".github/") and path.endswith((".yml", ".yaml")):
        return "-"
    if is_python(root, path):
        return "Python"
    return classes.get(path, "-")


def catalogue(root: str) -> List[Tool]:
    files = git_files(root)
    tools = [p for p in files if is_tool(root, p)]
    index = CallerIndex(root, files, set(tools))
    agentctl = set(read_lines(root, AGENTCTL_CALLERS))
    classes = migration_classes(root)
    present = set(files)
    return [Tool(p, purpose_of(root, p), area_of(p), run_as(root, p), callers_text(index.callers(p), p in agentctl),
                 self_test_of(p, present), migration_of(root, p, classes)) for p in sorted(tools)]


def cell(text: str) -> str:
    return text.replace("|", "\\|")


def render(tools: Sequence[Tool]) -> str:
    lines = ["# Tools", "",
             "GENERATED by `python3 scripts/tool_catalogue.py` from each tool's own one-line purpose (a `# Purpose:`",
             "line, a Python docstring's first line, an action's `description:`); edit the tool, then regenerate.",
             "`scripts/tool_catalogue_test.py` fails while a tool has no purpose or this page is stale. Callers: `wf`",
             "workflows, `hook` git hooks, `agentctl` ~/bin (scripts/tool-catalogue/agentctl-callers.txt), `skill`",
             "CLAUDE.md or a .claude skill, `N tools` other tools; `by hand` is none of them. Class: the shell tool's",
             "migration class (MIGRATE / KEEP), `Python` for one already in Python.", ""]
    for area in AREA_ORDER:
        rows = [t for t in tools if t.area == area]
        if not rows:
            continue
        lines += [f"## {area} ({len(rows)})", "", "| Tool | Purpose | Run as | Called by | Self-test | Class |",
                  "|---|---|---|---|---|---|"]
        lines += [f"| `{t.path}` | {cell(t.purpose or '')} | `{cell(t.run_as)}` | {cell(t.callers)} | {t.self_test} "
                  f"| {t.migration} |" for t in rows]
        lines.append("")
    return "\n".join(lines)


def problems(root: str) -> List[str]:
    tools = catalogue(root)
    found = [f"{t.path}: no purpose line (a `# Purpose: <one line>` in its first 40 lines)" for t in tools if not t.purpose]
    current = read_text(root, CATALOGUE) if os.path.exists(os.path.join(root, CATALOGUE)) else ""
    if current != render(tools):
        found.append(f"{CATALOGUE} is stale: regenerate it with `python3 scripts/tool_catalogue.py`")
    return found


def agentctl_callers(root: str, bin_dir: str, read: Callable[[str], str]) -> List[str]:
    """The tools agentctl's own code names: a path or a basename, whole, in ~/bin/agentctl or ~/bin/lib's Python."""
    text = "\n".join(read(p) for p in agentctl_sources(bin_dir))
    return [p for p in discover(root) if p.startswith(("scripts/", "infra/bin/", ".github/")) and named_in(p, text)]


def named_in(path: str, text: str) -> bool:
    """A tool is named by its path, or by a basename distinctive enough to be its alone (an extension or a dash)."""
    base = os.path.basename(path)
    if base in ("action.yml", "action.yaml"):
        return re.search(r"(?<![\w.-])" + re.escape(os.path.dirname(path)) + r"(?![\w-])", text) is not None
    names = [path] + ([base] if re.search(r"[-._]", base) else [])
    return any(re.search(r"(?<![\w.-])" + re.escape(name) + r"(?![\w-])", text) for name in names)


def agentctl_sources(bin_dir: str) -> List[str]:
    sources = [os.path.join(bin_dir, "agentctl")]
    for folder, _, names in os.walk(os.path.join(bin_dir, "lib")):
        sources += [os.path.join(folder, n) for n in names if n.endswith(".py")]
    return [s for s in sources if os.path.isfile(s)]


def read_file(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as handle:
        return handle.read()


def write(root: str, path: str, text: str) -> None:
    os.makedirs(os.path.dirname(os.path.join(root, path)), exist_ok=True)
    with open(os.path.join(root, path), "w", encoding="utf-8") as handle:
        handle.write(text)


def main(argv: Sequence[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0] if __doc__ else None)
    parser.add_argument("--root", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--refresh-agentctl", nargs="?", const=os.path.expanduser("~/bin"), metavar="BIN")
    args = parser.parse_args(argv)
    root = os.path.abspath(args.root)
    if args.refresh_agentctl:
        found = agentctl_callers(root, args.refresh_agentctl, read_file)
        write(root, AGENTCTL_CALLERS, "# The tools agentctl (~/bin) names: python3 scripts/tool_catalogue.py "
                                      "--refresh-agentctl\n" + "".join(p + "\n" for p in found))
    if args.check:
        found = problems(root)
        for line in found:
            print("tool-catalogue: " + line, file=sys.stderr)
        return 1 if found else 0
    write(root, CATALOGUE, render(catalogue(root)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
