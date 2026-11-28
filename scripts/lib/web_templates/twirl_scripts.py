"""THE INLINE `<script>` OF A TWIRL TEMPLATE, read as the production JavaScript it is (the owner's 2026-11-28 decision:
scripts/js_quality_report.py measures it with the files of web/src/main/assets/js, one virtual file per script, under the
JS/TS ratchet's limits and baseline). A script is `<script>` or one with an id and no `src` and no non-JS `type`;
`application/json`, `ld+json` and `speculationrules` blocks are data. Twirl is made readable as JS first: its comments
(`@* *@`) go, `@@` is `@`, `@if(...)` / `@for(...)` become `if (twirl)`, and each `@expr` / `@{expr}` becomes the name
`twirl`. A script is `<template path>.script<N>.js`, N counting the template's scripts from 1."""
from __future__ import annotations

import os
import re
from typing import Callable, Dict, List

from seams.tool_files import Files

COMMENTS = re.compile(r"@\*.*?\*@|<!--.*?-->", re.S)
SCRIPT = re.compile(r"<script(?P<attributes>\s[^>]*)?>(?P<body>.*?)</script>", re.S)
NOT_JS_TYPE = re.compile(r"""\btype\s*=\s*["'](?!module|text/javascript)""")
CONTROL = re.compile(r"@(?:if|for|while|defining|match)\s*\(")
PLACEHOLDER = "twirl"
PREFIX = "web/src/main/twirl/"


def balanced(text: str, start: int, opening: str, closing: str) -> int:
    """The index after the group opened at `text[start]`, or the text's end for an unclosed one."""
    depth = 0
    for i in range(start, len(text)):
        depth += (text[i] == opening) - (text[i] == closing)
        if depth == 0:
            return i + 1
    return len(text)


def name_char(text: str, i: int) -> bool:
    return text[i].isalnum() or text[i] == "_"


def expression_end(text: str, start: int) -> int:
    """The end of the Twirl expression whose name starts at `start`: `name`, then any `.name`, `(...)` or `[...]`."""
    i = start
    while i < len(text):
        if text[i] in "([":
            i = balanced(text, i, text[i], ")" if text[i] == "(" else "]")
        elif name_char(text, i) or (text[i] == "." and i + 1 < len(text) and name_char(text, i + 1)):
            i += 1
        else:
            break
    return i


def control(text: str, start: int, out: list) -> int:  # type: ignore[type-arg]
    """A `@if(...) {` stays a JS `if`; a `@for(...) {body}` (a repeat of its body) is the body alone. The next index."""
    i = balanced(text, text.index("(", start), "(", ")")
    if text.startswith("@if", start):
        out.append(f"if ({PLACEHOLDER})")
        return i
    rest = text[i:]
    gap = len(rest) - len(rest.lstrip())
    if not rest.lstrip().startswith("{"):
        return i
    end = balanced(text, i + gap, "{", "}")
    out.append(neutral(text[i + gap + 1:end - 1]))
    return end


def neutral(text: str) -> str:
    """Twirl made plain JavaScript (same lines, placeholders for the server's values)."""
    out, i = [], 0
    while i < len(text):
        if text[i] != "@":
            out.append(text[i])
            i += 1
        elif text.startswith("@@", i):
            out.append("@")
            i += 2
        elif CONTROL.match(text, i):
            i = control(text, i, out)
        elif text.startswith("@{", i):
            i = balanced(text, i + 1, "{", "}")
            out.append(PLACEHOLDER)
        else:
            i = expression_end(text, i + 1)
            out.append(PLACEHOLDER)
    return "".join(out)


def scripts(path: str, text: str) -> Dict[str, str]:
    """The template's inline scripts: virtual path -> JavaScript."""
    found: Dict[str, str] = {}
    plain = COMMENTS.sub(lambda m: "\n" * m.group(0).count("\n"), text)
    for match in SCRIPT.finditer(plain):
        attributes = match.group("attributes") or ""
        if "src=" in attributes or NOT_JS_TYPE.search(attributes):
            continue
        found[f"{path}.script{len(found) + 1}.js"] = neutral(match.group("body"))
    return found


def holds_templates(path: str) -> bool:
    return path.startswith(PREFIX) and path.endswith(".scala.html")


def template_paths(files: Files, root: str) -> List[str]:
    """The templates of the checkout `root`, as repository paths, sorted."""
    directory = os.path.join(root, PREFIX)
    if not files.is_dir(directory):
        return []
    return [path for path in (os.path.relpath(p, root) for p in files.under(directory)) if holds_templates(path)]


def materialize(files: Files, root: str, scratch: str) -> int:
    """Every inline script of the checkout `root` written as a file of the directory `scratch`, at its virtual path:
    how many."""
    count = 0
    for path in template_paths(files, root):
        for virtual, javascript in scripts(path, files.read_text(os.path.join(root, path))).items():
            files.write_text(os.path.join(scratch, virtual), javascript)
            count += 1
    return count


def merge_rows(files: Files, where: str, target: str, status: int) -> int:
    """After a measuring pass that ended with `status` 0, its over-limit rows (not its limits) appended to `target`'s;
    the status."""
    if status == 0:
        rows = [line for line in files.read_text(os.path.join(where, "over-limit.tsv")).splitlines()
                if line and not line.startswith("limit\t")]
        files.write_text(target, files.read_text(target) + "".join(f"{line}\n" for line in rows))
    return status


def measure(files: Files, root: str, run: Callable[[str, str], int], target: str) -> int:
    """The checkout's inline scripts measured: written into a scratch directory, `run(scratch, out)` the measuring pass
    over it, its over-limit rows appended to `target`; the pass's status (0 when there is no script)."""
    with files.scratch("js-quality-twirl-") as scratch:
        if materialize(files, root, scratch) == 0:
            return 0
        where = os.path.join(scratch, "measured")
        return merge_rows(files, where, target, run(scratch, where))
