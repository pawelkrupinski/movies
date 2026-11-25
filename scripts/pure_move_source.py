"""THE PURE MOVE CHECK'S SOURCE READING (scripts/pure_move_check.py): which paths are Scala, twirl, production or a
reference list, and a Scala source's package clauses and top-level names. Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import re
from typing import List

from pure_move_lexer import Unlexable, segments

PACKAGE_CLAUSE = re.compile(r"^\s*package\s+(?!object\b)([\w.]+)\s*;?\s*$")
TOP_LEVEL = re.compile(
    r"^(?:(?:private|protected)(?:\[\w+\])?\s+|final\s+|sealed\s+|abstract\s+|case\s+|implicit\s+|open\s+|opaque\s+"
    r"|transparent\s+|inline\s+|@\w+(?:\([^)]*\))?\s+)*(?:class|object|trait|enum|type|given)\s+([A-Za-z_]\w*)")
REFERENCE_LISTS = re.compile(
    r"(^|/)[^/]*\.md$|^docs/|^\.claude/|(^|/)aot-classes\.txt$"
    r"|^test/resources/(coupling|complexity|duplication|length)-baseline\.tsv$")


def is_scala(path: str) -> bool:
    return path.endswith(".scala") or path.endswith(".sbt")


def is_twirl(path: str) -> bool:
    return path.endswith(".scala.html")


def is_production(path: str) -> bool:
    return bool(re.match(r"^[^/]+/src/main/", path)) or path.startswith("conf/") \
        or (path.startswith("infra/") and not path.startswith("infra/test/"))


def code_text(src: str) -> str:
    """`src` with its comments blanked (a block comment to a space, keeping its line ends), literals kept."""
    parts: List[str] = []
    for kind, text in segments(src):
        parts.append(re.sub(r"[^\n]", "", text) or " " if kind == "comment" and text.startswith("/*") else
                     "" if kind == "comment" else text)
    return "".join(parts)


def _lines_without_comments(src: str) -> List[str]:
    try:
        text = code_text(src)
    except Unlexable:
        text = src
    return text.splitlines()


def packages_of(src: str) -> List[str]:
    """The package clauses of a Scala source, in order (chained clauses each one)."""
    return [m.group(1) for line in _lines_without_comments(src) if (m := PACKAGE_CLAUSE.match(line))]


def top_level_names(src: str) -> List[str]:
    return [m.group(1) for line in _lines_without_comments(src) if (m := TOP_LEVEL.match(line))]
