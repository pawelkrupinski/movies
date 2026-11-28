"""THE PYTHON CLONE GROUPS (scripts/py-quality/report.py): the clone groups (scripts/lib/code_tools/clone_groups.py) of
the Python tools' tokens -- whitespace, comments and import lines left out."""
from __future__ import annotations

import io
import os
import sys
import token
import tokenize
from typing import Dict, List, Set

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from code_tools.clone_groups import MIN_TOKENS, Group, Token, groups_of_tokens  # noqa: E402

__all__ = ["MIN_TOKENS", "Group", "Token", "groups", "tokens_of"]
SKIPPED = {token.NL, token.NEWLINE, token.INDENT, token.DEDENT, token.COMMENT, token.ENCODING, token.ENDMARKER}


def tokens_of(source: str) -> List[Token]:
    """The source's tokens with their lines, bar layout, comments and the lines of `import` / `from ... import`."""
    out: List[Token] = []
    import_lines: Set[int] = set()
    try:
        for tok in tokenize.generate_tokens(io.StringIO(source).readline):
            if tok.type in SKIPPED:
                continue
            if tok.type == token.NAME and tok.string in ("import", "from") and tok.line.lstrip().startswith(tok.string):
                import_lines.update(range(tok.start[0], tok.end[0] + 1))
            out.append((tok.string, tok.start[0]))
    except (tokenize.TokenError, IndentationError, SyntaxError):
        return []
    return [t for t in out if t[1] not in import_lines]


def groups(sources: Dict[str, str]) -> List[Group]:
    """The clone groups of `sources` (path -> Python text), largest first."""
    return groups_of_tokens({path: tokens_of(text) for path, text in sources.items()})
