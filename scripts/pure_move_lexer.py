"""THE PURE MOVE CHECK'S SCALA LEXER (scripts/pure_move_check.py): a source as code, string-literal and comment segments,
an interpolated literal's `${...}` splices lexed as code. Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import re
from typing import List, Optional, Tuple

Piece = Tuple[str, str]
# what one token of the scan gives: where the code before it ends (None: the token is part of the code), its pieces, and
# the index the scan goes on from
Token = Tuple[Optional[int], List[Piece], int]
CHAR_LITERAL = re.compile(r"'(\\u[0-9a-fA-F]{4}|\\.|[^\\'\n])'")


class Unlexable(Exception):
    pass


def _is_ident(ch: str) -> bool:
    return ch.isalnum() or ch == "_"


def _interpolation_skip(src: str, k: int, interpolated: bool) -> int:
    """k moved past the `$$` or `${...}` at src[k] of an interpolated literal; k itself when there is none."""
    if not interpolated:
        return k
    if src.startswith("$$", k):
        return k + 2
    if src.startswith("${", k):
        return splice_end(src, k + 2)
    return k


def _triple_end(src: str, j: int, interpolated: bool) -> int:
    n = len(src)
    k = j + 3
    while True:
        if k >= n:
            raise Unlexable("unterminated triple-quoted string")
        skipped = _interpolation_skip(src, k, interpolated)
        if skipped != k:
            k = skipped
        elif src.startswith('"""', k):
            return _after_closing_quotes(src, k)
        else:
            k += 1


def _after_closing_quotes(src: str, k: int) -> int:
    """The index after the `\"\"\"` at src[k], taking a run of further quotes into the literal's end."""
    while k + 3 < len(src) and src[k + 3] == '"':
        k += 1
    return k + 3


def _single_end(src: str, j: int, interpolated: bool) -> int:
    n = len(src)
    k = j + 1
    while True:
        if k >= n or src[k] == "\n":
            raise Unlexable("unterminated string")
        if src[k] == "\\":
            k += 2
            continue
        skipped = _interpolation_skip(src, k, interpolated)
        if skipped != k:
            k = skipped
        elif src[k] == '"':
            return k + 1
        else:
            k += 1


def string_end(src: str, j: int, interpolated: bool) -> int:
    """The index after the literal starting at src[j] == '"' (an interpolated one's splices skipped as code)."""
    if src.startswith('"""', j):
        return _triple_end(src, j, interpolated)
    return _single_end(src, j, interpolated)


def splice_end(src: str, k: int) -> int:
    """The index after the `}` closing the splice whose body starts at src[k]."""
    depth = 1
    while k < len(src):
        c = src[k]
        if c == '"':
            k = string_end(src, k, k > 0 and _is_ident(src[k - 1]))
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return k + 1
        k += 1
    raise Unlexable("unterminated splice")


def interpolated_pieces(src: str, start: int, end: int) -> List[Piece]:
    """An interpolated literal src[start:end] as its literal pieces and its `${...}` splices, each splice's body lexed as
    code: `s"p|${services.movies.ListingKey.serialised(k)}"` names a class in code, not in data."""
    pieces: List[Piece] = []
    k, lit = start, start
    while k < end:
        if src.startswith("$$", k):
            k += 2
        elif src.startswith("${", k):
            close = splice_end(src, k + 2)
            pieces.append(("str", src[lit:k + 2]))
            pieces += segments(src[k + 2:close - 1])
            k = lit = close - 1
        else:
            k += 1
    pieces.append(("str", src[lit:end]))
    return [p for p in pieces if p[1]]


def _line_comment(src: str, i: int) -> Token:
    j = src.find("\n", i)
    j = len(src) if j < 0 else j
    return i, [("comment", src[i:j])], j


def _block_comment(src: str, i: int) -> Token:
    depth, j = 1, i + 2
    while depth:
        if j >= len(src):
            raise Unlexable("unterminated comment")
        if src.startswith("/*", j):
            depth, j = depth + 1, j + 2
        elif src.startswith("*/", j):
            depth, j = depth - 1, j + 2
        else:
            j += 1
    return i, [("comment", src[i:j])], j


def _string_token(src: str, i: int, code_start: int) -> Token:
    start = i
    while start > code_start and _is_ident(src[start - 1]):
        start -= 1
    interpolated = start < i
    j = string_end(src, i, interpolated)
    return start, interpolated_pieces(src, start, j) if interpolated else [("str", src[start:j])], j


def _token_at(src: str, i: int, code_start: int) -> Optional[Token]:
    """The comment, string or character literal starting at src[i] (or the backquoted name, skipped as code), else None."""
    if src.startswith("//", i):
        return _line_comment(src, i)
    if src.startswith("/*", i):
        return _block_comment(src, i)
    c = src[i]
    if c == '"':
        return _string_token(src, i, code_start)
    if c == "'":
        m = CHAR_LITERAL.match(src[i:i + 8])
        return (i, [("str", m.group(0))], i + len(m.group(0))) if m else None
    if c == "`":
        j = src.find("`", i + 1)
        return None, [], len(src) if j < 0 else j + 1
    return None


def segments(src: str) -> List[Piece]:
    """[(kind, text)] with kind code / str / comment, concatenating back to `src`."""
    out: List[Piece] = []
    code_start, i, n = 0, 0, len(src)
    while i < n:
        token = _token_at(src, i, code_start)
        if token is None:
            i += 1
            continue
        upto, pieces, i = token
        if upto is not None:
            if upto > code_start:
                out.append(("code", src[code_start:upto]))
            out += pieces
            code_start = i
    if n > code_start:
        out.append(("code", src[code_start:n]))
    return out
