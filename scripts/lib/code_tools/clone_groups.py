"""THE CLONE GROUPS, over any language's tokens (what the quality measures share: scripts/py-quality/clones.py splits
Python, scripts/ci/mobile_quality_measure.py Swift and Kotlin through lizard): every run of MIN_TOKENS or more tokens
appearing twice or more -- in one file or across files -- as jscpd's `mild` mode finds them for the JS ratchet, names
kept as written (two copies differing only in a name are no clone). Copies whose spans overlap are joined into one
group; a group's tokens are its longest copy's."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Sequence, Set, Tuple

MIN_TOKENS = 50
MAX_SITES = 12  # a window repeated more often (a table's rows) is compared among its first MAX_SITES sites only


@dataclass(frozen=True)
class Copy:
    file: str
    start: int  # first line
    end: int    # last line


@dataclass(frozen=True)
class Group:
    tokens: int
    copies: Tuple[Copy, ...]


Token = Tuple[str, int]  # its text and its line
Site = Tuple[int, int]   # (file index, token index)


def window_sites(files: Sequence[List[Token]]) -> Dict[int, List[Site]]:
    sites: Dict[int, List[Site]] = {}
    for f, toks in enumerate(files):
        texts = [t[0] for t in toks]
        for i in range(len(toks) - MIN_TOKENS + 1):
            sites.setdefault(hash(tuple(texts[i:i + MIN_TOKENS])), []).append((f, i))
    return {h: s for h, s in sites.items() if len(s) > 1}


def apart(a: Site, b: Site) -> bool:
    """Two sites whose windows do not overlap: in two files, or MIN_TOKENS or more apart in one."""
    return a[0] != b[0] or abs(a[1] - b[1]) >= MIN_TOKENS


def matching_starts(files: Sequence[List[Token]]) -> Set[Tuple[Site, Site]]:
    """Every pair of distinct, non-overlapping sites whose MIN_TOKENS-token windows are equal."""
    pairs: Set[Tuple[Site, Site]] = set()
    for sites in window_sites(files).values():
        first = sites[:MAX_SITES]
        pairs.update((a, b) for n, a in enumerate(first) for b in first[n + 1:] if apart(a, b))
    return pairs


def runs(pairs: Set[Tuple[Site, Site]]) -> List[Tuple[Site, Site, int]]:
    """The maximal runs of consecutive matching starts: (first site, second site, tokens copied)."""
    out: List[Tuple[Site, Site, int]] = []
    for a, b in pairs:
        if ((a[0], a[1] - 1), (b[0], b[1] - 1)) in pairs:
            continue  # inside a run that started earlier
        n = 1
        while ((a[0], a[1] + n), (b[0], b[1] + n)) in pairs:
            n += 1
        out.append((a, b, n + MIN_TOKENS - 1))
    return out


def copy_of(names: Sequence[str], files: Sequence[List[Token]], site: Site, length: int) -> Copy:
    toks = files[site[0]]
    return Copy(file=names[site[0]], start=toks[site[1]][1], end=toks[site[1] + length - 1][1])


def find_root(parent: Dict[Copy, Copy], c: Copy) -> Copy:
    while parent[c] != c:
        c = parent[c]
    return c


def groups_of_tokens(tokenized: Dict[str, List[Token]]) -> List[Group]:
    """The clone groups of files already split into tokens (path -> its tokens with their lines), largest first."""
    names = sorted(tokenized)
    files = [tokenized[n] for n in names]
    parent: Dict[Copy, Copy] = {}
    size: Dict[Copy, int] = {}
    for a, b, length in runs(matching_starts(files)):
        ca, cb = copy_of(names, files, a, length), copy_of(names, files, b, length)
        for c in (ca, cb):
            parent.setdefault(c, c)
            size[c] = max(size.get(c, 0), length)
        parent[find_root(parent, cb)] = find_root(parent, ca)
    joined: Dict[Copy, List[Copy]] = {}
    for c in parent:
        joined.setdefault(find_root(parent, c), []).append(c)
    found = [Group(tokens=max(size[c] for c in cs), copies=tuple(sorted(cs, key=lambda c: (c.file, c.start))))
             for cs in joined.values()]
    return sorted(found, key=lambda g: (-g.tokens, g.copies[0].file, g.copies[0].start))
