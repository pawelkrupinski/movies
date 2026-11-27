"""THE DESIGN EXEMPTIONS (test/resources/design-exemptions.tsv): a NEW row past a general rule admitted on purpose, as
Scala's coupling and quality exemptions admit one -- each with its `added` date and a SPECIFIC why, linted by the one
reason lint every exemption table shares (scripts/ci/coupling_reasons.py), and a row added only in a commit carrying
its kind's trailer.

  <kind><TAB><key><TAB><added YYYY-MM-DD><TAB><role>: <what it does, and why it needs the exemption>"""
from __future__ import annotations

import os
from typing import Dict, List, Set, Tuple

import coupling_reasons
import trailers

TABLE = "test/resources/design-exemptions.tsv"
Key = Tuple[str, str]


def parse(text: str) -> Dict[Key, Tuple[str, str]]:
    out: Dict[Key, Tuple[str, str]] = {}
    for line in text.splitlines():
        if line.strip() and not line.startswith("#"):
            cells = (line.split("\t") + ["", "", "", ""])[:4]
            out[(cells[0], cells[1])] = (cells[2], cells[3])
    return out


def read(root: str, table: str = TABLE) -> Dict[Key, Tuple[str, str]]:
    path = os.path.join(root, table)
    if not os.path.isfile(path):
        return {}
    with open(path, encoding="utf-8") as f:
        return parse(f.read())


def keys(root: str, table: str = TABLE) -> Set[Key]:
    return set(read(root, table))


def row_problems(rows: Dict[Key, Tuple[str, str]]) -> List[str]:
    out: List[str] = []
    for (kind, key), (added, why) in sorted(rows.items()):
        if not coupling_reasons.ISO_DATE.match(added):
            out.append(f"{kind} {key}: its `added` column {added!r} is no YYYY-MM-DD date")
        name = key.rsplit(":", 1)[-1].rsplit("/", 1)[-1]
        out += [f"{kind} {key}: {p}" for p in coupling_reasons.reason_problems(name, why)]
    return out


def added_rows(kinds: Tuple[str, ...]):  # -> the `rose` reading trailers.unjustified_rises takes
    def rose(before: str, after: str) -> List[str]:
        old = parse(before)
        return [f"exemption {k} {key} added" for (k, key) in sorted(parse(after)) if k in kinds and (k, key) not in old]
    return rose


def problems(root: str, ranges: List[str], trailer_kinds: Dict[str, Tuple[str, ...]], table: str = TABLE) -> List[str]:
    out = row_problems(read(root, table))
    for rng in ranges:
        for trailer, kinds in trailer_kinds.items():
            if not added_rows(kinds)(trailers.text_at(rng.split("..")[0], table), trailers.text_at(rng.split("..")[1] or "HEAD", table)):
                continue
            out += [f"{u} (no `{trailer}:` trailer)" for u in trailers.unjustified_rises(rng, table, trailer, added_rows(kinds))]
    return out
