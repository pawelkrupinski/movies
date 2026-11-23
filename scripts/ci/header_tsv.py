"""A TSV WITH A HEADER, the one way the CI tools read and write one (scripts/ci; the sparse checkouts that run them
check out this directory): its rows as dicts by the header -- `#` and blank lines aside, no quoting (a raw title's `"`
is a character, never a CSV quote), a short row padded with "" -- and each cell written on one line.

  table(lines)                (header, rows) of these lines (the first kept line the header)
  rows(lines)                 the rows alone
  read(path, missing_ok)      the rows of a file ([] when it is absent and that is fine)
  write(path, columns, rows)  the header, then each row's cells, a tab or newline in one a space"""
from __future__ import annotations

import csv
import os
from typing import Dict, Iterable, List, Mapping, Sequence, Tuple


def table(lines: Iterable[str]) -> Tuple[List[str], List[Dict[str, str]]]:
    """The header ([] when there is none) and the rows."""
    kept = [line for line in lines if line.strip() and not line.startswith("#")]
    reader = csv.reader(kept, delimiter="\t", quoting=csv.QUOTE_NONE)
    header = next(reader, None) or []
    return header, [dict(zip(header, cols + [""] * (len(header) - len(cols)))) for cols in reader]


def rows(lines: Iterable[str]) -> List[Dict[str, str]]:
    return table(lines)[1]


def read(path: str, missing_ok: bool = False) -> List[Dict[str, str]]:
    if missing_ok and not os.path.exists(path):
        return []
    with open(path, newline="", encoding="utf-8") as f:
        return rows(f)


def cell(value: object) -> str:
    return str(value).replace("\t", " ").replace("\n", " ")


def write(path: str, columns: Sequence[str], table: Iterable[Mapping[str, object]]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write("\t".join(columns) + "\n")
        for row in table:
            f.write("\t".join(cell(row.get(c, "")) for c in columns) + "\n")
