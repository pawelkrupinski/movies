"""THE RATCHET'S FINDINGS as scripts/identity_ratchet_report.py reads and prints them: one row a take, the columns of
the ratchet's ratchet-takes.tsv (scripts.UnmatchedClustersRatchet), read from that file or, for a run from before the
artifact existed, from the assertion messages its pull-request job's log prints; written back as that TSV, or as the
table a person reads, a status a section.

  read_tsv(path) / write_tsv(rows, out)   the artifact's TSV (identity_takes.table, the one reader of the take files)
  parse_log(text)                         the findings a job log's WRONG / UNJUDGED / right takes LOST sections print
  table(rows, explain, out)               the table, in ORDER, each take's explanation under it with `explain`"""
from __future__ import annotations

import os
import re
import sys
from typing import Callable, Dict, List, Mapping, Optional, TextIO

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import identity_takes  # noqa: E402

Row = Dict[str, str]
COLUMNS = ["status", "country", "venue", "rawTitle", "film", "title", "basis", "ids", "was", "explanation"]
ORDER = ["WRONG", "UNJUDGED", "MOVED", "LOST", "NEW", "RIGHT"]
ANSI = re.compile(r"(\x1b|\^\[)\[[0-9;]*m")
SECTIONS = {"WRONG takes:": "WRONG", "UNJUDGED takes:": "UNJUDGED", "right takes LOST:": "LOST"}
LOST = re.compile(r"\(([a-z]{2}),(.*),([a-z]+:[^,]+)\)\s*")
COUNTRY = re.compile(r"[a-z]{2}")
# the table's columns: (column, width) of each cut cell after the country
WIDTHS = (("venue", 34), ("rawTitle", 46), ("film", 30), ("title", 40))
TAKE_TABLE: Callable[[str], List[Mapping[str, str]]] = getattr(identity_takes, "table")


def read_tsv(path: str) -> List[Row]:
    return [{c: (r.get(c) or "") for c in COLUMNS} for r in TAKE_TABLE(path)]


def write_tsv(rows: List[Row], out: TextIO) -> None:
    out.write("\t".join(COLUMNS) + "\n")
    for r in rows:
        out.write("\t".join(r[c] for c in COLUMNS) + "\n")


def row(*cells: str) -> Row:
    return dict(zip(COLUMNS, cells))


# ── a job log's findings ────────────────────────────────────────────────────────────────────────────────────────────

def parse_log(text: str) -> List[Row]:
    """The findings the ratchet's assertion messages print in a job log: sbt's ANSI colours, the log's job/step/time
    prefix and sbt's [info] tag stripped. A section ends at its first line that is no take."""
    rows: List[Row] = []
    section: Optional[str] = None
    for raw in text.splitlines():
        body = log_body(raw)
        if body.strip() in SECTIONS:
            section = SECTIONS[body.strip()]
            continue
        found = section_row(section, body)
        if found is None:
            section = None
        else:
            rows.append(found)
    return rows


def section_row(section: Optional[str], body: str) -> Optional[Row]:
    """The take a section's line holds, or None: no section, or a line that is no take, which ends the section."""
    if section is None:
        return None
    return lost_row(body) if section == "LOST" else take_row(section, body)


def log_body(raw: str) -> str:
    line = ANSI.sub("", raw)
    line = re.sub(r"^[^\t]*\t[^\t]*\t\S+Z ", "", line)  # job, step, timestamp
    line = re.sub(r"^\[info\] ?", "", line)
    return line[2:] if line.startswith("  ") else line


def lost_row(body: str) -> Optional[Row]:
    """`(cc,venue,raw title,film)`: the venue the text before the first comma, the title the rest."""
    m = LOST.fullmatch(body)
    if not m:
        return None
    venue, _, raw_title = m.group(2).partition(",")
    return row("LOST", m.group(1), venue, raw_title, "", "", "", "", m.group(3), "")


def take_row(section: str, body: str) -> Optional[Row]:
    """`cc venue rawTitle film title [basis ids]`, tab-separated: a WRONG take's first five cells, an UNJUDGED one's
    seven."""
    cells = body.rstrip().split("\t")
    if not COUNTRY.fullmatch(cells[0]) or len(cells) < 4:
        return None
    cells += [""] * 7
    if section == "WRONG":
        return row("WRONG", *cells[:5], "", "", "", "")
    return row("UNJUDGED", *cells[:5], cells[5], cells[6], "", "")


# ── the table ───────────────────────────────────────────────────────────────────────────────────────────────────────

def table(rows: List[Row], explain: bool, out: TextIO) -> None:
    for status in ORDER:
        group = [r for r in rows if r["status"] == status]
        if group:
            out.write(f"\n{status} ({len(group)})\n")
            out.writelines(line for r in group for line in take_lines(r, explain))


def take_lines(r: Row, explain: bool) -> List[str]:
    """The take's line, and with `explain` each part of its explanation under it."""
    parts = r["explanation"].split(" ¶ ") if explain and r["explanation"] else []
    return [take_line(r)] + [f"        {part}\n" for part in parts]


def take_line(r: Row) -> str:
    cells = {**r, "film": r["film"] + (f" (was {r['was']})" if r["was"] else "")}
    cut = "  ".join(f"{fit(cells[column], width):<{width}}" for column, width in WIDTHS)
    return f"  {r['country']}  {cut}  {r['basis']}\n"


def fit(text: str, width: int) -> str:
    return text if len(text) <= width else text[: width - 1] + "…"
