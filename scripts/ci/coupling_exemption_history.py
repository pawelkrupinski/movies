"""The exemption rows commits added, and what admits them (coupling_ratchet.py, AN AGENT'S ROW ADDED): a specific why,
the `added` date and the commit's own `Coupling: exempt <Class> — <reason>` trailer; and the weekly list of the rows
added since a day (`new-exemptions`) the owner strikes from."""
from __future__ import annotations

import re
import subprocess
import os
from typing import Callable, Dict, List, Optional, Tuple

import trailers
from coupling_exemption_table import exemption_columns, exemption_loosening, parse_exemptions
from coupling_reasons import EXEMPT_TRAILER, ISO_DATE, REASON_SHAPE, reason_problems
from coupling_rows import EXEMPTIONS_PATH
from trailers import text_at as file_at
import tool_git

# the ratchet's `rise_refused(rng, path, header, rises, rose)`: whether a commit of the range makes a rise without its trailer
RiseRefused = Callable[[str, str, str, List[str], Callable[[str, str], List[str]]], bool]


def coupling_trailers(sha: str) -> List[str]:
    """The `Coupling:` trailers of `sha` as git reads them (the final trailer block)."""
    out = tool_git.Git.here().text("log", "-1", "--format=%(trailers:key=Coupling,valueonly,unfold,separator=%x1e)", sha)
    return [v.strip() for v in out.split("\x1e") if v.strip()]


def exempt_trailer(pattern: str, values: List[str]) -> Optional[str]:
    """The reason of `sha`'s `Coupling: exempt <pattern> — <reason>` trailer (its full or simple name), None for none."""
    names = {pattern, pattern.rsplit(".", 1)[-1]}
    found = [m.group(2) for m in (EXEMPT_TRAILER.match(v) for v in values) if m and m.group(1) in names]
    return found[0] if found else None


def rows_added(sha: str) -> List[List[str]]:
    """The fan-out exemption rows commit `sha` adds (a commit creating the table adds none: no step)."""
    before, after = trailers.text_at(f"{sha}^", EXEMPTIONS_PATH), trailers.text_at(sha, EXEMPTIONS_PATH)
    if not before or not after:
        return []
    old = exemption_columns(before)
    return [cols for key, cols in sorted(exemption_columns(after).items()) if key not in old]


def row_problems(where: str, cols: List[str], values: List[str]) -> List[str]:
    """Why one added row of a commit (its columns, the commit's `Coupling:` trailers) is not admitted."""
    pattern, why = cols[1], cols[4]
    out = [f"{where}: its why ({why!r}): {p}" for p in reason_problems(pattern, why)]
    if len(cols) < 6 or not ISO_DATE.match(cols[5]):
        out.append(f"{where}: no `added` date (the sixth column, YYYY-MM-DD: the day it is added)")
    reason = exempt_trailer(pattern, values)
    if reason is None:
        out.append(f"{where}: no `Coupling: exempt {pattern.rsplit('.', 1)[-1]} — <reason>` trailer in the commit's"
                   " final trailer block")
    else:
        out += [f"{where}: its trailer's reason ({reason!r}): {p}" for p in reason_problems(pattern, reason)]
    return out


def unadmitted_exemptions(rng: str) -> List[str]:
    """Each fan-out or package exemption row a commit of `rng` adds without what admits it (AN AGENT'S ROW ADDED): a specific why,
    its `added` date, and the commit's own `Coupling: exempt <Class> — <reason>` trailer."""
    out: List[str] = []
    for sha in tool_git.Git.here().text("rev-list", "--reverse", rng, "--", EXEMPTIONS_PATH).split():
        added = rows_added(sha)
        if not added:
            continue
        values = coupling_trailers(sha)
        subject = tool_git.Git.here().text("log", "-1", "--format=%s", sha).strip()
        for cols in added:
            out += row_problems(f"{sha[:9]} {subject}: {cols[0]} {cols[1]}", cols, values)
    return out


def print_added(sha: str, present: Dict[Tuple[str, str], List[str]]) -> int:
    """One commit's added exemption rows, each with its date, reason and trailer; how many there were."""
    values = coupling_trailers(sha)
    day, subject = tool_git.Git.here().text("log", "-1", "--format=%cs%x1f%s", sha).strip().split("\x1f", 1)
    found = 0
    for cols in rows_added(sha):
        found += 1
        kind, pattern, why = cols[0], cols[1], cols[4]
        added = cols[5] if len(cols) > 5 else day
        struck = "" if (kind, pattern) in present else " (struck since)"
        print(f"- {added} {sha[:9]} `{kind}` **{pattern}**{struck} ({cols[2]}, {cols[3]}): {why}")
        print(f"  commit: {subject}")
        reason = exempt_trailer(pattern, values)
        print(f"  trailer: Coupling: exempt {pattern.rsplit('.', 1)[-1]} — {reason}" if reason is not None else "  trailer: none")
    return found


def new_exemptions(since: str, rev: str = "") -> int:
    """The exemption rows added since `since` (`7d`, or anything `git log --since` reads) on `rev` (default origin/main,
    else HEAD): each one's date, commit, reason and trailer, and whether it is still in the table -- the weekly list the
    owner strikes from (`scripts/coupling-report --new-exemptions --since 7d`)."""
    if not rev:
        rev = "origin/main" if subprocess.run(["git", "rev-parse", "-q", "--verify", "origin/main"],
                                              capture_output=True).returncode == 0 else "HEAD"
    days = re.fullmatch(r"(\d+)d", since)
    after = f"{days.group(1)} days ago" if days else since
    shas = tool_git.Git.here().text("log", "--reverse", f"--since={after}", "--format=%H", rev, "--", EXEMPTIONS_PATH).split()
    present = exemption_columns(file_at(rev, EXEMPTIONS_PATH))
    print(f"# Coupling exemptions added since {since} (on {rev}, {EXEMPTIONS_PATH})\n")
    found = sum(print_added(sha, present) for sha in shas)
    if not found:
        print("No exemption rows added.")
    print(f"\nTo strike one: drop its row from {EXEMPTIONS_PATH} (no trailer needed) and cut the class below the limit.")
    return 0


def loosening_refused(rise_refused: RiseRefused, rng: str, exemptions_path: str) -> bool:
    """Whether a commit of `rng` loosens the exemption table without its own `Coupling:` trailer."""
    base = rng.split("..")[0]
    loose = exemption_loosening(parse_exemptions(file_at(base, EXEMPTIONS_PATH)), parse_exemptions(open(exemptions_path).read()))
    return rise_refused(rng, EXEMPTIONS_PATH, f"THE EXEMPTION TABLE LOOSENED in {rng} ({exemptions_path}) with no"
                        " `Coupling: <why>` trailer on the commit changing it:", loose,
                        lambda a, b: exemption_loosening(parse_exemptions(a), parse_exemptions(b)))


def unadmitted_refused(rng: str, exemptions_path: str) -> bool:
    """Whether a commit of `rng` adds an exemption row that nothing admits."""
    unadmitted = unadmitted_exemptions(rng)
    if unadmitted:
        print(f"AN EXEMPTION ROW IS NOT ADMITTED in {rng} ({exemptions_path}): a row added carries a specific why, its"
              " `added` date and the commit's own `Coupling: exempt <Class> — <reason>` trailer:")
        for item in unadmitted:
            print(f"  - {item}")
        print(f"  The why: {REASON_SHAPE}.")
        trailers.print_misplaced(rng, "Coupling", EXEMPTIONS_PATH)
    return bool(unadmitted)


def check_exemptions(rise_refused: RiseRefused, ranges: List[str], exemptions_path: str) -> bool:
    """The exemption table: a loosening a commit makes without its trailer, a row added that
    nothing admits; whether any is refused."""
    failed = False
    if os.path.exists(exemptions_path):
        for rng in ranges:
            failed |= loosening_refused(rise_refused, rng, exemptions_path)
    for rng in ranges:
        failed |= unadmitted_refused(rng, exemptions_path)
    return failed
