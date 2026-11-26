"""Which types the coupling measure calls DATA and which COLLABORATORS, as the ratchet holds them (coupling_ratchet.py,
its module docstring, THE CLASSIFICATION): the checked-in verdicts (test/resources/coupling-collaborators.tsv, one
`<type><TAB>data|collaborator` row a top-level type), the verdicts measured (couplingReport's collaborators, folded
from its named types to their top-level types), and the flips between them. A type moving INTO the data bucket drops out
of every fan-out and abstractness count, so it is a loosening that needs its `Coupling: <why>` trailer."""
from __future__ import annotations

from typing import Dict, List, Tuple

VERDICTS_PATH = "test/resources/coupling-collaborators.tsv"
DATA, COLLABORATOR = "data", "collaborator"
HEADER = ("# THE CLASSIFICATION (scripts/coupling-report --update writes it; scripts/ci/coupling_ratchet.py holds every push to it):\n"
          "# each top-level production type, `data` (no coupling: it counts in no fan-out or abstractness) or `collaborator`.\n"
          "# A type that was a collaborator and is data now lands only in a commit carrying `Coupling: <why it is data>`.\n"
          "# A type absent here is new: it is no flip. A stale row (the file not updated since a type changed) is no failure.\n")

Verdicts = Dict[str, str]


def parse_verdicts(text: str) -> Verdicts:
    """The verdict rows of the checked-in file (`#` lines and blank lines are comments)."""
    out: Verdicts = {}
    for line in text.splitlines():
        cols = line.split("\t")
        if len(cols) == 2 and not line.startswith("#") and cols[1] in (DATA, COLLABORATOR):
            out[cols[0]] = cols[1]
    return out


def top_level(unit: str, types: Dict[str, bool]) -> str:
    """The top-level type a named type (`pkg.Outer.Inner`, an object's `pkg.Outer$`) belongs to: its name, less a
    trailing `$`, cut at the last `.` until it is one of `types`; the name itself when none is."""
    name = unit.rstrip("$")
    while name not in types and "." in name:
        name = name.rsplit(".", 1)[0]
    return name if name in types else unit.rstrip("$")


def measured_verdicts(collaborators_text: str, types: List[str]) -> Verdicts:
    """Each top-level production type's verdict from couplingReport's collaborators (`<named type><TAB>data|collaborator
    [<TAB>why]` a line) and fanouts (`types`: every top-level type): a collaborator when any of its named types is."""
    known = {t: False for t in types}
    for line in collaborators_text.splitlines():
        cols = line.split("\t")
        if len(cols) >= 2 and cols[1] == COLLABORATOR:
            top = top_level(cols[0], known)
            if top in known:
                known[top] = True
    return {t: COLLABORATOR if known[t] else DATA for t in sorted(known)}


def flips(before: Verdicts, after: Verdicts) -> Tuple[List[str], List[str]]:
    """The types whose verdict changed between two verdict sets (a type in only one is new or gone, no flip): the ones
    that became DATA (the guarded direction) and the ones that became collaborators."""
    to_data = [t for t, v in sorted(after.items()) if v == DATA and before.get(t) == COLLABORATOR]
    to_collaborator = [t for t, v in sorted(after.items()) if v == COLLABORATOR and before.get(t) == DATA]
    return to_data, to_collaborator


def verdict_file(measured: Verdicts) -> str:
    return HEADER + "".join(f"{t}\t{v}\n" for t, v in sorted(measured.items()))
