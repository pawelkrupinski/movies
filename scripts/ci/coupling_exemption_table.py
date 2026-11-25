"""The coupling exemption table as the ratchet reads it (coupling_ratchet.py, its module docstring;
project/Coupling.scala's Exemptions.parse judges the table's shape): its rows, how it loosened between two revisions,
and the fan-out rows an agent added."""
from __future__ import annotations

from typing import Dict, List, Tuple

FAN_OUT_KINDS = ("root", "registry", "config", "class")  # project/Coupling.scala's Exemptions.FanOutKinds
# The rows held to AN AGENT'S ROW ADDED: every fan-out kind, and `package` (one package's distance, by its role).
ADMITTED_KINDS = FAN_OUT_KINDS + ("package",)

ExemptionRows = Dict[Tuple[str, str], str]


def parse_exemptions(text: str) -> ExemptionRows:
    """The exemption table's rows, (kind, pattern) -> its value (project/Coupling.scala's Exemptions.parse judges the
    table's shape, an unknown kind refused there; this reads what loosening compares)."""
    rows: ExemptionRows = {}
    for line in text.splitlines():
        cols = line.split("\t")
        if line.strip() and not line.startswith("#") and len(cols) >= 3:
            rows[(cols[0], cols[1])] = cols[2]
    return rows


def exemption_loosening(old: ExemptionRows, new: ExemptionRows) -> List[str]:
    """How the exemption table loosened from `old` to `new`: a row added (a class or package let past a limit), or a
    row's limit changed. A row dropped tightens."""
    out: List[str] = []
    for key, value in sorted(new.items()):
        if key not in old:
            out.append(f"{key[0]} {key[1]} ({value}) added")
        elif old[key] != value:
            out.append(f"{key[0]} {key[1]} changed {old[key]} -> {value}")
    return out


def exemption_columns(text: str) -> Dict[Tuple[str, str], List[str]]:
    """The exemption table's admitted rows (fan-out and package), (kind, pattern) -> all its columns."""
    return {(cols[0], cols[1]): cols for cols in (line.split("\t") for line in text.splitlines()
                                                  if line.strip() and not line.startswith("#"))
            if len(cols) >= 5 and cols[0] in ADMITTED_KINDS}
