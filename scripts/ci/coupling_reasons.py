"""When an exemption's why is no SPECIFIC reason (coupling_ratchet.py, AN AGENT'S ROW ADDED; the exemption table's
header): it leads with the class's role, says 40+ characters and 6+ words besides the class's name, and no generic
phrase. A leaf of the coupling ratchet: quality_ratchet.py lints its own exemptions with the same patterns."""
from __future__ import annotations

import re
from typing import List

REASON_MIN_CHARS, REASON_MIN_WORDS, ROLE_MAX_WORDS = 40, 6, 5
# What says nothing of the class (the owner, 2026-11-19: "not a generic 'complex' or 'needed'"): each a pattern over the
# lowercased why, and the phrase to name.
GENERIC_REASONS = (
    (r"\bcomplex(ity)?\b", "complex"),
    (r"(^|\b(is|are|it's|they're|all|were|was|be)\s+)(needed|necessary|required)\b", "needed"),
    (r"\bfor now\b", "for now"),
    (r"\btodo\b", "TODO"),
    (r"\btemporar(y|ily)\b", "temporary"),
    (r"\b(fix|split|cut|clean|refactor)\w*\s+(\w+\s+)?later\b", "later"),
    (r"\bhard to (split|cut|refactor|break)\b", "hard to split"),
    (r"\b(can'?t|cannot|could not) be (split|cut)\b", "cannot be split"),
    (r"\btoo (many|big|large)\b", "too many"),
    (r"\blegacy\b", "legacy"),
    (r"\b(big|large|god) class\b", "big class"),
    (r"\buses (a lot|lots|many things)\b", "uses a lot"),
)
EXEMPT_TRAILER = re.compile(r"exempt\s+(\S+)\s*(?:—|–|--|-|:)\s*(.*)$")
ISO_DATE = re.compile(r"\d{4}-\d{2}-\d{2}$")
REASON_SHAPE = ("`<role>: <what it does, and why it needs these collaborators>`, e.g. `pipeline stage: clusters a family's"
                " members; needs the evidence, edges and solver it sequences`")


def reason_problems(pattern: str, why: str) -> List[str]:
    """Why `why` is no SPECIFIC reason for exempting `pattern` (AN AGENT'S ROW ADDED, the exemption table's header): it
    leads with the class's role, says 40+ characters and 6+ words besides the class's name, and no generic phrase."""
    text = why.strip()
    out: List[str] = []
    role, sep, _ = text.partition(":")
    if not sep or not role.strip() or len(role.split()) > ROLE_MAX_WORDS:
        out.append(f"lead with the class's role: {REASON_SHAPE}")
    rest = text
    for name in sorted({pattern, pattern.rsplit(".", 1)[-1]}, key=len, reverse=True):
        rest = rest.replace(name, " ")
    if not re.sub(r"[\W_]+", "", rest):
        out.append("the class's name is the whole reason")
    elif len(rest.strip()) < REASON_MIN_CHARS or len(rest.split()) < REASON_MIN_WORDS:
        out.append(f"too short: {REASON_MIN_CHARS}+ characters and {REASON_MIN_WORDS}+ words besides the class's name")
    lowered = text.lower()
    out += [f"`{phrase}` is generic: say what the class does and which collaborators it needs for it"
            for rx, phrase in GENERIC_REASONS if re.search(rx, lowered)]
    return out
