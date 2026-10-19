"""data/uk/roster.json: every UK venue, declared once.

Each row is one venue: the Scala name it is addressed by (`object`, a top-level
`models` val), the display name every per-cinema slot is stored under, the pill
name, its flicks.co.uk slug, and its PRIMARY source. A row with no `primary` is
scraped off Flicks; any other row names its own-site client and keeps the Flicks
slug (where Flicks lists it) as the aggregator fallback. `generate_roster.py`
turns it into `models.UkRosterData` (the venues) and
`services.cinemas.uk.UkSourcesData` (their scrapers and fallbacks).

The rows stand in GROUPS, each named after the town its venues were first wired
near, in the scrape catalog's order: that order is each page's scrape order, so
moving a group reorders the scrapers of every page it shares. `discover.py` wires
a new venue into the group of the venue nearest it.

Written one venue per line, so a diff names the venue it moves.
"""
from __future__ import annotations

import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
ROSTER = ROOT / "data/uk/roster.json"

# Each primary source and the arguments its row carries after the kind.
PRIMARY_ARGS = {
    "cineworld": ("slug",),
    "vue": ("id",),
    "showcase": ("id",),
    "everyman": ("id",),
    "odeon": ("id",),
    "savoy": ("dll",),
    "savoyBoxOffice": ("dll",),
    "spektrix": ("client", "building?"),
    "ticketsolve": ("client",),
    "quadDerby": (),
    "bfiSouthbank": (),
    "novaCinema": (),
    "theOldCourt": (),
}
# The chains whose own site is primary (`ChainFlicksFallback.ukChainSlugs`); every
# other own-site primary is an independent's box office.
CHAINS = frozenset({"cineworld", "vue", "showcase", "everyman", "odeon"})
# A primary whose client is built for one venue and names it itself (`QuadDerbyClient`).
FIXED_VENUE = {"quadDerby": "QuadDerby"}
KEYS = ("object", "displayName", "pillName", "flicks", "primary", "seasonal", "note")
IDENTIFIER = re.compile(r"[A-Z][A-Za-z0-9]*")


def load(path: Path = ROSTER) -> list[dict]:
    """The groups, in order: [{"group": name, "venues": [row, ...]}, ...]."""
    return json.loads(path.read_text(encoding="utf-8"))["groups"]


def venues(groups: list[dict]) -> list[dict]:
    """Every venue row, in scrape order."""
    return [v for g in groups for v in g["venues"]]


def dumps(groups: list[dict]) -> str:
    """roster.json's text: one venue per line, keys in a fixed order."""
    out = ["{", ' "groups": [']
    for gi, g in enumerate(groups):
        out.append(f'  {{"group": {json.dumps(g["group"], ensure_ascii=False)}, "venues": [')
        rows = g["venues"]
        for vi, v in enumerate(rows):
            row = {k: v[k] for k in KEYS if k in v}
            out.append("   " + json.dumps(row, ensure_ascii=False) + ("," if vi + 1 < len(rows) else ""))
        out.append("  ]}" + ("," if gi + 1 < len(groups) else ""))
    out += [" ]", "}"]
    return "\n".join(out) + "\n"


def dump(groups: list[dict], path: Path = ROSTER) -> None:
    path.write_text(dumps(groups), encoding="utf-8")


def problems(groups: list[dict]) -> list[str]:
    """What makes the roster unfit to generate from — empty when it is fine.

    The display name is the wire key every per-cinema slot is stored under and
    `Source.byDisplayName` a plain `toMap`, so two venues sharing one would read
    each other's showtimes; the object is the Scala name; a Flicks slug names one
    venue's page."""
    found, seen = [], {"object": {}, "displayName": {}, "flicks": {}}
    group_names = [g["group"] for g in groups]
    for name in sorted({n for n in group_names if group_names.count(n) > 1}):
        found.append(f"group {name!r} appears twice")
    for v in venues(groups):
        obj = v.get("object", "")
        unknown = sorted(set(v) - set(KEYS))
        if unknown:
            found.append(f"{obj}: unknown keys {unknown}")
        if not IDENTIFIER.fullmatch(obj):
            found.append(f"{obj!r}: not a Scala object name")
        if not v.get("displayName") or not v.get("pillName"):
            found.append(f"{obj}: no displayName or pillName")
        for key in seen:
            value = v.get(key)
            if value is None:
                continue
            if value in seen[key]:
                found.append(f"{obj}: {key} {value!r} is {seen[key][value]}'s too")
            seen[key][value] = obj
        primary = v.get("primary")
        if primary is None:
            if not v.get("flicks"):
                found.append(f"{obj}: scraped off Flicks but has no Flicks slug")
            continue
        kind, args = primary[0], primary[1:]
        if kind not in PRIMARY_ARGS:
            found.append(f"{obj}: unknown primary {kind!r}")
            continue
        if kind in FIXED_VENUE and obj != FIXED_VENUE[kind]:
            found.append(f"{obj}: {kind}'s client reads {FIXED_VENUE[kind]} and no other venue")
        spec = PRIMARY_ARGS[kind]
        required = [a for a in spec if not a.endswith("?")]
        if not (len(required) <= len(args) <= len(spec)) or not all(isinstance(a, str) and a for a in args):
            found.append(f"{obj}: {kind} takes {', '.join(spec) or 'nothing'}, not {args}")
    return found
