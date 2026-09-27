"""The venues a country has retired, shared by every roster generator.

`data/<country>/retired.json` maps a venue's upstream id (Filmstarts/SensaCine
theaterId, flicks.us slug) to why it left the roster:

    {"A1451": {"name": "...", "reason": "closed" | "duplicate" | "feedless",
               "retiredOn": "YYYY-MM-DD", "evidence": "..."}}

The generators DROP these ids rather than relying on them being deleted from the
harvest, so a re-harvest cannot bring a closed venue back. `CountrySpec` reads the
same files and fails if a retired id is rostered anyway.
"""
import json
import pathlib

REASONS = {"closed", "duplicate", "feedless"}
FIELDS = {"name", "reason", "retiredOn", "evidence"}


def load(country_dir: pathlib.Path) -> dict[str, dict]:
    path = country_dir / "retired.json"
    if not path.exists():
        return {}
    retired = json.loads(path.read_text())
    for venue_id, entry in retired.items():
        if set(entry) != FIELDS or entry["reason"] not in REASONS:
            raise SystemExit(f"ERROR: {path}: {venue_id!r} needs exactly {sorted(FIELDS)} "
                             f"with reason in {sorted(REASONS)}, got {entry}")
    return retired
