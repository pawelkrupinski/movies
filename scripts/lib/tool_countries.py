"""THE COUNTRIES every tool loops over, in one order, and the country flag (scripts/lib; tool-conventions.sh's
KINOWO_COUNTRIES and country_flag, identity-tools.sh's identity_country_code / _name).

  CODES                        ("pl", "de", "uk", "us", "es")
  code("germany") -> "de"      a country's code from its name (a job's `(germany)`); anything else passes through
  name("de") -> "germany"      and back
  selected(values) -> codes    the codes `--country cc[,cc]` / `--countries a,b` named, in order, lower-cased, a repeat
                               dropped; one unknown, or a value naming none, a UsageError"""
from __future__ import annotations

import re
from typing import List, Sequence

from tool_cli import UsageError

CODES = ("pl", "de", "uk", "us", "es")
NAMES = {"pl": "poland", "de": "germany", "uk": "united-kingdom", "es": "spain", "us": "united-states"}
CODE_OF = {name: cc for cc, name in NAMES.items()}


def code(country: str) -> str:
    return CODE_OF.get(country, country)


def name(cc: str) -> str:
    return NAMES.get(cc, cc)


def selected(flag: str, values: Sequence[str], before: Sequence[str] = ()) -> List[str]:
    """The codes named so far (`before`) and those `flag`'s value names."""
    chosen = list(before)
    for value in values:
        chosen += named(flag, value)
    return list(dict.fromkeys(chosen))


def named(flag: str, value: str) -> List[str]:
    if not value or value.startswith("-"):
        raise UsageError(f"{flag} needs a country code (one of {','.join(CODES)})")
    codes = [cc for cc in re.split(r"[, ]", value.lower()) if cc]
    if not codes:
        raise UsageError(f"{flag} names no country code (one of {','.join(CODES)})")
    unknown = [cc for cc in codes if cc not in CODES]
    if unknown:
        raise UsageError(f"unknown country {unknown[0]} (one of {','.join(CODES)})")
    return codes
