"""Which harvested German venues never join the roster, by name.

Shared by harvest_kinoheld.py and sweep_filmstarts_ids.py so both sources drop
the same screens: open-air / drive-in / seasonal screens (they list a summer,
not a programme, and never match a regular cinema), and sex cinemas and booth
venues, which list no films a visitor is looking for.
"""

import re

OPEN_AIR = re.compile(
    r"open[\s-]?air|freiluft|sommerkino|sommernachtskino|autokino|drive[\s-]?in|"
    r"strandkino|kino\s+am\s+strand|biergarten|kinosommer|filmn[aä]chte|parkkino|"
    r"mondscheinkino|sternenkino|kino\s+unter\s+(den\s+)?sternen|hofkino",
    re.I)

SEX = re.compile(
    r"\bsex|erotik|\bero\b|porno|blue\s*movie|beate\s*uhse|orion\b|kabinen|"
    r"videokabine|lovers?\s*(shop|kino)|\bxxx\b|adult|fkk|swinger|gay\s*kino",
    re.I)


def excluded_reason(name, is_open_air=False, is_drive_in=False):
    """Why a venue named `name` stays out of the roster, or None when it may join."""
    if is_open_air or is_drive_in or OPEN_AIR.search(name or ""):
        return "open-air/drive-in"
    if SEX.search(name or ""):
        return "sex cinema"
    return None
