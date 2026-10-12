"""Which rostered venues are SEASONAL, shared by every roster generator.

An open-air screen, a summer terrace or a drive-in shows films for part of the year
and is dark for the rest. Its winter silence is not a broken scraper or a closure,
and the worker needs to know that: off its season it is left out of the dead-venue
pages, the content census and the closure sweep (`models.Opening`). Its window is
the country's typical one (`models.SeasonWindow`), not the venue's own: no feed says
when a venue opens.

A venue is seasonal when its NAME says so, in its country's words (`PATTERNS`),
unless it is listed in `NOT_SEASONAL`: a name that matches but a venue that shows
films all year. Each of those is a judgement on evidence, so each carries it.

The generators emit the matches as `seasonal` beside their pages; `models.*Roster`
gives those venues `Opening.Seasonal`. Poland's and the UK's hand-declared venues are
marked in `Cinema.scala` instead.
"""
import re

# Each country's words for a screen that only runs in the warm months.
PATTERNS: dict[str, re.Pattern] = {
    "de": re.compile(
        r"open[\s-]?air|freiluft|sommer[\s-]?(nachts?[\s-]?)?(kino|film|open)|sommernacht|kinosommer"
        r"|autokino|\bdrive[\s-]?in\b|filmn(ä|ae)chte|strand(korb)?kino|sternenhimmel|mondscheinkino"
        r"|freilichtbühne",
        re.IGNORECASE),
    "es": re.compile(
        r"\bauto[\s-]?cine(ma)?\b|\bterraza\b|cine de verano|terraza de verano|\bd'estiu\b|a la fresca"
        r"|aire libre|open[\s-]?air|\bdrive[\s-]?in\b",
        re.IGNORECASE),
    "us": re.compile(r"\bdrive[\s-]?ins?\b|\boutdoor theat(re|er)\b", re.IGNORECASE),
}

# Names that match a pattern but whose venue screens all year, with the evidence.
# Reviewed 2026-10-12 against production (read-only): a venue whose current listing
# runs past its country's season (into October for DE/ES, November for the US) is
# open off-season, and a name that only borrows a drive-in's is an indoor cinema.
_DE_LISTS_INTO_NOVEMBER = "listing on 2026-10-12 runs into November, past the May–September season"
_US_LISTS_INTO_WINTER = "listing on 2026-10-12 runs past October, into the winter"
NOT_SEASONAL: dict[str, dict[str, str]] = {
    "de": {
        "Drive In Autokino Aschheim": _DE_LISTS_INTO_NOVEMBER,
        "Drive In Autokino Essen": _DE_LISTS_INTO_NOVEMBER,
        "Drive In Autokino Gravenbruch": _DE_LISTS_INTO_NOVEMBER,
        "Drive In Autokino Kornwestheim": _DE_LISTS_INTO_NOVEMBER,
        "Kino unterm Sternenhimmel Alpirsbach": _DE_LISTS_INTO_NOVEMBER + "; an indoor cinema of that name",
        "Campusfilmnächte auf dem Sportplatz": "listing on 2026-10-12 runs to 2026-10-30: a term-time programme",
    },
    "es": {
        "Auto Cine Drive In": "listing on 2026-10-12 runs past the June–September season",
        "Autocine El Sur": "listing on 2026-10-12 runs to 2026-10-25, past the June–September season",
        "Autocine Star": "listing on 2026-10-12 runs to 2026-10-29, past the June–September season",
    },
    "us": {
        "61 Drive In Theatre": _US_LISTS_INTO_WINTER,
        "Nite Owl Drive-In": "listing on 2026-10-12 is a December–January programme",
        "Quasar Drive-In Fremont": _US_LISTS_INTO_WINTER,
        "Skyline Drive-In Shelbyville": _US_LISTS_INTO_WINTER,
        "Stars & Stripes Drive In New Braunfels 4": _US_LISTS_INTO_WINTER,
        "Stars & Stripes Drive-in Lubbock": _US_LISTS_INTO_WINTER,
        "Triangle Drive In Raleigh": _US_LISTS_INTO_WINTER,
        "VIP Star-Lite Drive-In": _US_LISTS_INTO_WINTER,
        "West Wind El Rancho Reno Drive-In": _US_LISTS_INTO_WINTER,
        "West Wind Glendale Drive-In": _US_LISTS_INTO_WINTER,
        "West Wind Las Vegas Drive-In": _US_LISTS_INTO_WINTER,
        "West Wind Sacramento Drive-In": _US_LISTS_INTO_WINTER,
        "West Wind Solano Drive-In": _US_LISTS_INTO_WINTER,
        "Wellfleet Cinemas & Drive-In": "indoor screens and a drive-in under one listing, with daytime shows",
    },
}


def is_seasonal(country: str, name: str) -> bool:
    """Is the venue called `name` in `country` ("de", "es", "us") seasonal?"""
    return bool(PATTERNS[country].search(name)) and name not in NOT_SEASONAL[country]


def seasonal_names(country: str, names) -> list[str]:
    """The seasonal ones among `names`, sorted — what a generator emits."""
    return sorted({name for name in names if is_seasonal(country, name)})


def render_scala(names: list[str], scala_string) -> list[str]:
    """The generated `seasonal` member: the display names `models.Opening.Seasonal` is given to."""
    lines = [
        "  /** The venues that screen only in season — open-air, summer and drive-in screens, by name",
        "   *  (data/scripts/seasonal_venues.py): their display names as generated above. */",
        "  val seasonal: Set[String] = Set(",
    ]
    lines.extend(f"    {scala_string(name)}," for name in names)
    lines.append("  )")
    return lines
