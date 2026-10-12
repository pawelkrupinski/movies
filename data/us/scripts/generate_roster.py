#!/usr/bin/env python3
"""Generate common/src/main/scala/models/UsRosterData.scala from pages.json.

The JSON -> Scala step of the US roster, the counterpart of
data/germany/scripts/generate_roster.py. Reads the PAGES (`pages.json`, built by
`build_pages.py`: which town or cluster page each venue is on, and where each page
that stopped existing now redirects) and the venues (`venues.json` less
`retired.json`), and emits the flat tuple data `models.UsRoster` materialises into
City/Cinema objects.

Each page keeps its clock in the zone most of its venues are in, resolved from
their coordinates — the US spans six zones and fifteen states straddle a
boundary, so a state's zone put Knoxville on Central time. A venue on another
clock than its page's majority carries its own (`UsRosterData.venueZones`), which
`VenueClock` reads before its page's.

Things it refuses to do, because each fails SILENTLY downstream:

  * emit a duplicate `displayName` — that string is the wire key every stored
    showtime is filed under, and `Source.byDisplayName` is a plain `toMap`, so
    two venues sharing one quietly become one venue. A repeated Flicks title is
    qualified with its town, then its state;
  * emit a venue on no page, or on two, or a page listing a venue the roster does
    not have — a venue on no page is never scraped, and nothing else says so.

It needs `timezonefinder` (`python3 -m pip install timezonefinder`), only ever at
regeneration time — the roster is checked in.

Usage:  python3 data/us/scripts/generate_roster.py
"""
import json
import pathlib
import re
import sys
from collections import Counter, defaultdict
from datetime import datetime
from functools import lru_cache
from zoneinfo import ZoneInfo

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import build_pages  # noqa: E402
import seasonal_venues  # noqa: E402
from town_pages import placement_problems  # noqa: E402

ROOT = build_pages.ROOT
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "UsRosterData.scala"

CHUNK = 40

#: The two dates a timezone is compared on. One is not enough: Phoenix and Denver
#: share an offset in January and differ in July, because Arizona does not keep
#: DST — a single-date probe would call them the same zone.
TZ_PROBE_DATES = (datetime(2026, 1, 15, 12), datetime(2026, 7, 15, 12))


def scala_str(s: str) -> str:
    """A Scala double-quoted literal."""
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"').replace('$', '$$') + '"'


def clean(s):
    return re.sub(r'\s+', ' ', (s or '')).strip()


def ident(slug: str) -> str:
    return "p_" + re.sub(r"[^a-z0-9]", "_", slug)


@lru_cache(maxsize=1)
def _timezone_finder():
    """The lat/lon -> IANA zone lookup, imported lazily with an actionable error."""
    try:
        from timezonefinder import TimezoneFinder
    except ImportError:
        raise SystemExit("timezonefinder is not installed — `python3 -m pip install timezonefinder` "
                         "(needed only to regenerate the roster; see data/us/README.md)")
    return TimezoneFinder()


@lru_cache(maxsize=None)
def _offsets(zone):
    """A zone's UTC offsets on [[TZ_PROBE_DATES]] — what a CLOCK does, which is
    the only part of a zone name that reaches a reader. `America/Indiana/Indianapolis`
    and `America/New_York` are two names for one clock; a majority over names would
    split a page that has no disagreement in it."""
    tz = ZoneInfo(zone)
    return tuple(tz.utcoffset(d).total_seconds() for d in TZ_PROBE_DATES)


def zone_for(venues):
    """The IANA zone a group of venues keeps its clock in: the majority by OFFSET,
    then by name within the winning offset, ties alphabetical — a pure function of
    the input."""
    finder = _timezone_finder()
    zones = [z for z in (finder.timezone_at(lat=v['lat'], lng=v['lon']) for v in venues) if z]
    if not zones:
        raise SystemExit(f"no timezone resolves for any of {len(venues)} venues at "
                         f"{[(v['lat'], v['lon']) for v in venues][:3]}")
    by_offset = Counter(_offsets(z) for z in zones)
    winner = min(by_offset, key=lambda o: (-by_offset[o], o))
    names = Counter(z for z in zones if _offsets(z) == winner)
    return min(names, key=lambda n: (-names[n], n))


def same_clock(a, b):
    """Whether two IANA zones show the same time all year — see [[_offsets]]."""
    return _offsets(a) == _offsets(b)


def display_names(venues):
    """Flicks slug → displayName, unique across the roster: a title shared by
    several venues is qualified with each one's town, then — if that still
    collides — its state. Dies on anything left over."""
    title = {v['slug']: clean(v['title']) for v in venues}
    by_title = defaultdict(list)
    for v in venues:
        by_title[title[v['slug']]].append(v)
    for t, vs in by_title.items():
        if len(vs) > 1:
            for v in vs:
                city = clean(v.get('city'))
                title[v['slug']] = f"{t} ({city})" if city else f"{t} ({v['state']})"
    by_title = defaultdict(list)
    for v in venues:
        by_title[title[v['slug']]].append(v)
    for t, vs in by_title.items():
        if len(vs) > 1:
            for v in vs:
                title[v['slug']] = f"{t} ({v['state']})"
    dupes = {t: n for t, n in Counter(title.values()).items() if n > 1}
    if dupes:
        raise SystemExit(f"UNRESOLVED display-name collisions (wire keys): {dupes}")
    return title


def render(pages, retired, venues, town_names):
    by_slug = {v['slug']: v for v in venues}
    name_of = display_names(venues)
    point = {v['slug']: {'lat': float(v['lat']), 'lon': float(v['lon'])} for v in venues}
    zones = {p['slug']: zone_for([point[c] for c in p['cinemas']]) for p in pages}
    own_zone = sorted((name_of[c], z) for p in pages for c in p['cinemas']
                      for z in [zone_for([point[c]])] if not same_clock(z, zones[p['slug']]))
    states = {p['state'] for p in pages}
    lines = [
        "// GENERATED from data/us/pages.json by data/us/scripts/generate_roster.py",
        f"// — do NOT edit by hand. Full US cinema roster: {len(pages):,} pages over {len(states)} states and",
        f"// territories / {len(venues):,} cinemas (Flicks, www.flicks.us). Regenerate after re-harvesting or",
        "// re-clustering; see data/us/README.md.",
        "package models",
        "",
        "private[models] object UsRosterData {",
        "  // (displayName, pillName, flicks cinema slug, the venue's state, the venue's own town)",
        "  type C = (String, String, String, String, String)",
        "  // (slug, slug qualified with its state, name, state, lat, lon, zoneId, multiTown, towns, cinemas)",
        "  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])",
        "",
    ]
    for page in pages:
        rows = ",\n".join(
            "    ({}, {}, {}, {}, {})".format(
                scala_str(name_of[c]), scala_str(name_of[c]), scala_str(c), scala_str(by_slug[c]['state']),
                scala_str(clean(by_slug[c].get('city'))))
            for c in page['cinemas'])
        towns = list(dict.fromkeys(town_names(t) for t in page['towns']))
        lines.append(
            "  private def {}: R = ({}, {}, {}, {}, {}, {}, {}, {}, Seq({}), Seq(\n{}\n  ))".format(
                ident(page['slug']), scala_str(page['slug']), scala_str(page['qualifiedSlug']),
                scala_str(page['name']), scala_str(page['state']), page['lat'], page['lon'],
                scala_str(zones[page['slug']]), "true" if page['kind'] == 'cluster' else "false",
                ", ".join(scala_str(t) for t in towns), rows))
    lines.append("")
    names = [ident(p['slug']) for p in pages]
    chunks = [names[i:i + CHUNK] for i in range(0, len(names), CHUNK)]
    for index, chunk in enumerate(chunks):
        lines.append(f"  private def chunk{index}: Seq[R] = Seq({', '.join(chunk)})")
    lines.append("  val pages: Seq[R] = " + " ++ ".join(f"chunk{i}" for i in range(len(chunks))))
    lines.append("")
    lines.append("  /** (displayName, zoneId) — the venues whose own clock is not their page's. */")
    lines.append("  val venueZones: Seq[(String, String)] = Seq(")
    lines.extend(f"    ({scala_str(n)}, {scala_str(z)})," for n, z in own_zone)
    lines.append("  )")
    lines.append("")
    lines.append("  /** Pages that no longer exist — the metros the US pages were until 2026-10 among")
    lines.append("   *  them — and the page holding the town each was named after, else most of its")
    lines.append("   *  venues: (slug, slug qualified with its state, the page's slug). */")
    lines.append("  val retired: Seq[(String, String, String)] = Seq(")
    lines.extend(f"    ({scala_str(slug)}, {scala_str(r['qualifiedSlug'])}, {scala_str(r['page'])}),"
                 for slug, r in sorted(retired.items()))
    lines.append("  )")
    lines.append("")
    rostered = [name_of[c] for p in pages for c in p['cinemas']]
    lines.extend(seasonal_venues.render_scala(seasonal_venues.seasonal_names("us", rostered), scala_str))
    lines.append("}")
    return "\n".join(lines) + "\n", len(own_zone)


def main() -> int:
    roster = build_pages.roster_venues()
    data = json.loads(build_pages.OUT.read_text())
    problems = placement_problems({v['slug'] for v in roster}, data['pages'])
    if problems:
        for problem in problems:
            print(f"ERROR: {problem}", file=sys.stderr)
        return 1
    _, towns = build_pages.load_towns()
    text, own_zones = render(data['pages'], data['retired'], roster, lambda key: build_pages.display(towns, key))
    OUT.write_text(text)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(data['pages']):,} pages / {len(roster):,} cinemas, "
          f"{own_zones} on their own zone / {len(data['retired'])} retired slugs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
