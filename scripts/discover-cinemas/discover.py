#!/usr/bin/env python3
"""Discover new UK cinemas on flicks.co.uk and wire them into the catalog.

The UK roster is scraped via one `FlicksClient` per venue, wired by hand in two
files:

  * common/src/main/scala/models/Cinema.scala        — the `case object … extends
    Cinema(display, pill)` plus membership in a per-city grouping `val`.
  * worker/.../services/cinemas/CinemaScraperCatalog.scala — a `flicks("<slug>",
    Obj)` line inside the city's `<city>Scrapers` Seq.

Flicks publishes every cinema in sitemap-cinemas.xml. This tool diffs that list
against the slugs we already wire (minus an exclude list of known-defunct venues)
and, for each genuinely new venue, resolves its display name + coordinates +
whether it currently lists sessions, assigns it to the nearest wired UK city by
great-circle distance, and splices compilable Scala into both files.

Anything it can't wire safely — a London venue (needs a manual area-group
assignment), an object-name collision, missing coordinates, or a venue with no
sessions this week — is PARKED: reported for a human, never written as broken
Scala. It also reports GONE venues (wired by us but dropped from the sitemap) as
retirement candidates; it never auto-deletes.

Default mode is report-only. Pass --apply to edit the source files.

Network: stdlib only (urllib). Per-venue page fetches run in a small thread pool
(default 6 workers) to stay polite; see repo CLAUDE.md scraping guidance.
"""
from __future__ import annotations

import argparse
import concurrent.futures as futures
import datetime as dt
import math
import os
import re
import sys
import urllib.request
from dataclasses import dataclass, field

FLICKS = "https://www.flicks.co.uk"
SITEMAP = f"{FLICKS}/sitemap-cinemas.xml/"
UA = "Mozilla/5.0 (compatible; kinowo-cinema-discovery/1.0)"
SESSION_DAYS = 7  # today .. +6, matching FlicksClient.daysAhead

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
CINEMA_SCALA = os.path.join(REPO, "common/src/main/scala/models/Cinema.scala")
CITY_SCALA = os.path.join(REPO, "common/src/main/scala/models/City.scala")
CATALOG_SCALA = os.path.join(
    REPO, "worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala"
)
EXCLUDE_TXT = os.path.join(os.path.dirname(__file__), "exclude.txt")


# ── HTTP ────────────────────────────────────────────────────────────────────
def fetch(url: str, ajax: bool = False, timeout: int = 30) -> str:
    headers = {"User-Agent": UA}
    if ajax:
        headers["is-ajax-call"] = "yes"
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read().decode("utf-8", "replace")


# ── Flicks enumeration ──────────────────────────────────────────────────────
def sitemap_slugs(text: str) -> set[str]:
    """All /cinema/<slug>/ slugs in the cinemas sitemap."""
    return set(re.findall(r"/cinema/([a-z0-9-]+)/", text))


def wired_slugs(catalog_text: str) -> set[str]:
    """The Flicks slugs we already wire — the first arg of each flicks(...)."""
    return set(re.findall(r'flicks\("([^"]+)"', catalog_text))


def load_exclude(text: str) -> set[str]:
    out = set()
    for line in text.splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            out.add(line)
    return out


# ── City model (parsed from City.scala) ─────────────────────────────────────
@dataclass
class City:
    obj: str          # Scala object name, e.g. "London"
    slug: str         # City.slug == flicks region slug, e.g. "london"
    lat: float
    lon: float
    group_val: str    # Cinema grouping val, e.g. "london" (Cinema.london)


def parse_uk_cities(city_text: str) -> list[City]:
    """UK City case objects with coordinates and their Cinema grouping val.

    Only the objects listed in `allUkCities` are returned, so PL/DE cities never
    become assignment targets.
    """
    m = re.search(r"val allUkCities:\s*Seq\[City\]\s*=\s*Seq\((.*?)\)", city_text, re.S)
    if not m:
        raise SystemExit("could not find allUkCities in City.scala")
    uk_objs = [o.strip() for o in m.group(1).split(",") if o.strip()]

    cities = []
    for obj in uk_objs:
        # case object <obj> extends City("slug", CityLabels(...), <lat>, <lon>, ...) { ... val cinemas = Cinema.<group> ... }
        cm = re.search(
            rf'case object {obj} extends City\(\s*"([^"]+)",\s*CityLabels\([^)]*\),\s*'
            rf"(-?\d+\.?\d*),\s*(-?\d+\.?\d*),",
            city_text,
        )
        gm = re.search(rf"case object {obj} extends City\(.*?Cinema\.(\w+)", city_text, re.S)
        if not cm or not gm:
            raise SystemExit(f"could not parse City case object {obj}")
        cities.append(City(obj, cm.group(1), float(cm.group(2)), float(cm.group(3)), gm.group(1)))
    return cities


def haversine(lat1, lon1, lat2, lon2) -> float:
    r = 6371.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp = math.radians(lat2 - lat1)
    dl = math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * r * math.asin(math.sqrt(a))


def nearest_city(lat, lon, cities: list[City]) -> tuple[City, float]:
    best = min(cities, key=lambda c: haversine(lat, lon, c.lat, c.lon))
    return best, haversine(lat, lon, best.lat, best.lon)


# ── Per-venue metadata from the Flicks cinema page ──────────────────────────
@dataclass
class VenueMeta:
    slug: str
    display: str | None = None
    lat: float | None = None
    lon: float | None = None
    has_sessions: bool = False


def _title_name(html: str) -> str | None:
    m = re.search(r"<title>\s*([^|<]+?)\s*\|", html)
    return m.group(1).strip() if m else None


def _first_coords(html: str) -> tuple[float, float] | None:
    lat = re.search(r'data-lat(?:itude)?="(-?\d{1,2}\.\d+)"', html)
    lon = re.search(r'data-l(?:on|ng)[a-z]*="(-?\d{1,3}\.\d+)"', html)
    if lat and lon:
        return float(lat.group(1)), float(lon.group(1))
    return None


def probe_sessions(slug: str, today: dt.date) -> bool:
    """True if Flicks lists at least one session for the venue in today..+6d.

    Hits the per-venue sessions endpoint directly — the same URL FlicksClient
    scrapes — so it reflects what we could actually serve, independent of whether
    the venue is in Flicks' (incomplete) sitemap index.
    """
    for i in range(SESSION_DAYS):
        d = (today + dt.timedelta(days=i)).isoformat()
        try:
            frag = fetch(f"{FLICKS}/cinema/sessions/{slug}/{d}/", ajax=True)
        except Exception:
            continue
        if "cinema-times__article" in frag:
            return True
    return False


def venue_meta(slug: str, today: dt.date) -> VenueMeta:
    meta = VenueMeta(slug)
    try:
        page = fetch(f"{FLICKS}/cinema/{slug}/")
    except Exception:
        return meta
    meta.display = _title_name(page)
    coords = _first_coords(page)
    if coords:
        meta.lat, meta.lon = coords
    meta.has_sessions = probe_sessions(slug, today)
    return meta


# ── Scala generation ────────────────────────────────────────────────────────
def pascal(display: str) -> str:
    """Turn a display name into a Scala object identifier (PascalCase, alnum)."""
    cleaned = re.sub(r"[^0-9A-Za-z]+", " ", display).strip()
    parts = [p[:1].upper() + p[1:] for p in cleaned.split(" ") if p]
    name = "".join(parts)
    if name and name[0].isdigit():
        name = "N" + name
    return name


@dataclass
class Wired:
    slug: str
    obj: str
    display: str
    city: City


@dataclass
class Parked:
    slug: str
    reason: str
    display: str | None = None


@dataclass
class Plan:
    wire: list[Wired] = field(default_factory=list)
    park: list[Parked] = field(default_factory=list)
    # `gone_dead`: wired but dropped from the sitemap AND no longer serving any
    # session — genuine retirement candidates. `gone_live`: dropped from the
    # sitemap index yet still serving sessions — a benign Flicks-index gap, NOT a
    # closure, so it needs no action (deleting it would drop working coverage).
    gone_dead: list[str] = field(default_factory=list)
    gone_live: list[str] = field(default_factory=list)


def partition_gone(gone_slugs, probe) -> tuple[list[str], list[str]]:
    """Split slugs dropped from the sitemap into truly-dead vs still-serving.

    Flicks' sitemap-cinemas.xml index omits some live venues (e.g. Vue
    Colchester, the Isle of Man cinemas), while their per-venue sessions endpoint
    keeps returning films. `probe(slug) -> bool` reports session-liveness, so only
    venues that are BOTH gone from the index AND sessionless surface as retirement
    candidates — the index gap alone never does.
    """
    dead, live = [], []
    for slug in sorted(gone_slugs):
        (live if probe(slug) else dead).append(slug)
    return dead, live


def existing_objects(cinema_text: str) -> set[str]:
    return set(re.findall(r"case object (\w+) extends Cinema\(", cinema_text))


def scraper_val_for(catalog_text: str, city_slug: str) -> str | None:
    """The `<city>Scrapers` val name mapped to this city slug in baseByCity."""
    m = re.search(rf'"{re.escape(city_slug)}"\s*->\s*(\w+)', catalog_text)
    return m.group(1) if m else None


def build_plan(cinema_text, catalog_text, cities, metas, gone_dead, gone_live) -> Plan:
    """Pure planning step (no I/O) — decides wire vs park for each new venue."""
    plan = Plan(gone_dead=sorted(gone_dead), gone_live=sorted(gone_live))
    taken = existing_objects(cinema_text)
    for meta in metas:
        if not meta.display:
            plan.park.append(Parked(meta.slug, "could not read cinema page / display name"))
            continue
        if meta.lat is None:
            plan.park.append(Parked(meta.slug, "no coordinates on page — city unknown", meta.display))
            continue
        if not meta.has_sessions:
            plan.park.append(Parked(meta.slug, "no sessions listed this week — verify it is live", meta.display))
            continue
        obj = pascal(meta.display)
        if not obj or obj in taken:
            plan.park.append(Parked(meta.slug, f"object-name collision/empty ({obj!r})", meta.display))
            continue
        city, _ = nearest_city(meta.lat, meta.lon, cities)
        if city.group_val == "london":
            plan.park.append(Parked(meta.slug, "nearest city is London — needs manual area-group assignment", meta.display))
            continue
        if scraper_val_for(catalog_text, city.slug) is None:
            plan.park.append(Parked(meta.slug, f"no scrapers val for city {city.slug!r}", meta.display))
            continue
        taken.add(obj)
        plan.wire.append(Wired(meta.slug, obj, meta.display, city))
    return plan


def _insert_before_last(text: str, pattern: str, insertion: str) -> str:
    matches = list(re.finditer(pattern, text))
    if not matches:
        raise SystemExit(f"anchor not found: {pattern}")
    at = matches[-1].end()
    return text[:at] + insertion + text[at:]


def apply_plan(cinema_text: str, catalog_text: str, plan: Plan) -> tuple[str, str]:
    for w in plan.wire:
        # 1) case object, appended after the last existing Cinema case object.
        cinema_text = _insert_before_last(
            cinema_text,
            r"case object \w+ extends Cinema\([^\n]*\)\n",
            f'case object {w.obj} extends Cinema("{w.display}", "{w.display}")\n',
        )
        # 2) membership in the city grouping val: insert before its closing ')'.
        grp = re.compile(rf"(val {w.city.group_val}: Seq\[Cinema\] = Seq\((?:.*?))\)", re.S)
        cinema_text = grp.sub(lambda m: f"{m.group(1)}, {w.obj})", cinema_text, count=1)

        # 3) flicks(...) line in the city's <city>Scrapers Seq, on its own line
        #    just before the val's closing `\n  )`.
        sval = scraper_val_for(catalog_text, w.city.slug)
        sre = re.compile(rf"(private val {sval}: Seq\[CinemaScraper\] = Seq\((?:.*?))(\n  \))", re.S)
        catalog_text = sre.sub(
            lambda m: f'{m.group(1)}\n    flicks("{w.slug}", {w.obj}),{m.group(2)}', catalog_text, count=1
        )
    return cinema_text, catalog_text


# ── Report ──────────────────────────────────────────────────────────────────
def render_report(plan: Plan, applied: bool) -> str:
    lines = []
    verb = "Wired" if applied else "Would wire"
    lines.append(f"### {verb} {len(plan.wire)} new cinema(s)")
    for w in plan.wire:
        lines.append(f"- **{w.display}** (`{w.slug}`) → {w.city.obj} — `{w.obj}`")
    if not plan.wire:
        lines.append("- _none_")
    lines.append("")
    lines.append(f"### Parked {len(plan.park)} for manual review")
    for p in plan.park:
        name = f"**{p.display}** " if p.display else ""
        lines.append(f"- {name}(`{p.slug}`) — {p.reason}")
    if not plan.park:
        lines.append("- _none_")
    lines.append("")
    lines.append(f"### Retirement candidates — {len(plan.gone_dead)} slug(s) dropped from Flicks + no sessions")
    for g in plan.gone_dead:
        lines.append(f"- `{g}` — gone from sitemap-cinemas.xml AND no sessions this week; verify + remove if closed")
    if not plan.gone_dead:
        lines.append("- _none_")
    if plan.gone_live:
        lines.append("")
        lines.append(f"### Sitemap-index gaps (no action) — {len(plan.gone_live)} slug(s) still serving sessions")
        lines.append("_Dropped from Flicks' sitemap index but their sessions endpoint still returns films — a Flicks index gap, not a closure. Left wired._")
        for g in plan.gone_live:
            lines.append(f"- `{g}`")
    return "\n".join(lines)


# ── main ────────────────────────────────────────────────────────────────────
def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--apply", action="store_true", help="edit the source files (default: report only)")
    ap.add_argument("--report-path", help="also write the markdown report here (for the PR body)")
    ap.add_argument("--workers", type=int, default=6, help="concurrent venue-page fetches")
    ap.add_argument("--today", help="override today (YYYY-MM-DD), for tests")
    args = ap.parse_args(argv)

    today = dt.date.fromisoformat(args.today) if args.today else dt.date.today()

    cinema_text = open(CINEMA_SCALA).read()
    catalog_text = open(CATALOG_SCALA).read()
    cities = parse_uk_cities(open(CITY_SCALA).read())

    flicks = sitemap_slugs(fetch(SITEMAP))
    wired = wired_slugs(catalog_text)
    exclude = load_exclude(open(EXCLUDE_TXT).read())

    new_slugs = sorted(flicks - wired - exclude)
    gone = wired - flicks - exclude

    print(f"flicks sitemap: {len(flicks)}  wired: {len(wired)}  excluded: {len(exclude)}", file=sys.stderr)
    print(f"new candidates: {len(new_slugs)}  gone: {len(gone)}", file=sys.stderr)

    metas = []
    if new_slugs:
        with futures.ThreadPoolExecutor(max_workers=args.workers) as ex:
            metas = list(ex.map(lambda s: venue_meta(s, today), new_slugs))

    # A slug gone from the sitemap index is only a retirement candidate if its
    # sessions endpoint is ALSO empty — otherwise it's a live venue Flicks simply
    # dropped from the index, and must stay wired.
    gone_dead, gone_live = [], []
    if gone:
        with futures.ThreadPoolExecutor(max_workers=args.workers) as ex:
            liveness = dict(zip(sorted(gone), ex.map(lambda s: probe_sessions(s, today), sorted(gone))))
        gone_dead, gone_live = partition_gone(gone, lambda s: liveness[s])
    print(f"gone → retirement candidates: {len(gone_dead)}  live index-gaps: {len(gone_live)}", file=sys.stderr)

    plan = build_plan(cinema_text, catalog_text, cities, metas, gone_dead, gone_live)

    if args.apply and plan.wire:
        new_cinema, new_catalog = apply_plan(cinema_text, catalog_text, plan)
        open(CINEMA_SCALA, "w").write(new_cinema)
        open(CATALOG_SCALA, "w").write(new_catalog)

    report = render_report(plan, applied=args.apply and bool(plan.wire))
    print(report)
    if args.report_path:
        open(args.report_path, "w").write(report + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
