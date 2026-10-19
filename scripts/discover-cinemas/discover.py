#!/usr/bin/env python3
"""Discover new UK cinemas on flicks.co.uk and wire them into the catalog.

Every UK venue is declared once, as a row of data/uk/roster.json (see
data/uk/scripts/uk_roster.py): its Scala name, display and pill names, its
flicks.co.uk slug and its primary source. A venue Flicks scrapes is a row with
only a `flicks` slug; data/uk/scripts/generate_roster.py turns the rows into the
Scala (`UkRosterData`, `UkSourcesData`).

Which PAGE lists a venue is not wired by hand either: it is generated from the
venue's town (data/uk/scripts/build_pages.py → UkPages.scala), after the
workflow has harvested and geocoded the new venue.

UK chain venues (Cineworld / Vue / Showcase / Everyman / Odeon) and the
independents read off their own box office keep their Flicks slug as the
aggregator FALLBACK — a row with a `primary` as well as a `flicks` slug. Either
way the slug is in `flicks`, so it counts as covered.

Flicks publishes every cinema in sitemap-cinemas.xml. This tool diffs that list
against the slugs we already wire (minus an exclude list of known-defunct venues)
and, for each genuinely new venue, resolves its display name + coordinates +
whether it currently lists sessions, and appends its row to the roster group of
the nearest already-wired venue (great-circle distance over data/uk/venues.json).

Anything it can't wire safely — a London venue (needs a manual area-group
assignment), an object-name or display-name collision, missing coordinates, or a
venue with no sessions this week — is PARKED: reported for a human, never
written as a row the generator would refuse. It also reports GONE venues (wired by us but dropped from the sitemap) as
retirement candidates; it never auto-deletes.

Default mode is report-only. Pass --apply to write the rows into roster.json.

Network: stdlib only (urllib). Per-venue page fetches run in a small thread pool
(default 6 workers) to stay polite; see repo CLAUDE.md scraping guidance.
"""
from __future__ import annotations

import argparse
import concurrent.futures as futures
import copy
import datetime as dt
import json
import math
import os
import re
import sys
import urllib.request
from dataclasses import dataclass, field

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.insert(0, os.path.join(REPO, "data/uk/scripts"))
import uk_roster  # noqa: E402

FLICKS = "https://www.flicks.co.uk"
SITEMAP = f"{FLICKS}/sitemap-cinemas.xml/"
UA = "Mozilla/5.0 (compatible; kinowo-cinema-discovery/1.0)"
SESSION_DAYS = 7  # today .. +6, matching FlicksClient.daysAhead

UK_VENUES = os.path.join(REPO, "data/uk/venues.json")
UK_PAGES = os.path.join(REPO, "data/uk/pages.json")
EXCLUDE_TXT = os.path.join(os.path.dirname(__file__), "exclude.txt")


def read_text(path: str) -> str:
    with open(path) as f:
        return f.read()


def write_text(path: str, text: str) -> None:
    with open(path, "w") as f:
        f.write(text)


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


def wired_slugs(groups: list[dict]) -> set[str]:
    """Every Flicks slug we already cover, however the venue is scraped: a row's
    `flicks` is its slug whether Flicks is its primary or only its fallback."""
    return {v["flicks"] for v in uk_roster.venues(groups) if v.get("flicks")}


def load_exclude(text: str) -> set[str]:
    out = set()
    for line in text.splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            out.add(line)
    return out


# ── Where a new venue is wired (read off roster.json and data/uk) ─────────────
@dataclass
class Group:
    """The roster.json group a new venue is wired into: the one its nearest
    already-wired venue is in. Which PAGE lists the venue is not decided here —
    `data/uk/scripts/build_pages.py` places it by its town, and the catalog files
    every UK scraper under that page whatever group it sits in."""
    section: str      # e.g. "cardiff"
    near: str         # the nearest wired venue's object name
    km: float
    london: bool      # that venue is on the London page


def scraper_vals(groups: list[dict]) -> dict[str, str]:
    """Every venue object the roster wires → the group it is in."""
    return {v["object"]: g["group"] for g in groups for v in g["venues"]}


def nearest_group(lat, lon, venues: list[dict], vals: dict[str, str], london: set[str]) -> Group | None:
    """The group the wired venue nearest (lat, lon) is in. `venues`: data/uk/venues.json
    rows (cinemaObject, lat, lon); only those the catalog wires count."""
    placed = [v for v in venues if v["cinemaObject"] in vals and v.get("lat") is not None]
    if not placed:
        return None
    v = min(placed, key=lambda v: haversine(lat, lon, v["lat"], v["lon"]))
    return Group(vals[v["cinemaObject"]], v["cinemaObject"], haversine(lat, lon, v["lat"], v["lon"]),
                 v["cinemaObject"] in london)


def london_venues(pages_json: str) -> set[str]:
    """The venues the London page lists, off data/uk/pages.json."""
    return set(next(p for p in json.loads(pages_json)["pages"] if p["slug"] == "london")["cinemas"])


def haversine(lat1, lon1, lat2, lon2) -> float:
    r = 6371.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp = math.radians(lat2 - lat1)
    dl = math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * r * math.asin(math.sqrt(a))


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


# ── Roster rows ─────────────────────────────────────────────────────────────
def pascal(display: str) -> str:
    """Turn a display name into a venue's Scala name (PascalCase, alnum)."""
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
    group: Group


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


def build_plan(groups, venues, london, metas, gone_dead, gone_live) -> Plan:
    """Pure planning step (no I/O) — decides wire vs park for each new venue.
    `groups`: roster.json's; `venues`: data/uk/venues.json; `london`: the venues
    the London page lists."""
    plan = Plan(gone_dead=sorted(gone_dead), gone_live=sorted(gone_live))
    rows = uk_roster.venues(groups)
    taken = {v["object"] for v in rows}
    names = {v["displayName"] for v in rows}
    vals = scraper_vals(groups)
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
        if meta.display in names:
            plan.park.append(Parked(meta.slug, "display-name collision: another venue's slots are stored "
                                               "under that name", meta.display))
            continue
        group = nearest_group(meta.lat, meta.lon, venues, vals, london)
        if group is None:
            plan.park.append(Parked(meta.slug, "no wired UK venue to place it beside", meta.display))
            continue
        if group.london:
            plan.park.append(Parked(meta.slug, "nearest venue is on the London page — needs manual "
                                               "area-group assignment", meta.display))
            continue
        taken.add(obj)
        names.add(meta.display)
        plan.wire.append(Wired(meta.slug, obj, meta.display, group))
    return plan


def apply_plan(groups: list[dict], plan: Plan) -> list[dict]:
    """The roster with each wired venue's row at the end of its group, scraped off
    Flicks; `groups` itself is left as it was."""
    out = copy.deepcopy(groups)
    by_name = {g["group"]: g for g in out}
    for w in plan.wire:
        by_name[w.group.section]["venues"].append(
            {"object": w.obj, "displayName": w.display, "pillName": w.display, "flicks": w.slug})
    return out


# ── Report ──────────────────────────────────────────────────────────────────
def render_report(plan: Plan, applied: bool) -> str:
    lines = []
    verb = "Wired" if applied else "Would wire"
    lines.append(f"### {verb} {len(plan.wire)} new cinema(s)")
    for w in plan.wire:
        lines.append(f"- **{w.display}** (`{w.slug}`) → group `{w.group.section}`, beside `{w.group.near}` "
                     f"({w.group.km:.0f} km) — `{w.obj}`")
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
    ap.add_argument("--apply", action="store_true", help="write the rows into roster.json (default: report only)")
    ap.add_argument("--report-path", help="also write the markdown report here (for the PR body)")
    ap.add_argument("--workers", type=int, default=6, help="concurrent venue-page fetches")
    ap.add_argument("--today", help="override today (YYYY-MM-DD), for tests")
    args = ap.parse_args(argv)

    today = dt.date.fromisoformat(args.today) if args.today else dt.date.today()

    groups = uk_roster.load()
    venues = json.loads(read_text(UK_VENUES))
    london = london_venues(read_text(UK_PAGES))

    flicks = sitemap_slugs(fetch(SITEMAP))
    wired = wired_slugs(groups)
    exclude = load_exclude(read_text(EXCLUDE_TXT))

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

    plan = build_plan(groups, venues, london, metas, gone_dead, gone_live)

    if args.apply and plan.wire:
        uk_roster.dump(apply_plan(groups, plan))

    report = render_report(plan, applied=args.apply and bool(plan.wire))
    print(report)
    if args.report_path:
        write_text(args.report_path, report + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
