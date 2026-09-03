#!/usr/bin/env python3
"""Harvest the TOWN each UK venue sits in, into data/uk/venues.json.

The UK roster is the one that never had one. Germany, Spain and the US each
carry their venues' towns from their own harvest, so `City.coveredPlaces` can
say that /koeln/ covers Bonn and /san-diego/ covers Chula Vista. The UK's ~840
venues are hand-written `case object`s with a display name and nothing else, so
its region pages — /aberdeenshire/, /cornwall/, /cheshire/ — named no town at
all.

Flicks has the town: every `/cinema/<slug>/` page carries the venue's postal
address, and the repo already holds that slug for every UK venue, in the two
places a venue can be wired (`CinemaScraperCatalog`'s `flicks("<slug>", Obj)`
for the ones Flicks scrapes, `ChainFlicksFallback`'s `Obj -> "<slug>"` for the
chain venues that only fall back to it).

The town is the last address line before the postcode. That rule is not
guessed: `town_of` is checked against Cineworld's and Odeon's OWN `city` fields,
which the recorded fixtures carry for 189 of these venues — see
test_harvest_towns.py, which fails if agreement drops.

Usage:  python3 data/uk/scripts/harvest_towns.py [--limit N]
"""
from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
import threading
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor

ROOT = pathlib.Path(__file__).resolve().parents[3]
OUT = ROOT / "data" / "uk" / "venues.json"
CINEMA_SCALA = ROOT / "common/src/main/scala/models/Cinema.scala"
CATALOG = ROOT / "worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala"
FALLBACK = ROOT / "worker/src/main/scala/services/cinemas/ChainFlicksFallback.scala"

FLICKS = "https://www.flicks.co.uk"
UA = "Mozilla/5.0 (compatible; kinowo-cinema-discovery/1.0)"

# Flicks throttles by STALLING rather than by 429 and plateaus at 3-5 req/s
# however many workers you throw at it (see the flicks-multimarket memory), so
# the pool is small and paced. A full sweep is ~840 pages, a few minutes.
WORKERS = 3
PACE_SECONDS = 0.35

# A UK postcode at the end of an address line: "NR1 1XA", "AB24 5EN", "W1D 7DH".
POSTCODE = re.compile(r"\s*\b[A-Z]{1,2}\d[A-Z\d]?\s+\d[A-Z]{2}\b\s*")
ADDRESS = re.compile(
    r'<address[^>]*class="[^"]*cinema-hero[^"]*"[^>]*>(.*?)</address>', re.S)
TAGS = re.compile(r"<[^>]+>")

# A postcode can be attached to the COUNTY instead of the town — "Bridge Road,
# Haslemere, Surrey GU27 2AS" — which the postcode rule alone reads as "Surrey".
# Nothing in the shape of the string distinguishes that from "Norwich NR1 1XA",
# so the counties are named. Ceremonial counties plus the postal counties that
# actually turn up in the roster; a name that is BOTH a county and the town we
# want (Durham, Lincoln) is deliberately absent, since the town is the answer
# there anyway.
COUNTIES = frozenset(x.lower() for x in [
    "Angus", "Argyll", "Ayrshire", "Bedfordshire", "Berkshire", "Buckinghamshire",
    "Cambridgeshire", "Cheshire", "Clwyd", "Cornwall", "Cumbria", "Derbyshire",
    "Devon", "Dorset", "Dumfriesshire", "Dyfed", "East Sussex", "Essex", "Fife",
    "Gloucestershire", "Greater Manchester", "Gwent", "Gwynedd", "Hampshire",
    "Herefordshire", "Hertfordshire", "Kent", "Lanarkshire", "Lancashire",
    "Leicestershire", "Lincolnshire", "Merseyside", "Middlesex", "Midlothian",
    "Norfolk", "Northamptonshire", "Northumberland", "North Yorkshire",
    "Nottinghamshire", "Oxfordshire", "Perthshire", "Powys", "Renfrewshire",
    "Shropshire", "Somerset", "South Yorkshire", "Staffordshire", "Stirlingshire",
    "Suffolk", "Surrey", "Tayside", "Tyne and Wear", "Warwickshire", "West Lothian",
    "West Midlands", "West Sussex", "West Yorkshire", "Wiltshire", "Worcestershire",
    "Wirral",
    # The island groups fill the same slot: "Kenneth Street, Stornoway, Isle of
    # Lewis HS1 2DS" is Stornoway, and every island venue in the roster is
    # written this way. "Okney" is Flicks' own misspelling of Orkney, which the
    # display name in Cinema.scala carries too — spelled as harvested, because
    # this list has to match what the address actually says.
    "Isle of Lewis", "Isle of Man", "Isle of Wight", "Okney Islands",
    "Orkney Islands", "Shetland Islands", "Western Isles",
])


def scala_venues() -> dict[str, str]:
    """Every UK venue's Flicks slug -> its `case object` name."""
    catalog = CATALOG.read_text()
    fallback = FALLBACK.read_text()
    wired = dict(re.findall(r'flicks\("([^"]+)",\s*([A-Za-z0-9_]+)\)', catalog))
    for obj, slug in re.findall(r'([A-Za-z0-9_]+)\s*->\s*"([^"]+)"', fallback):
        wired.setdefault(slug, obj)
    return wired


def display_names() -> dict[str, str]:
    """`case object` name -> displayName, read off Cinema.scala."""
    return dict(re.findall(
        r'case object ([A-Za-z0-9_]+) extends \w*Cinema\("((?:[^"\\]|\\.)*)"',
        CINEMA_SCALA.read_text()))


def town_of(address: str) -> str:
    """The town in a UK address: whatever the POSTCODE is attached to.

    Not simply the last part. UK addresses run smallest-first, but they end
    three different ways, and only the postcode marks the town in all of them:

        Riverside, Wherry Road, Norwich NR1 1XA          -> the town carries it
        …, Links Road, AB24 5EN, Aberdeen                -> it sits before the town
        …, Speke Road, L24 8QB, Speke, Merseyside        -> and the COUNTY trails

    So: find the postcode, and take the town off the part holding it, or off the
    part after it when it stands alone. Reading the last part instead answers
    "Merseyside" to the third — which is how this rule was found, against
    Cineworld's own `city` field (see test_harvest_towns.py).
    """
    parts = [p.strip() for p in address.split(",") if p.strip()]
    if not parts:
        return ""

    chosen = len(parts) - 1                               # no postcode: the last part
    for i, part in enumerate(parts):
        if not POSTCODE.search(part):
            continue
        stripped = POSTCODE.sub("", part).strip()
        if stripped:                                      # "Norwich NR1 1XA"
            parts[i], chosen = stripped, i
        elif i + 1 < len(parts):                          # "AB24 5EN, Aberdeen"
            chosen = i + 1
        elif i:                                           # "London, WC2H 7NA"
            chosen = i - 1
        else:
            return ""
        break

    # A county or an island group is never the answer; the town is in front of
    # it. Walk, rather than step once, because both can trail at once.
    while chosen > 0 and _tidy(parts[chosen]).lower() in COUNTIES:
        chosen -= 1
    return _tidy(parts[chosen])


def _tidy(town: str) -> str:
    """Drop the "County" a couple of addresses wrap the town in — "Durham
    County DH1 1WA" is Durham, and the county is not where you go."""
    return re.sub(r"^County\s+|\s+County$", "", town).strip()


def parse(html: str) -> tuple[str, float | None, float | None]:
    m = ADDRESS.search(html)
    address = ""
    if m:
        address = " ".join(TAGS.sub(" ", m.group(1)).split())
    lat = re.search(r'data-lat(?:itude)?="(-?\d{1,2}\.\d+)"', html)
    lon = re.search(r'data-l(?:on|ng)[a-z]*="(-?\d{1,3}\.\d+)"', html)
    return (address,
            float(lat.group(1)) if lat else None,
            float(lon.group(1)) if lon else None)


def sync_plan(held: dict, wired: dict, names: dict) -> tuple[dict, list, list]:
    """What an incremental sweep keeps, forgets and re-keys.

    Split out from the fetching so the decision can be tested without a network,
    which is the half that can go quietly wrong. Returns the venues to KEEP (the
    caller fetches whatever `wired` holds beyond them), the slugs no longer wired,
    and the display names that changed.

    Re-reading the display name matters more than it looks: it is the key
    [[UkVenueTowns]] is keyed on, so a venue renamed in Cinema.scala would keep
    its row under the old name and quietly stop matching — the town would vanish
    from its city with nothing to say so.
    """
    keep = {slug: venue for slug, venue in held.items() if slug in wired}
    retired = sorted(set(held) - set(wired))
    renamed = []
    for slug, venue in keep.items():
        name = names.get(wired[slug], venue["displayName"])
        if name != venue["displayName"]:
            renamed.append(name)
            venue["displayName"] = name
    return keep, retired, sorted(renamed)


_pace = threading.Lock()
_last = [0.0]


def fetch(url: str) -> str:
    with _pace:
        wait = PACE_SECONDS - (time.monotonic() - _last[0])
        if wait > 0:
            time.sleep(wait)
        _last[0] = time.monotonic()
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read().decode("utf-8", "replace")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=0, help="harvest only the first N (a smoke run)")
    ap.add_argument("--sync", action="store_true",
                    help="top up venues.json against the wiring: harvest only the venues it does "
                         "not yet hold, drop the ones no longer wired, and refresh the display "
                         "names — what the weekly discovery job runs after wiring a new cinema")
    ap.add_argument("--reparse", action="store_true",
                    help="re-derive every town from the addresses already in venues.json, "
                         "hitting no network — what to run after changing town_of")
    args = ap.parse_args()

    if args.reparse:
        venues = json.loads(OUT.read_text())
        changed = 0
        for venue in venues:
            town = town_of(venue["address"])
            if town != venue["town"]:
                print(f"  {venue['displayName']}: {venue['town']!r} -> {town!r}", file=sys.stderr)
                venue["town"] = town
                changed += 1
        OUT.write_text(json.dumps(venues, indent=1, ensure_ascii=False) + "\n")
        print(f"re-parsed {len(venues)} venues, {changed} towns changed", file=sys.stderr)
        return 0

    wired = scala_venues()
    names = display_names()

    # --sync is the incremental pass. The weekly discovery job wires new venues
    # into Cinema.scala + the catalog, and a venue with no town simply drops out
    # of its city's list — quietly, which is the failure worth designing away.
    # So: harvest only what is missing, forget what is no longer wired, and
    # re-read the display names, which are the table's KEY and change under a
    # rename with nothing to notice it.
    held: dict[str, dict] = {}
    if args.sync and OUT.exists():
        held, retired, renamed = sync_plan(
            {v["flicksSlug"]: v for v in json.loads(OUT.read_text())}, wired, names)
        print(f"sync: {len(held)} held, {len(retired)} no longer wired, "
              f"{len(renamed)} renamed", file=sys.stderr)
        for name in renamed:
            print(f"  renamed -> {name}", file=sys.stderr)

    slugs = sorted(set(wired) - set(held))
    if args.limit:
        slugs = slugs[: args.limit]
    print(f"harvesting {len(slugs)} UK venues from Flicks", file=sys.stderr)

    done, failed = list(held.values()), []

    def one(slug: str):
        obj = wired[slug]
        try:
            address, lat, lon = parse(fetch(f"{FLICKS}/cinema/{slug}/"))
        except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError) as e:
            failed.append((slug, str(e)))
            return
        done.append({
            "flicksSlug": slug,
            "cinemaObject": obj,
            "displayName": names.get(obj, ""),
            "town": town_of(address),
            "address": address,
            "lat": lat,
            "lon": lon,
        })

    started = time.monotonic()
    with ThreadPoolExecutor(max_workers=WORKERS) as pool:
        for i, _ in enumerate(pool.map(one, slugs), 1):
            if i % 100 == 0:
                print(f"  {i}/{len(slugs)}", file=sys.stderr)

    done.sort(key=lambda v: v["flicksSlug"])
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(done, indent=1, ensure_ascii=False) + "\n")

    elapsed = time.monotonic() - started
    withtown = sum(1 for v in done if v["town"])
    fetched = len(done) - len(held)
    rate = f", {fetched / max(elapsed, 0.001):.1f} req/s" if fetched else ""
    print(f"wrote {OUT.relative_to(ROOT)}: {len(done)} venues, {withtown} with a town, "
          f"{fetched} fetched, {len(failed)} failed{rate}", file=sys.stderr)
    for slug, err in failed[:10]:
        print(f"  FAILED {slug}: {err}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
