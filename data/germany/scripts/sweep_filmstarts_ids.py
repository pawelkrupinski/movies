#!/usr/bin/env python3
"""
Sweep Filmstarts' venue ids for the cinemas its city directory does not link.

crawl_de_full.py discovers venues only through the cities its Bundesland pages
link, and some cities are linked from nowhere (Bochum lives at
/kinoprogramm/stadte-126174/ and no Land page names it), so their venues never
reached the roster. Every venue page is addressable by id, though:
/kinoprogramm/kino/A####/. This walks A0001-A2999, plus every rostered id
outside that range (the G-prefixed ones), and reads each page's JSON-LD
MovieTheater (name, street, postcode, town) and its `data-showtimes-dates`.

Writes:
  - filmstarts-addresses.json: every rostered venue's address,
    {theaterId: {name, street, postcode, town}}, which harvest_kinoheld.py
    dedupes kinoheld's venues against (the roster itself carries no address).
  - filmstarts-sweep.json: the RECOVERED venues -- live pages the roster does
    not carry, not retired, with at least one advertised showtime day, and not
    an open-air screen or a sex cinema -- sorted by town.

Filmstarts 429s a bulk crawl: at 2 req/s it blocked the sweep for minutes
after ~430 pages (2026-10-11), so this keeps the prod worker's own sustained
pace -- 2 workers, <= 0.6 req/s -- halving on 429/503 and
backing off up to 5 minutes (polite_fetch). Pages are cached under
~/.cache/kinowo/fs-sweep (FS_SWEEP_CACHE_DIR), so a rerun is free.

Run: python3 data/germany/scripts/sweep_filmstarts_ids.py
"""

import html
import json
import os
import pathlib
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from polite_fetch import PoliteFetcher, cache_dir  # noqa: E402
import roster_venues  # noqa: E402
from venue_filters import excluded_reason  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
OUT_ADDRESSES = os.path.join(DATA, "filmstarts-addresses.json")
OUT_SWEEP = os.path.join(DATA, "filmstarts-sweep.json")

BASE = "https://www.filmstarts.de"
SWEEP_IDS = [f"A{n:04d}" for n in range(1, 3000)]

LD_JSON = re.compile(r'<script type="application/ld\+json">(.*?)</script>', re.S)
SHOW_DATES = re.compile(r'data-showtimes-dates="([^"]*)"')
ISO_DATE = re.compile(r"\d{4}-\d{2}-\d{2}")


def parse_venue(text):
    """The page's MovieTheater facts and its advertised showtime days, or None
    for a page that is not a venue page."""
    theater = None
    for block in LD_JSON.findall(text):
        try:
            doc = json.loads(block)
        except ValueError:
            continue
        if isinstance(doc, dict) and doc.get("@type") == "MovieTheater":
            theater = doc
            break
    if theater is None:
        return None
    address = theater.get("address") or {}
    days = SHOW_DATES.search(text)
    return {
        "name": html.unescape(theater.get("name") or "").strip(),
        "street": html.unescape(address.get("streetAddress") or "").strip(),
        "postcode": (address.get("postalCode") or "").strip(),
        "town": html.unescape(address.get("addressLocality") or "").strip(),
        "showDays": sorted(set(ISO_DATE.findall(days.group(1)))) if days else [],
    }


def main():
    roster = {c["theaterId"]: c for c in roster_venues.load()}
    harvested = {t["theaterId"] for t in json.load(open(os.path.join(DATA, "theaters-raw.json")))}
    retired = set(json.load(open(os.path.join(DATA, "retired.json"))))
    ids = sorted(set(SWEEP_IDS) | set(roster))

    fx = PoliteFetcher(BASE, cache_dir("fs-sweep", os.environ, pathlib.Path.home(), "FS_SWEEP_CACHE_DIR"),
                       workers=2, max_rps=0.6, headers={"Accept-Language": "de-DE,de;q=0.9"},
                       max_attempts=8, max_backoff=300)

    def one(theater_id):
        path = f"/kinoprogramm/kino/{theater_id}/"
        status, final, text = fx.get(path)
        if status != 200 or theater_id not in final:
            return theater_id, status, None
        return theater_id, status, parse_venue(text)

    results = fx.map(one, ids)
    fx.report()

    statuses = {}
    for _, status, _ in results:
        statuses[status] = statuses.get(status, 0) + 1
    print("statuses:", dict(sorted(statuses.items(), key=lambda kv: str(kv[0]))))
    unanswered = [tid for tid, status, _ in results if status not in (200, 404, 410, 301, 302)]
    if unanswered:
        print(f"WARNING: {len(unanswered)} ids unanswered (rerun to retry): {unanswered[:20]}", file=sys.stderr)

    addresses, recovered, skipped = {}, [], []
    for tid, _, venue in results:
        if venue is None:
            continue
        if tid in roster:
            addresses[tid] = {k: venue[k] for k in ("name", "street", "postcode", "town")}
            continue
        if tid in harvested or tid in retired or not venue["showDays"]:
            continue
        reason = excluded_reason(venue["name"])
        if reason:
            skipped.append((tid, venue["name"], reason))
            continue
        recovered.append({"theaterId": tid, "name": venue["name"], "street": venue["street"],
                          "postcode": venue["postcode"], "town": venue["town"], "showDays": venue["showDays"]})

    recovered.sort(key=lambda v: (v["town"], v["name"], v["theaterId"]))
    with open(OUT_ADDRESSES, "w") as f:
        json.dump(dict(sorted(addresses.items())), f, ensure_ascii=False, indent=1)
        f.write("\n")
    with open(OUT_SWEEP, "w") as f:
        json.dump(recovered, f, ensure_ascii=False, indent=1)
        f.write("\n")
    missing = sorted(set(roster) - set(addresses))
    print(f"roster addresses: {len(addresses)}/{len(roster)} (no live page: {len(missing)})")
    print(f"recovered live venues: {len(recovered)}; excluded: {len(skipped)}")
    for tid, name, reason in skipped:
        print(f"  excluded {tid} {name}: {reason}")


if __name__ == "__main__":
    main()
