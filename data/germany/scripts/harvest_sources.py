#!/usr/bin/env python3
"""
Harvest the German cinemas kinoheld.de sells for, and split them into the ones
the roster already has and the ones it lacks.

kinoheld's GraphQL API (https://next-live.kinoheld.de/graphql, no auth) lists
~2,150 venues. The roster comes from Filmstarts' city directory, which skips
whole cities (Bochum, Aachen, Solingen...), so a few hundred venues with a live
programme are on kinoheld and not on our pages.

Method:
  1. Discovery. Page through `cinemas(first:100, page:n)`; keep the German ones
     (5-digit postcode, inside Germany's bounding box) that are not closed,
     hidden, open-air or drive-in, and not excluded by name (venue_filters.py:
     seasonal screens, sex cinemas).
  2. Live check. One `programShows(excludeExpired:true, first:1)` per venue:
     a venue with no upcoming show is left out (it may be on a break; a rerun
     picks it up when it lists again).
  3. Dedup against Filmstarts -- the roster (regions.json, with each venue's
     address from filmstarts-addresses.json) AND the venues the id sweep
     recovered (filmstarts-sweep.json). Filmstarts stays every such venue's
     primary source; kinoheld becomes only its fallback. A kinoheld venue is
     the same as a Filmstarts one when they share a postcode or town AND
       - the same street (house number included), or
       - names that score >= ACCEPT_SCORE (harvest_kinoprogramm.score: city
         and noise words dropped), or
       - the same chain (CineStar, UCI, ...) as the only branch of it there.
     A pair that scores between REVIEW_SCORE and ACCEPT_SCORE is ambiguous and
     printed; each was checked by hand and its verdict recorded in DECIDED
     (kinoheld id -> theaterId, or None for "a different venue").
     kinoheld venues within SAME_SITE_M of each other whose names also agree
     are one site listed twice; the one with more upcoming shows is kept.

Writes:
  - kinoheld-venues.json: the kinoheld-ONLY venues with an upcoming show --
    the ones kinoheld will be the primary source for. Sorted by town.
  - kinoheld-matches.json: {theaterId: {kinoheldId, path, how}} -- the
    Filmstarts venues kinoheld also lists, i.e. their fallback feed.

Politeness: 5 workers, <= 5 req/s, concurrency halving on 429/503
(polite_fetch). Answers are cached under ~/.cache/kinowo/kh-harvest
(KH_HARVEST_CACHE_DIR): delete it to see today's programme.

Run: python3 data/germany/scripts/harvest_kinoheld.py
"""

import json
import os
import pathlib
import re
import sys
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import harvest_kinoprogramm as kp  # noqa: E402  (shared name normalising + scoring)
from polite_fetch import PoliteFetcher, cache_dir  # noqa: E402
from venue_filters import excluded_reason  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
OUT_VENUES = os.path.join(DATA, "kinoheld-venues.json")
OUT_MATCHES = os.path.join(DATA, "kinoheld-matches.json")

API = "https://next-live.kinoheld.de"
ACCEPT_SCORE = 0.86
REVIEW_SCORE = 0.55
SAME_SITE_M = 300
GERMANY = (47.2, 55.1, 5.8, 15.1)   # lat min/max, lon min/max

# Hand-checked verdicts on the pairs the matcher could not decide (see the
# module doc): kinoheld id -> Filmstarts theaterId it IS, or None when it is a
# different venue. Each decided from both sites' addresses.
DECIDED = {
}

CINEMAS_QUERY = """query($page: Int!) { cinemas(first: 100, page: $page) {
  data { id name urlSlug street city { name urlSlug } postcode { postcode }
         latitude longitude isClosed isOpenAir isDriveIn isHidden } } }"""
LIVE_QUERY = """query($cinema: ID!) { programShows(cinemaIds: [$cinema], excludeExpired: true, first: 100, page: 1) {
  paginatorInfo { hasMorePages } data { beginning } } }"""


def gql(fx, query, variables):
    status, _, text = fx.post_json("/graphql", {"query": query, "variables": variables})
    if status != 200:
        raise RuntimeError(f"kinoheld answered {status} to {variables}")
    doc = json.loads(text)
    if "errors" in doc:
        raise RuntimeError(f"kinoheld errors for {variables}: {doc['errors']}")
    return doc["data"]


def all_cinemas(fx):
    out, page = [], 1
    while True:
        batch = gql(fx, CINEMAS_QUERY, {"page": page})["cinemas"]["data"]
        out.extend(batch)
        if len(batch) < 100:
            return out
        page += 1


def german(c):
    postcode = str((c.get("postcode") or {}).get("postcode") or "")
    lat, lon = c.get("latitude"), c.get("longitude")
    return (re.fullmatch(r"\d{5}", postcode) is not None and lat is not None and lon is not None
            and GERMANY[0] <= lat <= GERMANY[1] and GERMANY[2] <= lon <= GERMANY[3])


def metres(a, b):
    return 1000 * kp.km_between(a[0], a[1], b[0], b[1])


STREET_ABBREVIATION = re.compile(r"str\b\.?|strasse\b|straße\b", re.I)


def street_key(street):
    """"Kortumstr. 51" and "Kortumstraße 51" -> "kortumstrasse51"."""
    s = STREET_ABBREVIATION.sub("strasse", street or "")
    return kp.fold(s).replace(" ", "")


def town_key(town):
    return kp.fold(re.split(r"[,(]", town or "")[0])


def load_filmstarts():
    """Every Filmstarts venue a kinoheld one may duplicate: the roster's and the sweep's."""
    addresses = json.load(open(os.path.join(DATA, "filmstarts-addresses.json")))
    venues = []
    for region in json.load(open(os.path.join(DATA, "regions.json"))):
        for c in region["cinemas"]:
            a = addresses.get(c["theaterId"], {})
            venues.append({"theaterId": c["theaterId"], "name": c["name"], "town": a.get("town") or c["city"],
                           "rosterCity": c["city"], "postcode": a.get("postcode", ""), "street": a.get("street", ""),
                           "source": "roster"})
    for v in json.load(open(os.path.join(DATA, "filmstarts-sweep.json"))):
        venues.append({**v, "rosterCity": v["town"], "source": "sweep"})
    return venues


def candidates(k, by_postcode, by_town):
    seen = {}
    for v in by_postcode.get(k["postcode"], []) + by_town.get(town_key(k["town"]), []):
        seen[v["theaterId"]] = v
    return list(seen.values())


def decide(k, cands, chain_count):
    """(theaterId, how) for the Filmstarts venue `k` is, (None, None) for none,
    or ("?", review_rows) when only a human can tell."""
    best, best_score, review = None, 0.0, []
    for v in cands:
        same_place = v["postcode"] == k["postcode"] or town_key(v["town"]) == town_key(k["town"]) \
            or town_key(v["rosterCity"]) == town_key(k["town"])
        if not same_place:
            continue
        if v["street"] and street_key(v["street"]) == street_key(k["street"]) and v["postcode"] == k["postcode"]:
            return v["theaterId"], "same address"
        s = kp.score(v["name"], k["name"], k["town"])
        chain = kp.chain_of(k["name"])
        if chain and chain == kp.chain_of(v["name"]) and chain_count[(chain, town_key(k["town"]))] == 1:
            s = max(s, ACCEPT_SCORE)
        if s > best_score:
            best, best_score = v, s
        if s >= REVIEW_SCORE:
            review.append((round(s, 2), v["theaterId"], v["name"], v["street"], v["postcode"]))
    if best is not None and best_score >= ACCEPT_SCORE:
        return best["theaterId"], f"name {best_score:.2f}"
    if review:
        return "?", review
    return None, None


def main():
    fx = PoliteFetcher(API, cache_dir("kh-harvest", os.environ, pathlib.Path.home(), "KH_HARVEST_CACHE_DIR"),
                       workers=5, max_rps=5.0)
    raw = all_cinemas(fx)
    print(f"kinoheld venues: {len(raw)}")
    excluded = defaultdict(list)
    kept = []
    for c in raw:
        if not german(c):
            excluded["not German"].append(c["name"])
        elif c.get("isClosed"):
            excluded["closed"].append(c["name"])
        elif c.get("isHidden"):
            excluded["hidden"].append(c["name"])
        elif excluded_reason(c["name"], c.get("isOpenAir"), c.get("isDriveIn")):
            excluded[excluded_reason(c["name"], c.get("isOpenAir"), c.get("isDriveIn"))].append(c["name"])
        else:
            kept.append(c)

    def upcoming(c):
        doc = gql(fx, LIVE_QUERY, {"cinema": c["id"]})["programShows"]
        return len(doc["data"]) + (1000 if doc["paginatorInfo"]["hasMorePages"] else 0)

    counts = fx.map(upcoming, kept)
    live = []
    for c, n in zip(kept, counts):
        if n == 0:
            excluded["no upcoming show"].append(c["name"])
            continue
        live.append({"kinoheldId": c["id"], "name": c["name"].strip(),
                     "path": f"/kino/{c['city']['urlSlug']}/{c['urlSlug']}",
                     "street": (c.get("street") or "").strip(), "postcode": c["postcode"]["postcode"],
                     "town": c["city"]["name"], "lat": c["latitude"], "lon": c["longitude"], "shows": n})
    fx.report()
    for reason, names in sorted(excluded.items()):
        print(f"excluded ({reason}): {len(names)}")
    print(f"German, open, with upcoming shows: {len(live)}")

    # One site listed twice on kinoheld: keep the one with more shows.
    live.sort(key=lambda v: -v["shows"])
    sites = []
    for k in live:
        twin = next((s for s in sites if metres((s["lat"], s["lon"]), (k["lat"], k["lon"])) <= SAME_SITE_M
                     and kp.score(s["name"], k["name"], k["town"]) >= ACCEPT_SCORE), None)
        if twin:
            print(f"  same site twice: kept {twin['kinoheldId']} {twin['name']}, dropped {k['kinoheldId']} {k['name']}")
        else:
            sites.append(k)

    filmstarts = load_filmstarts()
    by_postcode, by_town = defaultdict(list), defaultdict(list)
    chain_count = defaultdict(int)
    for v in filmstarts:
        if v["postcode"]:
            by_postcode[v["postcode"]].append(v)
        by_town[town_key(v["town"])].append(v)
        if town_key(v["rosterCity"]) != town_key(v["town"]):
            by_town[town_key(v["rosterCity"])].append(v)
        chain = kp.chain_of(v["name"])
        if chain:
            chain_count[(chain, town_key(v["town"]))] += 1

    only, matches, undecided = [], {}, []
    for k in sites:
        if k["kinoheldId"] in DECIDED:
            tid, how = DECIDED[k["kinoheldId"]], "hand-checked"
        else:
            tid, how = decide(k, candidates(k, by_postcode, by_town), chain_count)
        if tid == "?":
            undecided.append((k, how))
            continue
        if tid is None:
            only.append({key: k[key] for key in ("kinoheldId", "name", "path", "street", "postcode", "town", "lat", "lon")})
        elif tid in matches:
            print(f"  {tid} matched twice: {matches[tid]['kinoheldId']} and {k['kinoheldId']} {k['name']} -- keeping the first")
        else:
            matches[tid] = {"kinoheldId": k["kinoheldId"], "path": k["path"], "how": how}

    for k, rows in undecided:
        print(f"REVIEW {k['kinoheldId']} {k['name']} | {k['street']} {k['postcode']} {k['town']}")
        for row in sorted(rows, reverse=True):
            print(f"         {row}")
    if undecided:
        print(f"{len(undecided)} venues need a verdict in DECIDED; nothing written.", file=sys.stderr)
        sys.exit(1)

    only.sort(key=lambda v: (v["town"], v["name"], v["kinoheldId"]))
    with open(OUT_VENUES, "w") as f:
        json.dump(only, f, ensure_ascii=False, indent=1)
        f.write("\n")
    with open(OUT_MATCHES, "w") as f:
        json.dump(dict(sorted(matches.items())), f, ensure_ascii=False, indent=1)
        f.write("\n")
    roster_matched = sum(1 for t in matches if not any(v["theaterId"] == t and v["source"] == "sweep" for v in filmstarts))
    print(f"kinoheld-only venues: {len(only)}; Filmstarts venues kinoheld also lists: {len(matches)} "
          f"({roster_matched} rostered, {len(matches) - roster_matched} recovered by the sweep)")
    towns = defaultdict(int)
    for v in only:
        towns[v["town"]] += 1
    print("towns with most kinoheld-only venues:", sorted(towns.items(), key=lambda t: -t[1])[:25])


if __name__ == "__main__":
    main()
