#!/usr/bin/env python3
"""
Every German cinema we could serve, and which source to serve each one from.

Four sources list German programmes, each incompletely:
  - Filmstarts (the roster's own source) stops at the end of the programme week
    and skips whole cities its directory never links (Bochum, Aachen...);
  - Cineamo (api.cineamo.com) lists ~1,300 venues, months ahead;
  - kinoheld.de (GraphQL) lists ~2,150, many of them ticketing-only;
  - kinoprogramm.com mirrors Filmstarts' window (the roster's fallback).

Method:
  1. Discovery. Cineamo's and kinoheld's German venues that are open, not an
     open-air/drive-in screen or a sex cinema (venue_filters.py) and list an
     upcoming show. A kinoheld and a Cineamo venue within SAME_SITE_M of each
     other whose names agree are one venue.
  2. Identity. Each discovered venue is matched to a Filmstarts venue -- the
     roster's (addresses from filmstarts-addresses.json, by the id sweep) or one
     the sweep recovered (filmstarts-sweep.json) -- when they share a postcode or
     town AND the same street, or names scoring >= ACCEPT_SCORE
     (harvest_kinoprogramm.score), or the same chain as its only branch there.
     A pair between REVIEW_SCORE and ACCEPT_SCORE was checked by hand; its
     verdict is in DECIDED. Unmatched venues are NEW: no Filmstarts page.
  3. Measurement. Every source a venue has is counted over the next WINDOW_DAYS
     days and over its whole published horizon: Cineamo and kinoheld off their
     APIs, kinoprogramm.com off its week pages (until three blank weeks), and
     Filmstarts off what showtimes.cc/de serves today (`/<page>/api/repertoire`:
     the roster as projected from Filmstarts).
  4. Choice. A venue's `rawWinner` is the source with the most shows in the
     window, the whole horizon breaking a tie (then SOURCES order) -- what the
     summary's split counts, over the rostered venues with a show. Its PRIMARY
     is the most complete source too, but a near-tie (within `tolerance`) stays
     with the source earlier in SOURCES; the rest are its fallback chain, most
     complete first. With a DEFAULT_ORDER instead, the primary is the first
     source in it that has a show, the rest following in that order.

Writes venue-sources.json: the measurement day, the summary (where each source
wins, the Filmstarts-only venues) and one row per venue with its handles in
every source, its counts and its chosen primary + fallbacks.

Politeness: polite_fetch pools -- Cineamo 6 workers / 10 req/s, kinoheld 5 / 5,
kinoprogramm.com 4 / 3, showtimes.cc 2 / 2 -- each halving on 429/503. Answers
are cached under ~/.cache/kinowo/sources-<day> (SOURCES_CACHE_DIR): a rerun on
the same day is free, the next day measures afresh.

Run: python3 data/germany/scripts/harvest_sources.py
"""

import datetime
import json
import os
import pathlib
import re
import sys
from collections import Counter, defaultdict
from zoneinfo import ZoneInfo

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import harvest_kinoprogramm as kp  # noqa: E402  (shared name normalising + scoring)
import roster_venues  # noqa: E402
from polite_fetch import PoliteFetcher, cache_dir  # noqa: E402
from venue_filters import excluded_reason  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
OUT = os.path.join(DATA, "venue-sources.json")

BERLIN = ZoneInfo("Europe/Berlin")
WINDOW_DAYS = 14
ACCEPT_SCORE = 0.86
REVIEW_SCORE = 0.55
SAME_SITE_M = 300
SAME_TOWN_M = 2000
GERMANY = (47.2, 55.1, 5.8, 15.1)   # lat min/max, lon min/max

SOURCES = ("filmstarts", "cineamo", "kinoheld", "kinoprogramm")
# The order a venue's sources are tried in, when Germany has one (README, "Which
# source serves a venue"); None ranks each venue's sources by what they list.
DEFAULT_ORDER = None

# Hand-checked verdicts on the pairs the matcher could not decide: discovered
# venue key ("cineamo:<id>" / "kinoheld:<id>") -> the Filmstarts theaterId it IS,
# or None for "a different venue". Each decided from both sites' addresses.
DECIDED = {
    "cineamo:141": "A1062",    # Residenz Arnsberg: Rumbecker Str. 6 / 6a
    "cineamo:458": "A0024",    # Cadillac Casablanca = Casablanca Ingelheim, Binger Str. 75
    "cineamo:504": "A0999",    # Neues Theater St. Wendel, Alter Woog 2
    "cineamo:579": "A0602",    # Harsefelder Lichtspiele, Marktstr. 17-21
    "cineamo:823": "A0858",    # Kommunales Kino Weiterstadt, Carl-Ulrich-Str. 9
    "cineamo:836": "A0566",    # Subiaco, Alpirsbach: the Kloster's cinema
    "cineamo:1006": "A0863",   # guckloch, Kalkofenstr. 3
    "cineamo:1180": "A0040",   # Friedrichsbau, Kaiser-Joseph-Str. 268-270
    "cineamo:1211": "A2747",   # SchanzenKino 73, Schulterblatt 73 (Filmstarts' postcode is Munich's)
    "cineamo:1235": None,      # Ostpassagen Theater, Konradstr. -- not the Passage Kinos, Hainstr.
    "cineamo:1397": None,      # Film-Shop Kassel, Grüner Weg -- not the Filmpalast, Karlsplatz
    "cineamo:1431": "A0240",   # Höchster Lichtspiele, Frankfurter Str.
    "kinoheld:291": None,      # Cineplex Thalia-Hollywood, Kirchgasse -- not Apollo nor Arkaden
    "kinoheld:1334": "A0632",  # Kaiserhof+ Detmold
    "cineamo:541": "A0696",    # City 46, Birkenstr. 1 -- Filmstarts lists it twice (A1378 "City-Filmtheater")
    "cineamo:1027": "A2784",   # Audi Programmkino, Ettinger Str., Ingolstadt
    # Reviewing the venues the first run called new, against every rostered venue in their towns:
    "cineamo:999": "A0886",    # Kino Berchtesgaden = Kurkino im Kurhaus, Maximilianstr. 9
    "kinoheld:1326": "A0111",  # Kino in der Brotfabrik, Prenzlauer Promenade 3 = Berlin's Brotfabrik
    "kinoheld:1332": "A1597",  # Rex Kinos, Grafenstr. = Darmstadt's programmkino rex
    "kinoheld:97": "A2729",    # Burg-Film-Theater Fehmarn, Breite Str. 13a
    "kinoheld:817": "A2661",   # Kino 35 = Kinoinitiative 35, Fulda
    "kinoheld:1853": "A2747",  # nestkino is kinoheld's SchanzenKino 73 (its page is /schanzenkino-73)
    "cineamo:887": "A1715",    # CineMotion Filmpalast Maxx Hameln, Bahnhofplatz 9
    "kinoheld:1304": "A0014",  # Kommunales Kino = Kino Kandern, Hauptstr. 28
    "kinoheld:66": "A0078",    # Bali-Kinos im KulturBahnhof Kassel
    "cineamo:600": "A0916",    # Lilo = Li-Lo-Lichtspiele Löningen, Poststr.
    "cineamo:1238": "G011E",   # Promenade Lichtspiele = ProLi Cinema, Unterer Sand 13
    "cineamo:782": "A1518",    # KiTT Tettnang, Schloßstr. 9
    "cineamo:123": "A1212",    # UCI Nova Eventis, Günthersdorf (Leuna): Einkaufszentrum Nova
}

# Two discovered listings of one venue the site rule cannot see as one: kinoheld id ->
# the Cineamo id it is. Same name and programme, two addresses or points.
SAME_VENUE = {
    "kinoheld:2172": "cineamo:1232",   # Regina-Filmtheater Pegnitz
    "kinoheld:3317": "cineamo:2981",   # Lichtspieltheater Willich
}

# Venues no aggregator lists, whose own site publishes a regular dated programme a
# client reads (checked by hand, 2026-10-12): Filmstarts theaterId -> the site. The
# harvest does not measure these; the site leads, the measured order follows it.
OWN_SITE_KINDS = ("wix-events", "rex-schifferstadt")
OWN_SITES = {
    # Film Wednesdays at 20:00 on its Wix events list, six weeks ahead, tickets at the
    # door; the list also holds a Bischofswerda club's evenings, told apart by postcode.
    "A2231": {"site": "wix-events", "page": "https://www.steinhaus-bautzen.de/kino", "postcode": "02625"},
    # The programme week, Thursday to Wednesday, as one table per film; phone bookings.
    "A1072": {"site": "rex-schifferstadt", "page": "https://rex-schifferstadt.de/"},
}

# Listed like a venue, but not one: never added.
NOT_A_VENUE = {
    "cineamo:778",   # "Innenstadtkinos": the brand of Stuttgart's six city-centre cinemas, each rostered
    "kinoheld:3665", # Sinema Transtopia's old listing (Lindower Str.); kinoheld 2933 is its current one
}

KINOHELD_CINEMAS = """query($page: Int!) { cinemas(first: 100, page: $page) {
  data { id name urlSlug street city { name urlSlug } postcode { postcode }
         latitude longitude isClosed isOpenAir isDriveIn isHidden } } }"""
KINOHELD_SHOWS = """query($cinema: ID!, $page: Int!) { programShows(cinemaIds: [$cinema], excludeExpired: true, first: 100, page: $page) {
  paginatorInfo { hasMorePages } data { beginning } } }"""


# ------------------------------------------------------------------ helpers
def berlin_day(iso):
    """The German calendar day an ISO instant ("...Z" or "+02:00") falls on."""
    return datetime.datetime.fromisoformat(iso.replace("Z", "+00:00")).astimezone(BERLIN).date().isoformat()


def counts(days, today):
    """[shows in the WINDOW_DAYS days from tomorrow, shows from tomorrow on] of a list of
    show days. Today is left out: a source that lists the day's earlier shows (kinoprogramm.com's
    week page does) would outscore one that has dropped them as past."""
    start = datetime.date.fromisoformat(today) + datetime.timedelta(days=1)
    end = (start + datetime.timedelta(days=WINDOW_DAYS)).isoformat()
    upcoming = [d for d in days if d >= start.isoformat()]
    return [sum(1 for d in upcoming if d < end), len(upcoming)]


def german_place(postcode, lat, lon):
    return (re.fullmatch(r"\d{5}", str(postcode or "")) is not None and lat is not None and lon is not None
            and GERMANY[0] <= lat <= GERMANY[1] and GERMANY[2] <= lon <= GERMANY[3])


def metres(a, b):
    return 1000 * kp.km_between(a["lat"], a["lon"], b["lat"], b["lon"])


STREET_ABBREVIATION = re.compile(r"str\b\.?|strasse\b|straße\b", re.I)


def street_key(street):
    """"Kortumstr. 51" and "Kortumstraße 51" -> "kortumstrasse51"."""
    return kp.fold(STREET_ABBREVIATION.sub("strasse", street or "")).replace(" ", "")


def town_key(town):
    return kp.fold(re.split(r"[,(]", town or "")[0])


# ---------------------------------------------------------------- discovery
def gql(fx, query, variables):
    status, _, text = fx.post_json("/graphql", {"query": query, "variables": variables})
    if status != 200:
        raise RuntimeError(f"kinoheld answered {status} to {variables}")
    doc = json.loads(text)
    if "errors" in doc:
        raise RuntimeError(f"kinoheld errors for {variables}: {doc['errors']}")
    return doc["data"]


def kinoheld_venues(fx, excluded):
    raw, page = [], 1
    while True:
        batch = gql(fx, KINOHELD_CINEMAS, {"page": page})["cinemas"]["data"]
        raw.extend(batch)
        if len(batch) < 100:
            break
        page += 1
    out = []
    for c in raw:
        why = ("not German" if not german_place((c.get("postcode") or {}).get("postcode"), c.get("latitude"), c.get("longitude"))
               else "closed" if c.get("isClosed") else "hidden" if c.get("isHidden")
               else excluded_reason(c["name"], c.get("isOpenAir"), c.get("isDriveIn")))
        if why:
            excluded[f"kinoheld: {why}"] += 1
            continue
        out.append({"key": f"kinoheld:{c['id']}", "source": "kinoheld", "name": c["name"].strip(),
                    "street": (c.get("street") or "").strip(), "postcode": c["postcode"]["postcode"],
                    "town": c["city"]["name"], "lat": c["latitude"], "lon": c["longitude"],
                    "handle": {"id": c["id"], "path": f"/kino/{c['city']['urlSlug']}/{c['urlSlug']}"}})
    return out


def kinoheld_days(fx, venue_id):
    days, page = [], 1
    while True:
        doc = gql(fx, KINOHELD_SHOWS, {"cinema": venue_id, "page": page})["programShows"]
        days += [berlin_day(s["beginning"]) for s in doc["data"]]
        if not doc["paginatorInfo"]["hasMorePages"] or not doc["data"]:
            return days
        page += 1


def cineamo_json(fx, path):
    status, _, text = fx.get(path)
    if status != 200:
        raise RuntimeError(f"Cineamo answered {status} to {path}")
    return json.loads(text)


def cineamo_venues(fx, excluded):
    first = cineamo_json(fx, "/cinemas?countryCode=DE&page=1")
    pages = [first] + fx.map(lambda p: cineamo_json(fx, f"/cinemas?countryCode=DE&page={p}"),
                             range(2, first["_page_count"] + 1))
    out = []
    for c in (c for page in pages for c in page["_embedded"]["cinemas"]):
        why = ("not German" if not german_place(c.get("postalCode"), c.get("latitude"), c.get("longitude"))
               else excluded_reason(c.get("name") or ""))
        if why:
            excluded[f"cineamo: {why}"] += 1
            continue
        out.append({"key": f"cineamo:{c['id']}", "source": "cineamo", "name": (c.get("name") or "").strip(),
                    "street": (c.get("street") or "").strip(), "postcode": c.get("postalCode") or "",
                    "town": c.get("city") or "", "lat": c["latitude"], "lon": c["longitude"],
                    "handle": {"id": str(c["id"])}})
    return out


def cineamo_days(fx, venue_id):
    first = cineamo_json(fx, f"/showings?cinemaId={venue_id}&page=1")
    pages = [first] + [cineamo_json(fx, f"/showings?cinemaId={venue_id}&page={p}") for p in range(2, first["_page_count"] + 1)]
    return [berlin_day(s["startDatetime"]) for page in pages for s in page["_embedded"]["showings"]
            if s.get("state", "scheduled") == "scheduled"]


KP_DAY = re.compile(r'<section[^>]*data-kino-week-day="(\d{4}-\d{2}-\d{2})"(.*?)</section>', re.S)


def kinoprogramm_days(fx, path, today):
    """Every show day on a venue's kinoprogramm.com week pages, walked until three blank weeks."""
    days, blank, week = [], 0, datetime.date.fromisoformat(today)
    while blank < 3:
        status, _, text = fx.get(f"{path}?datum={week.isoformat()}")
        if status != 200:
            raise RuntimeError(f"kinoprogramm.com answered {status} to {path}")
        found = [d for d, body in KP_DAY.findall(text) for _ in range(body.count("kino-week-time"))]
        days += found
        blank = 0 if found else blank + 1
        week += datetime.timedelta(days=7)
    return days


def filmstarts_days(fx, roster):
    """{theaterId: [show days]} as showtimes.cc/de serves the roster today."""
    by_name = {}
    for v in roster:
        by_name.setdefault(v["displayName"], v["theaterId"])
    days = defaultdict(list)
    for slug in roster_venues.page_slugs():
        status, _, text = fx.get(f"/de/{slug}/api/repertoire")
        if status != 200:
            print(f"  showtimes.cc /de/{slug}: {status}", file=sys.stderr)
            continue
        for film in json.loads(text):
            for showing in film.get("showings", []):
                for cinema in showing.get("cinemas", []):
                    tid = by_name.get(cinema["cinema"])
                    if tid:
                        days[tid] += [showing["date"]] * len(cinema.get("showtimes", []))
    return days


# ------------------------------------------------------------------ identity
def same_site(a, b):
    """Within SAME_SITE_M with names that agree; or one name in one town a little further
    apart -- the two sites geocode a venue to different ends of its block (Göttingen's
    Méliès, Pegnitz's Regina, 300-900 m apart)."""
    distance, score = metres(a, b), kp.score(a["name"], b["name"], a["town"])
    chain = kp.chain_of(a["name"])
    return ((distance <= SAME_SITE_M and (score >= REVIEW_SCORE or (chain and chain == kp.chain_of(b["name"]))))
            or (distance <= SAME_TOWN_M and score >= ACCEPT_SCORE))


def union(cineamo, kinoheld):
    """One venue per site: a kinoheld venue at a Cineamo venue's site joins it."""
    venues = [{**c, "handles": {"cineamo": c["handle"]}} for c in cineamo]
    for k in kinoheld:
        twin = next((v for v in venues if "kinoheld" not in v["handles"]
                     and (SAME_VENUE.get(k["key"]) == v["key"] or same_site(v, k))), None)
        if twin:
            twin["handles"]["kinoheld"] = k["handle"]
        else:
            venues.append({**k, "handles": {"kinoheld": k["handle"]}})
    return venues


def town_forms(town):
    """The spellings one town goes by across sources: "Eberswalde-Finow" is also
    "Eberswalde", "Gießen, Lahn" is "Gießen", "Lübben (Spreewald)" is "Lübben"."""
    whole = town_key(town)
    return {f for f in {whole, whole.split(" ")[0] if "-" in (town or "") else whole} if f}


class FilmstartsIndex:
    """The Filmstarts venues a discovered one may be: by postcode, by town and by street."""

    def __init__(self, venues):
        self.by_postcode, self.by_town, self.by_street = defaultdict(list), defaultdict(list), defaultdict(list)
        self.chains = Counter()
        for v in venues:
            if v["postcode"]:
                self.by_postcode[v["postcode"]].append(v)
            for town in town_forms(v["town"]) | town_forms(v["rosterCity"]):
                self.by_town[town].append(v)
            if re.search(r"\d", v["street"] or ""):
                self.by_street[street_key(v["street"])].append(v)
            if kp.chain_of(v["name"]):
                self.chains[(kp.chain_of(v["name"]), town_key(v["town"]))] += 1

    def candidates(self, k):
        seen = {}
        for v in (self.by_postcode.get(k["postcode"], []) + [x for t in town_forms(k["town"]) for x in self.by_town.get(t, [])]
                  + self.by_street.get(street_key(k["street"]), [])):
            seen[v["theaterId"]] = v
        return list(seen.values())

    @staticmethod
    def same_place(v, k):
        """One postcode, one town under any spelling, or one street in the same postcode area."""
        return (v["postcode"] == k["postcode"]
                or bool((town_forms(v["town"]) | town_forms(v["rosterCity"])) & town_forms(k["town"]))
                or (v["postcode"][:3] == k["postcode"][:3] and street_key(v["street"]) == street_key(k["street"])))

    @staticmethod
    def same_address(v, k):
        """The same street and house number, and a postcode in the same area -- house
        numbers drift ("25" / "25a") less often than the postcode a site files under."""
        return (bool(re.search(r"\d", v["street"] or "")) and street_key(v["street"]) == street_key(k["street"])
                and v["postcode"][:3] == k["postcode"][:3])

    def decide(self, k):
        """(theaterId, how) for the Filmstarts venue `k` is, (None, None) for none, or
        ("?", candidates) when only a human can tell. One address can hold two Filmstarts
        venues -- a Filmhaus's two cinemas, a Kurhaus's two screens -- and then the name
        decides between them."""
        scored = []
        for v in self.candidates(k):
            if not self.same_place(v, k):
                continue
            s = kp.score(v["name"], k["name"], k["town"])
            chain = kp.chain_of(k["name"])
            if chain and chain == kp.chain_of(v["name"]) and self.chains[(chain, town_key(k["town"]))] == 1:
                s = max(s, ACCEPT_SCORE)
            scored.append((s, self.same_address(v, k), v))
        # An open-air screen run by a cinema is filed under the cinema's address
        # (Aschaffenburg's campus open-air at the Casino's): an address never makes the
        # one the other.
        at_address = sorted([x for x in scored if x[1] and kp.is_event(x[2]["name"]) == kp.is_event(k["name"])],
                            key=lambda x: -x[0])
        if len(at_address) == 1 or (at_address and at_address[0][0] - at_address[1][0] >= 0.15):
            return at_address[0][2]["theaterId"], "same address"
        best = max(scored, key=lambda x: x[0], default=None)
        if best and best[0] >= ACCEPT_SCORE and not at_address:
            return best[2]["theaterId"], f"name {best[0]:.2f}"
        review = [(round(s, 2), v["theaterId"], v["name"], v["street"], v["postcode"]) for s, address, v in scored
                  if s >= REVIEW_SCORE or address]
        return ("?", review) if review else (None, None)


def load_filmstarts():
    addresses = json.load(open(os.path.join(DATA, "filmstarts-addresses.json")))
    venues = []
    sweep = {v["theaterId"]: v for v in json.load(open(os.path.join(DATA, "filmstarts-sweep.json")))}
    for c in roster_venues.load():
        a = addresses.get(c["theaterId"]) or sweep.get(c["theaterId"], {})
        venues.append({"theaterId": c["theaterId"], "name": c["name"], "town": a.get("town") or c["city"],
                       "rosterCity": c["city"], "postcode": a.get("postcode", ""), "street": a.get("street", ""),
                       "rostered": True})
    rostered = {v["theaterId"] for v in venues}
    for v in sweep.values():
        if v["theaterId"] not in rostered:
            venues.append({**v, "rosterCity": v["town"], "rostered": False})
    return venues


# -------------------------------------------------------------------- choice
def rank(shows):
    """A venue's sources, most complete first: shows in the window, then in all, then SOURCES order."""
    return sorted(shows, key=lambda s: (-shows[s][0], -shows[s][1], SOURCES.index(s)))


def tolerance(count):
    """How many more shows a source must list to count as more complete: a source that lists
    one more show today must not take a venue over -- each switch of source re-keys what the
    venue's films are matched by."""
    return max(2, -(-count // 10))


def beats(challenger, holder):
    """Is a [window, horizon] count more complete than another, beyond the tolerance? More
    shows in the window, or as many there and more in the whole horizon."""
    (cw, ch), (hw, hh) = challenger, holder
    return cw - hw > tolerance(hw) or (abs(cw - hw) <= tolerance(hw) and cw >= hw and ch - hh > tolerance(hh))


def choose(shows, order=None):
    """(primary, fallbacks, rawWinner) from a venue's {source: [window, horizon]}.

    `rawWinner` is the plain most-complete source. Without an `order`, the primary is
    decided in SOURCES order: each source takes the venue from the one before only when it
    `beats` it, so a near-tie stays with the source with the richer listing (Filmstarts:
    posters, film ids; then Cineamo and kinoheld; kinoprogramm.com last). The rest follow,
    most complete first."""
    ranked = rank(shows)
    raw = ranked[0] if ranked else None
    if order is None:
        listed = [s for s in SOURCES if s in shows]
        primary = listed[0] if listed else None
        for s in listed[1:]:
            if beats(shows[s], shows[primary]):
                primary = s
        return primary, [s for s in ranked if s != primary], raw
    listed = [s for s in order if s in shows]
    live = [s for s in listed if shows[s][1] > 0] or listed
    primary = live[0] if live else None
    return primary, [s for s in listed if s != primary], raw


def summarise(rows):
    """The split over the ROSTERED venues -- the ones whose Filmstarts listing we serve
    today, so whose Filmstarts count is measured -- and the new ones apart."""
    with_shows = [r for r in rows if not r.get("found") and any(c[0] > 0 for c in r["shows"].values())]
    wins = Counter(r["rawWinner"] for r in with_shows)
    primaries = Counter(r["primary"] for r in rows)
    filmstarts_only = [
        {"theaterId": r["filmstarts"], "name": r["name"], "town": r["town"], "next14": r["shows"]["filmstarts"][0],
         "seasonal": excluded_reason(r["name"]) is not None}
        for r in rows if not r.get("found") and r["shows"]["filmstarts"][0] > 0 and "cineamo" not in r["shows"]]
    return {
        "venuesWithShows": len(with_shows),
        "rawWinners": dict(wins.most_common()),
        "cineamoWinShare": round(wins["cineamo"] / len(with_shows), 3) if with_shows else 0,
        "primaries": dict(primaries.most_common()),
        "newVenues": sum(1 for r in rows if r.get("found")),
        "newVenuesFromFilmstartsSweep": sum(1 for r in rows if r.get("found") and r.get("filmstarts")),
        "newVenuesBySource": dict(Counter(r["primary"] for r in rows if r.get("found")).most_common()),
        "filmstartsOnly": sorted(filmstarts_only, key=lambda v: -v["next14"]),
    }


# ---------------------------------------------------------------------- main
def main():
    today = datetime.datetime.now(BERLIN).date().isoformat()
    cache = cache_dir(f"sources-{today}", os.environ, pathlib.Path.home(), "SOURCES_CACHE_DIR")
    cineamo_fx = PoliteFetcher("https://api.cineamo.com", os.path.join(cache, "cineamo"), workers=6, max_rps=10.0)
    kinoheld_fx = PoliteFetcher("https://next-live.kinoheld.de", os.path.join(cache, "kinoheld"), workers=5, max_rps=5.0)
    kp_fx = PoliteFetcher(kp.BASE, os.path.join(cache, "kinoprogramm"), workers=4, max_rps=3.0,
                          headers={"User-Agent": kp.UA, "Accept-Language": "de-DE,de;q=0.9"})
    site_fx = PoliteFetcher("https://showtimes.cc", os.path.join(cache, "showtimes"), workers=2, max_rps=2.0)

    excluded = Counter()
    cineamo = cineamo_venues(cineamo_fx, excluded)
    kinoheld = kinoheld_venues(kinoheld_fx, excluded)
    print(f"Cineamo venues: {len(cineamo)}; kinoheld venues: {len(kinoheld)}; excluded: {dict(excluded)}")

    cineamo_counts = dict(zip([c["key"] for c in cineamo],
                              cineamo_fx.map(lambda c: counts(cineamo_days(cineamo_fx, c["handle"]["id"]), today), cineamo)))
    kinoheld_counts = dict(zip([k["key"] for k in kinoheld],
                               kinoheld_fx.map(lambda k: counts(kinoheld_days(kinoheld_fx, k["handle"]["id"]), today), kinoheld)))
    cineamo = [c for c in cineamo if cineamo_counts[c["key"]][1] > 0]
    kinoheld = [k for k in kinoheld if kinoheld_counts[k["key"]][1] > 0]
    discovered = [d for d in union(cineamo, kinoheld) if d["key"] not in NOT_A_VENUE]
    print(f"with an upcoming show: Cineamo {len(cineamo)}, kinoheld {len(kinoheld)}; one per site: {len(discovered)}")

    roster = roster_venues.load()
    in_roster = roster_venues.keys()
    index = FilmstartsIndex(load_filmstarts())
    by_theater, new, undecided = {}, [], []
    for d in discovered:
        tid, how = (DECIDED[d["key"]], "hand-checked") if d["key"] in DECIDED else index.decide(d)
        if tid == "?":
            undecided.append((d, how))
        elif tid is None:
            new.append(d)
        elif tid in by_theater:
            # Two discovered venues on one Filmstarts venue: keep the sources the first lacks.
            for source, handle in d["handles"].items():
                by_theater[tid]["handles"].setdefault(source, handle)
        else:
            by_theater[tid] = {**d, "how": how}
    for d, rows in undecided:
        print(f"REVIEW {d['key']} {d['name']} | {d['street']} {d['postcode']} {d['town']}")
        for candidate in sorted(rows, reverse=True):
            print(f"         {candidate}")
    if undecided:
        print(f"{len(undecided)} venues need a verdict in DECIDED; nothing written.", file=sys.stderr)
        sys.exit(1)

    kp_paths = json.load(open(os.path.join(DATA, "kinoprogramm.json")))
    kp_counts = dict(zip(kp_paths, kp_fx.map(lambda tid: counts(kinoprogramm_days(kp_fx, kp_paths[tid], today), today), kp_paths)))
    fs_days = filmstarts_days(site_fx, roster)
    sweep = {v["theaterId"]: v for v in json.load(open(os.path.join(DATA, "filmstarts-sweep.json")))}

    rows = []
    rostered_ids = {v["theaterId"] for v in roster}
    for v in roster + [{"theaterId": t, "name": s["name"], "city": s["town"]} for t, s in sweep.items() if t not in rostered_ids]:
        tid = v["theaterId"]
        d = by_theater.get(tid, {"handles": {}})
        shows = {}
        # A venue the sweep recovered is not served yet, so its Filmstarts listing is
        # counted off its page's advertised days only: 1 a day, a floor.
        shows["filmstarts"] = (counts(sweep[tid]["showDays"], today) if tid in sweep
                               else counts(fs_days.get(tid, []), today))
        handles = d["handles"]
        if "cineamo" in handles:
            shows["cineamo"] = cineamo_counts[f"cineamo:{handles['cineamo']['id']}"]
        if "kinoheld" in handles:
            shows["kinoheld"] = kinoheld_counts[f"kinoheld:{handles['kinoheld']['id']}"]
        if tid in kp_counts:
            shows["kinoprogramm"] = kp_counts[tid]
        rows.append(row(v["name"], d.get("town") or v["city"], tid, handles, kp_paths.get(tid), shows, d.get("how"),
                        place={k: d[k] for k in ("street", "postcode", "lat", "lon")} if "lat" in d else None,
                        found=tid in sweep, rostered=tid in in_roster))
    for d in new:
        shows = {}
        if "cineamo" in d["handles"]:
            shows["cineamo"] = cineamo_counts[f"cineamo:{d['handles']['cineamo']['id']}"]
        if "kinoheld" in d["handles"]:
            shows["kinoheld"] = kinoheld_counts[f"kinoheld:{d['handles']['kinoheld']['id']}"]
        key = next(f"{s}:{h['id']}" for s, h in sorted(d["handles"].items()))
        rows.append(row(d["name"], d["town"], None, d["handles"], None, shows, None,
                        place={k: d[k] for k in ("street", "postcode", "lat", "lon")}, rostered=key in in_roster))

    for fx in (cineamo_fx, kinoheld_fx, kp_fx, site_fx):
        fx.report()
    rows.sort(key=lambda r: (r["town"], r["name"], r["key"]))
    summary = summarise(rows)
    with open(OUT, "w") as f:
        json.dump({"measuredOn": today, "windowDays": WINDOW_DAYS, "defaultOrder": list(DEFAULT_ORDER) if DEFAULT_ORDER else None,
                   "summary": summary, "venues": rows}, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(json.dumps({k: v for k, v in summary.items() if k != "filmstartsOnly"}, indent=1))
    print(f"Filmstarts-only venues: {len(summary['filmstartsOnly'])}")
    for v in summary["filmstartsOnly"][:40]:
        print(f"  {v['next14']:4d}  {v['theaterId']} {v['name']} ({v['town']}){'  [seasonal]' if v['seasonal'] else ''}")


def row(name, town, theater_id, handles, kp_path, shows, how, place=None, found=True, rostered=False):
    primary, fallbacks, raw = choose(shows, DEFAULT_ORDER)
    key = theater_id or next(f"{s}:{h['id']}" for s, h in sorted(handles.items()))
    # `found`: the venue is one this harvest found -- not in the roster Filmstarts' directory gave.
    # `rostered`: venues.json has it (add_found_venues.py adds the found ones).
    out = {"key": key, "name": name, "town": town, "found": found, "rostered": rostered}
    if place:
        out.update(place)
    if theater_id:
        out["filmstarts"] = theater_id
    if "cineamo" in handles:
        out["cineamo"] = handles["cineamo"]["id"]
    if "kinoheld" in handles:
        out["kinoheld"] = handles["kinoheld"]
    if kp_path:
        out["kinoprogramm"] = kp_path
    if how:
        out["matchedBy"] = how
    if theater_id in OWN_SITES:
        out["ownSite"] = OWN_SITES[theater_id]
        primary, fallbacks = "ownSite", [s for s in [primary, *fallbacks] if s]
    out.update({"shows": shows, "primary": primary, "fallbacks": fallbacks, "rawWinner": raw})
    return out


if __name__ == "__main__":
    main()
