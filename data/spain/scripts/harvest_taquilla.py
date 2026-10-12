#!/usr/bin/env python3
"""Map Spanish roster venues to their taquilla.com venue pages, and pick the ones
Taquilla should serve -> data/spain/taquilla.json.

taquilla.com is a ticket aggregator: each venue page (`/<city>/<venue-slug>`) is
server-rendered, and its date picker reads two JSON endpoints the scraper uses too
(services.cinemas.es.TaquillaClient):

  /json2/search/calendar?t10venue=<id>&t10type=15&t10num=-1   -> {day: count} for every
                                                                  day the venue sells
  /json2/search/events?t10venue=<id>&t10d=<day>&t10type=15&t10num=-1  -> that day's films
                                                                  and sessions

There is no venue index, so the harvest walks the city carteleras
(`/cartelera/<city>`, each linking its venues and its neighbouring cities) from
every roster town and province outwards, then reads each venue page found for its
id, name and address.

A roster venue matches a Taquilla venue only when they share a POSTCODE (the roster's
from SensaCine's schema.org address, venue-addresses.json) AND their names agree
(`name_score`, chain words and filler stripped) -- or, with no postcode in common,
when the names agree outright in the same town. A roster venue with two equally good
candidates is left out and printed for review; so is a Taquilla venue claimed twice.

Which venues Taquilla SERVES is measured, not assumed: a match becomes the venue's
primary only when showtimes.cc/es serves the venue no showtime today (SensaCine has no
programme for it) and Taquilla's calendar advertises at least one day. Everything else
stays on its current source.

Usage:
  python3 data/spain/scripts/harvest_taquilla.py   # -> data/spain/taquilla.json
  python3 data/spain/scripts/generate_own_sources.py
Cached under ~/.cache/kinowo/taquilla-<date>; 5 workers, <= 2 req/s, halving on 429/503.
"""

import datetime
import html
import json
import os
import pathlib
import re
import sys
import unicodedata
from collections import Counter, defaultdict

HERE = pathlib.Path(__file__).resolve().parent
DATA = HERE.parent
sys.path.insert(0, str(DATA.parent / "germany" / "scripts"))
from polite_fetch import PoliteFetcher, cache_dir  # noqa: E402

BASE = "https://www.taquilla.com"
OUT = DATA / "taquilla.json"

# Words that say "a cinema" or a format, not WHICH cinema.
FILLER = {
    "cine", "cines", "cinema", "cinemas", "multicines", "multicine", "3d", "4d", "imax", "luxe",
    "de", "del", "la", "las", "el", "los", "y", "en", "centro", "comercial", "cc", "c", "sa",
    "antiguos", "antiguo", "premium", "cite", "dreams", "the", "mk2", "yelmo", "cinesa",
    "kinepolis", "artesiete", "ocine", "odeon", "multicinema", "teatro", "sala", "salas",
    "madrid", "barcelona",
}
CHAINS = ("cinesa", "yelmo", "mk2", "kinepolis", "artesiete", "odeon", "ocine", "cinebox",
          "abc", "cinemax", "neocine", "aana", "zoco", "al-andalus", "cinesur", "megarama", "axion",
          "imf", "golem", "renoir", "verdi")

VENUE_LINK = re.compile(r'href="https://www\.taquilla\.com/([a-z0-9-]+)/([a-z0-9-]+)"')
CITY_LINK = re.compile(r'href="(?:https://www\.taquilla\.com)?/cartelera/([a-z0-9-]+)"')
VENUE_ID = re.compile(r'name="t10venue" value="(\d+)"')
TITLE = re.compile(r"<title>([^<]*?), [^,<]* - Cartelera", re.S)
ADDRESS = re.compile(r'<p class="ent-cine__direccion">([^<]*)</p>')
POSTCODE = re.compile(r"\b(\d{5})\b")
NOT_VENUES = {"cartelera", "entradas", "cine", "varios-recintos", "teatro", "conciertos", "u", "app",
              "themes", "data", "json2", "ws", "urlway", "evento", "eventos"}


def fold(text):
    text = unicodedata.normalize("NFKD", text.lower())
    return "".join(ch for ch in text if not unicodedata.combining(ch))


def slug(text):
    return re.sub(r"[^a-z0-9]+", "-", fold(text)).strip("-")


def tokens(name):
    return {t for t in re.split(r"[^a-z0-9]+", fold(name)) if t and t not in FILLER}


def chain_of(name):
    folded = fold(name)
    return next((c for c in CHAINS if re.search(rf"\b{re.escape(c)}\b", folded)), None)


def name_score(a, b):
    """How far two venue names agree, as (containment, overlap): the share of the
    shorter name's distinguishing words the other has too, then the share of both
    names' words they have in common -- chain names and filler stripped. (0, 0) when
    the two belong to different chains. The overlap breaks a tie "Cinesa Diagonal"
    would otherwise have between "Cinesa Diagonal" and "Cinesa Diagonal Mar 3D"."""
    ca, cb = chain_of(a), chain_of(b)
    if ca and cb and ca != cb:
        return (0.0, 0.0)
    ta, tb = tokens(a), tokens(b)
    if not ta or not tb:
        same = 1.0 if ca and ca == cb and not ta and not tb else 0.0
        return (same, same)
    common = len(ta & tb)
    return (common / min(len(ta), len(tb)), common / len(ta | tb))


def parse_venue_page(text):
    """(venue id, name, postcode, address) off a venue page, or None for any other page."""
    vid = VENUE_ID.search(text)
    title = TITLE.search(text)
    if not vid or not title:
        return None
    address = ADDRESS.search(text)
    address = html.unescape(address.group(1)).strip() if address else ""
    postcodes = POSTCODE.findall(address)
    return vid.group(1), html.unescape(title.group(1)).strip(), postcodes[0] if postcodes else None, address


def match(roster, taquilla):
    """{theaterId: taquilla path} for the confident matches, plus the review lines."""
    by_postcode = defaultdict(list)
    for path, v in taquilla.items():
        if v["postcode"]:
            by_postcode[v["postcode"]].append(path)
    by_town = defaultdict(list)
    for path, v in taquilla.items():
        by_town[path.split("/")[1]].append(path)

    chosen, review = {}, []
    for r in roster:
        candidates = [(name_score(r["name"], taquilla[p]["name"]), p) for p in by_postcode.get(r.get("postcode"), [])]
        candidates = [c for c in candidates if c[0][0] >= 0.5]
        if not candidates:
            # No postcode in common (the two sites disagree on it now and then): the names
            # must agree outright, in the town Taquilla files the venue under.
            candidates = [(name_score(r["name"], taquilla[p]["name"]), p) for p in by_town.get(slug(r["town"]), [])]
            candidates = [c for c in candidates if c[0][0] >= 1.0]
        if not candidates:
            continue
        candidates.sort(reverse=True)
        if len(candidates) > 1 and candidates[0][0] == candidates[1][0]:
            review.append(f"AMBIGUOUS {r['theaterId']} {r['name']!r}: " +
                          ", ".join(f"{taquilla[p]['name']!r} {p}" for _, p in candidates))
            continue
        chosen[r["theaterId"]] = candidates[0][1]
    claimed = Counter(chosen.values())
    for tid, path in list(chosen.items()):
        if claimed[path] > 1:
            review.append(f"CLAIMED TWICE {path}: {tid}")
            del chosen[tid]
    return chosen, review


def roster_venues():
    provinces = json.loads((DATA / "provinces.json").read_text())
    addresses = json.loads((DATA / "venue-addresses.json").read_text())
    towns = json.loads((DATA / "town-coords.json").read_text())["venues"]
    out = []
    for province in provinces:
        for c in province["cinemas"]:
            tid = c.get("theaterId")
            if not tid:
                continue
            out.append({"theaterId": tid, "name": c["displayName"], "province": province["name"],
                        "town": towns.get(c["displayName"], c["town"]),
                        "postcode": addresses.get(tid, {}).get("postalCode")})
    return out


def served_by_day(site_fx):
    """{displayName: Counter(day -> showtimes)} as showtimes.cc/es serves the roster today."""
    pages = json.loads((DATA / "pages.json").read_text())["pages"]
    served = defaultdict(Counter)
    for page in pages:
        status, text = 0, ""
        for s in (page["slug"], page.get("qualifiedSlug")):
            if s:
                status, _, text = site_fx.get(f"/es/{s}/api/repertoire")
                if status == 200:
                    break
        if status != 200:
            print(f"  showtimes.cc /es/{page['slug']}: {status}", file=sys.stderr)
            continue
        for film in json.loads(text):
            for showing in film.get("showings", []):
                for cinema in showing.get("cinemas", []):
                    served[cinema["cinema"]][showing["date"]] += len(cinema.get("showtimes", []))
    return served


def decide(served, taquilla_days, taquilla_sessions, horizon):
    """Whether Taquilla should be the venue's primary, and why -- `served` and
    `taquilla_sessions` are {day: sessions}, `taquilla_days` every day its calendar
    sells, `horizon` the last day of the near fortnight (ISO).

    Taquilla serves a venue SensaCine lists nothing for, once it sells a day. Otherwise
    it must be a SUPERSET: reach past SensaCine's last day, AND carry at least
    SUPERSET_SHARE of SensaCine's sessions on the days SensaCine lists (the near
    fortnight, where a thinner feed would cost the most) -- else a longer tail would buy
    the far future with the near one. Cinesa, whose Taquilla calendar ends days before
    SensaCine's, never qualifies."""
    if not taquilla_days:
        return False, "Taquilla sells no day"
    if not served:
        return True, "SensaCine lists nothing"
    if max(taquilla_days) <= max(served):
        return False, f"Taquilla ends {max(taquilla_days)}, SensaCine {max(served)}"
    near = [d for d in sorted(served) if d <= horizon]
    ours = sum(served[d] for d in near)
    theirs = sum(taquilla_sessions.get(d, 0) for d in near)
    if theirs < SUPERSET_SHARE * ours:
        return False, f"Taquilla {theirs} sessions vs SensaCine {ours} on the near days"
    return True, f"Taquilla reaches {max(taquilla_days)} vs SensaCine {max(served)}, {theirs} vs {ours} near"


SUPERSET_SHARE = 0.95
NEAR_DAYS = 14


def crawl(fx, seeds):
    """Every venue page path the carteleras reachable from `seeds` link: {path: listed city}."""
    seen, frontier, venues = set(), list(dict.fromkeys(seeds)), {}
    while frontier:
        batch = [c for c in frontier if c not in seen]
        seen.update(batch)
        frontier = []
        for city, (status, _, text) in zip(batch, fx.map(lambda c: fx.get(f"/cartelera/{c}"), batch)):
            if status != 200:
                continue
            for c in CITY_LINK.findall(text):
                if c not in seen:
                    frontier.append(c)
            for town, venue in VENUE_LINK.findall(text):
                if town not in NOT_VENUES:
                    venues[f"/{town}/{venue}"] = town
        frontier = list(dict.fromkeys(frontier))
        print(f"  carteleras {len(seen)}, venue links {len(venues)}", file=sys.stderr)
    return venues


def main():
    today = datetime.date.today().isoformat()
    cache = cache_dir(f"taquilla-{today}", os.environ, pathlib.Path.home(), "TAQUILLA_CACHE_DIR")
    fx = PoliteFetcher(BASE, os.path.join(cache, "taquilla"), workers=5, max_rps=2.0)
    site_fx = PoliteFetcher("https://showtimes.cc", os.path.join(cache, "showtimes"), workers=2, max_rps=2.0)

    roster = roster_venues()
    seeds = [slug(r["town"]) for r in roster] + [slug(r["province"]) for r in roster]
    links = crawl(fx, seeds)

    taquilla = {}
    for path, (status, _, text) in zip(links, fx.map(fx.get, list(links))):
        parsed = parse_venue_page(text) if status == 200 else None
        if parsed:
            vid, name, postcode, address = parsed
            taquilla[path] = {"venueId": vid, "name": name, "postcode": postcode, "address": address}
    print(f"taquilla venues: {len(taquilla)} of {len(links)} links", file=sys.stderr)

    chosen, review = match(roster, taquilla)
    served = served_by_day(site_fx)
    by_tid = {r["theaterId"]: r for r in roster}

    def calendar(tid):
        vid = taquilla[chosen[tid]]["venueId"]
        status, _, text = fx.get(f"/json2/search/calendar?lang=es&t10venue={vid}&t10type=15&t10num=-1")
        days = (json.loads(text).get("objects") or {}) if status == 200 else {}
        return sorted(d for d, n in days.items() if n > 0)

    def sessions(job):
        vid, day = job
        status, _, text = fx.get(f"/json2/search/events?lang=es&t10venue={vid}&t10d={day}"
                                 "&t10type=15&t10ptype=2&t10num=-1")
        return sum(len(e.get("sessions", [])) for e in json.loads(text).get("objects", [])) if status == 200 else 0

    tids = sorted(chosen)
    days = dict(zip(tids, fx.map(calendar, tids)))
    horizon = (datetime.date.today() + datetime.timedelta(days=NEAR_DAYS)).isoformat()
    # Only a venue whose Taquilla calendar outruns what we serve can gain: fetch its days
    # (the near fortnight SensaCine lists, to compare, and the tail, to count the gain).
    jobs = []
    for tid in tids:
        ours = served[by_tid[tid]["name"]]
        if days[tid] and (not ours or max(days[tid]) > max(ours)):
            vid = taquilla[chosen[tid]]["venueId"]
            jobs += [(tid, vid, d) for d in days[tid] if d > max(ours, default="") or (d in ours and d <= horizon)]
    counts = dict(zip(jobs, fx.map(lambda j: sessions(j[1:]), jobs)))
    by_venue = defaultdict(dict)
    for (tid, _, day), n in counts.items():
        by_venue[tid][day] = n

    venues, kept = {}, []
    for tid in tids:
        r, t = by_tid[tid], taquilla[chosen[tid]]
        last = max(served[r["name"]], default=None)
        serve, why = decide(served[r["name"]], days[tid], by_venue[tid], horizon)
        gained = sum(n for d, n in by_venue[tid].items() if last is None or d > last)
        row = {"name": r["name"], "taquillaName": t["name"], "path": chosen[tid], "venueId": t["venueId"],
               "sensacineLastDay": last, "taquillaLastDay": days[tid][-1] if days[tid] else None,
               "sessionsGained": gained, "why": why}
        (venues.__setitem__(tid, row) if serve else kept.append(row))

    for line in review:
        print(line)
    for row in sorted(kept, key=lambda r: r["name"]):
        print(f"KEPT {row['name']!r} -> {row['taquillaName']!r}: {row['why']}")
    for tid, row in sorted(venues.items(), key=lambda kv: kv[1]["name"]):
        print(f"TAQUILLA {tid} {row['name']!r} -> {row['taquillaName']!r} {row['path']}: {row['why']}, "
              f"+{row['sessionsGained']} sessions")
    print(f"matched {len(chosen)} roster venues; Taquilla serves {len(venues)}, "
          f"+{sum(r['sessionsGained'] for r in venues.values())} sessions past SensaCine's last day; "
          f"{len(kept)} stay where they are")
    previous = json.loads(OUT.read_text()) if OUT.exists() else {}
    OUT.write_text(json.dumps({
        "_comment": previous.get("_comment", []),
        "measured": today,
        "venues": venues,
    }, ensure_ascii=False, indent=1) + "\n")
    fx.report()
    site_fx.report()


if __name__ == "__main__":
    main()
