#!/usr/bin/env python3
"""
Map the German cinema roster (data/germany/regions.json, keyed by Filmstarts
theaterId) to kinoprogramm.com venue pages.

Method:
  1. Discovery. For every distinct roster city, collect kinoprogramm venue
     ids from: the town page /kino/<slug>/ (umlauts -> ae/oe/ue, ss; plus
     "(...)"/district/Sankt variants and the slug the site's own city search
     /suchApi?a=citysuche returns), whose "Kino auswaehlen" <select> lists
     the town's venues with a programme; the cinema autocomplete
     /jsonApi?a=autocompleateCinema&stadt=<town> (also lists venues without
     a current programme) for the city name, the town page titles and the
     site's own town spellings from a=autocompleateTown ("Nienburg" ->
     "Nienburg (Weser)"); for cities still empty, disambiguated slugs seen
     in links elsewhere ("esslingen" is a village near Tuttlingen; Esslingen
     am Neckar is "esslingen-am-neckar"). The autocomplete caps at 10 rows,
     so in big towns each roster venue is also searched by its most
     distinctive word.
  2. Verification. Every candidate that scores >= PRESCORE against some
     roster venue of the city has its venue page fetched (the canonical path
     from suchApi&kino=<id> when the linked one 404s, e.g. names with quotes).
     A candidate is the city's own only if its JSON-LD addressLocality names
     the roster city AND its geo lies within MAX_CITY_KM of the city in
     data/germany/city-coords.json. Same-named towns elsewhere (Landau an
     der Isar vs Landau in der Pfalz) fail the geo test.
  3. Matching. Names are normalised (HTML entities, casefold, umlaut/ss
     transliteration, punctuation stripped); the noise words kino /
     filmtheater / cinema and the city's own name are dropped for scoring
     only. Open-air / drive-in / pop-up screens never match a regular
     cinema. Chain venues (CineStar, CinemaxX, Cineplex, UCI, Kinopolis, ...)
     match only a verified-local candidate of the same chain, i.e. the same
     town's branch.

Precision beats recall: a wrong mapping serves another cinema's showtimes.
A match is accepted only above ACCEPT_SCORE with a clear margin over the
runner-up; if two roster venues want the same kinoprogramm venue, both are
dropped unless one is an exact match and the other is not close. Everything
not accepted goes to kinoprogramm-unmatched.json with its reason. The run
ends by printing SPOT_CHECK random accepted matches next to the address on
their venue page, for a human to confirm.

Politeness: 4 workers, <=3 req/s overall, concurrency halves and backs off on
429/503. Every response is cached (gzip) under CACHE_DIR, so reruns are free.

Run: python3 data/germany/scripts/harvest_kinoprogramm.py
"""

import gzip
import hashlib
import html
import json
import math
import os
import random
import re
import sys
import threading
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from difflib import SequenceMatcher

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
REGIONS = os.path.join(DATA, "regions.json")
COORDS = os.path.join(DATA, "city-coords.json")
OUT = os.path.join(DATA, "kinoprogramm.json")
OUT_UNMATCHED = os.path.join(DATA, "kinoprogramm-unmatched.json")
CACHE_DIR = os.environ.get(
    "KP_CACHE_DIR",
    "/private/tmp/claude-501/-Users-pawel-projects-movies/"
    "e61ce549-706a-4f9b-8a39-77e0133b9d6f/scratchpad/kp-harvest",
)

BASE = "https://www.kinoprogramm.com"
UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/126 Safari/537.36")
WORKERS = 4
MAX_RPS = 3.0

ACCEPT_SCORE = 0.86
MARGIN = 0.08
MAX_CITY_KM = 25.0
BIG_CITY_KM = 35.0          # cities over 1M inhabitants sprawl further
SPOT_CHECK = 30

NOISE = {"kino", "kinos", "kinowelt", "filmtheater", "filmtheatre", "cinema", "cinemas",
         "und", "e", "v", "ev", "gmbh", "the"}
# Only true multi-site operators are enforced as "same town's branch".
CHAIN_BRANDS = {"cinestar", "cinemaxx", "cineplex", "uci", "kinopolis",
                "cineworld", "kinostar", "traumpalast", "cinemotion",
                "cineparc", "yorck", "cinecitta", "megaplex", "cineplace",
                "cinemagnum"}


# ---------------------------------------------------------------- fetching
class Fetcher:
    """Polite cached fetcher: fixed worker pool, global rate limit, adaptive
    concurrency (halved on 429/503)."""

    def __init__(self):
        os.makedirs(os.path.join(CACHE_DIR, "pages"), exist_ok=True)
        self.lock = threading.Lock()
        self.next_slot = 0.0
        self.limit = WORKERS
        self.active = 0
        self.cond = threading.Condition()
        self.requests = 0
        self.cache_hits = 0
        self.throttled = 0
        self.failures = 0
        self.bytes = 0
        self.started = time.time()

    def _key(self, path):
        safe = re.sub(r"[^a-zA-Z0-9._-]+", "_", path.strip("/"))[:120]
        digest = hashlib.sha1(path.encode()).hexdigest()[:10]
        return os.path.join(CACHE_DIR, "pages", f"{safe}-{digest}")

    def _throttle(self):
        with self.lock:
            now = time.time()
            slot = max(now, self.next_slot)
            self.next_slot = slot + 1.0 / MAX_RPS
        if slot > now:
            time.sleep(slot - now)

    def get(self, path):
        """Return (status, final_path, text). Cached on disk, 404s included."""
        key = self._key(path)
        if os.path.exists(key + ".json"):
            with open(key + ".json") as f:
                meta = json.load(f)
            text = ""
            if os.path.exists(key + ".html.gz"):
                with gzip.open(key + ".html.gz", "rt", encoding="utf-8") as f:
                    text = f.read()
            with self.lock:
                self.cache_hits += 1
            return meta["status"], meta["final"], text

        attempt = 0
        while True:
            with self.cond:
                while self.active >= self.limit:
                    self.cond.wait()
                self.active += 1
            try:
                self._throttle()
                status, final, text = self._fetch(path)
            finally:
                with self.cond:
                    self.active -= 1
                    self.cond.notify_all()
            if status in (429, 503) or status < 0:
                attempt += 1
                if status in (429, 503):
                    with self.cond:
                        self.throttled += 1
                        self.limit = max(1, self.limit // 2)
                    print(f"  throttled {status} on {path}; concurrency -> "
                          f"{self.limit}", file=sys.stderr)
                if attempt > 4:
                    # Not cached: the next rerun tries again.
                    print(f"  giving up on {path}: {status}", file=sys.stderr)
                    with self.lock:
                        self.failures += 1
                    return status, path, ""
                time.sleep(min(60, 2 ** attempt * 2))
                continue
            break

        with open(key + ".json", "w") as f:
            json.dump({"path": path, "status": status, "final": final}, f)
        if text:
            with gzip.open(key + ".html.gz", "wt", encoding="utf-8") as f:
                f.write(text)
        return status, final, text

    def _fetch(self, path):
        url = BASE + urllib.parse.quote(path, safe="/?=&%-._~")
        req = urllib.request.Request(url, headers={
            "User-Agent": UA, "Accept-Language": "de-DE,de;q=0.9"})
        with self.lock:
            self.requests += 1
            if self.requests % 100 == 0:
                print(f"  ... {self.requests} requests, "
                      f"{self.requests / (time.time() - self.started):.2f} req/s")
        try:
            with urllib.request.urlopen(req, timeout=40) as resp:
                body = resp.read()
                final = urllib.parse.urlsplit(resp.geturl())
                final_path = final.path + ("?" + final.query if final.query else "")
                with self.lock:
                    self.bytes += len(body)
                return resp.status, final_path, body.decode("utf-8", "replace")
        except urllib.error.HTTPError as e:
            return e.code, path, ""
        except Exception as e:  # network hiccup: retried by caller
            print(f"  error {path}: {e}", file=sys.stderr)
            return -1, path, ""

    def map(self, fn, items):
        with ThreadPoolExecutor(WORKERS) as pool:
            return list(pool.map(fn, items))

    def report(self):
        elapsed = time.time() - self.started
        rate = self.requests / elapsed if elapsed else 0
        print(f"requests: {self.requests} network, {self.cache_hits} cache hits,"
              f" {self.bytes / 1e6:.1f} MB in {elapsed:.0f}s -> {rate:.2f} req/s;"
              f" 429/503: {self.throttled}; gave up: {self.failures}")


# -------------------------------------------------------------- normalising
TRANSLIT = str.maketrans({"ä": "ae", "ö": "oe", "ü": "ue", "ß": "ss",
                          "Ä": "ae", "Ö": "oe", "Ü": "ue"})


def fold(s):
    s = html.unescape(html.unescape(s or "")).translate(TRANSLIT).casefold()
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.replace("&", " ")
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def slugify(s):
    return fold(s).replace(" ", "-")


def city_tokens(city):
    """Every word of the city's name, "(...)" qualifier included."""
    return set(fold(city).split())


NOISE_SUFFIXES = ("filmtheater", "kinos", "kino", "cinema")
# Tokens that say nothing about WHICH venue: a containment match needs at
# least one token outside this set.
GENERIC = {"open", "air", "openair", "am", "im", "an", "der", "die", "das", "in",
           "auf", "lichtspiele", "theater", "film", "filmbuehne", "studio",
           "center", "centre", "sommerkino", "freiluftkino", "autokino",
           "kommunales", "kommunale", "stadtkino", "programmkino", "filmclub",
           "filmkunst", "neue", "neues", "kleines", "alte", "altes", "halle",
           "saal", "haus", "st"}
# Seasonal / pop-up formats: never map one onto a regular cinema. Matched
# as word stems so "Sommernachtskino" and "Open-Air-Kino" both count.
EVENT_STEMS = ("openair", "freiluft", "sommerkino", "sommernacht", "autokino",
               "strandkorb", "strandkino", "kinomobil", "drivein", "mondschein",
               "sommerfilm", "kinosommer", "filmnaechte", "filmnacht")


def core_tokens(name, city):
    ctoks = city_tokens(city)
    out = []
    for t in fold(name).split():
        if t in NOISE or t in ctoks:
            continue
        for suf in NOISE_SUFFIXES:
            if t.endswith(suf) and len(t) > len(suf) + 3:
                t = t[:-len(suf)]
                break
        out.append(t)
    return out


def core(name, city):
    return " ".join(core_tokens(name, city))


def is_event(name):
    f = fold(name)
    squashed = f.replace(" ", "")
    return " open air " in f" {f} " or any(stem in squashed for stem in EVENT_STEMS)


def chain_of(name):
    toks = set(fold(name).split())
    return next((c for c in sorted(CHAIN_BRANDS) if c in toks), None)


def score(roster_name, cand_name, city):
    """Similarity in [0,1] on the city-stripped core names."""
    if is_event(roster_name) != is_event(cand_name):
        return min(0.5, SequenceMatcher(None, fold(roster_name), fold(cand_name)).ratio())
    a, b = core(roster_name, city), core(cand_name, city)
    fa, fb = fold(roster_name), fold(cand_name)
    if not a or not b:
        # Nothing but noise/city left (e.g. "Kino Saulgau"): demand the
        # full normalised names agree.
        return 1.0 if fa == fb or a == b else SequenceMatcher(None, fa, fb).ratio() * 0.9
    if a == b or a.replace(" ", "") == b.replace(" ", ""):
        return 1.0
    ra = SequenceMatcher(None, a.replace(" ", ""), b.replace(" ", "")).ratio()
    rs = SequenceMatcher(None, " ".join(sorted(a.split())),
                         " ".join(sorted(b.split()))).ratio()
    ta, tb = set(a.split()), set(b.split())
    contain = 0.0
    small, big = (ta, tb) if len(ta) <= len(tb) else (tb, ta)
    if small <= big and small - GENERIC and len(" ".join(small)) >= 5:
        # All tokens of the shorter name appear in the longer one.
        contain = 0.88 + 0.1 * len(small) / len(big)
    return max(ra, rs, contain)


def place_forms(name):
    """Normalised spellings of a place name: full, without "(...)", before
    " - " or ",". "Nuernberg, Mittelfranken" -> {"nuernberg mittelfranken",
    "nuernberg"}."""
    forms = {fold(name), fold(re.sub(r"\(.*?\)", " ", name)),
             fold(name.split(" - ")[0]), fold(name.split(",")[0])}
    return {f for f in forms if f}


def paren_qualifier(city):
    m = re.search(r"\((.*?)\)", city)
    return fold(m.group(1)) if m else None


def locality_fits(city, locality):
    """Does a venue's addressLocality name the roster city? Equal forms, or
    one a word-prefix of the other ("Esslingen" / "Esslingen am Neckar").
    A qualifier like "(Oder)" must then appear too, so "Frankfurt (Oder)"
    never fits "Frankfurt am Main"."""
    if not locality:
        return False
    cf, lf = place_forms(city), place_forms(locality)
    if cf & lf:
        return True
    q = paren_qualifier(city)
    for c in cf:
        for l in lf:
            short, long_ = sorted((c, l), key=len)
            if long_.startswith(short + " ") and (not q or q in long_.split()):
                return True
    return False


# ------------------------------------------------------------------ parsing
VENUE_HREF = re.compile(r'href="/kino/([a-z0-9-]+)/(?!film/)([^"/?#]+)-(\d+)"')
SELECT = re.compile(r"Kino auswählen</span>(.*?)</select>", re.S)
# "city-<id>" options are the town's own venues; "nearby-<id>" ones are not.
OWN_OPTION = re.compile(r'<option value="city-(\d+)">(.*?)</option>', re.S)
TOWN_HREF = re.compile(r'href="/kino/([a-z0-9-]+)/')


def parse_town(text):
    """-> dict(title, own:[{id, name, path}], slugs_seen:set)"""
    paths = {}
    for town, slug, vid in VENUE_HREF.findall(text):
        paths.setdefault(vid, f"/kino/{town}/{slug}-{vid}")
    own = []
    m = SELECT.search(text)
    if m:
        for vid, name in OWN_OPTION.findall(m.group(1)):
            if vid in paths:
                own.append({"id": vid, "name": html.unescape(name).strip(),
                            "path": paths[vid]})
    title = re.search(r"<title>Kinoprogramm (.*?):", text)
    return {"title": html.unescape(title.group(1)) if title else None,
            "own": own, "slugs_seen": set(TOWN_HREF.findall(text))}


LDJSON = re.compile(r'<script type="application/ld\+json">(.*?)</script>', re.S)


def parse_venue(text):
    for blob in LDJSON.findall(text):
        try:
            d = json.loads(blob)
        except ValueError:
            continue
        if d.get("@type") == "MovieTheater":
            addr = d.get("address") or {}
            geo = d.get("geo") or {}
            return {"name": d.get("name"),
                    "street": addr.get("streetAddress"),
                    "zip": addr.get("postalCode"),
                    "locality": addr.get("addressLocality"),
                    "lat": geo.get("latitude"), "lon": geo.get("longitude")}
    return None


def km_between(lat1, lon1, lat2, lon2):
    p = math.pi / 180
    a = (math.sin((lat2 - lat1) * p / 2) ** 2 + math.cos(lat1 * p) *
         math.cos(lat2 * p) * math.sin((lon2 - lon1) * p / 2) ** 2)
    return 12742 * math.asin(math.sqrt(a))


# ------------------------------------------------------------------- driver
def slug_variants(city):
    out = []

    def add(s):
        s = s.strip("-")
        if s and s not in out:
            out.append(s)
    add(slugify(city))
    add(slugify(re.sub(r"\(.*?\)", " ", city)))
    add(slugify(city.split(" - ")[0]))
    add(slugify(re.sub(r"^Sankt ", "St. ", city)))
    add(slugify(re.sub(r"^St\.? ", "Sankt ", city)))
    return out


PRESCORE = 0.6     # candidates below this vs every roster venue are never fetched


def main():
    sys.stdout.reconfigure(line_buffering=True)
    regions = json.load(open(REGIONS))
    coords = json.load(open(COORDS))
    roster = [c for r in regions for c in r["cinemas"]]
    by_city = {}
    for r in roster:
        by_city.setdefault(r["city"], []).append(r)
    cities = sorted(by_city)
    print(f"roster: {len(roster)} venues in {len(cities)} cities")
    fx = Fetcher()
    lock = threading.Lock()

    def api(endpoint, **params):
        # %20 for spaces: a "+" would be re-quoted as a literal plus.
        query = urllib.parse.urlencode(params, quote_via=urllib.parse.quote)
        status, _, text = fx.get(f"/{endpoint}?{query}")
        return text.strip() if status == 200 else ""

    def autocomplete(town, query=""):
        try:
            rows = json.loads(api("jsonApi", a="autocompleateCinema",
                                  stadt=town, suche=query) or "[]")
        except ValueError:
            return []
        return [(str(r["kino_id"]), html.unescape(r["value"])) for r in rows
                if isinstance(r, dict) and r.get("kino_id")]

    def town_page(slug):
        status, _, text = fx.get(f"/kino/{slug}/")
        return parse_town(text) if status == 200 else None

    # -- discovery: every venue kinoprogramm files under a spelling of the city
    found = {c: {} for c in cities}    # city -> id -> {"id","name","path"}
    titles = {c: set() for c in cities}
    tried = {c: set() for c in cities}
    truncated = set()                  # autocomplete capped at 10 rows
    slugs_seen = set()

    def add(city, vid, name, path=None):
        cur = found[city].setdefault(vid, {"id": vid, "name": name, "path": path})
        cur["path"] = cur["path"] or path

    def harvest_slugs(city, slugs):
        for slug in slugs:
            if slug in tried[city]:
                continue
            tried[city].add(slug)
            page = town_page(slug)
            if page is None:
                continue
            with lock:
                slugs_seen.update(page["slugs_seen"])
            if page["title"]:
                titles[city].add(page["title"])
            for cand in page["own"]:
                add(city, cand["id"], cand["name"], cand["path"])

    def town_names(city):
        # kinoprogramm's own town spellings: "Nienburg" -> "Nienburg (Weser)".
        base = re.split(r" \(| - |,", city)[0]
        try:
            rows = json.loads(api("jsonApi", a="autocompleateTown", suche=base) or "[]")
        except ValueError:
            return set()
        return {html.unescape(r["value"]) for r in rows if isinstance(r, dict)
                and r.get("value") and locality_fits(city, html.unescape(r["value"]))}

    def harvest_names(city):
        titles[city] |= town_names(city)
        for town in sorted({city} | titles[city]):
            rows = autocomplete(town)
            if len(rows) >= 10:
                truncated.add(city)
            for vid, name in rows:
                add(city, vid, name)

    def discover(city):
        slugs = slug_variants(city)
        m = re.match(r"/kino/([a-z0-9-]+)/$", api("suchApi", a="citysuche", suche=city))
        if m:
            slugs.append(m.group(1))
        harvest_slugs(city, slugs)
        harvest_names(city)

    print("discovery: town pages + cinema autocomplete")
    fx.map(discover, cities)
    empty = [c for c in cities if not found[c]]
    print(f"  {len(cities) - len(empty)}/{len(cities)} cities have candidates")

    def rediscover(city):
        # Disambiguated slugs seen elsewhere: "esslingen" -> "esslingen-am-neckar".
        bases = slug_variants(city)
        harvest_slugs(city, sorted(s for s in slugs_seen for b in bases
                                   if s.startswith(b + "-")))
        harvest_names(city)

    fx.map(rediscover, empty)
    print(f"  after disambiguated slugs: "
          f"{sum(1 for c in cities if found[c])}/{len(cities)}")

    # Big towns: the autocomplete returns only 10 rows, so ask for each
    # roster venue by its most distinctive word.
    def targeted(city):
        for r in by_city[city]:
            best = max((score(r["name"], c["name"], city) for c in found[city].values()),
                       default=0.0)
            if best >= 1.0:
                continue
            words = sorted((t for t in core_tokens(r["name"], city)
                            if t not in GENERIC and len(t) >= 4), key=len, reverse=True)
            for town in sorted({city} | titles[city])[:2]:
                for word in words[:1]:
                    for vid, name in autocomplete(town, word):
                        add(city, vid, name)

    print(f"targeted autocomplete in {len(truncated)} big towns")
    fx.map(targeted, sorted(truncated))

    # -- verification: fetch the page of every plausible candidate; keep it
    # only if its JSON-LD address and geo put it in the roster city.
    venue_info = {}

    def canonical(vid):
        m = re.match(r"/kino/.+-\d+$", api("suchApi", a="citysuche", kino=vid))
        return m.group(0) if m else None

    def fetch_venue(vid, path):
        for attempt in (path, None):
            p = attempt or canonical(vid)
            if not p:
                continue
            status, final, text = fx.get(p)
            if status == 200:
                v = parse_venue(text)
                if v:
                    v["path"] = urllib.parse.quote(urllib.parse.unquote(final),
                                                   safe="/-._~+")
                    return v
        return None

    def city_km(city):
        return BIG_CITY_KM if coords[city].get("population", 0) > 1_000_000 else MAX_CITY_KM

    def is_local(city, v):
        if not v or not locality_fits(city, v["locality"]):
            return False
        if not v["lat"] or not v["lon"]:
            return True                   # no geo on the page: locality decides
        c = coords[city]
        if km_between(c["lat"], c["lon"], v["lat"], v["lon"]) <= city_km(city):
            return True
        # city-coords.json can place a qualified name wrongly
        # ("Frankfurt (Oder)" at Frankfurt am Main); the qualifier itself in
        # the venue's locality is then the evidence.
        q = paren_qualifier(city)
        return bool(q) and q in fold(v["locality"]).split()

    jobs = []
    for city in cities:
        for cand in found[city].values():
            if max(score(r["name"], cand["name"], city) for r in by_city[city]) >= PRESCORE:
                jobs.append((city, cand))
    print(f"verifying {len(jobs)} plausible candidates")

    def verify(job):
        city, cand = job
        v = fetch_venue(cand["id"], cand["path"])
        with lock:
            venue_info[cand["id"]] = v
        cand["local"] = is_local(city, v)

    fx.map(verify, jobs)

    # -- matching
    proposals, unmatched = {}, {}
    for r in roster:
        city, tid = r["city"], r["theaterId"]
        if not found[city]:
            unmatched[tid] = (r, None, 0.0, "town_absent")
            continue
        rchain = chain_of(r["name"]) or chain_of(r["displayName"])
        scored = []
        for cand in found[city].values():
            if not cand.get("local"):
                continue
            cchain = chain_of(cand["name"]) or chain_of(venue_info[cand["id"]]["name"])
            if rchain and cchain != rchain:
                continue                  # a chain maps only onto its own branch
            if cchain and not rchain:
                continue
            s = max(score(r["name"], cand["name"], city),
                    score(r["displayName"], cand["name"], city),
                    score(r["name"], venue_info[cand["id"]]["name"], city))
            scored.append((s, cand))
        scored.sort(key=lambda x: (-x[0], x[1]["id"]))
        if not scored:
            unmatched[tid] = (r, None, 0.0, "below_threshold")
            continue
        best_s, best = scored[0]
        second = scored[1][0] if len(scored) > 1 else 0.0
        if best_s < ACCEPT_SCORE:
            unmatched[tid] = (r, best, best_s, "below_threshold")
        elif best_s - second < MARGIN and not (best_s == 1.0 and second < 0.95):
            unmatched[tid] = (r, best, best_s, "ambiguous")
        else:
            proposals[tid] = {"r": r, "cand": best, "score": best_s}

    # -- one kinoprogramm venue per roster venue
    by_cand = {}
    for tid, pr in proposals.items():
        by_cand.setdefault(pr["cand"]["id"], []).append(tid)
    for tids in by_cand.values():
        if len(tids) < 2:
            continue
        tids.sort(key=lambda t: -proposals[t]["score"])
        top, rest = proposals[tids[0]], [proposals[t] for t in tids[1:]]
        keep_top = top["score"] == 1.0 and all(p["score"] < 0.95 for p in rest)
        for t in tids[(1 if keep_top else 0):]:
            pr = proposals.pop(t)
            unmatched[t] = (pr["r"], pr["cand"], pr["score"], "ambiguous")

    matched = {t: venue_info[proposals[t]["cand"]["id"]]["path"] for t in sorted(proposals)}
    with open(OUT, "w", encoding="utf-8") as f:
        json.dump(matched, f, indent=2, ensure_ascii=False, sort_keys=True)
        f.write("\n")
    rows = []
    for tid in sorted(unmatched):
        r, cand, s, why = unmatched[tid]
        v = venue_info.get(cand["id"]) if cand else None
        rows.append({"theaterId": tid, "name": r["name"], "city": r["city"],
                     "bestCandidate": ({"path": v["path"] if v else cand["path"],
                                        "name": cand["name"],
                                        "locality": v["locality"] if v else None}
                                       if cand else None),
                     "score": round(s, 3), "reason": why})
    with open(OUT_UNMATCHED, "w", encoding="utf-8") as f:
        json.dump(rows, f, indent=2, ensure_ascii=False, sort_keys=True)
        f.write("\n")

    reasons = {}
    for row in rows:
        reasons[row["reason"]] = reasons.get(row["reason"], 0) + 1
    print(f"matched {len(matched)}/{len(roster)} "
          f"({100 * len(matched) / len(roster):.1f}%); unmatched {len(rows)}: "
          f"{reasons}")

    # -- spot check: roster venue vs the venue page it maps to
    rnd = random.Random(os.environ.get("KP_SEED", "kinoprogramm"))
    sample = rnd.sample(sorted(proposals), min(SPOT_CHECK, len(proposals)))
    print(f"spot check ({len(sample)}):")
    for tid in sample:
        r, v = proposals[tid]["r"], venue_info[proposals[tid]["cand"]["id"]]
        print(f"  {tid} {r['name']!r} [{r['city']}] -> {v['name']!r}, "
              f"{v['street']}, {v['zip']} {v['locality']}  {v['path']}")
    fx.report()


if __name__ == "__main__":
    main()
