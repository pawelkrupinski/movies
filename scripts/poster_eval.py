#!/usr/bin/env python3
"""How strong a venue poster is as identity evidence, on prod's REAL candidate sets: the logic of scripts/poster-eval.

    scripts/poster-eval [--posters N] [--vote B] [--veto-match B] [--veto B] [--countries pl,uk] [--refresh]
    scripts/poster-eval coverage [--countries pl,uk] [--refresh]

THE DATASET (built once from the LOCAL prod mirror, kept in <cache>/dataset.npz, rebuilt when the mirror has synced more
than RefreshHours past it, or with --refresh): every model decision (`identity_model_families.decisions`) whose member
listings show a venue poster that counts (`PosterEvidence.shows`' source rule: no Webedia/Cineamo id, no kinoprogramm /
kinoheld page -- a feed's poster is its entry's; the stage-work and double-bill shapes are not reproduced), with its
REAL candidate set (every member's trace candidates not DENIED, plus the decision's film), prod's take, and the poster
answers prod filed (`identity_family_answers` poster|venue|<url>, poster|film|<id>). One row per listing for `coverage`.

A VARIANT is the poster count per film, the vote bits, the veto's match bits and the taken film's veto bits; prod's
(PosterEvidence: 8 / 4 / 8 / 10) is the baseline. A venue poster's hash is the one prod filed, else its image's; a film's
posters are the ones prod filed (its first N), else `PosterHashing.chosen` over TMDB's /images; above 8, the filed 8 plus
chosen N. A hash is production's bit for bit (poster_hash.py), cached by the image's sha1; an image or TMDB list not yet
cached is fetched (TMDB_API_KEY for the lists) -- 8 workers, a 429/503 halves them.

LABELS: identity-judgements.tsv (judged: the vote is TP when it names a film judged right, FP when it names a film
judged wrong or another than the right one), and prod's take for the rest (weak: agree / disagree). Never mix them:
the 2026-11-11 investigation's research sample (where the right film was always a candidate) gave ~99.4% precision; prod's
real sets, which often lack the right film or hold a near-twin, give ~92% (docs/design/poster-evidence-evaluation.md).
"""
import argparse
import hashlib
import itertools
import json
import os
import sqlite3
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor

import numpy as np

Here = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [Here, os.path.join(Here, "ci")]
import poster_hash  # noqa: E402
from identity_judgements import COLUMNS as Columns, normalise, read as read_judgements  # noqa: E402,F401

Root = os.path.dirname(Here)
Countries = ("pl", "uk", "de", "us", "es")
Databases = {"pl": "kinowo_prod_mirror", "uk": "kinowo_uk_prod_mirror", "de": "kinowo_de_prod_mirror",
             "us": "kinowo_us_prod_mirror", "es": "kinowo_es_prod_mirror"}
Language = {"pl": "pl", "uk": "en", "us": "en", "de": "de", "es": "es"}
Baseline = {"posters": 8, "vote": 4, "veto_match": 8, "veto": 10}  # PosterEvidence's constants
FiledPosters = 8  # PosterEvidence.FilmPosters: how many a film's filed answer holds
RefreshHours = 6
ExtractVersion = 1
Missing = 255  # no distance: the venue poster or every poster of the film unhashed
Mask = (1 << 64) - 1
UserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36"

# The mirror read: one JSON object a line. The candidate parse is prodval's (2026-11-11): a trace candidate line opens
# with its TMDB id, and one the trace DENIED is no candidate.
ExtractJs = r"""
const rows = [];
const out = o => rows.push(o);
const feedIds = new Set(["webedia", "cineamo"]);
const feedPages = ["https://www.kinoprogramm.com/", "https://kinoprogramm.com/", "https://www.kinoheld.de/film/"];
const key = (venue, raw) => venue + "\u0000" + raw;
const cands = {};
db.identity_traces.find({}, {"listing.venue": 1, "listing.rawTitle": 1, candidates: 1}).forEach(d => {
  const cs = [];
  for (const s of (d.candidates || [])) { const m = /^(\d+) /.exec(s); if (m && s.indexOf("DENIED") < 0) cs.push(+m[1]); }
  cands[key(d.listing.venue, d.listing.rawTitle)] = cs;
});
const take = {};
db.identity_model_families.find({}, {decisions: 1}).forEach(fam => {
  for (const dec of (fam.decisions || [])) {
    const ms = [].concat(...(dec.members || []).map(m => Array.isArray(m) ? m : [m]));
    const film = typeof dec.film === "number" ? dec.film : null;
    const keys = ms.map(m => [m.venue, m.rawTitle]);
    keys.forEach(k => take[key(k[0], k[1])] = film);
    out({t: "d", fam: fam._id, keys: keys, film: film, basis: dec.basis || null});
  }
});
db.identity_listings.find({}, {"films.movie.rawTitle": 1, "films.movie.title": 1, "films.posterUrl": 1, "films.externalIds": 1, "films.filmUrl": 1}).forEach(v => {
  for (const f of (v.films || [])) {
    const raw = f.movie.rawTitle || f.movie.title;
    const ids = Object.entries(f.externalIds || {}).filter(e => String(e[0]).trim() && String(e[1]).trim()).map(e => String(e[0]).trim());
    const page = (f.filmUrl || "").trim();
    const feed = ids.some(i => feedIds.has(i)) || feedPages.some(p => page.startsWith(p));
    const k = key(v._id, raw);
    out({t: "l", venue: v._id, raw: raw, url: f.posterUrl || null, feed: feed, cands: cands[k] || [], take: k in take ? take[k] : null, decided: k in take});
  }
});
db.identity_family_answers.find({_id: /^poster\|/}).forEach(d => {
  const hashes = (d.hashes || []).map(h => h.toString());
  out({t: "a", id: d._id, hashes: hashes, paths: d.paths || [], unread: !!d.unread});
});
require("fs").writeFileSync(process.env.POSTER_EVAL_OUT, JSON.stringify(rows));
"""


def log(*parts):
    print(*parts, file=sys.stderr, flush=True)


# ── the dataset ──────────────────────────────────────────────────────────────────────────────────────────────────────

SyncedJs = r"""
const out = {};
for (const cc of CCS) {
  const prodDb = prodDbOf(cc);
  const saved = db.getSiblingDB(MIRRORS[cc]).getCollection("__mirror_state").findOne({_id: prodDb}) || {};
  out[cc] = resumeTokenMs(saved.resumeToken && saved.resumeToken._data);
}
print(JSON.stringify(out));
"""


def mirror_uri(database=""):
    return f"mongodb://127.0.0.1:{os.environ.get('LOCAL_MIRROR_PORT', '28017')}/{database}?directConnection=true"


def mirror_synced():
    """{cc: epoch seconds} the local mirror last synced each country: the change-stream resume token freshness reads
    (local-mirror/freshness-rule.js), in one quick query -- the full freshness check runs before an extract."""
    with open(os.path.join(Here, "local-mirror", "freshness-rule.js")) as f:
        rule = f.read()
    js = f"var CCS={json.dumps(list(Countries))}; var MIRRORS={json.dumps(Databases)};\n{rule}\n{SyncedJs}"
    done = subprocess.run(["mongosh", "--quiet", mirror_uri(), "--eval", js],
                          capture_output=True, text=True, timeout=60)
    if done.returncode != 0:
        raise SystemExit(f"poster-eval: the local mirror does not answer (scripts/local-mirror/freshness):\n{done.stderr[-1000:]}")
    return {cc: (ms or 0) / 1000 for cc, ms in json.loads(done.stdout.strip().splitlines()[-1]).items()}


def check_freshness():
    done = subprocess.run([os.path.join(Here, "local-mirror", "freshness"), "--quiet", *Countries], capture_output=True, text=True)
    log(done.stdout.strip())
    if done.returncode == 3:
        log("poster-eval: WARNING the mirror is not fresh (above); reading it anyway")
    elif done.returncode != 0:
        raise SystemExit(f"poster-eval: the local mirror does not answer:\n{done.stderr}")


def read_country(cc):
    uri = mirror_uri(Databases[cc])
    fd, path = tempfile.mkstemp(suffix=f"-{cc}.json")
    os.close(fd)
    try:
        # one JSON file, not a line a row: mongosh's print takes minutes over a country's ~100k listings
        done = subprocess.run(["mongosh", "--quiet", uri, "--eval", ExtractJs], capture_output=True, text=True, timeout=1800,
                              env=dict(os.environ, POSTER_EVAL_OUT=path))
        if done.returncode != 0:
            raise SystemExit(f"poster-eval: reading {Databases[cc]} failed:\n{done.stdout[-1000:]}{done.stderr[-2000:]}")
        with open(path) as f:
            return cc, json.load(f)
    finally:
        os.remove(path)


def build(rows_by_country):
    """The dataset from the mirror's rows: {meta (JSON-able), arrays}. The (cluster, url, candidate) pair table is built
    here, once, so an evaluation never loops over pairs."""
    clusters, listings, urls, keys = [], [], {}, {}
    venue_filed, venue_unread, film_filed = {}, set(), {}
    for cc, rows in rows_by_country.items():
        poster = {}
        trace = {}
        for r in rows:
            if r["t"] == "l":
                if r["url"] and not r["feed"]:
                    poster[(r["venue"], r["raw"])] = r["url"]
                trace[(r["venue"], r["raw"])] = r["cands"]
                listings.append([cc, r["venue"], r["raw"], r["url"] or "", int(r["feed"]), r["cands"], r["take"], int(r["decided"])])
            elif r["t"] == "a":
                kind, _, ref = r["id"][len("poster|"):].partition("|")
                hashes = [int(h) & Mask for h in r["hashes"]]
                if kind == "venue":
                    if hashes:
                        venue_filed[ref] = hashes[0]
                    else:
                        venue_unread.add(ref)
                elif hashes:
                    film_filed[f"{cc}|{ref}"] = (hashes, r["paths"] if len(r["paths"]) == len(hashes) else [])
        for r in rows:
            if r["t"] != "d":
                continue
            members = [tuple(k) for k in r["keys"]]
            shown = sorted({poster[k] for k in members if k in poster})
            film = r["film"]
            cands = sorted({c for k in members for c in trace.get(k, [])} | ({film} if film is not None else set()))
            if not shown or not cands:
                continue
            clusters.append({"cc": cc, "fam": r["fam"], "keys": members, "film": film, "basis": r["basis"], "urls": shown,
                             "cands": cands})
    pair_cluster, pair_cu, pair_url, pair_key, pair_film, cu_cluster = [], [], [], [], [], []
    for i, c in enumerate(clusters):
        for u in c["urls"]:
            cu = len(cu_cluster)
            cu_cluster.append(i)
            ui = urls.setdefault(u, len(urls))
            for f in c["cands"]:
                pair_cluster.append(i); pair_cu.append(cu); pair_url.append(ui)
                pair_key.append(keys.setdefault(f"{c['cc']}|{f}", len(keys))); pair_film.append(f)
    url_list = sorted(urls, key=urls.get)
    key_list = sorted(keys, key=keys.get)
    meta = {"version": ExtractVersion, "clusters": clusters, "listings": listings, "urls": url_list, "keys": key_list,
            "venue_unread": sorted(venue_unread),
            "venue_filed_any": sorted(venue_filed),  # every url with a filed hash, coverage's "asked and read"
            "film_filed_any": sorted(film_filed),
            "film_paths": {k: film_filed[k][1] for k in key_list if k in film_filed}}
    film_offsets, film_hashes = [0], []
    for k in key_list:
        film_hashes += film_filed.get(k, ([], []))[0]
        film_offsets.append(len(film_hashes))
    arrays = {
        "pair_cluster": np.array(pair_cluster, np.int32), "pair_cu": np.array(pair_cu, np.int32),
        "pair_url": np.array(pair_url, np.int32), "pair_key": np.array(pair_key, np.int32),
        "pair_film": np.array(pair_film, np.int64), "cu_cluster": np.array(cu_cluster, np.int32),
        "venue_filed": np.array([venue_filed.get(u, 0) for u in url_list], np.uint64),
        "venue_has": np.array([u in venue_filed for u in url_list], bool),
        "film_hashes": np.array(film_hashes, np.uint64), "film_offsets": np.array(film_offsets, np.int64),
    }
    return meta, arrays


def save(path, meta, arrays):
    tmp = path + ".tmp.npz"
    np.savez(tmp, meta=np.frombuffer(json.dumps(meta).encode(), np.uint8), **arrays)
    os.replace(tmp, path)


def load(path):
    with np.load(path) as z:
        meta = json.loads(z["meta"].tobytes())
        arrays = {k: z[k] for k in z.files if k != "meta"}
    return meta, arrays


def dataset(cache, countries, refresh):
    path = os.path.join(cache, "dataset.npz")
    synced = mirror_synced()
    if os.path.exists(path) and not refresh:
        meta, arrays = load(path)
        stale = [cc for cc in Countries if synced.get(cc, 0) > meta.get("synced", {}).get(cc, 0) + RefreshHours * 3600]
        if meta.get("version") == ExtractVersion and not stale:
            return restrict(meta, arrays, countries)
        log(f"poster-eval: dataset stale ({', '.join(stale) or 'older extract'}), re-extracting")
    check_freshness()
    started = time.time()
    with ThreadPoolExecutor(2) as pool:  # the mirror is shared: two countries at a time
        rows = dict(pool.map(read_country, Countries))
    meta, arrays = build(rows)
    meta["synced"] = synced
    save(path, meta, arrays)
    log(f"poster-eval: extracted {len(meta['clusters'])} clusters, {len(meta['listings'])} listings in {time.time() - started:.0f}s")
    return restrict(meta, arrays, countries)


def restrict(meta, arrays, countries):
    """The dataset's listings of `countries` alone; every cluster stays (an evaluation reports only `countries`')."""
    return dict(meta, listings=[r for r in meta["listings"] if r[0] in countries]), arrays


# ── images and hashes ────────────────────────────────────────────────────────────────────────────────────────────────

class Pool:
    """A concurrency cap a 429/503 halves."""

    def __init__(self, n):
        self.n, self.active, self.cv = n, 0, threading.Condition()

    def __enter__(self):
        with self.cv:
            while self.active >= self.n:
                self.cv.wait()
            self.active += 1

    def __exit__(self, *_):
        with self.cv:
            self.active -= 1
            self.cv.notify_all()

    def halve(self):
        with self.cv:
            self.n = max(1, self.n // 2)
        log(f"poster-eval: 429/503, concurrency halved to {self.n}")


def http_get(url, pools, tries=3):
    for attempt in range(tries):
        with Stack(pools):
            try:
                request = urllib.request.Request(url, headers={"User-Agent": UserAgent, "Accept": "image/*,application/json,*/*"})
                with urllib.request.urlopen(request, timeout=20) as r:
                    return r.read()
            except urllib.error.HTTPError as e:
                if e.code in (429, 503):
                    pools[0].halve()
                    time.sleep(2 ** attempt * 2)
                    continue
                return None
            except Exception:  # noqa: BLE001 -- a network hiccup: tried again
                time.sleep(1 + attempt)
    return None


class Stack:
    def __init__(self, pools):
        self.pools = pools

    def __enter__(self):
        for p in self.pools:
            p.__enter__()

    def __exit__(self, *a):
        for p in reversed(self.pools):
            p.__exit__(*a)


class Progress:
    """A progress line every 10 s, and the throughput at the end."""

    def __init__(self, what, total):
        self.what, self.total, self.counts = what, total, Counter()
        self.started = self.last = time.time()

    def tick(self, outcome):
        self.counts[outcome] += 1
        if time.time() - self.last >= 10:
            self.last = time.time()
            done = sum(self.counts.values())
            log(f"poster-eval: {self.what} {done}/{self.total} {dict(self.counts)} {done / (self.last - self.started):.0f}/s")

    def done(self):
        elapsed = time.time() - self.started
        log(f"poster-eval: {self.what} done {dict(self.counts)} in {elapsed:.0f}s, {self.total / max(elapsed, 1e-3):.1f}/s")


class Images:
    """The image cache (<cache>/img/v/<sha1(url)>, img/t/<tmdb path>) and the hash cache (<cache>/hashes.sqlite: each
    image's content sha1 and the hash of that content, so no content is hashed twice)."""

    def __init__(self, cache):
        self.cache = cache
        for d in ("v", "t"):
            os.makedirs(os.path.join(cache, "img", d), exist_ok=True)
        self.db = sqlite3.connect(os.path.join(cache, "hashes.sqlite"), check_same_thread=False)
        self.db.execute("CREATE TABLE IF NOT EXISTS files (name TEXT PRIMARY KEY, sha1 TEXT NOT NULL)")
        self.db.execute("CREATE TABLE IF NOT EXISTS hashes (sha1 TEXT PRIMARY KEY, hash INTEGER)")  # NULL: unreadable
        by_sha = {s: h for s, h in self.db.execute("SELECT sha1, hash FROM hashes")}
        self.known = {n: by_sha.get(s, -1) for n, s in self.db.execute("SELECT name, sha1 FROM files")}
        self.by_sha = by_sha
        self.lock = threading.Lock()

    @staticmethod
    def venue_name(url):
        return "v/" + hashlib.sha1(url.encode()).hexdigest()

    @staticmethod
    def film_name(path):
        return "t/" + path.strip("/")

    def path(self, name):
        return os.path.join(self.cache, "img", name)

    def hash_of(self, name):
        """The cached hash, None when unreadable or unfetchable, -1 when not hashed yet."""
        h = self.known.get(name, -1)
        if h == -1 and os.path.exists(self.path(name) + ".fail"):
            return None
        return h

    def ensure(self, wanted):
        """Fetch and hash every image of `wanted` ({name: source url}) not cached yet, with a progress line: the cached
        files hashed first, then the rest fetched (interleaved by host, so one slow host never holds every worker)."""
        todo = {n: u for n, u in wanted.items() if self.hash_of(n) == -1}
        if not todo:
            return
        present = [n for n in todo if os.path.exists(self.path(n))]
        missing = [(n, todo[n]) for n in todo if not os.path.exists(self.path(n))]
        log(f"poster-eval: {len(todo)} images to hash, {len(missing)} of them to fetch first")
        progress = Progress("images", len(todo))
        with ThreadPoolExecutor(10) as pool:
            for _ in pool.map(lambda n: self.add(n, self.path(n)), present):
                progress.tick("hashed")
        by_host = defaultdict(list)
        for name, url in missing:
            by_host[urllib.parse.urlsplit(url).netloc].append((name, url))
        interleaved = [item for group in itertools.zip_longest(*by_host.values()) for item in group if item]
        cdn, venues, hosts, hosts_lock = Pool(8), Pool(8), defaultdict(lambda: Pool(2)), threading.Lock()

        def fetch(item):
            name, url = item
            path = self.path(name)
            if name.startswith("v/"):
                parts = urllib.parse.urlsplit(url)
                url = parts._replace(path=urllib.parse.quote(parts.path, safe="/%:@!$&'()*+,;=-._~")).geturl()
                with hosts_lock:
                    host = hosts[parts.netloc]
                body = http_get(url, [venues, host], tries=2)
            else:
                body = http_get(url, [cdn])
            os.makedirs(os.path.dirname(path), exist_ok=True)
            if not body:
                open(path + ".fail", "w").close()
                return "unfetched"
            with open(path, "wb") as f:
                f.write(body)
            self.add(name, path)
            return "fetched"
        with ThreadPoolExecutor(16) as pool:
            for outcome in pool.map(fetch, interleaved):
                progress.tick(outcome)
        with self.lock:
            self.db.commit()
        progress.done()

    def add(self, name, path):
        with open(path, "rb") as f:
            sha = hashlib.sha1(f.read()).hexdigest()
        with self.lock:
            known = sha in self.by_sha
            h = self.by_sha.get(sha)
        if not known:
            h = poster_hash.hash_file(path)
        with self.lock:
            self.by_sha[sha] = h
            self.known[name] = h
            self.db.execute("INSERT OR REPLACE INTO hashes VALUES (?, ?)", (sha, h))  # 63 bits: fits SQLite's int64
            self.db.execute("INSERT OR REPLACE INTO files VALUES (?, ?)", (name, sha))


def chosen(posters, cc, n):
    """PosterHashing.chosen: the country's language, English, none, the rest; each by votes; then the path."""
    language = Language[cc]

    def order(p):
        path, lang, votes = p
        return (0 if lang == language else 1 if lang == "en" else 2 if lang is None else 3, -votes, path)
    return list(dict.fromkeys(p[0] for p in sorted(posters, key=order)))[:n]


class TmdbLists:
    """TMDB's /movie/{id}/images poster lists, cached in <cache>/tmdb-images.json ({id: [[path, language, votes]]})."""

    def __init__(self, cache):
        self.path = os.path.join(cache, "tmdb-images.json")
        self.lists = json.load(open(self.path)) if os.path.exists(self.path) else {}

    def ensure(self, films):
        need = sorted({str(f) for f in films} - set(self.lists))
        if not need:
            return
        key = os.environ.get("TMDB_API_KEY")
        if not key:
            raise SystemExit(f"poster-eval: {len(need)} films' TMDB poster lists are not cached; run under "
                             "`secrets run movies TMDB_API_KEY -- scripts/poster-eval ...`")
        log(f"poster-eval: fetching {len(need)} TMDB poster lists")
        api, started = Pool(8), time.time()

        def one(f):
            body = http_get(f"https://api.themoviedb.org/3/movie/{f}/images?api_key={key}", [api])
            posters = json.loads(body).get("posters", []) if body else []
            return f, [[p["file_path"], p.get("iso_639_1"), p.get("vote_count", 0)] for p in posters]
        with ThreadPoolExecutor(8) as pool:
            for f, posters in pool.map(one, need):
                self.lists[f] = posters
        with open(self.path + ".tmp", "w") as out:
            json.dump(self.lists, out)
        os.replace(self.path + ".tmp", self.path)
        log(f"poster-eval: {len(need)} lists in {time.time() - started:.0f}s")

    def posters(self, film):
        return [tuple(p) for p in self.lists.get(str(film), [])]


def film_posters(meta, arrays, tmdb, n):
    """{key index: [(name or None, filed hash or None)]}: each candidate's N posters for the variant."""
    out, lists_needed = {}, set()
    paths, offsets, hashes = meta["film_paths"], arrays["film_offsets"], arrays["film_hashes"]
    for j, k in enumerate(meta["keys"]):
        filed = [int(h) for h in hashes[offsets[j]:offsets[j + 1]]]
        if not filed or n > FiledPosters:
            lists_needed.add(int(k.split("|")[1]))
    tmdb.ensure(lists_needed)
    for j, k in enumerate(meta["keys"]):
        cc, film = k.split("|")
        filed = [int(h) for h in hashes[offsets[j]:offsets[j + 1]]]
        filed_paths = paths.get(k) or [None] * len(filed)
        mine = [(p, h) for p, h in zip(filed_paths, filed)][:n]
        if not filed or n > FiledPosters:
            have = {p for p, _ in mine}
            mine += [(Images.film_name(p), None) for p in chosen(tmdb.posters(film), cc, n) if p not in have]
        out[j] = mine
    return out


def resolve(meta, arrays, images, tmdb, n):
    """(venue hash per url, has, film hashes flat, offsets) for a variant with `n` posters per film."""
    wanted = {}
    venue = arrays["venue_filed"].copy()
    has = arrays["venue_has"].copy()
    for i, u in enumerate(meta["urls"]):
        if not has[i]:
            wanted[Images.venue_name(u)] = u
    per_film = film_posters(meta, arrays, tmdb, n)
    for posters in per_film.values():
        for name, h in posters:
            if h is None:
                wanted[name] = "https://image.tmdb.org/t/p/w185/" + name[2:]
    images.ensure(wanted)
    for i, u in enumerate(meta["urls"]):
        if not has[i]:
            h = images.hash_of(Images.venue_name(u))
            if h not in (None, -1):
                venue[i], has[i] = h, True
    flat, offsets = [], [0]
    for j in range(len(meta["keys"])):
        for name, h in per_film[j]:
            h = h if h is not None else images.hash_of(name)
            if h not in (None, -1):
                flat.append(h)
        offsets.append(len(flat))
    return venue, has, np.array(flat, np.uint64), np.array(offsets, np.int64)


# ── the evaluation ───────────────────────────────────────────────────────────────────────────────────────────────────

def nearest(arrays, venue, has, film_hashes, film_offsets):
    """Each (cluster, url, candidate) pair's nearest bits between the venue poster and any of the candidate's posters
    (Missing when either has none): numpy XOR + popcount over every (pair, poster), a min per pair."""
    keys = arrays["pair_key"]
    counts = (film_offsets[1:] - film_offsets[:-1])[keys]
    counts = np.where(has[arrays["pair_url"]], counts, 0)
    total = int(counts.sum())
    out = np.full(len(keys), Missing, np.int32)
    if total == 0:
        return out
    pair = np.repeat(np.arange(len(keys)), counts)
    starts = np.cumsum(counts) - counts
    poster = film_offsets[keys][pair] + (np.arange(total) - starts[pair])
    bits = np.bitwise_count(venue[arrays["pair_url"]][pair] ^ film_hashes[poster]).astype(np.int32)
    nonempty = counts > 0
    out[nonempty] = np.minimum.reduceat(bits, starts[nonempty])
    return out


def vetoed(arrays, d, target, n_clusters, veto_match, veto):
    """Per cluster: does any venue poster name another candidate within `veto_match` while the cluster's `target` film
    (-1: none) stays beyond `veto` of it (or shows no poster)? PosterEvidence.veto."""
    cu, cl = arrays["pair_cu"], arrays["pair_cluster"]
    n_cu = len(arrays["cu_cluster"])
    is_target = arrays["pair_film"] == target[cl]
    target_bits = np.full(n_cu, Missing, np.int32)
    np.minimum.at(target_bits, cu[is_target], d[is_target])
    other = np.zeros(n_cu, bool)
    other[cu[~is_target & (d <= veto_match)]] = True
    by_cu = (target_bits > veto) & other
    out = np.zeros(n_clusters, bool)
    out[arrays["cu_cluster"][by_cu]] = True
    return out


def evaluate(meta, arrays, d, variant):
    """Per cluster: the film the posters vote for (-1 none), whether prod's take is vetoed, the vote's bits."""
    n = len(meta["clusters"])
    cl, film = arrays["pair_cluster"], arrays["pair_film"]
    hit = d <= variant["vote"]
    pairs = np.unique(np.stack([cl[hit], film[hit]]), axis=1) if hit.any() else np.zeros((2, 0), np.int64)
    per = np.bincount(pairs[0], minlength=n)
    vote = np.full(n, -1, np.int64)
    sole = per[pairs[0]] == 1
    vote[pairs[0][sole]] = pairs[1][sole]
    vote[vetoed(arrays, d, vote, n, variant["veto_match"], variant["veto"]) & (vote >= 0)] = -1
    taken = np.array([c["film"] if c["film"] is not None else -1 for c in meta["clusters"]], np.int64)
    take_vetoed = vetoed(arrays, d, taken, n, variant["veto_match"], variant["veto"]) & (taken >= 0)
    bits = np.full(n, Missing, np.int32)
    voted_rows = vote[cl] == film
    np.minimum.at(bits, cl[voted_rows], d[voted_rows])
    return vote, take_vetoed, bits


def labels(meta, rows):
    """Per cluster: (right films, wrong films, judged right as no TMDB film, judged at all)."""
    table = defaultdict(lambda: {"right": set(), "wrong": set(), "other": False})
    for r in rows:
        k = (r.country, normalise(r.venue), normalise(r.rawTitle))
        if r.verdict == "right" and not r.film.startswith("tmdb:"):
            table[k]["other"] = True
        elif r.film.startswith("tmdb:") and r.verdict in ("right", "wrong"):
            table[k][r.verdict].add(int(r.film[5:]))
    out = []
    for c in meta["clusters"]:
        right, wrong, other = set(), set(), False
        for venue, raw in c["keys"]:
            j = table.get((c["cc"], normalise(venue), normalise(raw))) or table.get((c["cc"], "*", normalise(raw)))
            if j:
                right |= j["right"]; wrong |= j["wrong"]; other |= j["other"]
        wrong -= right
        out.append((right, wrong, other, bool(right or wrong or other)))
    return out


def judge(film, label):
    right, wrong, other, _ = label
    if film < 0:
        return None
    return "TP" if film in right else ("FP" if (film in wrong or right or other) else None)


def tally(meta, labs, vote, take_vetoed, ccs):
    t = Counter()
    for c, lab, f, v in zip(meta["clusters"], labs, vote, take_vetoed):
        if c["cc"] not in ccs:
            continue
        taken = c["film"]
        if lab[3]:
            j = judge(int(f), lab)
            t["tp"] += j == "TP"; t["fp"] += j == "FP"
            if taken is not None:
                if taken in lab[0]:
                    t["right_takes"] += 1; t["right_vetoed"] += bool(v)
                elif taken in lab[1] or lab[0] or lab[2]:
                    t["wrong_takes"] += 1; t["wrong_vetoed"] += bool(v)
        elif taken is not None:
            if f >= 0:
                t["agree"] += f == taken; t["disagree"] += f != taken
            t["weak_takes"] += 1; t["weak_vetoed"] += bool(v)
    return t


def describe(variant):
    return f"posters {variant['posters']} vote<={variant['vote']} veto-match<={variant['veto_match']} veto>{variant['veto']}"


def report(meta, labs, results, variant, out=print):
    names = ["baseline"] + (["variant"] if "variant" in results else [])
    out(f"baseline: {describe(Baseline)}" + (f" | variant: {describe(variant)}" if "variant" in results else ""))
    present = [cc for cc in Countries if any(c["cc"] == cc for c in meta["clusters"])]
    header = f"{'':4} {'':8} {'clusters':>8} {'judged':>6} | {'vote TP':>7} {'FP':>4} {'prec':>6} | {'weak agree':>10} {'dis':>4} {'prec':>6} | {'veto: right-takes vetoed':>24} {'wrong-takes vetoed':>18} {'weak vetoed':>11}"
    out(header)
    for cc in present + ["all"]:
        ccs = set(present) if cc == "all" else {cc}
        n = sum(c["cc"] in ccs for c in meta["clusters"])
        judged = sum(lab[3] for c, lab in zip(meta["clusters"], labs) if c["cc"] in ccs)
        for name in names:
            t = tally(meta, labs, *results[name][:2], ccs)
            out(f"{cc:4} {name:8} {n:8} {judged:6} | {t['tp']:7} {t['fp']:4} {t['tp'] / max(1, t['tp'] + t['fp']):6.3f} | "
                f"{t['agree']:10} {t['disagree']:4} {t['agree'] / max(1, t['agree'] + t['disagree']):6.3f} | "
                f"{t['right_vetoed']:>12}/{t['right_takes']:<11} {t['wrong_vetoed']:>9}/{t['wrong_takes']:<8} {t['weak_vetoed']:>5}/{t['weak_takes']:<5}")
    if "variant" not in results:
        return
    (v0, k0, b0), (v1, k1, b1) = results["baseline"], results["variant"]
    sections = defaultdict(list)
    for i, (c, lab) in enumerate(zip(meta["clusters"], labs)):
        venue, title = c["keys"][0]
        where = f"{c['cc']} {venue} | {title}"
        if lab[3]:
            j0, j1 = judge(int(v0[i]), lab), judge(int(v1[i]), lab)
            films = f"right {sorted(lab[0]) or '-'} wrong {sorted(lab[1]) or '-'} prod {c['film']}"
            if j1 == "FP" and j0 != "FP":
                sections["NEW FP (judged)"].append(f"{where}: votes {v1[i]} at {b1[i]} bits (baseline {fmt(v0[i])}); {films}")
            if j0 == "TP" and j1 != "TP":
                sections["LOST TP (judged)"].append(f"{where}: baseline voted {v0[i]} at {b0[i]} bits, now {fmt(v1[i])}; {films}")
            if k1[i] and not k0[i] and c["film"] in lab[0]:
                sections["NEW WRONG VETO (judged right take)"].append(f"{where}: vetoes prod's {c['film']}")
        elif c["film"] is not None:
            if v1[i] >= 0 and v1[i] != c["film"] and v1[i] != v0[i]:
                sections["NEW DISAGREEMENT (weak)"].append(f"{where}: votes {v1[i]} at {b1[i]} bits (baseline {fmt(v0[i])}); prod {c['film']}")
            if v0[i] == c["film"] and v1[i] != c["film"]:
                sections["LOST AGREEMENT (weak)"].append(f"{where}: baseline voted prod's {c['film']}, now {fmt(v1[i])}")
    for title in ("NEW FP (judged)", "LOST TP (judged)", "NEW WRONG VETO (judged right take)", "NEW DISAGREEMENT (weak)", "LOST AGREEMENT (weak)"):
        lines = sections.get(title, [])
        out(f"{title}: {len(lines)}")
        for line in lines[:25]:
            out("  " + line)
        if len(lines) > 25:
            out(f"  ... {len(lines) - 25} more")


def fmt(film):
    return "none" if film < 0 else str(film)


def run_variant(meta, arrays, images, tmdb, variant):
    venue, has, film_hashes, film_offsets = resolve(meta, arrays, images, tmdb, variant["posters"])
    d = nearest(arrays, venue, has, film_hashes, film_offsets)
    return evaluate(meta, arrays, d, variant)


# ── coverage ─────────────────────────────────────────────────────────────────────────────────────────────────────────

Buckets = ("1 no poster url", "2 feed poster (no evidence by design)", "3 url never asked", "4 asked, unread",
           "5 venue hashed, no candidates", "6 venue hashed, no candidate film hashed", "7 usable")


def bucket(row, read, unread, film_hashed):
    cc, _, _, url, feed, cands, take, _ = row
    if not url:
        return 0
    if feed:
        return 1
    if url in read:
        films = set(cands) | ({take} if take is not None else set())
        if not films:
            return 4
        return 6 if any(f"{cc}|{f}" in film_hashed for f in films) else 5
    return 3 if url in unread else 2


def coverage(meta, rows, out=print):
    read, unread, film_hashed = set(meta["venue_filed_any"]), set(meta["venue_unread"]), set(meta["film_filed_any"])
    right = defaultdict(set)
    for r in rows:
        if r.verdict == "right" and r.film.startswith("tmdb:"):
            right[(r.country, normalise(r.venue), normalise(r.rawTitle))].add(int(r.film[5:]))
    by = defaultdict(lambda: [Counter() for _ in Buckets])
    venues = defaultdict(lambda: [Counter() for _ in Buckets])
    hosts = defaultdict(lambda: [Counter() for _ in Buckets])
    for row in meta["listings"]:
        cc, venue, raw, url, _, _, take, _ = row
        b = bucket(row, read, unread, film_hashed)
        judged = right.get((cc, normalise(venue), normalise(raw))) or right.get((cc, "*", normalise(raw)))
        by[cc][b]["all"] += 1
        by[cc][b]["judged"] += bool(judged)
        by[cc][b]["right take"] += bool(judged) and take in judged
        venues[cc][b][venue] += 1
        if url:
            hosts[cc][b][urllib.parse.urlsplit(url).netloc] += 1
    for cc in [c for c in Countries if c in by]:
        total = sum(c["all"] for c in by[cc])
        out(f"== {cc}: {total} listings")
        out(f"  {'':42} {'all':>7} {'%':>5} {'judged':>6} {'right take':>10}  top venues / hosts")
        for b, name in enumerate(Buckets):
            c = by[cc][b]
            if not c["all"]:
                continue
            top_v = ", ".join(f"{v} {n}" for v, n in venues[cc][b].most_common(3))
            top_h = ", ".join(f"{h} {n}" for h, n in hosts[cc][b].most_common(3))
            out(f"  {name:42} {c['all']:7} {100 * c['all'] / total:5.1f} {c['judged']:6} {c['right take']:10}  {top_v}" + (f" | {top_h}" if top_h else ""))


# ── the command line ─────────────────────────────────────────────────────────────────────────────────────────────────

def main(argv=None):
    parser = argparse.ArgumentParser(prog="poster-eval", add_help=True)
    parser.add_argument("command", nargs="?", default="eval", choices=("eval", "coverage"))
    parser.add_argument("--posters", type=int, default=Baseline["posters"])
    parser.add_argument("--vote", type=int, default=Baseline["vote"])
    parser.add_argument("--veto-match", type=int, default=Baseline["veto_match"])
    parser.add_argument("--veto", type=int, default=Baseline["veto"])
    parser.add_argument("--countries", default=",".join(Countries))
    parser.add_argument("--refresh", action="store_true", help="re-extract the dataset from the mirror now")
    parser.add_argument("--cache", default=os.environ.get("POSTER_EVAL_CACHE", os.path.expanduser("~/.cache/movies-posters")))
    parser.add_argument("--judgements", default=os.path.join(Root, "test/resources/fixtures/identity-judgements.tsv"))
    args = parser.parse_args(argv)
    countries = [c.strip() for c in args.countries.split(",") if c.strip()]
    if unknown := set(countries) - set(Countries):
        parser.error(f"unknown countries {sorted(unknown)}")
    os.makedirs(args.cache, exist_ok=True)
    started = time.time()
    meta, arrays = dataset(args.cache, countries, args.refresh)
    rows = read_judgements(args.judgements)
    if args.command == "coverage":
        coverage(meta, rows)
    else:
        variant = {"posters": args.posters, "vote": args.vote, "veto_match": args.veto_match, "veto": args.veto}
        images, tmdb = Images(args.cache), TmdbLists(args.cache)
        results = {"baseline": run_variant(meta, arrays, images, tmdb, Baseline)}
        if variant != Baseline:
            results["variant"] = run_variant(meta, arrays, images, tmdb, variant)
        keep = [i for i, c in enumerate(meta["clusters"]) if c["cc"] in countries]
        labs = labels(meta, rows)
        report(dict(meta, clusters=[meta["clusters"][i] for i in keep]), [labs[i] for i in keep],
               {k: tuple(a[keep] for a in v) for k, v in results.items()}, variant)
    log(f"poster-eval: {time.time() - started:.1f}s")


if __name__ == "__main__":
    main()
