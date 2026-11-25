"""THE DATASET of poster_eval (the module doc of poster_eval.py says what it holds): read once from the LOCAL prod mirror,
built into a cluster table plus the (cluster, url, candidate) pair arrays, kept in <cache>/dataset.npz."""
from __future__ import annotations

import json
import os
import subprocess
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Any, Dict, List, Optional, Set, Tuple

from poster_eval_common import (Arrays, Countries, Databases, ExtractVersion, Here, Key, Mask, Meta, RefreshHours, log, np)
import mongo_local  # noqa: E402  (after poster_eval_common, which puts scripts/lib on sys.path)

Rows = List[Dict[str, Any]]

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

SyncedJs = r"""
const out = {};
for (const cc of CCS) {
  const prodDb = prodDbOf(cc);
  const saved = db.getSiblingDB(MIRRORS[cc]).getCollection("__mirror_state").findOne({_id: prodDb}) || {};
  out[cc] = resumeTokenMs(saved.resumeToken && saved.resumeToken._data);
}
print(JSON.stringify(out));
"""


def mirror_synced() -> Dict[str, float]:
    """{cc: epoch seconds} the local mirror last synced each country: the change-stream resume token freshness reads
    (local-mirror/freshness-rule.js), in one quick query -- the full freshness check runs before an extract."""
    with open(os.path.join(Here, "local-mirror", "freshness-rule.js")) as f:
        rule = f.read()
    js = f"var CCS={json.dumps(list(Countries))}; var MIRRORS={json.dumps(Databases)};\n{rule}\n{SyncedJs}"
    done = subprocess.run(["mongosh", "--quiet", mongo_local.local_mongo_uri(os.environ), "--eval", js],
                          capture_output=True, text=True, timeout=60)
    if done.returncode != 0:
        raise SystemExit(f"poster-eval: the local mirror does not answer (scripts/local-mirror/freshness):\n{done.stderr[-1000:]}")
    return {cc: (ms or 0) / 1000 for cc, ms in json.loads(done.stdout.strip().splitlines()[-1]).items()}


def check_freshness() -> None:
    done = subprocess.run([os.path.join(Here, "local-mirror", "freshness"), "--quiet", *Countries], capture_output=True, text=True)
    log(done.stdout.strip())
    if done.returncode == 3:
        log("poster-eval: WARNING the mirror is not fresh (above); reading it anyway")
    elif done.returncode != 0:
        raise SystemExit(f"poster-eval: the local mirror does not answer:\n{done.stderr}")


def read_country(cc: str) -> Tuple[str, Rows]:
    uri = mongo_local.local_mongo_uri(os.environ, Databases[cc])
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


class FiledAnswers:
    """The poster answers prod filed (`identity_family_answers` poster|venue|<url>, poster|film|<id>) over every country."""

    def __init__(self) -> None:
        self.venue: Dict[str, int] = {}  # url -> its filed hash
        self.unread: Set[str] = set()
        self.film: Dict[str, Tuple[List[int], List[str]]] = {}  # "cc|film" -> (hashes, their paths when all known)

    def add(self, cc: str, row: Dict[str, Any]) -> None:
        kind, _, ref = row["id"][len("poster|"):].partition("|")
        hashes = [int(h) & Mask for h in row["hashes"]]
        if kind == "venue":
            self._venue(ref, hashes)
        elif hashes:
            self.film[f"{cc}|{ref}"] = (hashes, row["paths"] if len(row["paths"]) == len(hashes) else [])

    def _venue(self, ref: str, hashes: List[int]) -> None:
        if hashes:
            self.venue[ref] = hashes[0]
        else:
            self.unread.add(ref)


class PairTable:
    """The (cluster, url, candidate) pair table, built once so an evaluation never loops over pairs."""

    def __init__(self, clusters: List[Dict[str, Any]]) -> None:
        self.cluster: List[int] = []
        self.cu: List[int] = []  # the (cluster, url) each pair belongs to
        self.url: List[int] = []
        self.key: List[int] = []
        self.film: List[int] = []
        self.cu_cluster: List[int] = []
        self.urls: Dict[str, int] = {}
        self.keys: Dict[str, int] = {}
        for i, c in enumerate(clusters):
            for u in c["urls"]:
                self._add(i, c, u)

    def _add(self, i: int, c: Dict[str, Any], u: str) -> None:
        cu = len(self.cu_cluster)
        self.cu_cluster.append(i)
        ui = self.urls.setdefault(u, len(self.urls))
        for f in c["cands"]:
            self.cluster.append(i)
            self.cu.append(cu)
            self.url.append(ui)
            self.key.append(self.keys.setdefault(f"{c['cc']}|{f}", len(self.keys)))
            self.film.append(f)


def _scan(cc: str, rows: Rows, listings: List[List[Any]], filed: FiledAnswers) -> Tuple[Dict[Key, str], Dict[Key, List[int]]]:
    """One country's listings (appended to `listings`) and answers (to `filed`): each venue poster that counts, and each
    listing's trace candidates."""
    poster: Dict[Key, str] = {}
    trace: Dict[Key, List[int]] = {}
    for r in rows:
        if r["t"] == "l":
            if r["url"] and not r["feed"]:
                poster[(r["venue"], r["raw"])] = r["url"]
            trace[(r["venue"], r["raw"])] = r["cands"]
            listings.append([cc, r["venue"], r["raw"], r["url"] or "", int(r["feed"]), r["cands"], r["take"], int(r["decided"])])
        elif r["t"] == "a":
            filed.add(cc, r)
    return poster, trace


def _candidates(members: List[Key], trace: Dict[Key, List[int]], film: Optional[int]) -> List[int]:
    """Every member's trace candidates, and the decision's film."""
    return sorted({c for k in members for c in trace.get(k, [])} | ({film} if film is not None else set()))


def _clusters(cc: str, rows: Rows, poster: Dict[Key, str], trace: Dict[Key, List[int]]) -> List[Dict[str, Any]]:
    """The model decisions whose members show a venue poster, each with its real candidate set."""
    out: List[Dict[str, Any]] = []
    for r in rows:
        if r["t"] != "d":
            continue
        members = [tuple(k) for k in r["keys"]]
        shown = sorted({poster[k] for k in members if k in poster})
        cands = _candidates(members, trace, r["film"])
        if shown and cands:
            out.append({"cc": cc, "fam": r["fam"], "keys": members, "film": r["film"], "basis": r["basis"], "urls": shown, "cands": cands})
    return out


def _film_arrays(key_list: List[str], filed: FiledAnswers) -> Arrays:
    offsets = [0]
    hashes: List[int] = []
    for k in key_list:
        hashes += filed.film.get(k, ([], []))[0]
        offsets.append(len(hashes))
    return {"film_hashes": np.array(hashes, np.uint64), "film_offsets": np.array(offsets, np.int64)}


def build(rows_by_country: Dict[str, Rows]) -> Tuple[Meta, Arrays]:
    """The dataset from the mirror's rows: {meta (JSON-able), arrays}."""
    clusters: List[Dict[str, Any]] = []
    listings: List[List[Any]] = []
    filed = FiledAnswers()
    for cc, rows in rows_by_country.items():
        poster, trace = _scan(cc, rows, listings, filed)
        clusters += _clusters(cc, rows, poster, trace)
    table = PairTable(clusters)
    url_list, key_list = list(table.urls), list(table.keys)
    meta: Meta = {"version": ExtractVersion, "clusters": clusters, "listings": listings, "urls": url_list, "keys": key_list,
                  "venue_unread": sorted(filed.unread),
                  "venue_filed_any": sorted(filed.venue),  # every url with a filed hash, coverage's "asked and read"
                  "film_filed_any": sorted(filed.film),
                  "film_paths": {k: filed.film[k][1] for k in key_list if k in filed.film}}
    arrays: Arrays = {
        "pair_cluster": np.array(table.cluster, np.int32), "pair_cu": np.array(table.cu, np.int32),
        "pair_url": np.array(table.url, np.int32), "pair_key": np.array(table.key, np.int32),
        "pair_film": np.array(table.film, np.int64), "cu_cluster": np.array(table.cu_cluster, np.int32),
        "venue_filed": np.array([filed.venue.get(u, 0) for u in url_list], np.uint64),
        "venue_has": np.array([u in filed.venue for u in url_list], bool),
        **_film_arrays(key_list, filed),
    }
    return meta, arrays


def save(path: str, meta: Meta, arrays: Arrays) -> None:
    tmp = path + ".tmp.npz"
    np.savez(tmp, meta=np.frombuffer(json.dumps(meta).encode(), np.uint8), **arrays)
    os.replace(tmp, path)


def load(path: str) -> Tuple[Meta, Arrays]:
    with np.load(path) as z:
        meta: Meta = json.loads(z["meta"].tobytes())
        arrays: Arrays = {k: z[k] for k in z.files if k != "meta"}
    return meta, arrays


def restrict(meta: Meta, arrays: Arrays, countries: List[str]) -> Tuple[Meta, Arrays]:
    """The dataset's listings of `countries` alone; every cluster stays (an evaluation reports only `countries`')."""
    return dict(meta, listings=[r for r in meta["listings"] if r[0] in countries]), arrays


def dataset(cache: str, countries: List[str], refresh: bool) -> Tuple[Meta, Arrays]:
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
