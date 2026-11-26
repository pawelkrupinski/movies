"""IMAGES AND HASHES of poster_eval: the image cache and the hash cache, the fetches (a concurrency cap a 429/503 halves),
TMDB's poster lists, and PosterHashing.chosen."""
from __future__ import annotations

import hashlib
import itertools
import json
import os
import sqlite3
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import atomic  # noqa: E402
import poster_hash  # noqa: E402
from poster_eval_common import Language, UserAgent, log  # noqa: E402

Poster = Tuple[str, Optional[str], int]  # TMDB's file path, its language, its votes
Hashed = Optional[int]  # the hash; None: unreadable or unfetchable; -1 (Images.hash_of only): not hashed yet
VenueQuote = "/%:@!$&'()*+,;=-._~"


class Pool:
    """A concurrency cap a 429/503 halves."""

    def __init__(self, n: int) -> None:
        self.n, self.active, self.cv = n, 0, threading.Condition()

    def __enter__(self) -> None:
        with self.cv:
            while self.active >= self.n:
                self.cv.wait()
            self.active += 1

    def __exit__(self, *_: Any) -> None:
        with self.cv:
            self.active -= 1
            self.cv.notify_all()

    def halve(self) -> None:
        with self.cv:
            self.n = max(1, self.n // 2)
        log(f"poster-eval: 429/503, concurrency halved to {self.n}")


class Stack:
    def __init__(self, pools: Sequence[Pool]) -> None:
        self.pools = pools

    def __enter__(self) -> None:
        for p in self.pools:
            p.__enter__()

    def __exit__(self, *a: Any) -> None:
        for p in reversed(self.pools):
            p.__exit__(*a)


def http_get(url: str, pools: Sequence[Pool], tries: int = 3) -> Optional[bytes]:
    for attempt in range(tries):
        with Stack(pools):
            try:
                request = urllib.request.Request(url, headers={"User-Agent": UserAgent, "Accept": "image/*,application/json,*/*"})
                with urllib.request.urlopen(request, timeout=20) as r:
                    body: bytes = r.read()
                    return body
            except urllib.error.HTTPError as e:
                if e.code in (429, 503):
                    pools[0].halve()
                    time.sleep(2 ** attempt * 2)
                    continue
                return None
            except Exception:  # noqa: BLE001 -- a network hiccup: tried again
                time.sleep(1 + attempt)
    return None


class Progress:
    """A progress line every 10 s, and the throughput at the end."""

    def __init__(self, what: str, total: int) -> None:
        self.what, self.total, self.counts = what, total, Counter[str]()
        self.started = self.last = time.time()

    def tick(self, outcome: str) -> None:
        self.counts[outcome] += 1
        if time.time() - self.last >= 10:
            self.last = time.time()
            done = sum(self.counts.values())
            log(f"poster-eval: {self.what} {done}/{self.total} {dict(self.counts)} {done / (self.last - self.started):.0f}/s")

    def done(self) -> None:
        elapsed = time.time() - self.started
        log(f"poster-eval: {self.what} done {dict(self.counts)} in {elapsed:.0f}s, {self.total / max(elapsed, 1e-3):.1f}/s")


class Images:
    """The image cache (<cache>/img/v/<sha1(url)>, img/t/<tmdb path>) and the hash cache (<cache>/hashes.sqlite: each
    image's content sha1 and the hash of that content, so no content is hashed twice)."""

    def __init__(self, cache: str) -> None:
        self.cache = cache
        for d in ("v", "t"):
            os.makedirs(os.path.join(cache, "img", d), exist_ok=True)
        self.db = sqlite3.connect(os.path.join(cache, "hashes.sqlite"), check_same_thread=False)
        self.db.execute("CREATE TABLE IF NOT EXISTS files (name TEXT PRIMARY KEY, sha1 TEXT NOT NULL)")
        self.db.execute("CREATE TABLE IF NOT EXISTS hashes (sha1 TEXT PRIMARY KEY, hash INTEGER)")  # NULL: unreadable
        by_sha: Dict[str, Hashed] = {s: h for s, h in self.db.execute("SELECT sha1, hash FROM hashes")}
        self.known: Dict[str, Hashed] = {n: by_sha.get(s, -1) for n, s in self.db.execute("SELECT name, sha1 FROM files")}
        self.by_sha = by_sha
        self.lock = threading.Lock()

    @staticmethod
    def venue_name(url: str) -> str:
        return "v/" + hashlib.sha1(url.encode()).hexdigest()

    @staticmethod
    def film_name(path: str) -> str:
        return "t/" + path.strip("/")

    def path(self, name: str) -> str:
        return os.path.join(self.cache, "img", name)

    def hash_of(self, name: str) -> Hashed:
        """The cached hash, None when unreadable or unfetchable, -1 when not hashed yet."""
        h = self.known.get(name, -1)
        if h == -1 and os.path.exists(self.path(name) + ".fail"):
            return None
        return h

    def ensure(self, wanted: Dict[str, str]) -> None:
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
            for _ in pool.map(self._add_cached, present):
                progress.tick("hashed")
        with ThreadPoolExecutor(16) as pool:
            for outcome in pool.map(Fetcher(self), _interleaved(missing)):
                progress.tick(outcome)
        with self.lock:
            self.db.commit()
        progress.done()

    def _add_cached(self, name: str) -> None:
        self.add(name, self.path(name))

    def add(self, name: str, path: str) -> None:
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


def _interleaved(missing: Iterable[Tuple[str, str]]) -> List[Tuple[str, str]]:
    """The images to fetch, one host's after another's in turn."""
    by_host: Dict[str, List[Tuple[str, str]]] = defaultdict(list)
    for name, url in missing:
        by_host[urllib.parse.urlsplit(url).netloc].append((name, url))
    return [item for group in itertools.zip_longest(*by_host.values()) for item in group if item]


class Fetcher:
    """One image's fetch into the cache, under the CDN's, the venues' and each host's concurrency caps."""

    def __init__(self, images: Images) -> None:
        self.images = images
        self.cdn, self.venues = Pool(8), Pool(8)
        self.hosts: Dict[str, Pool] = defaultdict(lambda: Pool(2))
        self.hosts_lock = threading.Lock()

    def body(self, name: str, url: str) -> Optional[bytes]:
        if not name.startswith("v/"):
            return http_get(url, [self.cdn])
        parts = urllib.parse.urlsplit(url)
        quoted = parts._replace(path=urllib.parse.quote(parts.path, safe=VenueQuote)).geturl()
        with self.hosts_lock:
            host = self.hosts[parts.netloc]
        return http_get(quoted, [self.venues, host], tries=2)

    def __call__(self, item: Tuple[str, str]) -> str:
        name, url = item
        path = self.images.path(name)
        body = self.body(name, url)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        if not body:
            open(path + ".fail", "w").close()
            return "unfetched"
        with open(path, "wb") as f:
            f.write(body)
        self.images.add(name, path)
        return "fetched"


def chosen(posters: Iterable[Poster], cc: str, n: int) -> List[str]:
    """PosterHashing.chosen: the country's language, English, none, the rest; each by votes; then the path."""
    language = Language[cc]

    def order(p: Poster) -> Tuple[int, int, str]:
        path, lang, votes = p
        return (0 if lang == language else 1 if lang == "en" else 2 if lang is None else 3, -votes, path)
    return list(dict.fromkeys(p[0] for p in sorted(posters, key=order)))[:n]


class TmdbLists:
    """TMDB's /movie/{id}/images poster lists, cached in <cache>/tmdb-images.json ({id: [[path, language, votes]]})."""

    def __init__(self, cache: str) -> None:
        self.path = os.path.join(cache, "tmdb-images.json")
        self.lists: Dict[str, List[List[Any]]] = {}
        if os.path.exists(self.path):
            with open(self.path) as f:
                self.lists = json.load(f)

    def ensure(self, films: Iterable[int]) -> None:
        need = sorted({str(f) for f in films} - set(self.lists))
        if not need:
            return
        key = os.environ.get("TMDB_API_KEY")
        if not key:
            raise SystemExit(f"poster-eval: {len(need)} films' TMDB poster lists are not cached; run under "
                             "`secrets run movies TMDB_API_KEY -- scripts/poster-eval ...`")
        log(f"poster-eval: fetching {len(need)} TMDB poster lists")
        started = time.time()
        api = Pool(8)

        def one(f: str) -> Tuple[str, List[List[Any]]]:
            body = http_get(f"https://api.themoviedb.org/3/movie/{f}/images?api_key={key}", [api])
            posters: List[Dict[str, Any]] = json.loads(body).get("posters", []) if body else []
            return f, [[p["file_path"], p.get("iso_639_1"), p.get("vote_count", 0)] for p in posters]
        with ThreadPoolExecutor(8) as pool:
            for f, posters in pool.map(one, need):
                self.lists[f] = posters
        atomic.write_json(self.path, self.lists)
        log(f"poster-eval: {len(need)} lists in {time.time() - started:.0f}s")

    def posters(self, film: object) -> List[Poster]:
        return [(p[0], p[1], p[2]) for p in self.lists.get(str(film), [])]
