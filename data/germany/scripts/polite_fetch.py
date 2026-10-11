"""A polite, cached fetcher for the German harvest scripts.

A fixed worker pool under a global request rate, whose concurrency HALVES on
every 429/503 -- and, once down to one worker, whose rate halves instead --
backing off exponentially before retrying that request, with
every answer -- 404s included -- cached gzip-compressed on disk, so a rerun
costs nothing but the requests it has not made yet. Ends with a throughput line
(`report`). Used by harvest_kinoprogramm.py, harvest_kinoheld.py and
sweep_filmstarts_ids.py; per-service limits are in the
`external-api-rate-limits` skill.
"""

import gzip
import hashlib
import json
import os
import re
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor

BROWSER_UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
              "(KHTML, like Gecko) Chrome/126 Safari/537.36")


def cache_dir(name, env, home, override=None):
    """`env[override]` when set, else `<cache home>/kinowo/<name>` -- the user's cache
    directory, never a session scratchpad: it must outlive the session that ran it."""
    if override and env.get(override):
        return env[override]
    base = env.get("XDG_CACHE_HOME") or str(home / ".cache")
    return os.path.join(base, "kinowo", name)


class PoliteFetcher:
    def __init__(self, base, cache, workers, max_rps, headers=None, timeout=40, max_attempts=5,
                 max_backoff=120, min_rps=0.2):
        self.base = base
        self.cache = cache
        self.workers = workers
        self.max_rps = max_rps
        self.headers = {"User-Agent": BROWSER_UA, **(headers or {})}
        self.timeout = timeout
        self.max_attempts = max_attempts
        self.max_backoff = max_backoff
        self.min_rps = min_rps
        os.makedirs(os.path.join(cache, "pages"), exist_ok=True)
        self.lock = threading.Lock()
        self.next_slot = 0.0
        self.limit = workers
        self.active = 0
        self.cond = threading.Condition()
        self.requests = 0
        self.cache_hits = 0
        self.throttled = 0
        self.failures = 0
        self.bytes = 0
        self.started = time.time()

    def _key(self, path, body):
        safe = re.sub(r"[^a-zA-Z0-9._-]+", "_", path.strip("/"))[:120]
        digest = hashlib.sha1((path + "\0" + (body or "")).encode()).hexdigest()[:10]
        return os.path.join(self.cache, "pages", f"{safe}-{digest}")

    def _throttle(self):
        with self.lock:
            now = time.time()
            slot = max(now, self.next_slot)
            self.next_slot = slot + 1.0 / self.max_rps
        if slot > now:
            time.sleep(slot - now)

    def get(self, path):
        """(status, final_path, text) for a GET of `path` under the base URL."""
        return self._cached(path, None)

    def post_json(self, path, payload):
        """(status, final_path, text) for a JSON POST -- cached by path AND body."""
        return self._cached(path, json.dumps(payload, sort_keys=True))

    def _cached(self, path, body):
        key = self._key(path, body)
        if os.path.exists(key + ".json"):
            with open(key + ".json") as f:
                meta = json.load(f)
            text = ""
            if os.path.exists(key + ".gz"):
                with gzip.open(key + ".gz", "rt", encoding="utf-8") as f:
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
                status, final, text = self._fetch(path, body)
            finally:
                with self.cond:
                    self.active -= 1
                    self.cond.notify_all()
            if status in (429, 503) or status < 0:
                attempt += 1
                if status in (429, 503):
                    with self.cond:
                        self.throttled += 1
                        if self.limit > 1:
                            self.limit //= 2
                        else:
                            # Already one at a time: slow the pace itself.
                            self.max_rps = max(self.min_rps, self.max_rps / 2)
                    print(f"  throttled {status} on {path}; concurrency -> {self.limit}, "
                          f"{self.max_rps:.2f} req/s", file=sys.stderr)
                if attempt >= self.max_attempts:
                    # Not cached: the next rerun tries again.
                    print(f"  giving up on {path}: {status}", file=sys.stderr)
                    with self.lock:
                        self.failures += 1
                    return status, path, ""
                time.sleep(min(self.max_backoff, 2 ** attempt * 4))
                continue
            break

        with open(key + ".json", "w") as f:
            json.dump({"path": path, "status": status, "final": final}, f)
        if text:
            with gzip.open(key + ".gz", "wt", encoding="utf-8") as f:
                f.write(text)
        return status, final, text

    def _fetch(self, path, body):
        url = self.base + urllib.parse.quote(path, safe="/?=&%-._~")
        headers = dict(self.headers)
        data = None
        if body is not None:
            data = body.encode()
            headers["Content-Type"] = "application/json"
        req = urllib.request.Request(url, data=data, headers=headers)
        with self.lock:
            self.requests += 1
            if self.requests % 100 == 0:
                print(f"  ... {self.requests} requests, "
                      f"{self.requests / (time.time() - self.started):.2f} req/s", file=sys.stderr)
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as resp:
                raw = resp.read()
                final = urllib.parse.urlsplit(resp.geturl())
                final_path = final.path + ("?" + final.query if final.query else "")
                with self.lock:
                    self.bytes += len(raw)
                return resp.status, final_path, raw.decode("utf-8", "replace")
        except urllib.error.HTTPError as e:
            return e.code, path, ""
        except Exception as e:  # network hiccup: retried by the caller
            print(f"  error {path}: {e}", file=sys.stderr)
            return -1, path, ""

    def map(self, fn, items):
        with ThreadPoolExecutor(self.workers) as pool:
            return list(pool.map(fn, items))

    def report(self):
        elapsed = time.time() - self.started
        rate = self.requests / elapsed if elapsed else 0
        print(f"requests: {self.requests} network, {self.cache_hits} cache hits,"
              f" {self.bytes / 1e6:.1f} MB in {elapsed:.0f}s -> {rate:.2f} req/s;"
              f" 429/503: {self.throttled}; gave up: {self.failures}")
