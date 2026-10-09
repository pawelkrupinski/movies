#!/usr/bin/env python3
"""Fetch every SensaCine venue's postal address into data/spain/venue-addresses.json.

The town a venue is filed under in `theaters-raw.json` is the nearest preceding
section header on SensaCine's province listing, and that header is not always the
venue's town: "Estacion De Espiel" (Córdoba) heads Huércal-Overa's municipal
cinema (Almería) and Mota del Cuervo's (Cuenca), "Fraile" (Jaén) heads the
Autocinema Tenerife. While a page was a whole province that cost little; now that
a page is a town, the town has to be right.

Each venue's own page (`/cines/cine/<theaterId>/`) carries a schema.org
`PostalAddress` — `streetAddress`, `postalCode`, `addressLocality` — and that is
what `build_venue_towns.py` reads instead. The postal code's first two digits are
the province (INE numbering), which is how a venue filed under the wrong province
is caught.

Five concurrent workers, halved on any 429/503 (the repo's default budget for a
scraped site); SensaCine served the whole run without one.

Usage:  python3 data/spain/scripts/fetch_venue_addresses.py
"""
import concurrent.futures
import json
import pathlib
import re
import sys
import threading
import time
import urllib.error
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
OUT = DATA / "venue-addresses.json"
URL = "https://www.sensacine.com/cines/cine/{}/"
UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/126.0 Safari/537.36")
FIELDS = ("streetAddress", "postalCode", "addressLocality")


def parse(html: str) -> dict:
    """The venue's PostalAddress fields, from the page's JSON-LD."""
    out = {}
    for field in FIELDS:
        m = re.search(r'"%s"\s*:\s*"((?:[^"\\]|\\.)*)"' % field, html)
        if m:
            out[field] = json.loads('"' + m.group(1) + '"').strip()
    return out


class Pool:
    """Concurrency that halves on a 429/503 and never grows back within a run."""

    def __init__(self, workers: int):
        self.permits = threading.Semaphore(workers)
        self.lock = threading.Lock()
        self.workers = workers
        self.throttled = 0

    def throttle(self):
        with self.lock:
            self.throttled += 1
            if self.workers > 1:
                self.workers //= 2
                self.permits.acquire(blocking=False)


def fetch(theater_id: str, pool: Pool) -> tuple[str, dict]:
    for attempt in range(4):
        with pool.permits:
            try:
                req = urllib.request.Request(URL.format(theater_id), headers={"User-Agent": UA})
                with urllib.request.urlopen(req, timeout=30) as resp:
                    return theater_id, parse(resp.read().decode("utf-8", "replace"))
            except urllib.error.HTTPError as e:
                if e.code in (429, 503):
                    pool.throttle()
                elif e.code in (404, 410):
                    return theater_id, {"gone": e.code}
            except (urllib.error.URLError, TimeoutError):
                pass
        time.sleep(2 ** attempt)
    return theater_id, {}


def main() -> int:
    raw = json.loads((DATA / "theaters-raw.json").read_text())
    ids = sorted({t["theaterId"] for t in raw})
    pool = Pool(5)
    start = time.time()
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as ex:
        results = dict(ex.map(lambda i: fetch(i, pool), ids))
    missing = sorted(i for i, r in results.items() if not r)
    OUT.write_text(json.dumps(results, ensure_ascii=False, indent=1, sort_keys=True) + "\n")
    took = time.time() - start
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(ids)} venues in {took:.0f}s "
          f"({len(ids) / took:.1f}/s), {pool.throttled} throttled, {len(missing)} without an address")
    if missing:
        print(f"  no address: {missing}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
