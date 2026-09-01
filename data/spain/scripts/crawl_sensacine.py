#!/usr/bin/env python3
"""
Crawl the sensacine.com (Webedia/AlloCiné, Spanish deployment) cinema
directory to harvest every theater (cinema) in Spain, keyed by province.

Structure (verified by manual inspection, see data/spain/README.md):
  - https://www.sensacine.com/cines/
      links to all 52 provinces at /cines/provincias-<id>/ (anchor text /
      title attribute = province display name). It also links to
      /cines/ciudades-<id>/ (21 major-city shortcuts) and
      /cines/circuitos-<id>/ (chains) -- both IGNORED; the 52 provinces are
      a complete partition of Spain (50 provinces + Ceuta + Melilla).
  - https://www.sensacine.com/cines/provincias-<id>/?page=N
      paginated listing for one province (page 1 has no ?page= suffix).
      Venues are grouped under a per-town header:
        <div class="titlebar section-title"><h2 class="titlebar-title
        titlebar-title-md" ><a class="titlebar-link"
        href="/cines/ciudades-<id>/">TOWN</a></h2></div>
      -- or, when the town link is lazy-loaded/obfuscated, the same h2 with
      a <span> instead of an <a>. Either way the visible text is the town
      name and it always precedes the venues that belong to it.
  - Each theater is announced by a `data-theater` attribute holding an
    HTML-entity-escaped JSON blob:
        <span class="add-theater-anchor"
              data-theater="{&quot;id&quot;:&quot;E0291&quot;,&quot;name&quot;:
                             &quot;Yelmo Cines Premium Parque Corredor&quot;}">
      giving both the theaterId and the display name directly -- this is
      the ONLY reliable extraction path. A regex over
      href="/cines/cine/E\\d+/" undercounts badly (3 of 29 on one page)
      because most cards render the id only via this JSON attribute.
  - Pagination stops when a page yields no new theater ids (defensive: the
    task's manual check found Madrid has exactly 3 pages, but we don't
    hard-code that -- we keep paging any province until it stops adding).

Politeness: plain requests, no proxy needed (no 403/429 observed against
this host with a realistic desktop Chrome UA). ~400ms between requests,
sequential. Retries once on any fetch failure before giving up on a page.

Output: data/spain/theaters-raw.json, one object per venue:
  {"theaterId", "name", "town", "provinceId", "provinceName"}
sorted by (provinceName, theaterId) for determinism.
"""
import html
import json
import re
import sys
import time
import urllib.error
import urllib.request

BASE = "https://www.sensacine.com"
UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
HEADERS = {"User-Agent": UA, "Accept-Language": "es-ES,es;q=0.9,en;q=0.8"}
REQUEST_DELAY = 0.4
OUT_PATH = "data/spain/theaters-raw.json"

PROVINCE_RE = re.compile(
    r'<a class="mdl-more-item" href="/cines/provincias-(\d+)/" title="([^"]*)"'
)
# Matches the per-town section header, town name in either an <a> or a <span>.
TOWN_RE = re.compile(
    r'<h2 class="titlebar-title titlebar-title-md" >'
    r'(?:<a[^>]*>([^<]*)</a>|<span[^>]*>([^<]*)</span>)</h2>'
)
THEATER_RE = re.compile(r'data-theater="([^"]*)"')

stats = {"requests": 0, "retries": 0, "failures": 0}


def fetch(url: str) -> str:
    """GET a URL, retrying once on any failure. Returns '' on persistent failure."""
    for attempt in range(2):
        try:
            req = urllib.request.Request(url, headers=HEADERS)
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = resp.read().decode("utf-8", errors="replace")
            stats["requests"] += 1
            time.sleep(REQUEST_DELAY)
            return data
        except (urllib.error.URLError, urllib.error.HTTPError, TimeoutError) as e:
            stats["retries" if attempt == 0 else "failures"] += 1
            print(f"  ! fetch failed ({e}) on {url}"
                  f"{' -- retrying' if attempt == 0 else ' -- giving up'}",
                  file=sys.stderr)
            time.sleep(2.0)
    return ""


def parse_theaters_in_order(page_html: str):
    """Walk the page in document order, yielding (town, theaterId, name) for
    every venue, attributed to the most recently seen town header."""
    markers = []
    for m in TOWN_RE.finditer(page_html):
        town = html.unescape(m.group(1) or m.group(2) or "").strip()
        markers.append((m.start(), "town", town))
    for m in THEATER_RE.finditer(page_html):
        raw = html.unescape(m.group(1))
        try:
            obj = json.loads(raw)
        except json.JSONDecodeError:
            continue
        tid, name = obj.get("id"), obj.get("name")
        if tid and name:
            markers.append((m.start(), "theater", (tid, name)))
    markers.sort(key=lambda t: t[0])

    current_town = ""
    for _, kind, payload in markers:
        if kind == "town":
            current_town = payload
        else:
            tid, name = payload
            yield current_town, tid, name


def crawl_province(province_id: str, province_name: str):
    """Page through one province until a page adds no new theater id."""
    seen = {}
    page = 1
    while True:
        url = f"{BASE}/cines/provincias-{province_id}/"
        if page > 1:
            url += f"?page={page}"
        page_html = fetch(url)
        if not page_html:
            print(f"    page {page}: fetch failed, stopping province", file=sys.stderr)
            break
        before = len(seen)
        for town, tid, name in parse_theaters_in_order(page_html):
            if tid not in seen:
                seen[tid] = {
                    "theaterId": tid,
                    "name": name,
                    "town": town,
                    "provinceId": province_id,
                    "provinceName": province_name,
                }
        added = len(seen) - before
        print(f"    page {page}: +{added} (total {len(seen)})")
        if added == 0:
            break
        page += 1
    return list(seen.values())


def main():
    index_html = fetch(f"{BASE}/cines/")
    if not index_html:
        print("FATAL: could not fetch province index", file=sys.stderr)
        sys.exit(1)

    provinces = [(pid, html.unescape(name)) for pid, name in PROVINCE_RE.findall(index_html)]
    # de-dup while preserving first occurrence (the index page can list a
    # province more than once in different widgets)
    seen_ids = set()
    unique_provinces = []
    for pid, name in provinces:
        if pid not in seen_ids:
            seen_ids.add(pid)
            unique_provinces.append((pid, name))
    provinces = unique_provinces
    print(f"{len(provinces)} provinces found on the index page")
    if len(provinces) != 52:
        print(f"  WARNING: expected 52 provinces, found {len(provinces)}", file=sys.stderr)

    all_theaters = {}
    for i, (pid, name) in enumerate(sorted(provinces, key=lambda p: p[1]), 1):
        print(f"[{i}/{len(provinces)}] {name} (id={pid})")
        for t in crawl_province(pid, name):
            all_theaters[t["theaterId"]] = t

    theaters = sorted(all_theaters.values(), key=lambda t: (t["provinceName"], t["theaterId"]))
    with open(OUT_PATH, "w", encoding="utf-8") as f:
        json.dump(theaters, f, ensure_ascii=False, indent=2)

    by_province = {}
    for t in theaters:
        by_province.setdefault(t["provinceName"], 0)
        by_province[t["provinceName"]] += 1
    zero = [name for _, name in provinces if name not in by_province]

    print(f"\nWROTE {len(theaters)} unique theaters across {len(by_province)} provinces "
          f"(of {len(provinces)} total) to {OUT_PATH}")
    if zero:
        print(f"Provinces with ZERO venues: {zero}")
    print(f"requests={stats['requests']} retries={stats['retries']} failures={stats['failures']}")


if __name__ == "__main__":
    main()
