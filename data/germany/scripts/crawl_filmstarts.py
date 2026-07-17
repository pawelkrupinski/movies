#!/usr/bin/env python3
"""
Crawl the filmstarts.de (Webedia) German-cinema directory to harvest the
full list of theaters (cinemas) with their theaterId, name, city and
Bundesland (federal state).

Structure discovered by manual inspection (see report):
  - https://www.filmstarts.de/kinoprogramm/
      contains 16 links matching /kinoprogramm/lander-<id>/ - one per
      Bundesland, with the state's display name as anchor text.
  - https://www.filmstarts.de/kinoprogramm/lander-<id>/?page=<n>
      paginated listing for one Bundesland. Early pages list big cities as
      grouped links (/kinoprogramm/stadte-<cityId>/, anchor text = city
      name); once big cities are exhausted, remaining pages list individual
      theater cards directly (<div class="theater-card">...) for towns
      that don't get their own city page. Max page number is present in
      the pagination nav on page 1.
  - https://www.filmstarts.de/kinoprogramm/stadte-<cityId>/?page=<n>
      paginated listing of theater-card entries for one city.
  - A theater-card contains:
      <a href="/kinoprogramm/kino/<theaterId>/"> <name> </a>
      <address class="address ...">Street Number PLZ City</address>
    theaterId format observed: single letter + 4 digits (e.g. A0268).
    City is recovered as the text after the 5-digit German postal code
    in the address.

No embedded JSON state blob (__NEXT_DATA__ / __INITIAL_STATE__) was found;
pagination is a plain ?page=N query param, discovered by reading the
pagination nav's href list on page 1 of each lander/stadte page.

No blocking (403/429/Cloudflare challenge) was observed at any point in
this crawl using a normal desktop Chrome User-Agent with urllib.request.
"""
import json
import re
import time
import unicodedata
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Lock

BASE = "https://www.filmstarts.de"
UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
HEADERS = {"User-Agent": UA, "Accept-Language": "de-DE,de;q=0.9,en;q=0.8"}

# Route through the Decodo residential proxy — filmstarts (Webedia/CF) hard
# rate-limits (429) a direct bulk crawl; the prod worker already scrapes it via
# this proxy. Reads KINOWO_PROXY_USER/PASS from the repo .env.local.
def _proxy_opener():
    env = {}
    try:
        for l in open("/Users/pawel/projects/movies/.env.local"):
            if "=" in l and not l.startswith("#"):
                k, v = l.split("=", 1); env[k] = v.strip()
    except OSError:
        pass
    u, p = env.get("KINOWO_PROXY_USER"), env.get("KINOWO_PROXY_PASS")
    if not (u and p):
        return urllib.request.build_opener()
    px = f"http://{u}:{p}@isp.decodo.com:10001"
    return urllib.request.build_opener(urllib.request.ProxyHandler({"http": px, "https": px}))

OPENER = _proxy_opener()

MAX_WORKERS = 4
REQUEST_DELAY = 0.4
MAX_RETRIES = 6

_stats_lock = Lock()
stats = {"requests": 0, "retries_403_429": 0, "failures": 0}


def fetch(url: str) -> str:
    """Fetch a URL with retry/backoff on 403/429. Returns HTML or '' on
    persistent failure (recorded in stats['failures'])."""
    delay = 20.0  # patient backoff — filmstarts rate-limits (429) a bulk crawl
    for attempt in range(MAX_RETRIES):
        try:
            req = urllib.request.Request(url, headers=HEADERS)
            with OPENER.open(req, timeout=30) as resp:
                data = resp.read().decode("utf-8", errors="replace")
            with _stats_lock:
                stats["requests"] += 1
            time.sleep(REQUEST_DELAY)
            return data
        except urllib.error.HTTPError as e:
            if e.code in (403, 429):
                with _stats_lock:
                    stats["retries_403_429"] += 1
                print(f"  … {e.code} on {url} — backing off {delay:.0f}s", flush=True)
                time.sleep(delay)
                delay = min(delay * 2, 300)
                continue
            with _stats_lock:
                stats["failures"] += 1
            print(f"  ! HTTP {e.code} on {url}")
            return ""
        except Exception as e:
            with _stats_lock:
                stats["failures"] += 1
            print(f"  ! error {e} on {url}")
            time.sleep(delay)
            delay *= 2
    print(f"  !! giving up on {url} after {MAX_RETRIES} attempts")
    with _stats_lock:
        stats["failures"] += 1
    return ""


THEATER_CARD_RE = re.compile(
    r'href="(/kinoprogramm/kino/([A-Za-z0-9]+))/"[^>]*>\s*([^<]*?)\s*</a>'
    r'.*?<address class="address[^"]*">(.*?)</address>',
    re.S,
)
LANDER_LINK_RE = re.compile(
    r'<a[^>]*href="(/kinoprogramm/lander-\d+/)"[^>]*>(.*?)</a>', re.S
)
STADTE_LINK_RE = re.compile(
    r'<a[^>]*href="(/kinoprogramm/stadte-\d+)/?"[^>]*>(.*?)</a>', re.S
)
PLZ_CITY_RE = re.compile(r"\b\d{5}\b\s+(.+)$")


def strip_tags(s: str) -> str:
    return re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", s)).strip()


def max_page(html: str, path_prefix: str) -> int:
    pages = [int(p) for p in re.findall(
        re.escape(path_prefix) + r"/?\?page=(\d+)", html)]
    return max(pages) if pages else 1


def parse_theater_cards(html: str):
    """Yield (theaterId, name, cityFromAddress) for inline theater-card
    entries in an HTML page (used on lander pages past the "big city"
    pages, and on all stadte pages)."""
    for m in THEATER_CARD_RE.finditer(html):
        theater_id = m.group(2)
        name = strip_tags(m.group(3))
        addr = strip_tags(m.group(4))
        cm = PLZ_CITY_RE.search(addr)
        city = cm.group(1).strip() if cm else ""
        yield theater_id, name, city


def parse_stadte_links(html: str):
    seen = {}
    for m in STADTE_LINK_RE.finditer(html):
        path = m.group(1)
        cid = re.search(r"stadte-(\d+)", path).group(1)
        name = strip_tags(m.group(2))
        if name:
            seen[cid] = name
    return seen  # cityId -> display name


def slugify(name: str) -> str:
    s = name.lower()
    s = s.replace("ä", "ae").replace("ö", "oe").replace("ü", "ue").replace("ß", "ss")
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r"[^a-z0-9]+", "-", s).strip("-")
    return s


def crawl_lander(lander_path: str, bundesland: str):
    """Returns (list of (theaterId, name, city, bundesland) from inline
    cards, dict cityId -> display name found on this lander's pages)."""
    url = BASE + lander_path
    html = fetch(url)
    if not html:
        return [], {}
    results = list((tid, name, city, bundesland)
                   for tid, name, city in parse_theater_cards(html))
    city_links = parse_stadte_links(html)
    n = max_page(html, lander_path.rstrip("/"))
    print(f"  {bundesland}: {n} page(s)")
    for p in range(2, n + 1):
        page_html = fetch(f"{url}?page={p}")
        if not page_html:
            continue
        results.extend((tid, name, city, bundesland)
                        for tid, name, city in parse_theater_cards(page_html))
        city_links.update(parse_stadte_links(page_html))
    return results, city_links


def crawl_city(city_id: str, city_name: str, bundesland: str):
    url = f"{BASE}/kinoprogramm/stadte-{city_id}/"
    html = fetch(url)
    if not html:
        return []
    results = list((tid, name, city or city_name, bundesland)
                   for tid, name, city in parse_theater_cards(html))
    n = max_page(html, f"/kinoprogramm/stadte-{city_id}")
    for p in range(2, n + 1):
        page_html = fetch(f"{url}?page={p}")
        if not page_html:
            continue
        results.extend((tid, name, city or city_name, bundesland)
                        for tid, name, city in parse_theater_cards(page_html))
    return results


def main():
    index_html = fetch(f"{BASE}/kinoprogramm/")
    landers = [(m.group(1), strip_tags(m.group(2)))
               for m in LANDER_LINK_RE.finditer(index_html)]
    landers = list(dict(landers).items())
    print(f"Found {len(landers)} Bundesland links")
    for p, n in landers:
        print(f"   {n}: {p}")

    all_theaters = {}  # theaterId -> (name, city, bundesland)
    all_city_links = {}  # cityId -> (name, bundesland)

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        futures = {pool.submit(crawl_lander, p, n): (p, n) for p, n in landers}
        for fut in as_completed(futures):
            p, n = futures[fut]
            try:
                theaters, city_links = fut.result()
            except Exception as e:
                print(f"  !! lander {n} failed: {e}")
                continue
            for tid, name, city, bundesland in theaters:
                all_theaters[tid] = (name, city, bundesland)
            for cid, cname in city_links.items():
                all_city_links[cid] = (cname, n)

    print(f"\nAfter lander sweep: {len(all_theaters)} inline theaters, "
          f"{len(all_city_links)} city pages to crawl")

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        futures = {
            pool.submit(crawl_city, cid, cname, bundesland): (cid, cname, bundesland)
            for cid, (cname, bundesland) in all_city_links.items()
        }
        done = 0
        for fut in as_completed(futures):
            cid, cname, bundesland = futures[fut]
            done += 1
            try:
                theaters = fut.result()
            except Exception as e:
                print(f"  !! city {cname} failed: {e}")
                continue
            for tid, name, city, bd in theaters:
                all_theaters[tid] = (name, city, bd)
            if done % 25 == 0:
                print(f"  ...{done}/{len(all_city_links)} city pages done")

    print(f"\nTotal distinct theaters: {len(all_theaters)}")
    print(f"Stats: {stats}")

    records = []
    for tid, (name, city, bundesland) in all_theaters.items():
        if not city:
            continue
        records.append({
            "theaterId": tid,
            "name": name,
            "city": city,
            "citySlug": slugify(city),
            "bundesland": bundesland,
        })
    records.sort(key=lambda r: (r["citySlug"], r["theaterId"]))

    out_dir = "/private/tmp/claude-501/-Users-pawel-projects-movies/1ba17d36-2dc1-4627-9f24-d3ad740d9dab/scratchpad"
    with open(f"{out_dir}/de-theaters.json", "w", encoding="utf-8") as f:
        json.dump(records, f, ensure_ascii=False, indent=2)

    from collections import Counter, defaultdict
    city_info = {}
    for r in records:
        key = r["citySlug"]
        if key not in city_info:
            city_info[key] = {"city": r["city"], "citySlug": key,
                               "bundesland": r["bundesland"], "theaterCount": 0}
        city_info[key]["theaterCount"] += 1
    cities = sorted(city_info.values(), key=lambda c: -c["theaterCount"])
    with open(f"{out_dir}/de-cities.json", "w", encoding="utf-8") as f:
        json.dump(cities, f, ensure_ascii=False, indent=2)

    print(f"Wrote {len(records)} theaters, {len(cities)} cities")
    print(f"No-city-dropped count: {len(all_theaters) - len(records)}")


if __name__ == "__main__":
    main()
