#!/usr/bin/env python3
"""Consolidated, single-writer German cinema harvest via the Decodo proxy.
Reuses fetch()/parsers from crawl_filmstarts.py. Handles NRW's shell page-1 +
repeating-footer pagination by stopping a lander only after 2 consecutive pages
that add NO new city AND NO new theater. Also crawls the homepage-featured major
cities (Köln/Düsseldorf/Dortmund/… which the lander sub-pages omit). Writes once."""
import json, re, collections, importlib.util
spec = importlib.util.spec_from_file_location("cf", "/private/tmp/claude-501/-Users-pawel-projects-movies/1ba17d36-2dc1-4627-9f24-d3ad740d9dab/scratchpad/crawl_filmstarts.py")
cf = importlib.util.module_from_spec(spec); spec.loader.exec_module(cf)
BASE = cf.BASE

by_id = {}
def add(rows):
    for tid, tn, city, bl in rows:
        if tid and tid not in by_id:
            by_id[tid] = {"theaterId": tid, "name": tn, "city": city or "",
                          "citySlug": cf.slugify(city or ""), "bundesland": bl or "?"}

index = cf.fetch(f"{BASE}/kinoprogramm/")
landers = list(dict((m.group(1), cf.strip_tags(m.group(2)))
                    for m in cf.LANDER_LINK_RE.finditer(index)).items())
print(f"{len(landers)} Bundesländer")
all_city_links = {}   # cid -> (name, bundesland)

for lpath, bl in landers:
    no_new, p = 0, 1
    while p <= 40 and no_new < 2:
        html = cf.fetch(f"{BASE}{lpath}?page={p}")
        cl = cf.parse_stadte_links(html)
        th = list(cf.parse_theater_cards(html))
        new = False
        for cid, name in cl.items():
            if cid not in all_city_links:
                all_city_links[cid] = (name, bl); new = True
        before = len(by_id); add((t, n, c, bl) for t, n, c in th)
        if len(by_id) > before: new = True
        no_new = 0 if new else no_new + 1
        p += 1
    print(f"  {bl}: {p-1} pages")

# Homepage-featured major cities (listed separately from the lander sub-pages)
home = cf.parse_stadte_links(cf.fetch(f"{BASE}/kinoprogramm/"))
for cid, name in home.items():
    all_city_links.setdefault(cid, (name, "?"))
print(f"{len(all_city_links)} distinct city pages to crawl")

for i, (cid, (name, bl)) in enumerate(all_city_links.items(), 1):
    add(cf.crawl_city(cid, name, bl))
    if i % 50 == 0: print(f"  ...{i}/{len(all_city_links)} city pages")

theaters = sorted(by_id.values(), key=lambda x: (x["bundesland"], x["city"], x["name"]))
json.dump(theaters, open("de-theaters.json", "w"), ensure_ascii=False, indent=2)
cc = collections.Counter(x["city"] for x in theaters if x["city"])
cities = [{"city": c, "citySlug": cf.slugify(c), "theaters": n} for c, n in cc.most_common()]
json.dump(cities, open("de-cities.json", "w"), ensure_ascii=False, indent=2)
print(f"\nWROTE {len(theaters)} theaters, {len(cities)} cities. requests={cf.stats}")
for c in ["Berlin", "Köln", "München", "Hamburg", "Düsseldorf", "Dortmund", "Essen", "Bochum", "Duisburg", "Frankfurt am Main"]:
    print(f"  {c}: {cc.get(c,0)}")
