#!/usr/bin/env python3
"""Generate geodata/src/main/resources/generated/slug-index/ — every slug a country
answers to besides its pages' (read by `models.CitySlugIndexData`).

Every country owns its own city slugs (`/uk/york/` is York, `/us/york/` York,
Pennsylvania), and a known place never 404s. Per country, slug → the page it
301s to, read off the countries' committed page data:
  - every town a page covers that is not a page itself (`/uk/ilkley/` is on the
    Ilkley cluster's page … unless Ilkley is a page; `/de/bonn/` → Bonn's own);
    for Poland, the towns its venues sit in (`data/pl/venues.json`) as well;
  - every page's qualified slug (`/uk/york-north-yorkshire/` → `/uk/york/`).
A slug that is a page, or a retired slug (which `City` already 301s by its own
record), is never an alias. Where two pages of one country cover a town of one
slug, the MORE POPULOUS town's page has it.

Also, for every page whose slug another country has too, as a page or a retired
page's (`york`, the UK's and Pennsylvania's; `kent`, Washington's and the UK's
retired county), the page's qualified slug — the name installed app
builds from before slugs were per country know the page by where it is not its
slug's old owner (`models.InstalledBuildSlugs`). TEMPORARY, with
`models.LegacySlugOwners`.

Which country a bare `showtimes.cc/{slug}/` means is not decided here: see
`models.LegacySlugOwners`.

    python3 data/scripts/generate_slug_index.py
"""
import collections
import importlib.util
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
OUT = ROOT / "geodata" / "src" / "main" / "resources" / "generated" / "slug-index"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import town_pages  # noqa: E402
from generated_tables import write_table  # noqa: E402


def _module(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load(path):
    return json.loads((ROOT / path).read_text(encoding="utf-8"))


def country_data():
    """code → (pages, retired, town key → (display name, population), slug function)."""
    uk_towns = load("data/uk/towns.json")["towns"]
    us_towns = load("data/us/towns.json")["towns"]
    de_coords = load("data/germany/city-coords.json")
    es_towns = load("data/spain/town-coords.json")["towns"]
    de_slug = _module(ROOT / "data/germany/scripts/build_pages.py", "germany_build_pages").de_slug
    out = {
        "pl": (load("data/pl/pages.json"), {}, town_pages.slugify),
        "uk": (load("data/uk/pages.json"), {k: (t["name"], t["population"]) for k, t in uk_towns.items()},
               town_pages.slugify),
        "de": (load("data/germany/pages.json"), {k: (k, t.get("population", 0)) for k, t in de_coords.items()},
               de_slug),
        "us": (load("data/us/pages.json"), {k: (t["name"], t["population"]) for k, t in us_towns.items()},
               town_pages.slugify),
        "es": (load("data/spain/pages.json"), {k: (k, t.get("pop", 0)) for k, t in es_towns.items()},
               town_pages.slugify),
    }
    return out


def polish_venue_towns():
    """Polish page slug → the towns its venues sit in (`data/pl/venues.json`)."""
    pages = load("data/pl/pages.json")["pages"]
    page_of = {c: p["slug"] for p in pages for c in p["cinemas"]}
    towns = collections.defaultdict(list)
    for v in load("data/pl/venues.json"):
        if v.get("town") and v["cinemaObject"] in page_of:
            towns[page_of[v["cinemaObject"]]].append(v["town"])
    return towns


def index(code, data, towns, slug_of, extra_towns=None):
    """alias slug → the page it 301s to, for one country."""
    pages, retired = data["pages"], data["retired"]
    live = {p["slug"] for p in pages}
    retired_keys = set(retired) | {r["qualifiedSlug"] for r in retired.values()
                                   if isinstance(r, dict) and r.get("qualifiedSlug")}

    def town(key):
        return towns.get(key, (key.split(",")[0], 0))

    candidates = collections.defaultdict(list)    # alias slug → [(population, page slug)]
    for p in pages:
        names = [town(t) for t in p.get("towns", [])] + [(t, 0) for t in (extra_towns or {}).get(p["slug"], [])]
        for name, pop in names:
            for alias in {slug_of(name), town_pages.slugify(name)}:
                candidates[alias].append((pop, p["slug"]))
        if p.get("qualifiedSlug"):
            candidates[p["qualifiedSlug"]].append((town(p.get("anchor") or p["name"])[1], p["slug"]))
    aliases = {}
    for alias, options in candidates.items():
        if not alias or alias in live or alias in retired_keys:
            continue
        aliases[alias] = max(options, key=lambda o: (o[0], o[1]))[1]
    return dict(sorted(aliases.items()))


def shared_qualified(data):
    """code → {page slug → its qualified slug} for every page whose slug another
    country also has, as a page or as a retired page's."""
    owners = collections.defaultdict(set)
    for code, (pages_json, _, _) in data.items():
        for slug in [p["slug"] for p in pages_json["pages"]] + list(pages_json.get("retired", {})):
            owners[slug].add(code)
    out = {}
    for code, (pages_json, _, _) in data.items():
        shared = {}
        for p in pages_json["pages"]:
            if len(owners[p["slug"]]) > 1:
                if not p.get("qualifiedSlug") or p["qualifiedSlug"] == p["slug"]:
                    raise SystemExit(f"{code} page {p['slug']} shares its slug with another country but has no qualified slug")
                shared[p["slug"]] = p["qualifiedSlug"]
        out[code] = dict(sorted(shared.items()))
    return out


def main():
    data = country_data()
    aliases = {code: index(code, pages_json, towns, slug_of, polish_venue_towns() if code == "pl" else None)
               for code, (pages_json, towns, slug_of) in data.items()}

    for code in data:
        write_table(OUT / f"aliases-{code}.tsv", aliases[code].items())
    shared = shared_qualified(data)
    write_table(OUT / "shared-qualified-slugs.tsv",
                [(code, slug, qualified) for code in data for slug, qualified in shared[code].items()])
    print(f"Wrote {OUT.relative_to(ROOT)}/: aliases " + ", ".join(f"{c} {len(a)}" for c, a in aliases.items())
          + "; shared pages " + ", ".join(f"{c} {len(q)}" for c, q in shared.items()))


if __name__ == "__main__":
    main()
