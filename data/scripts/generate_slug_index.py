#!/usr/bin/env python3
"""Generate common/src/main/scala/models/CitySlugIndexData.scala — every slug a
country answers to besides its pages', and who owns a bare slug on the front door.

Every country owns its own city slugs (`/uk/york/` is York, `/us/york/` York,
Pennsylvania), and a known place never 404s. Two things make that so, and both
are read off the countries' committed page data here:

  ALIASES — per country, slug → the page it 301s to:
    - every town a page covers that is not a page itself (`/uk/ilkley/` is on the
      Ilkley cluster's page … unless Ilkley is a page; `/de/bonn/` → Bonn's own);
      for Poland, the towns its venues sit in (`data/pl/venues.json`) as well;
    - every page's qualified slug (`/uk/york-north-yorkshire/` → `/uk/york/`).
    A slug that is a page, or a retired slug (which `City` already 301s by its
    own record), is never an alias. Where two pages of one country cover a town
    of one slug, the MORE POPULOUS town's page has it.

  FRONT DOOR — `showtimes.cc/{slug}/`, the bare brand apex, which Poland's
    deployment answers: a slug that one mounted country (UK, Germany, US,
    Spain) answers to, as a page, a retired page or an alias, 301s to that
    country's page; one SEVERAL answer to goes to the country whose town of that
    name is the MOST POPULOUS (a page's anchor town, an alias's town, a retired
    page's namesake; 0 where there is none). This file lists only those
    contested slugs and their winner.

    python3 data/scripts/generate_slug_index.py
"""
import collections
import importlib.util
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "CitySlugIndexData.scala"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import town_pages  # noqa: E402

#: The countries mounted under the brand apex, in `Country.all`'s order (the tie-break).
MOUNTED = ["uk", "de", "us", "es"]
CHUNK = 400


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
    """(aliases: slug → page, answers: slug → population of the town it names)
    for one country."""
    pages, retired = data["pages"], data["retired"]
    live = {p["slug"] for p in pages}
    retired_keys = set(retired) | {r["qualifiedSlug"] for r in retired.values()
                                   if isinstance(r, dict) and r.get("qualifiedSlug")}

    def town(key):
        return towns.get(key, (key.split(",")[0], 0))

    answers = {}
    for p in pages:
        answers[p["slug"]] = town(p.get("anchor") or p["name"])[1]
    for slug, r in retired.items():
        anchor = r.get("anchor") if isinstance(r, dict) else None
        pop = town(anchor)[1] if anchor else 0
        for key in {slug, (r.get("qualifiedSlug") if isinstance(r, dict) else None)} - {None}:
            answers.setdefault(key, pop)

    candidates = collections.defaultdict(list)    # alias slug → [(population, page slug)]
    for p in pages:
        names = [town(t) for t in p.get("towns", [])] + [(t, 0) for t in (extra_towns or {}).get(p["slug"], [])]
        for name, pop in names:
            for alias in {slug_of(name), town_pages.slugify(name)}:
                candidates[alias].append((pop, p["slug"]))
        if p.get("qualifiedSlug"):
            candidates[p["qualifiedSlug"]].append((answers[p["slug"]], p["slug"]))
    aliases = {}
    for alias, options in candidates.items():
        if not alias or alias in live or alias in retired_keys:
            continue
        pop, page = max(options, key=lambda o: (o[0], o[1]))
        aliases[alias] = page
        answers.setdefault(alias, pop)
    return dict(sorted(aliases.items())), answers


def scala_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def main():
    data = country_data()
    aliases, answers = {}, {}
    for code, (pages_json, towns, slug_of) in data.items():
        extra = polish_venue_towns() if code == "pl" else None
        aliases[code], answers[code] = index(code, pages_json, towns, slug_of, extra)

    contested = {}
    every = sorted({s for c in MOUNTED for s in answers[c]})
    for slug in every:
        owners = [c for c in MOUNTED if slug in answers[c]]
        if len(owners) > 1:
            contested[slug] = max(owners, key=lambda c: (answers[c][slug], -MOUNTED.index(c)))

    out = [
        "// GENERATED by data/scripts/generate_slug_index.py from every country's pages.json",
        "// — do NOT edit by hand.",
        "package models",
        "",
        "/** Every slug a country answers to besides its pages', and who owns a contested",
        " *  bare slug on the brand front door — see `data/scripts/generate_slug_index.py`. */",
        "private[models] object CitySlugIndexData {",
        "",
    ]
    for code in data:
        rows = [f"    {scala_string(a)} -> {scala_string(p)}," for a, p in aliases[code].items()]
        chunks = [rows[i:i + CHUNK] for i in range(0, len(rows), CHUNK)] or [[]]
        for i, chunk in enumerate(chunks):
            out.append(f"  private def {code}{i}: Seq[(String, String)] = Seq(")
            out.extend(chunk)
            out.append("  )")
        out.append(f"  /** {code}: alias slug → the page it 301s to. */")
        out.append(f"  def {code}Aliases: Map[String, String] = ("
                   + " ++ ".join(f"{code}{i}" for i in range(len(chunks))) + ").toMap")
        out.append("")
    out.append("  /** A bare slug several mounted countries answer to → the code of the one whose")
    out.append("   *  town of that name is the most populous. */")
    out.append("  val frontDoorContested: Map[String, String] = Map(")
    out.extend(f"    {scala_string(s)} -> {scala_string(c)}," for s, c in contested.items())
    out.append("  )")
    out.append("}")
    OUT.write_text("\n".join(out) + "\n", encoding="utf-8")
    print(f"Wrote {OUT.relative_to(ROOT)}: aliases " +
          ", ".join(f"{c} {len(a)}" for c, a in aliases.items()) + f"; {len(contested)} contested front-door slugs")


if __name__ == "__main__":
    main()
