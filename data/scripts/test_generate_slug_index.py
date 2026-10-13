#!/usr/bin/env python3
"""Tests for generate_slug_index.py — the slugs a country answers to besides its
pages'.

    python3 data/scripts/test_generate_slug_index.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import generate_slug_index as g  # noqa: E402
import town_pages  # noqa: E402


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def synthetic():
    data = {
        "pages": [
            {"slug": "harrogate", "name": "Harrogate", "anchor": "Harrogate, NY", "qualifiedSlug": "harrogate-ny",
             "towns": ["Harrogate, NY", "Knaresborough, NY"]},
            {"slug": "leeds", "name": "Leeds", "anchor": "Leeds, WY", "qualifiedSlug": "leeds-wy",
             "towns": ["Leeds, WY"]},
            {"slug": "otley", "name": "Otley", "anchor": "Otley, WY", "qualifiedSlug": "otley-wy",
             "towns": ["Otley, WY", "Knaresborough, WY"]},
        ],
        "retired": {"west-yorkshire": {"page": "leeds", "qualifiedSlug": "west-yorkshire", "anchor": None},
                    "knaresborough-old": {"page": "otley", "qualifiedSlug": "knaresborough-old", "anchor": None}},
    }
    towns = {"Harrogate, NY": ("Harrogate", 75_000), "Knaresborough, NY": ("Knaresborough", 15_000),
             "Leeds, WY": ("Leeds", 536_000), "Otley, WY": ("Otley", 14_000), "Knaresborough, WY": ("Knaresborough", 9_000)}
    aliases = g.index("xx", data, towns, town_pages.slugify)
    check("a town on a cluster is an alias of its page", aliases["knaresborough"] == "harrogate")
    check("…of the page holding the more populous town of that name", aliases["knaresborough"] != "otley")
    check("a page's qualified slug is an alias of the page", aliases["leeds-wy"] == "leeds")
    check("a page slug, or a retired one, is never an alias",
          "leeds" not in aliases and "west-yorkshire" not in aliases and "harrogate" not in aliases)


def shared():
    def country(*pages):
        return ({"pages": [dict(slug=s, qualifiedSlug=q) for s, q in pages], "retired": {}}, {}, town_pages.slugify)
    out = g.shared_qualified({"uk": country(("york", "york-north-yorkshire"), ("leeds", "leeds-west-yorkshire")),
                              "us": country(("york", "york-pennsylvania"), ("boise", "boise-idaho"))})
    check("a page whose slug another country has too is named with its qualified slug, in both countries",
          out == {"uk": {"york": "york-north-yorkshire"}, "us": {"york": "york-pennsylvania"}})
    retired_uk = ({"pages": [], "retired": {"kent": {"page": "chatham"}}}, {}, town_pages.slugify)
    check("…and one whose slug another country retired",
          g.shared_qualified({"uk": retired_uk, "us": country(("kent", "kent-washington"))}) ==
          {"uk": {}, "us": {"kent": "kent-washington"}})
    try:
        g.shared_qualified({"uk": country(("york", "york")), "us": country(("york", "york-pennsylvania"))})
        check("a shared page with no qualified slug of its own fails the generator", False)
    except SystemExit:
        check("a shared page with no qualified slug of its own fails the generator", True)


def committed():
    data = g.country_data()
    check("the five countries are indexed", set(data) == {"pl", "uk", "de", "us", "es"})
    source = g.OUT.read_text(encoding="utf-8")
    check("CitySlugIndexData.scala carries the UK's town aliases (Ilkley)", '"ilkley" -> ' in source)


def main():
    synthetic()
    shared()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
