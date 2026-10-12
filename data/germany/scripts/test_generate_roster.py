#!/usr/bin/env python3
"""Unit tests for Germany's generate_roster: what it refuses, and the tuple shape
`GermanRoster` reads.

Run: python3 data/germany/scripts/test_generate_roster.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import generate_roster as gr  # noqa: E402

ROSTER = [
    {"theaterId": "A1", "name": "Lichtburg", "city": "Frankfurt", "displayName": "Lichtburg"},
    {"theaterId": "A2", "name": "Capitol", "city": "Eschborn", "displayName": "Capitol"},
]
PAGES = [
    {"slug": "frankfurt-am-main", "qualifiedSlug": "frankfurt-am-main-hessen", "name": "Frankfurt am Main",
     "bundesland": "Hessen", "kind": "major", "lat": 50.1, "lon": 8.7, "towns": ["Frankfurt am Main"],
     "cinemas": ["Lichtburg"]},
    {"slug": "eschborn", "qualifiedSlug": "eschborn-hessen", "name": "Eschborn", "bundesland": "Hessen",
     "kind": "cluster", "lat": 50.1, "lon": 8.5, "towns": ["Eschborn", "Kelkheim"], "cinemas": ["Capitol"]},
]


def test_refuses_a_duplicate_display_name():
    assert gr.roster_problems(ROSTER) == []
    assert gr.roster_problems(ROSTER + [dict(ROSTER[0], theaterId="A3")]) == ["duplicate displayName 'Lichtburg'"]


def test_renders_each_venue_with_its_fallback_and_town_and_each_page_with_its_land():
    out = gr.render(ROSTER, PAGES, {"koeln-alt": {"page": "eschborn", "qualifiedSlug": "koeln-alt-hessen"}},
                    {"A1": "/kino/frankfurt/lichtburg-1"}, lambda city: {"Frankfurt": "Frankfurt am Main"}.get(city, city))
    assert '("Lichtburg", "Lichtburg", "A1", Some("/kino/frankfurt/lichtburg-1"), "Frankfurt am Main")' in out
    assert '("Capitol", "Capitol", "A2", None, "Eschborn")' in out
    assert ('("eschborn", "eschborn-hessen", "Eschborn", "Hessen", 50.1, 8.5, true, '
            'Seq("Eschborn", "Kelkheim")') in out
    assert '"frankfurt-am-main-hessen", "Frankfurt am Main", "Hessen", 50.1, 8.7, false' in out
    assert '("koeln-alt", "koeln-alt-hessen", "eschborn"),' in out


def test_renders_the_seasonal_venues_by_name():
    roster = ROSTER + [{"theaterId": "A9", "name": "Freiluftkino", "city": "Frankfurt", "displayName": "Freiluftkino Main"}]
    pages = [dict(PAGES[0], cinemas=["Lichtburg", "Freiluftkino Main"]), PAGES[1]]
    out = gr.render(roster, pages, {}, {}, lambda city: city)
    assert '  val seasonal: Set[String] = Set(\n    "Freiluftkino Main",\n  )' in out


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for t in tests:
        t()
        print("PASS", t.__name__)
    print(f"\n{len(tests)} tests passed")


if __name__ == "__main__":
    main()
