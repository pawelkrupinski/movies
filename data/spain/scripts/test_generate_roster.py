#!/usr/bin/env python3
"""Unit test for generate_roster's refusals: a venue on no page or two, and an
Ocine table that no longer lines up with the harvest — each a roster that
compiles and silently loses a venue or its scrape.

Run: python3 data/spain/scripts/test_generate_roster.py
"""
import importlib.util
import os

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "generate_roster", os.path.join(HERE, "generate_roster.py"))
gr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gr)


def test_every_venue_on_exactly_one_page():
    venues = {"A": {}, "B": {}, "C": {}}
    assert gr.placement_problems(venues, [{"cinemas": ["A", "B"]}, {"cinemas": ["C"]}]) == []
    problems = gr.placement_problems(venues, [{"cinemas": ["A", "B"]}, {"cinemas": ["B", "Z"]}])
    assert any("'B' is on 2 pages" in p for p in problems)
    assert any("'C' is on no page" in p for p in problems)
    assert any("'Z' is not in the roster" in p for p in problems)
    assert len(problems) == 3


def _harvest():
    return [
        {"name": "Girona", "cinemas": [
            {"theaterId": "E0362", "name": "Ocine Girona", "town": "Girona", "displayName": "Ocine Girona"},
            {"theaterId": "E0999", "name": "Truffaut", "town": "Girona", "displayName": "Truffaut"},
        ]},
        {"name": "Asturias", "cinemas": [
            {"theaterId": "E0784", "name": "Autocine Gijón", "town": "Gijon", "displayName": "Autocine Gijón"},
            {"theaterId": "E0100", "name": "Yelmo Los Prados", "town": "Oviedo", "displayName": "Yelmo Los Prados"},
        ]},
    ]


def _unlisted(name, server, province="Asturias", town="Gijón"):
    return {"name": name, "ticketingServer": server, "province": province, "town": town}


def test_merge_ocine_tags_listed_venues_and_adds_unlisted_ones_in_name_order():
    provinces = _harvest()
    problems = gr.merge_ocine(provinces, {"listed": {"E0362": "tickets.ocinegirona.es"},
                                          "unlisted": [_unlisted("Ocine Los Fresnos", "tickets.ocinepremiumlosfresnos.es")]})
    assert problems == []
    girona, asturias = provinces
    assert girona["cinemas"][0]["ocineServer"] == "tickets.ocinegirona.es"
    assert "ocineServer" not in girona["cinemas"][1]
    assert [c["displayName"] for c in asturias["cinemas"]] == \
        ["Autocine Gijón", "Ocine Los Fresnos", "Yelmo Los Prados"]
    added = asturias["cinemas"][1]
    assert added["theaterId"] is None and added["ocineServer"] == "tickets.ocinepremiumlosfresnos.es"


def test_merge_ocine_refuses_a_table_the_harvest_does_not_match():
    problems = gr.merge_ocine(_harvest(), {
        "listed": {"E0362": "tickets.ocinegirona.es", "E4040": "tickets.ocinegone.es"},
        "unlisted": [_unlisted("Ocine Nowhere", "tickets.ocinenowhere.es", province="Atlantis"),
                     _unlisted("Ocine Girona Bis", "tickets.ocinegirona.es")]})
    assert any("E4040" in p for p in problems)
    assert any("Atlantis" in p for p in problems)
    assert any("'tickets.ocinegirona.es'" in p and "2 venues" in p for p in problems)
    assert len(problems) == 3


def test_an_unlisted_venue_emits_no_theater_id():
    assert gr.scala_option(None) == "None"
    assert gr.scala_option("E0362") == 'Some("E0362")'


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
