#!/usr/bin/env python3
"""Unit test for generate_roster's town naming and its refusal to run without
the accent table.

The refusal is the point. A missing table does not break the generator — it
emits a roster that is valid in every way except that 100 town names lose the
accents Spanish writes them with, on 48 province pages, with nothing to say so.
Silent-but-worse is the failure this file exists to make loud.

Run: python3 data/spain/scripts/test_generate_roster.py
"""
import importlib.util
import json
import os
import pathlib
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "generate_roster", os.path.join(HERE, "generate_roster.py"))
gr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gr)


def test_refuses_to_run_without_the_accent_table():
    missing = pathlib.Path(tempfile.gettempdir()) / "no-such-town-names.json"
    if missing.exists():
        missing.unlink()
    try:
        gr.load_corrections(missing)
    except SystemExit as e:
        assert e.code and e.code != 0
    else:
        raise AssertionError("expected a non-zero SystemExit when the accent table is missing")


def test_loads_the_table_when_it_is_there():
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump({"Alcala De Henares": "Alcalá de Henares"}, f)
        path = pathlib.Path(f.name)
    assert gr.load_corrections(path) == {"Alcala De Henares": "Alcalá de Henares"}
    path.unlink()


def test_the_table_wins_over_the_casing_rule():
    corrections = {"Alcala De Henares": "Alcalá de Henares"}
    assert gr.town_name("Alcala De Henares", corrections) == "Alcalá de Henares"


def test_the_casing_rule_covers_what_the_table_does_not():
    # The 60 towns GeoNames does not know under the harvested name still get
    # their particles lowercased.
    assert gr.town_name("Aguilar De Campo", {}) == "Aguilar de Campo"
    assert gr.town_name("Alfas Del Pi", {}) == "Alfas del Pi"


def test_the_first_word_keeps_its_capital():
    assert gr.town_name("La Coruna", {}) == "La Coruna"


def test_towns_of_ranks_by_venue_count_then_alphabetically():
    province = {"cinemas": [
        {"town": "Alcorcon"}, {"town": "Madrid"}, {"town": "Madrid"}, {"town": "Getafe"},
    ]}
    assert gr.towns_of(province, {"Alcorcon": "Alcorcón"}) == ["Madrid", "Alcorcón", "Getafe"]


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


def _unlisted(name, slug, province="Asturias", town="Gijón"):
    return {"name": name, "ticketingSlug": slug, "province": province, "town": town}


def test_merge_ocine_tags_listed_venues_and_adds_unlisted_ones_in_name_order():
    provinces = _harvest()
    problems = gr.merge_ocine(provinces, {"listed": {"E0362": "girona"},
                                          "unlisted": [_unlisted("Ocine Los Fresnos", "losfresnos")]})
    assert problems == []
    girona, asturias = provinces
    assert girona["cinemas"][0]["ocineSlug"] == "girona"
    assert "ocineSlug" not in girona["cinemas"][1]
    assert [c["displayName"] for c in asturias["cinemas"]] == \
        ["Autocine Gijón", "Ocine Los Fresnos", "Yelmo Los Prados"]
    added = asturias["cinemas"][1]
    assert added["theaterId"] is None and added["ocineSlug"] == "losfresnos"


def test_merge_ocine_refuses_a_table_the_harvest_does_not_match():
    problems = gr.merge_ocine(_harvest(), {
        "listed": {"E0362": "girona", "E4040": "gone"},
        "unlisted": [_unlisted("Ocine Nowhere", "nowhere", province="Atlantis"),
                     _unlisted("Ocine Girona Bis", "girona")]})
    assert any("E4040" in p for p in problems)
    assert any("Atlantis" in p for p in problems)
    assert any("'girona'" in p and "2 venues" in p for p in problems)
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
