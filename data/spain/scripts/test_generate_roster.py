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


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
