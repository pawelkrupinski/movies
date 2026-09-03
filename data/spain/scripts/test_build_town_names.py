#!/usr/bin/env python3
"""Unit test for build_town_names — the accent restoration, and the property
that keeps it safe.

The correction is applied by name, from an external dataset, to strings that go
straight onto the page. The thing that makes that safe is that it can only ever
re-spell a town, never substitute one: a GeoNames name is a candidate only when
it folds to the same ASCII as the harvested name. These pin that.

Run: python3 data/spain/scripts/test_build_town_names.py
"""
import importlib.util
import os

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "build_town_names", os.path.join(HERE, "build_town_names.py"))
btn = importlib.util.module_from_spec(spec)
spec.loader.exec_module(btn)


def index(*names):
    """A GeoNames index over the given (population, name) pairs."""
    out = {}
    for population, name in names:
        out.setdefault(btn.fold(name), []).append((population, name))
    return out


def test_fold_ignores_accents_case_and_spacing():
    assert btn.fold("Alcalá de Henares") == btn.fold("Alcala De Henares")
    assert btn.fold("A Coruña") == "a coruna"
    assert btn.fold("  Leganés ") == "leganes"


def test_restores_the_accent():
    fixed = btn.corrections({"Alcala De Henares"}, index((195907, "Alcalá de Henares")))
    assert fixed == {"Alcala De Henares": "Alcalá de Henares"}


def test_never_substitutes_a_different_town():
    # Toledo is in the dump; Talavera is the harvested name. They do not fold
    # together, so nothing is offered — the town is left exactly as harvested.
    assert btn.corrections({"Talavera"}, index((83226, "Toledo"))) == {}


def test_a_town_geonames_does_not_know_is_left_alone():
    assert btn.corrections({"Aguilar De Campo"}, index((0, "Somewhere Else"))) == {}


def test_the_most_populous_of_a_fold_class_wins():
    # Two real municipalities spelled the same; the one with the cinema is the
    # one people mean, and population is the proxy geocode_provinces.py uses.
    fixed = btn.corrections({"Cabezon"}, index((8000, "Cabezón"), (300, "Cabezon")))
    assert fixed == {"Cabezon": "Cabezón"}


def test_a_town_already_written_correctly_needs_no_entry():
    assert btn.corrections({"A Coruña"}, index((246056, "A Coruña"))) == {}


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
