#!/usr/bin/env python3
"""
Unit test for build_provinces.build_display_names -- the displayName
uniqueness/qualification logic -- plus slugify.

Spain's actual harvest (2026-09-01, 595 venues) has ZERO duplicate raw
venue names, so a real run never exercises the qualification branches. This
test drives them directly with synthetic data engineered to hit all three
cases the spec requires:
  1. a unique name passes through unchanged;
  2. a name repeated with different towns gets qualified with the town;
  3. a name+town repeated (different province) gets qualified with the
     province too;
and the refusal path: a name+town+province collision that survives both
qualification passes must make the script exit non-zero rather than
silently emit a duplicate displayName.

Run: python3 data/spain/scripts/test_build_provinces.py
"""
import importlib.util
import os

_here = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location("build_provinces", os.path.join(_here, "build_provinces.py"))
bp = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(bp)


def theater(theater_id, name, town, province):
    return {"theaterId": theater_id, "name": name, "town": town, "provinceName": province}


def test_unique_name_passes_through():
    theaters = [theater("A1", "Cines Only", "Madrid", "Madrid")]
    display, qt, qp, _, _ = bp.build_display_names(theaters)
    assert display["A1"] == "Cines Only"
    assert (qt, qp) == (0, 0)


def test_same_name_different_town_qualified_with_town():
    theaters = [
        theater("A1", "Cinesa Diagonal", "Barcelona", "Barcelona"),
        theater("A2", "Cinesa Diagonal", "Sabadell", "Barcelona"),
    ]
    display, qt, qp, _, _ = bp.build_display_names(theaters)
    assert display["A1"] == "Cinesa Diagonal (Barcelona)"
    assert display["A2"] == "Cinesa Diagonal (Sabadell)"
    assert (qt, qp) == (2, 0)


def test_same_name_same_town_qualified_with_province():
    theaters = [
        theater("A1", "Cines Van Dyck", "Frontera", "Las Palmas"),
        theater("A2", "Cines Van Dyck", "Frontera", "Santa Cruz de Tenerife"),
    ]
    display, qt, qp, _, _ = bp.build_display_names(theaters)
    assert display["A1"] == "Cines Van Dyck (Frontera, Las Palmas)"
    assert display["A2"] == "Cines Van Dyck (Frontera, Santa Cruz de Tenerife)"
    assert (qt, qp) == (0, 2)


def test_unresolvable_collision_refuses_loudly():
    theaters = [
        theater("A1", "Cines Van Dyck", "Frontera", "Las Palmas"),
        theater("A2", "Cines Van Dyck", "Frontera", "Las Palmas"),
    ]
    try:
        bp.build_display_names(theaters)
    except SystemExit as e:
        assert e.code and e.code != 0
    else:
        raise AssertionError("expected a SystemExit (non-zero) on an unresolvable displayName collision")


def test_slugify_matches_spec_examples():
    assert bp.slugify("Álava") == "alava"
    assert bp.slugify("A Coruña") == "a-coruna"
    assert bp.slugify("Santa Cruz de Tenerife") == "santa-cruz-de-tenerife"


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
