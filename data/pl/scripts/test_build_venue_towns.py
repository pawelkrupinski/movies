#!/usr/bin/env python3
"""Unit test for build_venue_towns — reading a town out of a venue annotation.

The annotations are prose written by hand over years, and what they say goes
onto the page. These pin the two things that matter: that a real town is read
out of the shapes they are actually written in, and that something which is not
a town is refused rather than served.

Run: python3 data/pl/scripts/test_build_venue_towns.py
"""
import importlib.util
import os

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location(
    "build_venue_towns", os.path.join(HERE, "build_venue_towns.py"))
bvt = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bvt)

# A gazetteer standing in for the GeoNames dump.
EXACT = {bvt.fold(t) for t in [
    "Biecz", "Bochnia", "Dąbrowa Tarnowska", "Ostrów Wielkopolski",
    "Ostrów Mazowiecka", "Połczyn-Zdrój", "Warszawa", "Łódź",
]}
PREFIXES = {}
for key in EXACT:
    head = key.split("-")[0].split(" ")[0]
    if head != key:
        PREFIXES.setdefault(head, set()).add(key)


def town(comment):
    return bvt.town_of(comment, EXACT, PREFIXES)


def test_reads_the_town_in_front_of_the_source():
    assert town("Biecz — filmweb 2315") == "Biecz"
    assert town("Bochnia — filmweb 1294") == "Bochnia"


def test_reads_a_town_whose_own_name_has_spaces():
    assert town("Dąbrowa Tarnowska — filmweb 1488") == "Dąbrowa Tarnowska"


def test_refuses_an_annotation_that_names_a_venue():
    # The one in the real roster. Serving it as a place is worse than nothing.
    assert town("Ursynowskie Centrum Kultury — own site") == ""


def test_expands_an_abbreviated_qualifier():
    # Expanded, not dropped: Ostrów Mazowiecka is a different town.
    assert town("Ostrów Wlkp. — filmweb 1234") == "Ostrów Wielkopolski"


def test_accepts_a_shortened_name_only_when_it_is_unambiguous():
    # Kept AS WRITTEN, not expanded to "Połczyn-Zdrój": the gazetteer only says
    # whether a place exists here, it does not supply the spelling — Polish
    # towns are filed in it under English exonyms.
    assert town("Połczyn — filmweb 999") == "Połczyn"
    # "Ostrów" alone starts two towns, so it resolves to neither.
    assert town("Ostrów — filmweb 999") == ""


def test_fold_handles_the_polish_l():
    # 'ł' has no combining form, so NFKD alone leaves it and the fold misses.
    assert bvt.fold("Łódź") == bvt.fold("Lodz")
    assert bvt.fold("Wałbrzych") == "walbrzych"


def test_an_annotation_with_no_source_half_is_still_a_town():
    assert town("Warszawa") == "Warszawa"


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
