#!/usr/bin/env python3
"""Unit test for harvest_taquilla's decisions -- which Taquilla page is a roster venue's,
and whether Taquilla should serve it -- and generate_own_sources' refusals. Each a
mapping that would compile and scrape the WRONG cinema, or swap a venue onto a thinner
feed.

Run: python3 data/spain/scripts/test_harvest_taquilla.py
"""
import importlib.util
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def _load(name):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, f"{name}.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ht = _load("harvest_taquilla")
gos = _load("generate_own_sources")

# The parts of https://www.taquilla.com/madrid/cine-yelmo-la-vaguada-madrid the harvest
# reads, verbatim from the 2026-10-12 page.
VAGUADA = (
    '<title>Cine Yelmo La Vaguada, Madrid - Cartelera, Horarios y Entradas.</title>'
    '<input type="hidden" name="t10venue" value="200595">'
    '<p class="ent-cine__direccion">C. de Santiago de Compostela, Fuencarral-El Pardo, Madrid, '
    'España, 28029, (Madrid)</p>'
)


def test_a_venue_page_yields_its_id_name_and_postcode():
    assert ht.parse_venue_page(VAGUADA) == (
        "200595", "Cine Yelmo La Vaguada", "28029",
        "C. de Santiago de Compostela, Fuencarral-El Pardo, Madrid, España, 28029, (Madrid)")
    assert ht.parse_venue_page("<title>Cartelera Madrid</title>") is None


def test_names_of_different_chains_never_agree():
    # Granada's Megarama and Kinépolis share a postcode area and the town word.
    assert ht.name_score("Megarama Granada", "Kinépolis Granada")[0] == 0.0
    assert ht.name_score("Yelmo Cines La Vaguada", "Cine Yelmo La Vaguada")[0] == 1.0


def _venue(path, name, postcode, vid="1"):
    return path, {"venueId": vid, "name": name, "postcode": postcode, "address": ""}


def test_the_closer_name_wins_a_shared_postcode():
    taquilla = dict([_venue("/barcelona/cinesa-diagonal-mar-3d-barcelona", "Cinesa Diagonal Mar 3D", "08019"),
                     _venue("/barcelona/cinesa-diagonal-barcelona", "Cinesa Diagonal", "08019")])
    roster = [{"theaterId": "E0381", "name": "Cinesa Diagonal", "town": "Barcelona", "postcode": "08019"}]
    chosen, review = ht.match(roster, taquilla)
    assert chosen == {"E0381": "/barcelona/cinesa-diagonal-barcelona"} and review == []


def test_two_roster_venues_on_one_page_are_left_for_review():
    taquilla = dict([_venue("/valencia/yelmo-campanar-valencia", "Yelmo Campanar", "46015")])
    roster = [{"theaterId": "E0248", "name": "Yelmo Cines Campanar", "town": "Valencia", "postcode": "46015"},
              {"theaterId": "E0773", "name": "Yelmo Campanar", "town": "Valencia", "postcode": "46015"}]
    chosen, review = ht.match(roster, taquilla)
    assert chosen == {} and len(review) == 2


def test_without_a_shared_postcode_the_names_must_agree_in_the_same_town():
    taquilla = dict([_venue("/gijon/yelmo-cines-los-prados-gijon", "Cine Yelmo Los Prados", "33203"),
                     _venue("/gijon/cines-centro-gijon", "Cines Centro", "33201")])
    roster = [{"theaterId": "E1", "name": "Yelmo Cines Los Prados", "town": "Gijón", "postcode": "33204"},
              {"theaterId": "E2", "name": "Cines Centro Plaza", "town": "Gijón", "postcode": "33209"}]
    chosen, _ = ht.match(roster, taquilla)
    assert chosen == {"E1": "/gijon/yelmo-cines-los-prados-gijon"}


H = "2026-10-26"


def test_taquilla_serves_a_venue_sensacine_lists_nothing_for():
    assert ht.decide({}, ["2026-10-12"], {}, H)[0]
    assert not ht.decide({}, [], {}, H)[0]


def test_taquilla_must_reach_further_than_sensacine():
    # Cinesa: Taquilla's calendar ends on the 15th, SensaCine runs to November.
    served = {"2026-10-12": 40, "2026-11-08": 2}
    serve, why = ht.decide(served, ["2026-10-12", "2026-10-15"], {"2026-10-12": 40}, H)
    assert not serve and "ends 2026-10-15" in why


def test_taquilla_must_carry_the_near_days_sensacine_does():
    served = {"2026-10-12": 38, "2026-10-13": 38, "2026-11-08": 2}
    days = ["2026-10-12", "2026-10-13", "2026-12-17"]
    assert ht.decide(served, days, {"2026-10-12": 41, "2026-10-13": 41, "2026-12-17": 9}, H)[0]
    # Thinner on the near days: a longer tail does not buy the near fortnight.
    assert not ht.decide(served, days, {"2026-10-12": 20, "2026-10-13": 20, "2026-12-17": 9}, H)[0]


def test_generate_refuses_a_venue_the_roster_lacks_or_ocine_serves():
    problems = gos.problems({"E1", "E2", "E3"}, {"E3"}, {"E1": {}, "E9": {}, "E3": {}})
    assert any("E9 is not a roster venue" in p for p in problems)
    assert any("E3 is an Ocine venue" in p for p in problems)
    assert len(problems) == 2
    assert gos.problems({"E1"}, set(), {"E1": {}}) == []


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
    sys.exit(0)
