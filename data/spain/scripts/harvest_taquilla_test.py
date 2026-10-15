#!/usr/bin/env python3
"""Unit test for harvest_taquilla's decisions -- which Taquilla page is a roster venue's,
and whether Taquilla should serve it -- and generate_own_sources' refusals. Each a
mapping that would compile and scrape the WRONG cinema, or swap a venue onto a thinner
feed.

Run: python3 data/spain/scripts/harvest_taquilla_test.py
"""
import sys

import generate_own_sources as gos  # beside this test, whose directory leads sys.path
import harvest_taquilla as ht

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


CAMPANAR = dict([_venue("/valencia/yelmo-campanar-valencia", "Yelmo Campanar", "46015")])
CAMPANAR_ROSTER = [{"theaterId": "E0248", "name": "Yelmo Cines Campanar", "town": "Valencia", "postcode": "46015"},
                   {"theaterId": "E0773", "name": "Yelmo Cines Mercado de Campanar", "town": "Valencia",
                    "postcode": "46015"}]


def test_two_roster_venues_on_one_page_are_left_for_review():
    chosen, review = ht.match(CAMPANAR_ROSTER, CAMPANAR, decided={})
    assert chosen == {} and len(review) == 2


def test_a_hand_checked_verdict_settles_a_pair_the_names_cannot():
    # The page's address, Av. de Tirso de Molina 16, is the Mercado de Campanar centre
    # both roster rows give; E0773 is the row SensaCine serves, E0248 its idle twin.
    chosen, review = ht.match(CAMPANAR_ROSTER, CAMPANAR)
    assert chosen == {"E0773": "/valencia/yelmo-campanar-valencia"} and review == []


def test_every_review_pair_of_the_2026_10_12_harvest_has_a_verdict():
    for tid in ("E0304", "E0420", "E0193", "E0690", "E0390", "E0427", "E0248", "E0773",
                "E0346", "E0763", "E0754", "E0036"):
        assert tid in ht.DECIDED, tid
    # A verdict names a page once: two roster rows on one page is the doubt it settles.
    pages = [p for p in ht.DECIDED.values() if p]
    assert len(pages) == len(set(pages))


def test_a_verdict_of_none_keeps_the_venue_off_every_page():
    # Cines Gran Marbella (Puerto Banús) shares only the postcode with La Cañada's page.
    taquilla = dict([_venue("/marbella/kinepolis-marbella-la-canada", "Kinépolis Marbella La Cañada", "29600")])
    roster = [{"theaterId": "E0390", "name": "Kinépolis La Cañada", "town": "Marbella", "postcode": "29600"},
              {"theaterId": "E0427", "name": "Cines Gran Marbella", "town": "Marbella", "postcode": "29600"}]
    chosen, review = ht.match(roster, taquilla)
    assert chosen == {"E0390": "/marbella/kinepolis-marbella-la-canada"} and review == []


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


def test_a_venue_already_on_taquilla_is_not_judged_by_its_own_feed():
    # Once moved, showtimes.cc serves Taquilla's calendar, so the superset rule would
    # compare Taquilla with itself, find it reaches no further, and move the venue back.
    moved = {"sensacineLastDay": "2026-10-15", "why": "Taquilla reaches 2026-12-15 vs SensaCine 2026-10-15"}
    days = ["2026-10-12", "2026-12-15"]
    assert not ht.decide({"2026-10-12": 35, "2026-12-15": 4}, days, {"2026-10-12": 35}, H)[0]
    assert ht.keep_moved(moved, days, "2026-10-12", "2026-10-12")[0]
    # ...until Taquilla's own data gets worse: nothing on sale, or reaching fewer days
    # ahead than SensaCine did when the venue moved (3 days, measured 2026-10-12).
    assert not ht.keep_moved(moved, [], "2026-10-12", "2026-10-12")[0]
    serve, why = ht.keep_moved(moved, ["2026-10-12", "2026-10-14"], "2026-10-12", "2026-10-12")
    assert not serve and "3 days" in why


def test_a_moved_venue_is_judged_by_how_far_ahead_taquilla_runs_not_by_an_old_date():
    # SensaCine reached 3 days ahead on the move (2026-10-12 -> 10-15). Weeks later that
    # date has long passed: a calendar selling one day ahead is still thinner.
    moved = {"sensacineLastDay": "2026-10-15", "why": "moved"}
    serve, why = ht.keep_moved(moved, ["2026-11-01", "2026-11-02"], "2026-11-01", "2026-10-12")
    assert not serve and "1 days" in why
    assert ht.keep_moved(moved, ["2026-11-01", "2026-11-04"], "2026-11-01", "2026-10-12")[0]
    # A row's own movedOn wins over the file's measured date (reach then: 1 day).
    later = {**moved, "movedOn": "2026-10-14"}
    assert ht.keep_moved(later, ["2026-11-01", "2026-11-02"], "2026-11-01", "2026-10-12")[0]
    assert not ht.keep_moved(later, ["2026-11-01"], "2026-11-01", "2026-10-12")[0]


def test_a_failed_calendar_fetch_is_unknown_not_empty():
    assert ht.calendar_days(503, "") is None
    assert ht.calendar_days(200, '{"objects": {"2026-10-13": 4, "2026-10-12": 2, "2026-10-14": 0}}') == [
        "2026-10-12", "2026-10-13"]
    assert ht.calendar_days(200, '{"objects": []}') == []


def test_every_moved_venue_is_kept_judged_or_dropped_out_loud():
    row = {"name": "Yelmo Ideal", "sensacineLastDay": "2026-10-15", "why": "moved"}
    moved = {"E1": row, "E2": {**row, "name": "Gone"}, "E3": {**row, "name": "Flaky"}, "E4": {**row, "name": "Empty"}}
    days = {"E1": ["2026-10-12", "2026-12-01"], "E3": None, "E4": []}
    verdicts = ht.judge_moved(moved, days, "2026-10-12", "2026-10-12")
    assert verdicts["E1"] == ("serve", "moved")
    # The crawl or match lost its page (404, renamed, left for review): dropped, but said so.
    assert verdicts["E2"][0] == "drop" and "page" in verdicts["E2"][1]
    # A calendar fetch that failed says nothing: the previous row stands.
    assert verdicts["E3"][0] == "unknown"
    # A calendar that answered and sells nothing is a real demotion.
    assert verdicts["E4"] == ("drop", "Taquilla sells no day")
    assert ht.dropped_lines(moved, verdicts) == ["DROPPED Empty: Taquilla sells no day",
                                                 f"DROPPED Gone: {verdicts['E2'][1]}"]


def test_generate_refuses_a_venue_the_roster_lacks_ocine_serves_or_two_sources_claim():
    problems = gos.problems({"E1", "E2", "E3"}, {"E3"}, {"E1": {}, "E9": {}}, {"E1": {}, "E3": {}})
    assert any("E9 is not a roster venue" in p for p in problems)
    assert any("artesiete.json: E3 is an Ocine venue" in p for p in problems)
    assert any("E1 is in both" in p for p in problems)
    assert len(problems) == 3
    assert gos.problems({"E1", "E2"}, set(), {"E1": {}}, {"E2": {}}) == []


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
    sys.exit(0)
