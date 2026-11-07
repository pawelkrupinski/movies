#!/usr/bin/env python3
"""The decisions feed_mirrors.py and generate_sources.py make about a feed serving another venue's programme.

Run: python3 data/germany/scripts/feed_mirrors_test.py
"""
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import feed_mirrors as fm  # noqa: E402
import generate_sources as gs  # noqa: E402


def feed(name, town, source, handle, urls):
    return {"name": name, "town": town, "source": source, "handle": handle, "urls": {fm.normalise(u) for u in urls}}


# The DE prod mirror of 2026-11-06: kinoheld's Grünstadt entry books Luxor Heidelberg's ticket-cloud tenant.
LUXOR = [f"https://ticket-cloud.de/Luxor-Heidelberg/Show/{n}" for n in range(2450600, 2450620)]


def test_the_venue_the_booking_urls_name_owns_the_programme_and_the_other_feed_is_its_copy():
    feeds = {"A2700": feed("Filmwelt Grünstadt", "Grünstadt", "kinoheld", "695", [u + "/" for u in LUXOR]),
             "A2874": feed("Luxor Filmpalast Heidelberg", "Heidelberg", "cineamo", "1222", LUXOR)}
    copies, kept, systems = fm.mirrors(feeds, decided={})
    assert [(c["venue"], c["source"], c["handle"], c["of"], c["shared"]) for c in copies] == \
        [("A2700", "kinoheld", "695", "A2874", 20)]
    assert kept == systems == []


def test_a_chain_host_names_its_branch_and_not_a_cinema_of_another_name():
    urls = [f"https://www.cinemaxx.de/buchtickets/zusammenfassung/1304/HO0000{n}/4{n}" for n in range(1000, 1015)]
    feeds = {"A0296": feed("CinemaxX Raschplatz", "Hannover", "cineamo", "933", urls),
             "A0590": feed("Kino am Raschplatz", "Hannover", "kinoheld", "246", urls)}
    assert [c["venue"] for c in fm.mirrors(feeds, decided={})[0]] == ["A0590"]


def test_urls_naming_both_alike_need_a_hand_verdict():
    urls = [f"https://kinotickets.express/dorfen/booking/{n}" for n in range(17500, 17520)]
    feeds = {"A2671": feed("sKino im Jakobmayer", "Dorfen", "cineamo", "1091", urls),
             "kinoheld:2323": feed("Kino in Dorfen", "Dorfen", "kinoheld", "2323", urls)}
    try:
        fm.mirrors(feeds, decided={})
        raise AssertionError("an undecided pair must stop the run")
    except ValueError as e:
        assert "Kino in Dorfen" in str(e)
    assert [c["venue"] for c in fm.mirrors(feeds, decided={frozenset(feeds): "kinoheld:2323"})[0]] == ["kinoheld:2323"]
    copies, kept, _ = fm.mirrors(feeds, decided={frozenset(feeds): None})
    assert copies == [] and kept[0]["shared"] == 20


def test_houses_sharing_a_booking_system_for_part_of_their_programmes_are_no_copy():
    shared = [f"https://www.kinopolis.de/kp/programm/vorstellung/{n}" for n in range(20)]
    feeds = {"A1": feed("Kinopolis Darmstadt", "Darmstadt", "cineamo", "112", shared + [f"https://www.kinopolis.de/kp/x/{n}" for n in range(8)]),
             "A2": feed("Citydome Darmstadt", "Darmstadt", "cineamo", "425", shared + [f"https://www.kinopolis.de/cd/x/{n}" for n in range(8)])}
    copies, _, systems = fm.mirrors(feeds, decided={})
    assert copies == [] and systems == [{"venues": ["A1", "A2"], "shared": 20, "totals": [28, 28]}]


def test_a_copy_once_found_stays_when_a_later_run_no_longer_sees_it_served():
    old = {"venue": "A2700", "source": "kinoheld", "handle": "695", "shared": 393}
    new = {"venue": "A0590", "source": "kinoheld", "handle": "246", "shared": 295}
    assert fm.merged([old], [new]) == [new, old]
    assert fm.merged([old], [{**old, "shared": 400}]) == [{**old, "shared": 400}]


def test_a_booking_url_is_one_screening_whatever_its_case_or_trailing_slash():
    assert fm.normalise("https://ticket-cloud.de/Luxor-Heidelberg/Show/2450683/") == \
        fm.normalise("http://ticket-cloud.de/Luxor-Heidelberg/Show/2450683") == "ticket-cloud.de/luxor-heidelberg/show/2450683"
    assert fm.source_of("kinoheld.de/cinema/695") == ("kinoheld", "695")
    assert fm.source_of("api.cineamo.com/cinema/448") == ("cineamo", "448")


def venue(key, primary, fallbacks, **handles):
    return {"key": key, "name": key, "rostered": True, "primary": primary, "fallbacks": fallbacks, **handles}


def test_the_generated_chain_leaves_a_copy_out_and_serves_the_venue_from_its_next_feed():
    doc = {"venues": [venue("A2700", "kinoheld", ["cineamo", "filmstarts"], filmstarts="A2700", cineamo="448",
                            kinoheld={"id": "695", "path": "/kino/gruenstadt/filmwelt-gruenstadt"})]}
    copy = {"venue": "A2700", "source": "kinoheld", "handle": "695"}
    assert gs.entries(doc) == [("A2700", ['k("695", "/kino/gruenstadt/filmwelt-gruenstadt")', 'c("448")', 'f("A2700")'])]
    assert gs.entries(doc, [copy]) == [("A2700", ['c("448")', 'f("A2700")'])]
    # a handle the harvest re-measured is another entry: the copy names the one it saw
    assert gs.entries(doc, [{**copy, "handle": "999"}])[0][1][0].startswith("k(")


def test_a_venue_whose_every_feed_is_a_copy_must_be_retired():
    doc = {"venues": [venue("kinoheld:2323", "kinoheld", [], kinoheld={"id": "2323", "path": "/kino/x/kino-in-dorfen"})]}
    copy = {"venue": "kinoheld:2323", "source": "kinoheld", "handle": "2323"}
    try:
        gs.entries(doc, [copy])
        raise AssertionError("a feedless venue must stop the generation")
    except ValueError as e:
        assert "retire" in str(e)
    assert gs.entries(doc, [copy], retired={"kinoheld:2323"}) == []


if __name__ == "__main__":
    tests = [(n, f) for n, f in sorted(globals().items()) if n.startswith("test_")]
    for name, test in tests:
        test()
        print(f"PASS {name}")
    print(f"all {len(tests)} passed")
