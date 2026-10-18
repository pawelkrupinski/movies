#!/usr/bin/env python3
"""Unit test for retire_venues' two decisions: which venues the live re-check lets
through, and how the roster size is rewritten.

The first is the last gate before a venue leaves the site as "closed", so it needs
the aggregator page to answer a clean 404/410 (a 200, a runner blocked with 403, no
answer at all keep it) AND a page about the venue itself to re-confirm the closure:
an aggregator dropping a venue is not the venue closing (ACME Screening Room).

Run: python3 data/scripts/retire_venues_test.py
"""
import re
import os

HERE = os.path.dirname(os.path.abspath(__file__))
import retire_venues as rv  # noqa: E402  (this file's directory leads sys.path)

PAGE = "https://example.test/kino/{id}/"
SITE = "https://kino.example/"
FIXTURES = os.path.join(HERE, "..", "..", "worker", "src", "test", "resources", "fixtures", "venue-websites")


def venue(venue_id, checks=None):
    return {"id": venue_id, "name": f"Kino {venue_id}", "evidence": "gone since 2026-09-01.",
            "checks": [{"kind": "website", "url": SITE}] if checks is None else checks}


def site_answers(answer, body=""):
    return lambda url: (answer, body)


def test_retires_only_a_page_that_answers_gone():
    statuses = {"A1": "404", "A2": "410", "A3": "200", "A4": "403", "A5": "no answer (TimeoutError)"}
    added, kept = rv.decide({}, [venue(i) for i in statuses], PAGE,
                            lambda url: statuses[url.split("/")[-2]], "2026-09-27", site_answers("404"))
    assert sorted(added) == ["A1", "A2"], added
    assert added["A1"] == {"name": "Kino A1", "reason": "closed", "retiredOn": "2026-09-27",
                           "evidence": "gone since 2026-09-01. Re-checked 2026-09-27: primary page HTTP 404; "
                                       f"own site {SITE} answered HTTP 404."}
    assert [v["id"] for v, _ in kept] == ["A3", "A4", "A5"], kept
    assert "403" in dict((v["id"], why) for v, why in kept)["A4"]


def test_rechecks_the_page_the_primary_scrapes_when_the_sweep_names_one():
    # ACME Screening Room: Flicks 404s it for good, but the venue is scraped off its own
    # site, so only that site's page can say it closed.
    own = dict(venue("acme"), page="https://www.acmescreeningroom.org/now-playing")
    statuses = {"https://www.acmescreeningroom.org/now-playing": "200", PAGE.format(id="acme"): "404"}
    added, kept = rv.decide({}, [own], PAGE, statuses.__getitem__, "2026-10-12", site_answers("404"))
    assert added == {} and kept == [(own, "primary page answered 200, not 404/410")], (added, kept)


def test_skips_a_venue_already_retired_without_probing_it():
    def refuse(url):
        raise AssertionError(f"probed {url}")
    added, kept = rv.decide({"A1": {}}, [venue("A1")], PAGE, refuse, "2026-09-27")
    assert added == {} and kept == [(venue("A1"), "already retired")]


def test_rewrites_the_count_only_as_a_whole_number():
    text = "Germany's 1,517 venues; 11,517 is not it, nor 1,5170, nor 21,517."
    assert rv.rewrite_count(text, 1517, 1516) == "Germany's 1,516 venues; 11,517 is not it, nor 1,5170, nor 21,517."


def test_rewrites_the_count_only_in_prose_files():
    # Spain's 602 has no thousands comma, so a bare `git grep 602` also took fonts, OG images and a
    # roster-audit fixture: read_text() crashed on the binaries after retired.json was written, and
    # a fixture whose ids or coordinates hold 602 would have been silently rewritten.
    # The roster's size as CountrySpec pins it today, so a retirement does not break this test.
    size = int(re.search(rv.PINNED_SIZE["spain"], rv.COUNTRY_SPEC.read_text()).group(2))
    files = rv.files_quoting(size, "spain")
    assert "common/src/main/scala/models/Cinema.scala" in files, files
    assert "data/spain/README.md" in files, files
    assert all(f.endswith(".scala") or f.endswith("README.md") for f in files), files


def test_retries_a_page_that_did_not_answer_but_not_one_that_did():
    answers = iter(["no answer (URLError)", "no answer (TimeoutError)", "404"])
    assert rv.probe("https://example.test/", fetch=lambda url: next(answers), sleep=lambda s: None) == "404"
    calls = []
    def blocked(url):
        calls.append(url)
        return "403"
    assert rv.probe("https://example.test/", fetch=blocked, sleep=lambda s: None) == "403"
    assert len(calls) == 1
    never = rv.probe("https://example.test/", fetch=lambda url: "no answer (URLError)", sleep=lambda s: None)
    assert never == "no answer (URLError)"


def test_accepts_a_venue_retired_for_not_being_a_cinema_and_rejects_an_unknown_reason():
    import json
    import tempfile
    entry = {"name": "Kleines Theater", "retiredOn": "2026-10-12", "evidence": "a stage theatre"}
    with tempfile.TemporaryDirectory() as directory:
        path = os.path.join(directory, "retired.json")
        with open(path, "w") as file:
            json.dump({"A1": {**entry, "reason": "not-a-cinema"}}, file)
        assert set(rv.retired_venues.load(rv.pathlib.Path(directory))) == {"A1"}
        with open(path, "w") as file:
            json.dump({"A1": {**entry, "reason": "renovating"}}, file)
        try:
            rv.retired_venues.load(rv.pathlib.Path(directory))
            raise AssertionError("an unknown reason was accepted")
        except SystemExit:
            pass


# ACME Screening Room, Lambertville: flicks.us 404'd it and it was retired as closed while
# its own site sold tickets to its October festival. Recorded page, shared with ClosureSweepSpec.
def test_keeps_a_venue_its_aggregator_dropped_while_its_own_site_is_live():
    with open(os.path.join(FIXTURES, "acme_screening_room_open_with_programme.html")) as page:
        acme = page.read()
    added, kept = rv.decide({}, [venue("acme-screening-room-lambertville")], PAGE, lambda url: "404",
                            "2026-10-12", site_answers("200", acme))
    assert added == {}, added
    assert "source dropped, but the venue shows life" in kept[0][1], kept


def test_keeps_a_venue_with_nothing_about_the_venue_itself_to_check():
    added, kept = rv.decide({}, [venue("A1", checks=[])], PAGE, lambda url: "404", "2026-10-12",
                            site_answers("404"))
    assert added == {} and "nothing re-confirms the venue closed" in kept[0][1], kept


def test_keeps_a_venue_whose_own_site_cannot_be_read():
    for answer in ["403", "503", "no answer (TimeoutError)"]:
        added, kept = rv.decide({}, [venue("A1")], PAGE, lambda url: "404", "2026-10-12", site_answers(answer))
        assert added == {}, (answer, added)


def test_retires_on_each_kind_of_venue_level_closure():
    cases = {
        "dns":     site_answers(rv.NO_SUCH_HOST),
        "parked":  site_answers("200", "<h1>This domain is for sale</h1>"),
        "notice":  site_answers("200", "<p>Das Kino ist   dauerhaft\ngeschlossen.</p>"),
        "listing": site_answers("410"),
    }
    for case, read in cases.items():
        checks = [{"kind": "listing", "url": "https://listing.example/x/"}] if case == "listing" else None
        added, _ = rv.decide({}, [venue("A1", checks)], PAGE, lambda url: "404", "2026-10-12", read)
        assert list(added) == ["A1"], (case, added)


def test_reads_a_day_off_or_a_script_as_no_closure():
    assert rv.website_signal(SITE, "200", "<p>Montag geschlossen</p>")[0] == "life"
    assert rv.website_signal(SITE, "200", "<script>var s='permanently closed';</script><p>Now playing</p>")[0] == "life"


def test_shares_its_notices_with_the_worker():
    assert "dauerhaft geschlossen" in rv.NOTICES["closure"] and "this domain is for sale" in rv.NOTICES["parked"]


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
