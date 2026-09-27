#!/usr/bin/env python3
"""Unit test for retire_venues' two decisions: which venues the live re-check lets
through, and how the roster size is rewritten.

The first is the last gate before a venue leaves the site, so every answer that is
not a clean 404/410 (a 200, a runner blocked with 403, no answer at all) must keep it.

Run: python3 data/scripts/test_retire_venues.py
"""
import importlib.util
import os

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location("retire_venues", os.path.join(HERE, "retire_venues.py"))
rv = importlib.util.module_from_spec(spec)
spec.loader.exec_module(rv)

PAGE = "https://example.test/kino/{id}/"


def venue(venue_id):
    return {"id": venue_id, "name": f"Kino {venue_id}", "evidence": "gone since 2026-09-01."}


def test_retires_only_a_page_that_answers_gone():
    statuses = {"A1": "404", "A2": "410", "A3": "200", "A4": "403", "A5": "no answer (TimeoutError)"}
    added, kept = rv.decide({}, [venue(i) for i in statuses], PAGE,
                            lambda url: statuses[url.split("/")[-2]], "2026-09-27")
    assert sorted(added) == ["A1", "A2"], added
    assert added["A1"] == {"name": "Kino A1", "reason": "closed", "retiredOn": "2026-09-27",
                           "evidence": "gone since 2026-09-01. Re-checked 2026-09-27: HTTP 404."}
    assert [v["id"] for v, _ in kept] == ["A3", "A4", "A5"], kept
    assert "403" in dict((v["id"], why) for v, why in kept)["A4"]


def test_skips_a_venue_already_retired_without_probing_it():
    def refuse(url):
        raise AssertionError(f"probed {url}")
    added, kept = rv.decide({"A1": {}}, [venue("A1")], PAGE, refuse, "2026-09-27")
    assert added == {} and kept == [(venue("A1"), "already retired")]


def test_rewrites_the_count_only_as_a_whole_number():
    text = "Germany's 1,517 venues; 11,517 is not it, nor 1,5170, nor 21,517."
    assert rv.rewrite_count(text, 1517, 1516) == "Germany's 1,516 venues; 11,517 is not it, nor 1,5170, nor 21,517."


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


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
