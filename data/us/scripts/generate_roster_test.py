#!/usr/bin/env python3
"""Tests for the US generate_roster.py: the wire keys (display names) it emits,
and the clock it gives a page and the venues on another one.

Run: python3 data/us/scripts/generate_roster_test.py
"""
import os
import sys

sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import generate_roster as gr  # noqa: E402


def venue(slug, title, city, state, lat=0.0, lon=0.0):
    return {'slug': slug, 'title': title, 'city': city, 'state': state, 'lat': lat, 'lon': lon}


def test_a_unique_title_is_the_display_name_unchanged():
    names = gr.display_names([venue('a', 'Grand  Theatre', 'Perry', 'Iowa')])
    assert names == {'a': 'Grand Theatre'}, names


def test_a_shared_title_is_qualified_with_its_town_then_its_state():
    names = gr.display_names([
        venue('a', 'Cinema 6', 'Salem', 'Oregon'),
        venue('b', 'Cinema 6', 'Salem', 'Massachusetts'),
        venue('c', 'Cinema 6', 'Albany', 'Oregon'),
    ])
    assert names == {'a': 'Cinema 6 (Salem) (Oregon)', 'b': 'Cinema 6 (Salem) (Massachusetts)',
                     'c': 'Cinema 6 (Albany)'}, names


def test_an_unresolvable_collision_is_refused():
    try:
        gr.display_names([venue('a', 'Cinema', 'Salem', 'Oregon'), venue('b', 'Cinema', 'Salem', 'Oregon')])
    except SystemExit:
        return
    raise AssertionError('two identical wire keys were emitted')


def test_same_clock_reads_offsets_not_names():
    # The two names Indiana straddles are one clock; Phoenix and Denver are two,
    # and only in summer -- which is why a single probe date is not enough.
    assert gr.same_clock('America/Indiana/Indianapolis', 'America/New_York')
    assert gr.same_clock('America/Kentucky/Louisville', 'America/New_York')
    assert not gr.same_clock('America/Phoenix', 'America/Denver')
    assert not gr.same_clock('America/Chicago', 'America/New_York')


def test_zone_for_takes_the_majority_clock():
    if not _timezonefinder_available():
        print("  (skipped zone_for: timezonefinder not installed)")
        return
    # Knoxville: entirely Eastern, in a state whose predominant zone is Central.
    knoxville = [{'lat': 35.96064, 'lon': -83.92074}, {'lat': 35.75652, 'lon': -83.97210}]
    assert gr.zone_for(knoxville) == 'America/New_York', gr.zone_for(knoxville)
    # El Paso: Mountain, in Central-predominant Texas.
    el_paso = [{'lat': 31.88086, 'lon': -106.44034}, {'lat': 31.75774, 'lon': -106.49182}]
    assert gr.zone_for(el_paso) == 'America/Denver', gr.zone_for(el_paso)
    # A page straddling a boundary takes the side most of it is on.
    straddle = knoxville + [{'lat': 35.94896, 'lon': -85.02690}]
    assert gr.zone_for(straddle) == 'America/New_York', gr.zone_for(straddle)


def test_a_venue_across_a_zone_line_from_its_page_carries_its_own_zone():
    if not _timezonefinder_available():
        print("  (skipped render: timezonefinder not installed)")
        return
    # Crossville (Central) on a Knoxville (Eastern) page.
    venues = [venue('k1', 'Regal Pinnacle', 'Knoxville', 'Tennessee', '35.96064', '-83.92074'),
              venue('k2', 'AMC Foothills', 'Maryville', 'Tennessee', '35.75652', '-83.97210'),
              venue('c1', 'AMC Crossville', 'Crossville', 'Tennessee', '35.94896', '-85.02690')]
    pages = [{'slug': 'knoxville', 'qualifiedSlug': 'knoxville-tennessee', 'name': 'Knoxville', 'state': 'Tennessee',
              'kind': 'cluster', 'lat': 35.96, 'lon': -83.92, 'towns': ['Knoxville, Tennessee'],
              'cinemas': ['k1', 'k2', 'c1']}]
    text, own = gr.render(pages, {}, venues, lambda key: key.split(',')[0])
    assert own == 1, own
    assert '("AMC Crossville", "America/Chicago"),' in text, text
    assert '"America/New_York", true, Seq("Knoxville")' in text, text


def _timezonefinder_available():
    try:
        import timezonefinder  # noqa: F401
        return True
    except ImportError:
        return False


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith('test_')]
    for test in tests:
        test()
        print(f"  ok  {test.__name__}")
    print(f"{len(tests)} passed")


if __name__ == '__main__':
    sys.exit(main())
