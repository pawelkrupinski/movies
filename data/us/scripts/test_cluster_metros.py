#!/usr/bin/env python3
"""
Unit test for the coordinate guards and the per-metro timezone in
cluster_metros -- the two things that decide which City a venue lands in and
what clock that City keeps.

Both guards were written against errors that had been SHIPPING. A venue's
coordinates are the only input to the metro clustering, and a wrong one does
not fail to cluster: it clusters somewhere plausible and wrong, silently, and
drags every cinema sharing its town name along with it. The four real cases,
each contradicted by the venue's own postcode:

  1. `Grand Theatre Perry` (Iowa 50220) had a flipped longitude sign and sat in
     Mongolia, so it became a one-venue metro of its own instead of joining Des
     Moines 65 km away.
  2. `Newport Performing Arts` is in Newport, OREGON (97365) and was filed under
     Newport News, Virginia -- 3,900 km off, dragging two real Newport News
     cinemas into a metro of their own.
  3. `Regal Largo Mall` (Largo, 33771) arrived as city "Key Largo", so it and
     the real Key Largo cinema averaged to a point near Fort Myers and BOTH
     landed in the Naples metro, 380 km apart.
  4. `Sky Vu Drive In Monroe` (Monroe, 53566) arrived as city "Tomah", putting
     the real Tomah cinema in Madison rather than La Crosse.

Run: python3 data/us/scripts/test_cluster_metros.py
"""
import importlib.util
import os
import sys

_here = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location(
    "cluster_metros", os.path.join(_here, "cluster_metros.py"))
cm = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(cm)


def venue(slug, city, lat, lon):
    return {'slug': slug, 'city': city, 'lat': lat, 'lon': lon, 'metro': ''}


def refuses(state, venues, expected):
    """Assert check_coordinates dies, naming `expected` in the reason."""
    try:
        cm.check_coordinates(state, venues)
    except SystemExit as exit_:
        assert expected in str(exit_), f"expected {expected!r} in the refusal, got: {exit_}"
        return
    raise AssertionError(f"{state}: expected a refusal naming {expected!r}, none came")


# A real state passes: Des Moines' own venues, plus Perry with the sign it
# should have had all along.
DES_MOINES = [
    venue('grand-theatre-perry', 'Perry', 41.84101, -94.10478),
    venue('flix-brewhouse-des-moines', 'Des Moines', 41.58684, -93.61510),
    venue('cinemark-ankeny', 'Ankeny', 41.72938, -93.60577),
]


def test_clean_state_passes():
    cm.check_coordinates('Iowa', DES_MOINES)


def test_flipped_longitude_is_refused():
    # The Perry shape: +94 rather than -94 puts an Iowa cinema in Mongolia.
    flipped = [venue('grand-theatre-perry', 'Perry', 41.84101, 94.10478)] + DES_MOINES[1:]
    refuses('Iowa', flipped, 'grand-theatre-perry')


def test_venue_filed_under_the_wrong_state_is_refused():
    # The Newport shape: an Oregon venue among Virginia's.
    virginia = [
        venue('cinemark-newport-news', 'Newport News', 37.08553, -76.47165),
        venue('regal-kiln-creek', 'Newport News', 37.11010, -76.46814),
        venue('regal-macarthur', 'Norfolk', 36.85293, -76.28869),
        venue('newport-performing-arts', 'Newport News', 44.63618, -124.06194),
    ]
    refuses('Virginia', virginia, 'newport-performing-arts')


def test_town_whose_venues_are_far_apart_is_refused():
    # The Key Largo shape. Neither venue is individually implausible for
    # Florida, so only the TOWN check sees it.
    florida = [
        venue('ocean-reef-theater', 'Key Largo', 25.31981, -80.27856),
        venue('regal-largo-mall', 'Key Largo', 27.89075, -82.78376),
        venue('regal-hollywood-naples', 'Naples', 26.21696, -81.77161),
    ]
    refuses('Florida', florida, 'Key Largo')


def test_a_genuinely_wide_town_passes():
    # Houston is the widest real one at 56 km; the ceiling is the 75 km metro
    # radius, so a big city must not trip the town check.
    houston = [
        venue('amc-gulf-pointe-30', 'Houston', 29.60483, -95.21313),
        venue('star-cinema-grill-vintage-park', 'Houston', 29.99463, -95.57264),
        venue('amc-houston-8', 'Houston', 29.76212, -95.36652),
    ]
    cm.check_coordinates('Texas', houston)
    span = cm.haversine_km((29.60483, -95.21313), (29.99463, -95.57264))
    assert 50 < span < cm.MAX_TOWN_SPAN_KM, span


def test_same_clock_reads_offsets_not_names():
    # The two names Indiana straddles are one clock; Phoenix and Denver are two,
    # and only in summer -- which is why a single probe date is not enough.
    assert cm.same_clock('America/Indiana/Indianapolis', 'America/New_York')
    assert cm.same_clock('America/Kentucky/Louisville', 'America/New_York')
    assert not cm.same_clock('America/Phoenix', 'America/Denver')
    assert not cm.same_clock('America/Chicago', 'America/New_York')


def test_zone_for_takes_the_majority_clock():
    if not _timezonefinder_available():
        print("  (skipped zone_for: timezonefinder not installed)")
        return
    # Knoxville: entirely Eastern, in a state whose predominant zone is Central.
    # This is the case that shipped wrong.
    knoxville = [venue('regal-pinnacle', 'Knoxville', 35.96064, -83.92074),
                 venue('amc-classic-foothills', 'Maryville', 35.75652, -83.97210)]
    assert cm.zone_for(knoxville) == 'America/New_York', cm.zone_for(knoxville)
    # El Paso: Mountain, in Central-predominant Texas.
    el_paso = [venue('amc-el-paso-16', 'El Paso', 31.88086, -106.44034),
               venue('plaza-theatre-el-paso', 'El Paso', 31.75774, -106.49182)]
    assert cm.zone_for(el_paso) == 'America/Denver', cm.zone_for(el_paso)
    # A straddling metro takes the side most of it is on, not the first one seen.
    straddle = knoxville + [venue('amc-classic-crossville', 'Crossville', 35.94896, -85.02690)]
    assert cm.zone_for(straddle) == 'America/New_York', cm.zone_for(straddle)


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
