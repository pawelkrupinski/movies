#!/usr/bin/env python3
"""Tests for geocode_towns.py: which GeoNames town a venue is matched to, on a
synthetic gazetteer written in GeoNames' own format.

    python3 data/us/scripts/geocode_towns_test.py
"""
import itertools
import math
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import geocode_towns as g  # noqa: E402


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def refuses(f, naming):
    try:
        f()
    except SystemExit as e:
        return naming in str(e)
    return False


def at(km_north, km_east, lat0=40.0, lon0=-95.0):
    lat = lat0 + km_north / 111.2
    return lat, lon0 + km_east / (111.2 * math.cos(math.radians(lat)))


def row(name, country, admin1, admin2, lat, lon, pop):
    cols = [""] * 19
    cols[1], cols[4], cols[5], cols[8], cols[10], cols[11], cols[14] = name, str(lat), str(lon), country, admin1, admin2, str(pop)
    return "\t".join(cols)


def gazetteer(rows):
    with tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False, encoding="utf-8") as f:
        f.write("\n".join(rows) + "\n")
    return g.load_gazetteer(f.name)


def venue(slug, city, state, point):
    return {"slug": slug, "city": city, "state": state, "lat": str(point[0]), "lon": str(point[1])}


NYC = at(0, 0, 40.71427, -74.00597)
HOUSTON = at(0, 0, 29.76328, -95.36327)
GAZ = gazetteer([
    row("New York City", "US", "NY", "", *NYC, 8_804_190),
    row("Bayside", "US", "NY", "081", *at(12, 18, *NYC), 66_455),         # Queens
    row("Yonkers", "US", "NY", "119", *at(25, 3, *NYC), 201_116),         # Westchester: a city of its own
    row("Houston", "US", "TX", "201", *HOUSTON, 2_296_224),
    row("Humble", "US", "TX", "201", *at(30, 12, *HOUSTON), 15_665),
    row("Springfield", "US", "IL", "167", *at(0, 0), 114_000),
    row("Springfield", "US", "MO", "077", *at(-10, -400), 169_000),
    row("Madison", "US", "WI", "025", *at(300, 300), 270_000),
    row("Madison", "US", "MN", "073", *at(300, -300), 1_500),
    row("Hagåtña", "GU", "00", "", *at(0, 0, 13.47, 144.75), 1_000),
])


def place(*venues):
    return g.place(list(venues), GAZ)


def main():
    towns, town_of = place(venue("b", "Brooklyn", "New York", at(-8, 5, *NYC)),
                           venue("q", "Bayside", "New York", at(12, 18, *NYC)),
                           venue("y", "Yonkers", "New York", at(25, 3, *NYC)))
    check("a borough Flicks names is New York City", town_of["b"] == "New York City, New York")
    check("a Queens neighbourhood GeoNames lists as a town is New York City too",
          town_of["q"] == "New York City, New York" and towns[town_of["q"]]["population"] == 8_804_190)
    check("a city beside New York is a town of its own", town_of["y"] == "Yonkers, New York")

    towns, town_of = place(venue("vp", "Houston", "Texas", at(25, -22, *HOUSTON)),
                           venue("h", "Humble", "Texas", at(30, 12, *HOUSTON)))
    check("a big city's own cinema 33 km from its centre is still in it",
          town_of["vp"] == "Houston, Texas" and towns["Houston, Texas"]["population"] == 2_296_224)

    towns, town_of = place(venue("s", "Springfield", "Illinois", at(2, 1)))
    check("a name is matched in the venue's own state", town_of["s"] == "Springfield, Illinois")
    towns, town_of = place(venue("x", "Riverton", "Illinois", at(4, 0)))
    check("an unlisted name takes the state's town within 6 km", town_of["x"] == "Springfield, Illinois")
    towns, town_of = place(venue("v", "Tinytown", "Illinois", at(50, 50)), venue("w", "Tinytown", "Illinois", at(52, 50)))
    check("a town the gazetteer lacks is centred on its venues, with no population",
          town_of["v"] == "Tinytown, Illinois" and towns["Tinytown, Illinois"]["population"] == 0
          and abs(towns["Tinytown, Illinois"]["lat"] - (at(50, 50)[0] + at(52, 50)[0]) / 2) < 1e-4)
    towns, town_of = place(venue("gu", "Hagatna", "Guam", at(1, 1, 13.47, 144.75)))
    check("a territory is matched against its own GeoNames country", town_of["gu"] == "Hagåtña, Guam")

    check("a venue harvested at a namesake town's coordinates in another state is refused",
          refuses(lambda: place(venue("grand-theater-madison", "Madison", "Minnesota", at(300, 300))),
                  "grand-theater-madison"))
    check("a venue too far from its own town's gazetteer centre to be matched to it is refused",
          refuses(lambda: place(venue("far", "Springfield", "Illinois", at(80, 0)),
                                venue("near", "Springfield", "Illinois", at(1, 0))), "far"))

    # Spencer, Iowa's longitude sits exactly on a rounding boundary: a naive float
    # sum lands below it and a compensated one on it, so the mean is exact.
    spencer = ["-95.1247219", "-95.1499786", "-94.6684079", "-94.6794721", "-94.9932445"]
    check("a centroid is the exact mean, rounded half-even, in any order",
          all(g.town_pages.exact_mean(order) == -94.92316 for order in itertools.permutations(spencer)))
    print("all passed")


if __name__ == "__main__":
    main()
