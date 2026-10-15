#!/usr/bin/env python3
"""Tests for the shared page rule's reach cap (`town_pages.within_reach`, step 4)
and the retired-slug succession every country's build writes through.

    python3 data/scripts/town_pages_test.py
"""
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import town_pages as tp  # noqa: E402


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def at(km_north, km_east=0.0):
    lat = 52.0 + km_north / 111.2
    return lat, 0.0 + km_east / (111.2 * math.cos(math.radians(lat)))


def reach_cap():
    # Hub (1 venue, biggest) clusters Satellite at 20 km; Far sits 34 km out —
    # inside the 35 km absorb — but its venue is 8 km beyond its centre.
    pos = {"Hub": at(0), "Satellite": at(20), "Far": at(-34), "Side": at(10, 25)}
    pop = {"Hub": 50_000, "Satellite": 5_000, "Far": 4_000, "Side": 60_000}
    count = {"Hub": 1, "Satellite": 1, "Far": 1, "Side": 2}
    venue = dict(pos, Far=at(-42))

    def reach(t, anchor):
        return tp.km(pos[anchor], venue[t])
    uncapped = tp.cluster(count, pos, pop, tp.Rules(), reach=reach)
    check("without a cap, the far town is absorbed into the hub's cluster",
          any("Far" in c and "Hub" in c for c in uncapped))
    capped = tp.cluster(count, pos, pop, tp.Rules(max_reach_km=40), reach=reach)
    check("with a 40 km cap, a town whose venue is 42 km from the anchor leaves the cluster",
          not any("Far" in c and "Hub" in c for c in capped))
    check("…and, within reach of nothing else, stands as a page of its own", ["Far"] in capped)
    check("every town is still on exactly one page",
          sorted(t for c in capped for t in c) == sorted(count))


def rejoin_and_resort():
    # Within reach of another cluster, an ejected town joins it — and the
    # cluster's towns stay in rank order, anchor first.
    count = {"Big": 2, "Small": 1, "Alone": 1, "Mid": 1}
    pop = {"Big": 9_000, "Small": 100, "Alone": 50, "Mid": 5_000}
    pos = {"Big": at(0), "Small": at(5), "Alone": at(100), "Mid": at(-10)}

    def dist(a, b):
        return tp.km(pos[a], pos[b])
    out = tp.within_reach([["Big", "Small"], ["Alone", "Mid"]], count, pos, pop, tp.Rules(max_reach_km=40),
                          dist, lambda t, a: dist(t, a), None)
    big = next(c for c in out if c[0] == "Big")
    check("an ejected town within reach of another cluster joins it", "Mid" in big)
    check("…which is re-sorted, anchor first, then by venues and people",
          big == ["Big", "Mid", "Small"])
    check("the cluster it left keeps its anchor", ["Alone"] in out)


def succession():
    result = {"leeds": {"lat": 53.8, "lon": -1.55, "kind": "major"},
              "bradford": {"lat": 53.79, "lon": -1.75, "kind": "major"}}
    page_of_town = {"Leeds": "leeds", "Bradford": "bradford"}
    venue_town = {"le1": "Leeds", "br1": "Bradford", "br2": "Bradford"}
    previous = {"west-yorkshire": {"cinemas": ["le1", "br1", "br2"], "lat": 53.8, "lon": -1.6, "anchor": None,
                                   "qualifiedSlug": "west-yorkshire", "name": "West Yorkshire"}}
    prior = {"old": {"page": "leeds", "qualifiedSlug": "old", "anchor": None, "cinemas": ["br1"],
                     "lat": 53.79, "lon": -1.75},
             "us-style": {"page": "leeds", "qualifiedSlug": "us-style", "anchor": None}}
    retired = tp.succession(previous, prior, result, page_of_town, venue_town, carry=("name", "cinemas"))
    check("a gone page goes to the page holding most of its venues", retired["west-yorkshire"]["page"] == "bradford")
    check("an earlier retirement recording its venues follows them", retired["old"]["page"] == "bradford")
    check("one recording none keeps the page it pointed at", retired["us-style"]["page"] == "leeds")
    check("the carried fields ride along, and only those",
          retired["west-yorkshire"]["name"] == "West Yorkshire" and "lat" not in retired["west-yorkshire"])


def main():
    reach_cap()
    rejoin_and_resort()
    succession()
    print("all passed")


if __name__ == "__main__":
    main()
