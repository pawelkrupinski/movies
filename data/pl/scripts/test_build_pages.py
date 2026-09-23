#!/usr/bin/env python3
"""Tests for build_pages.py's clustering rule, on a synthetic map (no GeoNames).

    python3 data/pl/scripts/test_build_pages.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_pages as b  # noqa: E402

# One degree of latitude is ~111 km, so offsets below are in km north of 52°N.
def at(km_north, km_east=0.0):
    return {"lat": 52.0 + km_north / 111.0, "lon": 20.0 + km_east / 68.5, "pop": 0, "admin1": "78"}

MAJOR = {"poznan": ("Poznan", "Poznań", 60.0, 20.0)}


def run(towns, venues, previous=None, max_drift=10 ** 6):
    """towns: name -> (km_north, km_east, population). venues: [(town, obj)]."""
    gaz = {n: {**at(k, e), "pop": p} for n, (k, e, p) in towns.items()}
    forms = {n: n + "-loc" for n in towns}
    rows = [{"citySlug": "poznan", "cinemaObject": obj, "displayName": obj,
             "town": "" if t == "Poznań" else t, "annotation": ""} for t, obj in venues]
    pages = {"poznan": MAJOR["poznan"]}
    saved = b.MAJOR_SLUGS, b.MAX_DRIFT_KM
    b.MAJOR_SLUGS, b.MAX_DRIFT_KM = ["poznan"], max_drift
    try:
        out, retired = b.build(rows, gaz, forms, pages, previous or {"poznan": "Poznań"}, {})
    finally:
        b.MAJOR_SLUGS, b.MAX_DRIFT_KM = saved
    return {p["slug"]: p for p in out}, retired


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def main():
    towns = {
        "Big":      (0, 0, 50000),    # 3 venues → its own page
        "Two":      (100, 0, 20000),  # 2 venues → anchors a 10 km cluster
        "NearTwo":  (106, 0, 3000),   # 6 km from Two → joins it
        "Mid":      (115, 0, 9000),   # 15 km from Two: outside its 10 km → clusters with MidFriend
        "MidFriend": (125, 0, 1000),
        "Hub":      (200, 0, 8000),   # 1 venue, biggest of its group → anchor
        "Satellite": (220, 0, 2000),  # 20 km from Hub → joins at 25 km
        "Loner":    (232, 0, 1000),   # 32 km from Hub → alone, then absorbed (≤35 km)
        "Island":   (400, 0, 5000),   # nothing within 35 km → a page of its own
        "Poznań":   (60, 0, 500000),
    }
    venues = [("Big", "B1"), ("Big", "B2"), ("Big", "B3"), ("Two", "T1"), ("Two", "T2"),
              ("NearTwo", "N1"), ("Mid", "M1"), ("MidFriend", "M2"), ("Hub", "H1"), ("Satellite", "S1"),
              ("Loner", "L1"), ("Island", "I1"), ("Poznań", "P1")]
    pages, retired = run(towns, venues)

    check("a major city keeps only its own venues", pages["poznan"]["cinemas"] == ["P1"])
    check("a 3-venue town is a page of its own", pages["big"]["kind"] == "town" and pages["big"]["towns"] == ["Big"])
    check("a 2-venue town takes the towns within 10 km", pages["two"]["towns"] == ["Two", "NearTwo"])
    check("a 1-venue town 15 km from a 2-venue town clusters with its own neighbours instead",
          "Mid" not in pages["two"]["towns"] and pages["mid"]["towns"] == ["Mid", "MidFriend"])
    check("1-venue towns cluster within 25 km of the biggest", pages["hub"]["towns"][:2] == ["Hub", "Satellite"])
    check("a lone town joins the nearest cluster within 35 km", "Loner" in pages["hub"]["towns"])
    check("a town with nothing within 35 km is its own page", pages["island"]["kind"] == "town")
    check("a cluster is named after its anchor", pages["hub"]["name"] == "Hub" and pages["hub"]["kind"] == "cluster")
    check("every venue is on exactly one page",
          sorted(c for p in pages.values() for c in p["cinemas"]) == sorted(o for _, o in venues))

    # A page that existed before and is gone now redirects to its town's new page.
    _, retired = run(towns, venues, previous={"poznan": "Poznań", "satellite": "Satellite"})
    check("a retired page redirects to the page holding its town", retired == {"satellite": "hub"})

    # The drift guard: a town placed far from the page its venue was on (Poznań,
    # 60 km north) — the shape a GeoNames namesake produces.
    try:
        run({"Poznań": (60, 0, 500000), "Namesake": (400, 300, 3000)},
            [("Namesake", "N1"), ("Poznań", "P1")], max_drift=b.MAX_DRIFT_KM)
        check("the drift guard stops a town placed far from its old page", False)
    except SystemExit:
        check("the drift guard stops a town placed far from its old page", True)
    print("all passed")


if __name__ == "__main__":
    main()
