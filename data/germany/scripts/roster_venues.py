"""The German roster's venues and pages, as the harvest scripts read them.

The roster is venues.json less retired.json, grouped into the pages of pages.json
(build_pages.py). Every harvest script reads it through here, so a change to how
the roster is kept changes this file alone.
"""

import json
import os

DATA = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def load(data=DATA):
    """Every rostered venue with a Filmstarts page: {theaterId, name, displayName, city}."""
    venues = json.load(open(os.path.join(data, "venues.json")))
    retired_path = os.path.join(data, "retired.json")
    retired = set(json.load(open(retired_path))) if os.path.exists(retired_path) else set()
    return [{"theaterId": v["theaterId"], "name": v["name"], "displayName": v.get("displayName") or v["name"],
             "city": v.get("city", "")}
            for v in venues if v.get("theaterId") and v["theaterId"] not in retired]


def page_slugs(data=DATA):
    """The slugs of the pages the roster is served under -- each one's `/de/<slug>/api/repertoire`."""
    return [p["slug"] for p in json.load(open(os.path.join(data, "pages.json")))["pages"]]
