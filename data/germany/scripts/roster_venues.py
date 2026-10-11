"""The German roster's venues, whichever file holds them.

The roster input is moving from regions.json (regions, each with its cinemas)
to venues.json (one flat list of venues, which build_pages.py groups into
pages). Every harvest script reads the roster through `load()`, so the move
changes this file alone.
"""

import json
import os

DATA = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def load(data=DATA):
    """Every rostered venue: {theaterId, name, displayName, city}."""
    venues_path = os.path.join(data, "venues.json")
    if os.path.exists(venues_path):
        doc = json.load(open(venues_path))
        rows = doc["venues"] if isinstance(doc, dict) else doc
    else:
        rows = [c for region in json.load(open(os.path.join(data, "regions.json"))) for c in region["cinemas"]]
    return [{"theaterId": r["theaterId"], "name": r["name"], "displayName": r.get("displayName") or r["name"],
             "city": r.get("city") or r.get("town") or ""} for r in rows if r.get("theaterId")]


def page_slugs(data=DATA):
    """The slugs of the pages the roster is served under -- each one's `/de/<slug>/api/repertoire`."""
    pages_path = os.path.join(data, "pages.json")
    if os.path.exists(pages_path):
        doc = json.load(open(pages_path))
        rows = doc["pages"] if isinstance(doc, dict) else doc
        return [p["slug"] for p in rows]
    return [r["slug"] for r in json.load(open(os.path.join(data, "regions.json")))]
