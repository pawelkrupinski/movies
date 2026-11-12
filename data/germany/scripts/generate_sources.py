#!/usr/bin/env python3
"""Generate geodata/src/main/resources/generated/de-sources/feeds.tsv (read by `services.cinemas.de.GermanSourcesData`)
from venue-sources.json.

Each ROSTERED venue whose sources differ from the default arrangement
(Filmstarts, then its kinoprogramm.com page) becomes one entry: its venue key --
its Filmstarts theaterId, or its `feed` ("cineamo:997") when Filmstarts does not
list it -- -> its feeds, primary first, as harvest_sources.py chose them, less every
feed feed-mirrors.json records as another venue's programme (feed_mirrors.py): the
venue is served by the next. A venue that leaves feedless is a duplicate, and must
be retired.

Run: python3 data/germany/scripts/generate_sources.py
"""

import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(DATA))
SRC = os.path.join(DATA, "venue-sources.json")
MIRRORS = os.path.join(DATA, "feed-mirrors.json")
RETIRED = os.path.join(DATA, "retired.json")
OUT = os.path.join(ROOT, "geodata/src/main/resources/generated/de-sources/feeds.tsv")
sys.path.insert(0, os.path.join(ROOT, "data", "scripts"))
from generated_tables import list_cell, write_table  # noqa: E402


def feed(source, venue):
    """(kind, args) of the venue's feed in `source`: the kind `GermanSourcesData` parses into a GermanFeed."""
    if source == "filmstarts":
        return source, [venue["filmstarts"]]
    if source == "cineamo":
        return source, [venue["cineamo"]]
    if source == "kinoheld":
        return source, [venue["kinoheld"]["id"], venue["kinoheld"]["path"]]
    if source == "kinoprogramm":
        return source, [venue["kinoprogramm"]]
    if source == "ownSite":
        site = venue["ownSite"]
        if site["site"] == "wix-events":
            return site["site"], [site["page"], site["postcode"]]
        if site["site"] == "rex-schifferstadt":
            return site["site"], [site["page"]]
        if site["site"] == "arsenal-berlin":
            return site["site"], []
        raise ValueError(f"{venue['key']}: no client reads a {site['site']!r} site")
    raise ValueError(source)


def handle(source, venue):
    """The venue's handle in `source`, as feed-mirrors.json records it."""
    h = venue.get(source)
    return h.get("id") if isinstance(h, dict) else h


def entries(doc, copies=(), retired=()):
    """(venue key, its feeds) for each venue served other than by default. `copies` are
    feed-mirrors.json's rows: a feed serving another venue's programme is left out."""
    copied = {(c["venue"], c["source"], c["handle"]) for c in copies}
    out = []
    for v in doc["venues"]:
        if not v.get("rostered") or not v.get("primary") or v["key"] in retired:
            continue
        order = [s for s in [v["primary"]] + v["fallbacks"] if (v["key"], s, handle(s, v)) not in copied]
        if not order:
            raise ValueError(f"{v['key']} {v['name']}: its every feed is another venue's programme "
                             "(feed-mirrors.json) -- retire it as a duplicate (retired.json)")
        default = ["filmstarts"] + (["kinoprogramm"] if v.get("kinoprogramm") else [])
        if order != default:
            out.append((v["key"], [feed(s, v) for s in order]))
    return sorted(out)


def render(doc, copies=(), retired=()):
    """The table's rows: venue key, the feed's place in its order, its kind, its args."""
    return [[key, str(order), kind, list_cell(args)]
            for key, feeds in entries(doc, copies, retired)
            for order, (kind, args) in enumerate(feeds)]


def main():
    doc = json.load(open(SRC))
    copies = json.load(open(MIRRORS))["copies"] if os.path.exists(MIRRORS) else []
    retired = set(json.load(open(RETIRED))) if os.path.exists(RETIRED) else set()
    write_table(OUT, render(doc, copies, retired))
    print(f"wrote {OUT}: {len(entries(doc, copies, retired))} venues")


if __name__ == "__main__":
    main()
