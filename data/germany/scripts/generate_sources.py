#!/usr/bin/env python3
"""Generate GermanSourcesData.scala from venue-sources.json.

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

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(DATA))
SRC = os.path.join(DATA, "venue-sources.json")
MIRRORS = os.path.join(DATA, "feed-mirrors.json")
RETIRED = os.path.join(DATA, "retired.json")
OUT = os.path.join(ROOT, "scrapers/src/main/scala/services/cinemas/de/GermanSourcesData.scala")
CHUNK = 250   # entries per method: one literal of them all would outgrow a JVM method


def lit(s):
    return json.dumps(s, ensure_ascii=False)


def feed(source, venue):
    if source == "filmstarts":
        return f"f({lit(venue['filmstarts'])})"
    if source == "cineamo":
        return f"c({lit(venue['cineamo'])})"
    if source == "kinoheld":
        return f"k({lit(venue['kinoheld']['id'])}, {lit(venue['kinoheld']['path'])})"
    if source == "kinoprogramm":
        return f"p({lit(venue['kinoprogramm'])})"
    if source == "ownSite":
        site = venue["ownSite"]
        if site["site"] == "wix-events":
            return f"GermanFeed.WixEvents({lit(site['page'])}, {lit(site['postcode'])})"
        if site["site"] == "rex-schifferstadt":
            return f"GermanFeed.RexSchifferstadt({lit(site['page'])})"
        if site["site"] == "arsenal-berlin":
            return "GermanFeed.ArsenalBerlin"
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
    rows = entries(doc, copies, retired)
    chunks = [rows[i:i + CHUNK] for i in range(0, len(rows), CHUNK)]
    lines = [
        "// GENERATED from data/germany/venue-sources.json by data/germany/scripts/generate_sources.py",
        f"// -- do NOT edit by hand. Measured {doc['measuredOn']}: {len(rows)} venues served other than",
        "// Filmstarts-then-kinoprogramm.com. Regenerate after re-running harvest_sources.py; see data/germany/README.md.",
        "package services.cinemas.de",
        "",
        "import services.cinemas.common.{CineamoClient, KinoheldVenue}",
        "",
        "private[de] object GermanSourcesData {",
        "  private def f(theaterId: String): GermanFeed = GermanFeed.Filmstarts(theaterId)",
        "  private def c(id: String): GermanFeed = GermanFeed.Cineamo(CineamoClient.CinemaId(id))",
        "  private def k(id: String, path: String): GermanFeed = GermanFeed.Kinoheld(KinoheldVenue(id, path))",
        "  private def p(path: String): GermanFeed = GermanFeed.Kinoprogramm(path)",
        "",
    ]
    for i, chunk in enumerate(chunks):
        lines.append(f"  private def part{i}: Seq[(String, Seq[GermanFeed])] = Seq(")
        lines += [f"    {lit(tid)} -> Seq({', '.join(feeds)})," for tid, feeds in chunk]
        lines.append("  )")
        lines.append("")
    parts = " ++ ".join(f"part{i}" for i in range(len(chunks))) or "Seq.empty"
    lines.append(f"  /** Each venue's feeds, primary first, by its venue key. */")
    lines.append(f"  val byVenueKey: Map[String, Seq[GermanFeed]] = ({parts}).toMap")
    lines.append("}")
    return "\n".join(lines) + "\n"


def main():
    doc = json.load(open(SRC))
    copies = json.load(open(MIRRORS))["copies"] if os.path.exists(MIRRORS) else []
    retired = set(json.load(open(RETIRED))) if os.path.exists(RETIRED) else set()
    with open(OUT, "w") as f:
        f.write(render(doc, copies, retired))
    print(f"wrote {OUT}: {len(entries(doc, copies, retired))} venues")


if __name__ == "__main__":
    main()
