#!/usr/bin/env python3
"""Regenerate the AMC venue map from AMC's OWN theatre roster.

Emits two files, both checked in:
  docs/venue-maps/AMC-VENUE-MAP.tsv                       (the table + provenance)
  worker/src/main/scala/services/cinemas/us/AmcVenueMap.scala   (what the catalogue reads)

WHY A JOIN AND NOT A RULE. Our US roster (data/us/venues.json, harvested from
flicks.us) and AMC's own roster disagree on venue NAMES in ~30% of cases: flicks
appends the city ("AMC Phipps Plaza 14 Atlanta" vs AMC's "AMC Phipps Plaza 14"),
disagrees on the CLASSIC/DINE-IN brand prefix, and differs on spelling
("Pavillion"/"Pavilion"). Slug munging those into agreement is guesswork, so the
join is done on COORDINATES — which both sides carry and neither editorialises —
with the name used only to break ties between two genuinely distinct AMC venues in
the same town (Schererville 12 vs Schererville 16 sit 0.8 km apart).

A venue with no row keeps its flicks.us scraper, which is why the unmatched tail is
harmless: those 13 are sites AMC's roster no longer carries (closed or sold) and
the aggregator is the only source that still lists them.

Requires ZYTE_API_KEY in .env.local: every amctheatres.com host Cloudflare-blocks
plain datacenter/ISP clients, so the roster is fetched through the Zyte seam.
"""
import base64
import collections
import json
import math
import os
import re
import subprocess
import sys
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
GRAPH = "https://graph.amctheatres.com/"

# A row is kept only when the join is one of these, in order. Anything else is
# reported as unmatched rather than guessed.
NAME_TIE_BREAK_KM = 25.0   # a name match this close is the same venue
NEAR_KM           = 1.0    # ...or an unnamed match this close with an agreeing ZIP
VERY_NEAR_KM      = 0.3    # ...or this close regardless of ZIP


def zyte_key():
    """The Zyte key, from the environment or from a .env.local we can find.

    Checked out as a git WORKTREE the repo has no .env.local of its own — it lives
    in the main checkout — so the main worktree's copy is tried too rather than
    making the script only runnable from one directory.
    """
    if os.environ.get("ZYTE_API_KEY"):
        return os.environ["ZYTE_API_KEY"]
    candidates = [os.path.join(ROOT, ".env.local")]
    try:
        main_checkout = subprocess.run(
            ["git", "-C", ROOT, "worktree", "list", "--porcelain"],
            capture_output=True, text=True).stdout.split("\n")[0].removeprefix("worktree ")
        candidates.append(os.path.join(main_checkout, ".env.local"))
    except Exception:
        pass
    for path in candidates:
        if not os.path.exists(path):
            continue
        for line in open(path):
            if line.startswith("ZYTE_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    sys.exit(f"ZYTE_API_KEY not in the environment nor in any of: {candidates}")


def graphql(query, key):
    req = {"url": GRAPH, "geolocation": "US", "httpResponseBody": True,
           "httpRequestMethod": "POST", "httpRequestText": json.dumps({"query": query}),
           "customHttpRequestHeaders": [{"name": "Content-Type", "value": "application/json"}]}
    proc = subprocess.run(
        ["curl", "-sS", "--max-time", "180", "-u", f"{key}:",
         "-H", "Content-Type: application/json", "--data-binary", "@-",
         "https://api.zyte.com/v1/extract"],
        input=json.dumps(req).encode(), capture_output=True)
    body = json.loads(proc.stdout)
    if "httpResponseBody" not in body:
        sys.exit(f"Zyte returned no body: {json.dumps(body)[:400]}")
    return json.loads(base64.b64decode(body["httpResponseBody"]))


def amc_roster(key):
    """Every theatre AMC serves, off its own paginated `viewer.theatres` connection."""
    nodes, after = [], None
    while True:
        cursor = f', after: "{after}"' if after else ""
        page = graphql(
            "query { viewer { theatres(first: 200%s) { count pageInfo { hasNextPage endCursor } "
            "edges { node { theatreId slug name city state stateCode latitude longitude "
            "postalCode marketSlug } } } } }" % cursor, key)
        connection = page["data"]["viewer"]["theatres"]
        nodes += [edge["node"] for edge in connection["edges"]]
        print(f"  fetched {len(nodes)} of {connection['count']}")
        if not connection["pageInfo"]["hasNextPage"]:
            return nodes
        after = connection["pageInfo"]["endCursor"]
        time.sleep(0.6)          # pace the roster pull like every other AMC request


def km(lat_a, lon_a, lat_b, lon_b):
    radius, rad = 6371.0, math.pi / 180
    h = (math.sin((lat_b - lat_a) * rad / 2) ** 2
         + math.cos(lat_a * rad) * math.cos(lat_b * rad) * math.sin((lon_b - lon_a) * rad / 2) ** 2)
    return 2 * radius * math.asin(math.sqrt(h))


def zip5(value):
    found = re.search(r"\d{5}", value or "")
    return found.group(0) if found else None


def name_key(name):
    """A venue name with the bits the two rosters disagree about removed."""
    key = re.sub(r"[^a-z0-9]+", "", name.lower().replace("dine-in", "dinein").replace("dine in", "dinein"))
    for brand in ("amcclassic", "amcdinein", "amc"):
        if key.startswith(brand):
            return key[len(brand):]
    return key


def match(venue, roster):
    """The AMC theatre this venue IS, plus the rule that established it — or None."""
    lat, lon = float(venue["lat"]), float(venue["lon"])
    by_distance = sorted(((km(lat, lon, t["latitude"], t["longitude"]), t) for t in roster),
                         key=lambda pair: pair[0])
    # Name first: it is the only thing that separates two real AMC venues in one town.
    named = [(d, t) for d, t in by_distance
             if d <= NAME_TIE_BREAK_KM and name_key(t["name"]) == name_key(venue["title"])]
    if len(named) == 1:
        return named[0][1], "name+geo", named[0][0]
    distance, nearest = by_distance[0]
    ours, theirs = zip5(venue.get("postCode")), zip5(nearest.get("postalCode"))
    if distance <= NEAR_KM and (ours is None or theirs is None or ours == theirs):
        return nearest, "geo<=1km+zip", distance
    if distance <= VERY_NEAR_KM:
        return nearest, "geo<=300m", distance
    return None, None, distance


def main():
    print("fetching AMC's own theatre roster...")
    roster = amc_roster(zyte_key())
    venues = [v for v in json.load(open(os.path.join(ROOT, "data", "us", "venues.json")))
              if v["title"].startswith("AMC")]
    print(f"AMC roster: {len(roster)}   our AMC venues: {len(venues)}")

    matched, unmatched = {}, []
    for venue in venues:
        theatre, rule, distance = match(venue, roster)
        if theatre is None:
            unmatched.append(venue)
        else:
            matched[venue["slug"]] = dict(theatre=theatre, rule=rule, km=round(distance, 2),
                                          title=venue["title"])
    claimed = collections.Counter(m["theatre"]["slug"] for m in matched.values())
    duplicates = {slug: n for slug, n in claimed.items() if n > 1}
    if duplicates:
        sys.exit(f"two venues claim one AMC theatre, refusing to emit: {duplicates}")

    rows = sorted(matched.items())
    header = [
        "# AMC Theatres venue map — our flicks.us venue slug -> AMC's own market/theatre slugs.",
        "# GENERATED by data/us/scripts/generate_amc_map.py from AMC's own GraphQL roster",
        f"# (viewer.theatres, {len(roster)} theatres) joined to data/us/venues.json by coordinates.",
        "# A row exists only where the join was VERIFIED: the AMC theatre is our venue's",
        "# nearest, and either the normalised names agree or it is within 1km with an",
        f"# agreeing ZIP. {len(rows)} of our {len(venues)} AMC venues matched; the {len(unmatched)} that did not are listed",
        "# at the foot of this file and stay on the flicks.us aggregator.",
        "#",
        "# flicksSlug\tamcMarketSlug\tamcTheatreSlug\ttheatreId\tourTitle\tamcName\trule\tkm",
    ]
    lines = list(header)
    for slug, m in rows:
        t = m["theatre"]
        lines.append("\t".join([slug, t["marketSlug"], t["slug"], str(t["theatreId"]),
                                m["title"], t["name"], m["rule"], str(m["km"])]))
    lines.append("#")
    lines.append(f"# UNMATCHED ({len(unmatched)}) — not on AMC's current roster; left on flicks.us:")
    for v in unmatched:
        lines.append(f"#   {v['slug']}\t{v['title']}\t{v['city']}, {v['state']}")
    tsv_path = os.path.join(ROOT, "docs", "venue-maps", "AMC-VENUE-MAP.tsv")
    open(tsv_path, "w").write("\n".join(lines) + "\n")

    scala = [
        "package services.cinemas.us", "", "/**",
        " * Our US venues that AMC serves from its OWN origin, mapped to the two slugs its",
        " * public URL and GraphQL are addressed by: the market its theatre page sits under",
        " * and the theatre itself (`/movie-theatres/<market>/<theatre>`).", " *",
        " * GENERATED — see `docs/venue-maps/AMC-VENUE-MAP.tsv` for the same table with the",
        " * provenance columns, and `data/us/scripts/generate_amc_map.py` for the join. It is",
        " * built from AMC's OWN roster (`viewer.theatres`, %d theatres) matched to" % len(roster),
        " * `data/us/venues.json` by coordinates, and a row exists only where that join was",
        " * verified — %d of our %d AMC venues. The %d that did not match are absent" % (len(rows), len(venues), len(unmatched)),
        " * from AMC's current roster (closed or sold sites the aggregator still lists) and",
        " * stay on flicks.us, which is why this is a lookup rather than a rule: a venue with",
        " * no row simply keeps its aggregator scraper.", " *",
        " * Keyed by the venue's flicks.us slug rather than its `Cinema`, because that slug is",
        " * the stable identifier `UsRoster` already carries per venue",
        " * (`UsRoster.flicksSlugByCinema`) — keying by `Cinema` would mean materialising %d" % len(rows),
        " * roster lookups here just to invert them again in the catalogue.", " */",
        "object AmcVenueMap {",
        "  /** flicks.us venue slug -> (AMC market slug, AMC theatre slug). */",
        "  val byFlicksSlug: Map[String, (String, String)] = Map(",
    ]
    for slug, m in rows:
        scala.append(f'    "{slug}" -> ("{m["theatre"]["marketSlug"]}", "{m["theatre"]["slug"]}"),')
    scala += ["  )", "}", ""]
    open(os.path.join(ROOT, "worker", "src", "main", "scala", "services", "cinemas", "us",
                      "AmcVenueMap.scala"), "w").write("\n".join(scala))

    print(f"wrote {len(rows)} rows; {len(unmatched)} unmatched")
    for v in unmatched:
        print(f"  unmatched: {v['slug']}  ({v['city']}, {v['state']})")


if __name__ == "__main__":
    main()
