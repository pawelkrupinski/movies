#!/usr/bin/env python3
"""Which German venues' feeds serve ANOTHER venue's programme, and which venue it is.

An aggregator's venue entry can carry another cinema's whole programme: kinoheld's
"Filmwelt Grünstadt" (695) lists Luxor Filmpalast Heidelberg's shows, every one booking
`ticket-cloud.de/Luxor-Heidelberg/Show/<id>`; the harvest matched kinoheld's "CinemaxX
Hannover - Raschplatz" (246) to Kino am Raschplatz and "Cineplex Berlin" (295) to the Adria.
Served, such a feed puts the other cinema's programme under the venue's name (a phantom:
one screening counted at two venues, the copy often under another film page), and hides
the venue's own programme, which its next feed has. harvest_sources.py cannot see it: it
counts a feed's shows, and the copy is the more complete one.

A booking URL is one screening's ticket page, so two venues' feeds serving one programme
are told by their showtimes' booking URLs: a pair of venues whose served feeds share at
least MIRROR_SHARE of each one's booking URLs (and MIN_SHARED of them) serve one
programme, and one of them is the copy. The OWNER is the venue the booking URLs name:
the shared URLs naming a word of its name or town (the ticket-cloud tenant
"Luxor-Heidelberg", the chain's host `cinemaxx.de`, `tickets.cineplex.de` for "Cineplex
Germering" and not "Cinema+Studio Lippstadt") more often than the other's. Where they
name both alike or neither, the pair was checked by hand and its verdict is in DECIDED.

The copy is written to feed-mirrors.json, and stays there: once dropped the copy is no
longer served, so a later run cannot see it again, and keeps the copies the file holds
(delete a row by hand to serve the feed again). generate_sources.py drops it from the venue's
chain, so the venue is served by the next feed, its own (Grünstadt by Cineamo 448, whose
shows book `kinotickets.express/filmwelt-gruenstadt`). A venue the copy was the only feed
of is the other venue listed twice, and belongs in retired.json as a duplicate.

Two feeds sharing less than that are a booking system shared by houses of one operator
(Citydome and Kinopolis Darmstadt, the Filmhaus-Huber houses): neither is a copy, and
the run prints them for the record.

Reads every venue's last served scrape (`cinema_scrapes`, the feed recorded in
`guard.sourceKey`) from a Mongo it is pointed at -- the local prod mirror, read-only:

Run: scripts/local-mirror/freshness de && \
     python3 data/germany/scripts/feed_mirrors.py [mongodb://127.0.0.1:28017/kinowo_de_prod_mirror]
"""

import datetime
import json
import os
import re
import subprocess
import sys
import unicodedata
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
OUT = os.path.join(DATA, "feed-mirrors.json")
DEFAULT_MONGO = "mongodb://127.0.0.1:28017/kinowo_de_prod_mirror"

# One programme: each feed's booking URLs mostly the other's. On the DE prod mirror of
# 2026-11-06 the pairs sharing any were either >= 92% of both feeds or <= 78%.
MIRROR_SHARE = 0.9
MIN_SHARED = 10

# Words of a venue name that tell no cinema from another.
GENERIC = {"kino", "kinos", "cinema", "cinemas", "theater", "theatre", "filmtheater", "lichtspiele",
           "lichtspielhaus", "filmpalast", "center", "centre", "studio", "film", "filme", "und", "der",
           "die", "das", "the", "and", "bei", "stadt", "neue", "neues"}

# Pairs serving one programme whose booking URLs name both venues alike, or neither,
# checked by hand: the two venue keys -> the copy's key, or None to keep both (and why).
DECIDED = {
    # kinoheld's "Kino in Dorfen" is sKino im Jakobmayer listed twice: all of its shows are
    # sKino's, booking kinotickets.express/dorfen (2026-11-06: 83 of 83); retired as a duplicate.
    frozenset({"kinoheld:2323", "A2671"}): "kinoheld:2323",
    # Cineamo lists the joint programme of Ludwigsburg's two houses, one cinster tenant
    # (central-union.cinster.online), under each (1304 and 712 answer the same 166 showings,
    # 2026-11-07); which house a show plays in is in neither feed.
    frozenset({"A0195", "A1238"}): None,
}


def deburr(s):
    return "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))


def words(s):
    """The words of a name or a URL: lower-cased, deburred and transliterated ("Grünstadt" is
    both "grunstadt" and "gruenstadt"), three letters or more, digits alone not words."""
    lower = s.lower()
    out = set()
    for form in (lower, lower.replace("ä", "ae").replace("ö", "oe").replace("ü", "ue").replace("ß", "ss")):
        out |= {w for w in re.split(r"[^a-z0-9]+", deburr(form)) if len(w) >= 3 and not w.isdigit()}
    return out


def name_words(venue):
    return words(venue["name"] + " " + venue["town"]) - GENERIC


def normalise(url):
    """A booking URL as one screening: scheme, case and a trailing slash say nothing (Cineamo
    gives `…/Show/2450683`, kinoheld `…/Show/2450683/`)."""
    url = re.sub(r"^https?://", "", url.strip()).lower()
    return re.sub(r"/+(\?|$)", r"\1", url)


def source_of(source_key):
    """`kinoheld.de/cinema/695` -> ("kinoheld", "695"); the feeds a roster names a venue by."""
    for prefix, source in (("kinoheld.de/cinema/", "kinoheld"), ("api.cineamo.com/cinema/", "cineamo"),
                           ("filmstarts.de/kinoprogramm/kino/", "filmstarts")):
        if source_key.startswith(prefix):
            return source, source_key[len(prefix):]
    return None, source_key


def mirrors(feeds, decided=DECIDED):
    """The pairs of `feeds` ({key: {name, town, source, handle, urls}}) serving one programme:
    (copies, kept, shared_systems). A copy is {venue, source, handle, of, shared, total,
    example, decidedBy}; `kept` the hand-kept pairs; `shared_systems` the pairs sharing
    fewer, for the record. Raises on a pair the URLs cannot decide and DECIDED does not."""
    by_url = defaultdict(set)
    for key, feed in feeds.items():
        for url in feed["urls"]:
            by_url[url].add(key)
    shared = defaultdict(set)
    for url, keys in by_url.items():
        ks = sorted(keys)
        for i, a in enumerate(ks):
            for b in ks[i + 1:]:
                shared[(a, b)].add(url)
    copies, kept, systems, undecided = [], [], [], []
    for (a, b), urls in sorted(shared.items()):
        fa, fb = feeds[a], feeds[b]
        if len(urls) < MIN_SHARED or min(len(urls) / len(fa["urls"]), len(urls) / len(fb["urls"])) < MIRROR_SHARE:
            systems.append({"venues": [a, b], "shared": len(urls), "totals": [len(fa["urls"]), len(fb["urls"])]})
            continue
        pair = frozenset({a, b})
        if pair in decided:
            copy, how = decided[pair], "hand-checked"
        else:
            named = {k: sum(1 for u in urls if words(u) & name_words(feeds[k])) for k in (a, b)}
            copy, how = (b if named[a] > named[b] else a if named[b] > named[a] else "?"), "booking urls name the other"
        if copy == "?":
            undecided.append((a, b, len(urls)))
        elif copy is None:
            kept.append({"venues": [a, b], "shared": len(urls), "totals": [len(fa["urls"]), len(fb["urls"])]})
        else:
            owner = b if copy == a else a
            c = feeds[copy]
            copies.append({"venue": copy, "name": c["name"], "source": c["source"], "handle": c["handle"],
                           "of": owner, "ofName": feeds[owner]["name"], "shared": len(urls), "total": len(c["urls"]),
                           "example": min(urls), "decidedBy": how})
    if undecided:
        raise ValueError("pairs serving one programme that need a verdict in DECIDED: " +
                         "; ".join(f"{feeds[a]['name']} ({a}) / {feeds[b]['name']} ({b}), {n} shared" for a, b, n in undecided))
    return copies, kept, systems


def merged(previous, measured):
    """The copies of an earlier run and this one's, by venue and feed, this run's measurement
    winning: a dropped copy is no longer served, so this run cannot see it again."""
    ident = lambda c: (c["venue"], c["source"], c["handle"])
    out = {ident(c): c for c in previous}
    out.update({ident(c): c for c in measured})
    return sorted(out.values(), key=ident)


EXPORT = r"""
db.cinema_scrapes.find({}, {films: 1, guard: 1}).forEach(d => {
  const urls = new Set();
  (d.films || []).forEach(f => (f.showtimes || []).forEach(s => {
    const u = s.bookingUrl || (s.bookingUrlRest !== undefined && f.bookingUrlPrefix !== undefined ? f.bookingUrlPrefix + s.bookingUrlRest : null);
    if (u) urls.add(u);
  }));
  print(JSON.stringify({venue: d._id, sourceKey: (d.guard && d.guard.sourceKey) || "", urls: [...urls]}));
});
"""


def read_feeds(mongo, data=DATA):
    """Every rostered venue's served feed and booking URLs, by venue key, off `mongo`'s cinema_scrapes."""
    venues = {v.get("displayName") or v["name"]: v for v in json.load(open(os.path.join(data, "venues.json")))}
    out = subprocess.run(["mongosh", "--quiet", mongo, "--eval", EXPORT], check=True, capture_output=True, text=True).stdout
    feeds = {}
    for line in out.splitlines():
        row = json.loads(line)
        venue = venues.get(row["venue"])
        if not venue or not row["urls"]:
            continue
        source, handle = source_of(row["sourceKey"])
        feeds[venue.get("theaterId") or venue["feed"]] = {
            "name": row["venue"], "town": venue.get("city", ""), "source": source, "handle": handle,
            "urls": {normalise(u) for u in row["urls"]}}
    return feeds


def main():
    mongo = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_MONGO
    if ":27017" in mongo:
        sys.exit("refusing :27017 -- point this at the local prod mirror (:28017), read-only")
    feeds = read_feeds(mongo)
    copies, kept, systems = mirrors(feeds)
    for c in copies:
        print(f"COPY  {c['name']} ({c['venue']}, {c['source']} {c['handle']}) serves {c['ofName']}'s programme: "
              f"{c['shared']} of {c['total']} booking URLs ({c['decidedBy']}), e.g. {c['example']}")
    for k in kept:
        print(f"KEPT  {' / '.join(feeds[v]['name'] for v in k['venues'])}: {k['shared']} shared, hand-checked")
    for s in systems:
        print(f"SHARED SYSTEM  {' / '.join(feeds[v]['name'] for v in s['venues'])}: {s['shared']} of {s['totals']}")
    previous = json.load(open(OUT))["copies"] if os.path.exists(OUT) else []
    copies = merged(previous, copies)
    with open(OUT, "w") as f:
        json.dump({"measuredOn": datetime.date.today().isoformat(), "mirrorShare": MIRROR_SHARE,
                   "copies": copies}, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"wrote {OUT}: {len(copies)} copies")


if __name__ == "__main__":
    main()
