# UK venue towns

The UK roster is the odd one out. Germany's regions, Spain's provinces and the
US's metros are all GENERATED from a harvested `venues.json`, so each venue's
town rides along in the generated tuple. The UK's ~840 venues are hand-written
`case object`s in `common/src/main/scala/models/Cinema.scala` — a display name
and a pill name, nothing else — so there was nowhere for a town to live, and
its pages named none.

That matters more here than anywhere: most UK "cities" in the roster are
COUNTIES or travel-sheds. `/aberdeenshire/` covers Aberdeen, Peterhead,
Banchory, Huntly and Ellon. `/cheshire/` covers Chester, Warrington and Crewe.
Before this, none of those words appeared on the page — not in the title, the
description, the structured data, or (for most of them) a single cinema's name.

So the town is kept beside the roster instead of inside it, in `venues.json`
here, and generated into `models.UkVenueTowns` as a display-name → town table
that `UkCity.extraPlaces` reads.

## Re-harvesting

```
python3 data/uk/scripts/harvest_towns.py     # ~840 pages off Flicks, a few minutes
python3 data/uk/scripts/test_harvest_towns.py
python3 data/uk/scripts/generate_towns.py    # -> common/src/main/scala/models/UkVenueTowns.scala
```

`harvest_towns.py` needs no venue list of its own: every UK venue's Flicks slug
is already in the repo, in the two places a venue can be wired —
`CinemaScraperCatalog`'s `flicks("<slug>", Obj)` for the venues Flicks scrapes,
and `ChainFlicksFallback`'s `Obj -> "<slug>"` for the chain venues that only
fall back to it. It reads both, and reads the display names off `Cinema.scala`.

Flicks throttles by STALLING rather than by returning 429, and plateaus at
3-5 req/s however many workers you point at it, so the sweep runs 3 workers
paced at ~2 req/s and takes a few minutes. Do not raise it; extra concurrency
buys nothing and risks the origin dropping the sweep half-done.

## Why the town parser is trusted

Flicks gives a free-text postal address, not a town field. `town_of` takes the
town off whichever part carries the POSTCODE, which is the only marker that
survives all three shapes a UK address ends in — including
`…, Speke Road, L24 8QB, Speke, Merseyside`, where the last part is a county
and the obvious "take the last part" rule answers "Merseyside".

That rule is scored, not assumed: `test_harvest_towns.py` runs it against the
87 UK venues in the recorded Cineworld fixture, which carry the chain's OWN
`addressInfo.city` alongside the address. It has to agree on every one.
