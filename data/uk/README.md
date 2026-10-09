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
here, and generated into `models.VenueTowns` as a display-name → town table that
`City.extraPlaces` reads — shared with Poland, the other hand-written roster.

## Re-harvesting

```
python3 data/uk/scripts/harvest_towns.py     # ~840 pages off Flicks, a few minutes
python3 data/uk/scripts/test_harvest_towns.py
python3 data/scripts/generate_venue_towns.py  # -> common/src/main/scala/models/VenueTowns.scala
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

## Which page a venue is on

The pages stay the Flicks regions — mostly counties — and each venue is filed
under one by hand in `Cinema.scala`. Two straight-line (haversine) rules,
borrowed from Poland's clustering (`data/pl/scripts/build_pages.py`), hold that
filing to the venues' own fixes here; `UkPageGeographySpec` enforces both
against this file and the roster's hubs (each `UkCity`'s lat/lon):

- **A misfiled venue moves.** A venue more than 40 km from its page's hub that
  is within 25 km of ANOTHER page's hub belongs to that page. Ystradgynlais's
  Miners Welfare Hall, which Flicks files under Dyfed (47 km), is on the
  Glamorgan page (18 km). A remote venue with no hub near it — Lerwick, 285 km
  up the Highlands and Islands — stays where its region put it; so do Plymouth
  (Cornwall, 56 km; Devon's hub is 45 km), Goole (Lincolnshire; South
  Yorkshire 36 km), Wrexham (Powys; Liverpool 41 km) and King's Lynn (Norwich;
  Cambridgeshire 45 km), none of which is within 25 km of another hub.
- **One urban area is one page.** Two hubs under 15 km apart are halves of the
  same town, so the smaller is folded into the larger: Dudley (13 km) and
  Sandwell (9 km) into Birmingham, Lanarkshire (East Kilbride/Hamilton, 12 km)
  into Glasgow. `City.ukMergedPages` is the one place that says so — the old
  slugs 301 onto the absorbing page and stay searchable in the picker. Pairs
  between 15 and 25 km are separate places and are left alone: Belfast/Down
  (17.5), Belfast/Antrim (20.1), Glasgow/Renfrewshire (22.7), South
  Yorkshire/Yorkshire (16.0), Edinburgh/Fife (21.1), Hampshire/Isle of Wight
  (23.9).
