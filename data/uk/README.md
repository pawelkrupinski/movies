# UK venues and pages

The UK's ~840 venues are hand-written `case object`s in
`common/src/main/scala/models/Cinema.scala` — a display name and a pill name,
nothing else. Everything a page needs beside that lives here:

```
venues.json   each venue's postal town (off its Flicks address) and coordinates
towns.json    the town each venue is in: GeoNames centre, population, county, island
pages.json    which page lists each venue, and where every retired slug now goes
```

and is generated into `models.UkPages` (the pages) and `models.VenueTowns` (the
town each venue's address names).

## Re-harvesting

```
python3 data/uk/scripts/harvest_towns.py --sync   # only the venues venues.json lacks
python3 data/uk/scripts/harvest_towns_test.py
python3 data/scripts/generate_venue_towns.py      # -> VenueTowns.scala
curl -sO https://download.geonames.org/export/dump/cities1000.zip && unzip cities1000.zip
python3 data/uk/scripts/geocode_towns.py cities1000.txt   # -> towns.json
python3 data/uk/scripts/build_pages.py                    # -> pages.json
python3 data/uk/scripts/generate_roster.py                # -> UkPages.scala + the apps' lists
python3 data/uk/scripts/geocode_towns_test.py
python3 data/uk/scripts/build_pages_test.py
```

`harvest_towns.py` needs no venue list of its own: every UK venue's Flicks slug
is already in the repo, in the two places a venue can be wired —
`CinemaScraperCatalog`'s `flicks("<slug>", Obj)` for the venues Flicks scrapes,
and `ChainFlicksFallback`'s `Obj -> "<slug>"` for the chain venues that only
fall back to it. A venue Flicks does not list at all (The Old Court, Windsor,
scraped off its own site) is entered by hand with `"flicksSlug": null`, and every
sweep keeps it. `--reparse` re-derives every town from the stored addresses with
no network — what to run after changing the parser.

Flicks throttles by STALLING rather than by returning 429, and plateaus at
3-5 req/s however many workers you point at it, so the sweep runs 3 workers
paced at ~2 req/s and takes a few minutes. Do not raise it.

## Why the town parser is trusted

Flicks gives a free-text postal address, not a town field. `town_of` takes the
town off whichever part carries the POSTCODE, which is the only marker that
survives all three shapes a UK address ends in — including
`…, Speke Road, L24 8QB, Speke, Merseyside`, where the last part is a county
and the obvious "take the last part" rule answers "Merseyside". A county or an
island after the town is walked past (`COUNTIES`: "Paradise Place, Goole, East
Riding of Yorkshire" is Goole).

That rule is scored, not assumed: `harvest_towns_test.py` runs it against the
87 UK venues in the recorded Cineworld fixture, which carry the chain's OWN
`addressInfo.city` alongside the address. It has to agree on every one.

## From a postal town to a town

A postal town is a sorting office's, not always the town a venue is in.
`geocode_towns.py` matches each to a GeoNames place (`cities1000.txt`, GB plus
the Isle of Man, Jersey and Guernsey), and is reviewed rather than permissive —
a name it cannot place stops the run:

- **Spelling** (`ALIASES`): Flicks' "Hull" is GeoNames' Kingston upon Hull,
  "Leamington Spa" Royal Leamington Spa. A page shows the name a visitor uses
  (`DISPLAY_NAMES`: Hull, Londonderry, Bishop's Stortford).
- **Greater London is London.** Its boroughs' postal towns — Croydon, Romford,
  Kingston, Harrow — are London, as New York's boroughs are New York City.
- **A venue its address misnames** (`VENUE_TOWNS`, one reviewed line each): a
  neighbourhood Flicks files it under (Hampstead and Maida Vale are London), a
  postal town 30 km off (Hay-on-Wye's bookshop is "Hereford"), an island's name
  for its town (Jersey is St Helier).
- **A postal town past its council's edge.** A venue more than 8 km from the
  town its address names, beside (3 km) a place in another council area, is in
  that area's biggest town nearby: Cumbernauld's Lanternhouse ("Glasgow G67") is
  Cumbernauld, Pocklington's arts centre ("York YO42") Pocklington, the NEC
  ("Birmingham") Solihull. `MISPLACED` exempts a venue harvested at the wrong
  coordinates (Odeon Rochdale, at Bolton's).
- **Towns GeoNames lacks** (`OWN_TOWNS`: Beeston, Cromarty, Crosby, Wellington
  in Somerset) are centred on their venues, with no population.
- **A town is its name AND its county.** Newport (Isle of Wight) and Newport
  (Gwent), 102 km apart, are two towns; so are the two Bangors and the two
  Wellingtons.

Each town carries its NATION and its picker COUNTY (England's ceremonial
counties, Wales' preserved counties, Scotland's councils grouped the way the
Flicks regions grouped them, Northern Ireland's councils) and its LANDMASS — the
island it is on.

## Which page a venue is on

Until 2026-10 a UK page was one of 76 Flicks REGIONS, filed by hand — mostly a
county, so `/west-yorkshire/` listed Leeds, Bradford and Ilkley, and the
Highlands and Islands reached 285 km from Inverness to Lerwick. Pages are now
the rule Poland, Spain, Germany and the US share (`data/scripts/town_pages.py`),
with Poland's and Spain's radii, since British towns are close together:

1. A town of 100,000+ (GeoNames' figure) is a MAJOR page with only its own
   venues.
2. Any other town with 3+ venues is a page of its own.
3. 1-2 venue towns cluster: a 2-venue town takes those within 10 km, 1-venue
   towns cluster within 25 km, a town left alone joins a cluster within 35 km.
   A cluster is named after its biggest town ("Epsom area").
4. **No venue is more than 40 km from the town its page is named after**
   (`Rules.max_reach_km`). The radii are centre to centre, and a venue can sit
   well out from its town's centre; the three that would have been further
   (Bala, Malton, Woodbridge) were clustered again or stand alone.
5. **No cluster crosses water.** The Isle of Wight, Bute, Orkney, Shetland, the
   Western Isles, the Crown Dependencies and Northern Ireland are each their own
   landmass: Ryde is 11 km from Portsmouth across the Solent.

264 pages (88 major, 75 town, 101 cluster), 60 with one cinema; a page's furthest
venue is a median 4 km from its namesake, p95 26 km, at most 35 km.
`build_pages_test.py` and `UkPagesSpec` hold the 40 km cap over the generated
pages.

**One conurbation, many majors.** Postal towns cut a conurbation into its
boroughs' towns, and they stay separate pages: Manchester, Salford, Stockport,
Oldham, Bolton and Wigan; Birmingham, Wolverhampton, Dudley, Walsall, West
Bromwich, Solihull and Sutton Coldfield. That is how Poland keeps Katowice,
Sosnowiec and Gliwice and Germany Essen and Bochum: each is a town people name,
its venues are its own, and a page of all of Greater Manchester is the county
page this replaced. Greater London is the one exception, because it is one city
(above). The old rule's merges (Dudley and Sandwell into Birmingham,
Lanarkshire into Glasgow) are gone with it: `/dudley/` is Dudley's page again.

## Slugs

A slug names one town within the UK: every country owns its own slugs, so
`/uk/york/` is York and `/us/york/` York, Pennsylvania. Where another UK page has
the name, a page is qualified with its county:

- a slug a UK page had before keeps meaning its town (`town_pages.reserve`);
- of two UK towns of one name the more populous takes the bare slug (`/bangor/`
  is County Down's, Gwynedd's `/bangor-gwynedd/`).

A town a page covers that is not a page itself (`/uk/ilkley/`), and a page's
county-qualified slug (`/uk/york-north-yorkshire/`), 301 to the page.

Every one of the 79 old slugs (the 76 regions and the three merged ones) still
answers: as a page where its town is one (`/manchester/`, `/dudley/`), else as a
301 (`Country.renamedSlugs`) to the page holding its namesake town (`/antrim/` → the
Lisburn area, Antrim's cluster), else to the page holding most of its venues
(`/west-yorkshire/` → Leeds).
