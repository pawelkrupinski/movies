# Polish venue towns

Poland's pages named no town but Trójmiasto's, on the assumption that a Polish
city page covers one town. It is not true of 36 of the 41: `/tarnow/` lists
cinemas in Biecz, Gorlice, Bochnia, Brzesko, Tuchów, Solec-Zdrój and Dąbrowa
Tarnowska, `/walbrzych/` reaches Kłodzko, Świdnica and Dzierżoniów, and
`/wloclawek/` reaches Płock, which is its own city elsewhere in the roster.

Unlike the UK — the other hand-written roster, whose towns had to be harvested
off Flicks — Poland needed no harvest, because the answer was already written
down. Whoever wired an out-of-town venue annotated it on the line:

```scala
case object KinoFarys extends Cinema("Farys", "Farys")   // Biecz — filmweb 2315
```

That is exactly the set worth having: the annotation exists BECAUSE the venue
is somewhere the city's name does not say. 139 of the 298 Polish venues carry
one, across 36 cities.

## Rebuilding

```
mkdir -p data/pl/geonames
curl -sL https://download.geonames.org/export/dump/PL.zip -o data/pl/geonames/PL.zip
unzip -o data/pl/geonames/PL.zip -d data/pl/geonames
python3 data/pl/scripts/build_venue_towns.py      # -> data/pl/venues.json
python3 data/pl/scripts/test_build_venue_towns.py
rm -rf data/pl/geonames                            # ~4MB dump, not checked in
python3 data/pl/scripts/build_venue_towns.py --audit   # needs no dump
python3 data/scripts/generate_venue_towns.py       # -> models.VenueTowns
```

## Is the annotation set complete?

Yes, checked rather than assumed. The table is built from annotations, so it
cannot tell you about a venue nobody annotated — an in-town venue and a
forgotten out-of-town one look identical in it.

Filmweb can tell you, because it files each cinema under the town it is actually
in. A venue that does not appear under its own city's listing is somewhere else,
and if it has no annotation either, nothing on the page will ever say where.

```
python3 data/pl/scripts/build_venue_towns.py --audit    # one GET per city, writes nothing
```

Run 2026-09-03: 39 cities audited, **no venue is out of town without an
annotation**. Two cities are skipped because Filmweb has no listing of their own
name — `trojmiasto` (Filmweb files those venues under Gdańsk and Gdynia, and the
city carries a complete hand-written list anyway) and `gorzow-wielkopolski`
(three venues, two named after the city and one annotated as it).

A guess from the venue NAMES instead finds nothing but noise: the 15 candidates
it produces are shopping centres (Plaza, Posnania), Warsaw and Kraków districts
that share a name with a town elsewhere (Bemowo, Mokotów, Kazimierz), and the
trailing word of a two-word city (Jelenia **Góra**). Worth knowing before
anybody tries that shortcut.

## Why the gazetteer

A comment is prose, and one of them names a venue rather than a town
("Ursynowskie Centrum Kultury — own site"). Serving that as a place is worse
than serving nothing, so every candidate is checked against the GeoNames Polish
populated-place list and dropped if it is not a real place.

It only VALIDATES. GeoNames files Polish towns under English exonyms — Warszawa
is "Warsaw" there — so the name kept is the one in the comment, which is already
correct Polish. That is the opposite of Spain, where the harvested names are the
damaged ones and GeoNames supplies the spelling.

Matching allows for the two ways an annotation shortens a name: the qualifier
half abbreviated (`Ostrów Wlkp.` for Ostrów Wielkopolski — which has to be
expanded rather than dropped, since Ostrów Mazowiecka is a different town), and
the qualifier left off entirely (`Połczyn` for Połczyn-Zdrój), which is accepted
only when exactly one town starts that way.
