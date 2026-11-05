# Recovering real films the resolver leaves unmatched

Started 2026-11-01 (data at about 20:00 UTC), following
[2026-11-01-filmweb-backup-identity.md](2026-11-01-filmweb-backup-identity.md), which found that most unmatched real
films are TMDB films the resolver refused or missed. This notebook holds the whole effort: the census, the causes, and
one section per fix, kept up to date as each one lands. Each fix's commits name it in an `Identity-Design:` trailer.

## 1. Census

**Source.** The local prod mirror (`kinowo*_prod_mirror` on :28017, fresh to the second), read-only. A listing is a
`listingKey` of a screening on or after 2026-11-01 whose film has no `tmdbId`. Its `identity_traces` row gives the
blocker, the searches run and the candidates with their scores.

**Events first.** A broad title pattern (concerts, relays, talks, workshops, marathons, secret screenings, shorts
blocks, festival passes, every Kino PCKul title) set aside the obvious events. That pattern is wider than
`NonFilmEvents` (which is tuned for precision). Every other cluster (one film id) was judged by hand against TMDB's
API, with the trace's candidates beside it: event, a film on TMDB (with its id), a film not on TMDB, or unclear. 1,980
clusters were judged in 29 batches. An "unclear" verdict was used wherever namesakes could not be told apart.

| Country | Unmatched listings | Events (pattern) | Events (judged) | Refused (films) | Title miss (films) | Not on TMDB | Unclear |
|---|---:|---:|---:|---:|---:|---:|---:|
| PL | 931 | 549 | 279 | 26 (24) | 23 (22) | 9 | 45 |
| DE | 2,157 | 631 | 1,098 | 84 (72) | 19 (17) | 66 | 259 |
| UK | 1,920 | 1,241 | 551 | 50 (46) | 23 (14) | 5 | 50 |
| US | 476 | 441 | 23 | 4 (4) | 5 (1) | 2 | 1 |
| ES | 51 | 12 | 38 | 0 | 0 | 1 | 0 |

"Refused" means the right TMDB film was among the candidates and a rule or cut turned it down. A "title miss" means
no search found it. In all, **about 230 listings (4% of the unmatched) are recoverable TMDB films**; the rest are events
(about 85%), films TMDB lacks, or namesakes nothing tells apart. The pattern's event set was spot-checked afterwards:
it had swallowed the UK "<film> + Q&A" listings, which section 3 counts separately.

## 2. Causes, by listings

| Cause | PL | DE | UK | US | All | Examples |
|---|---:|---:|---:|---:|---:|---|
| Refused below the rating cut, the facts weighing against (runtime gap, one venue, low popularity) | 13 | 43 | 23 | 3 | 82 | DE "Asterix in Nubien", UK "Here Lived" (venue 135′ against an 81′ film) |
| Banner or strand text left in the search (each its own banner) | 17 | 10 | 13 | 5 | 45 | PL "Alim \| UFF", "Wielki żar \| Pięć Smaków", US "TOUCH OF EVIL: THE RESTORED CUT" |
| **A companion event joined to the film ("+ Q&A", "+ panel", "+ Interval")** | 0 | 3 | 30+ | 0 | 33+ | UK "The Assassin + Q&A", "The Odyssey + Interval" |
| Refused by a learned cannot-link veto | 3 | 8 | 6 | 1 | 18 | DE "Kult-Special: Blade Runner - The Final Cut" |
| Found only by IMDb (the venue spells the title otherwise) | 1 | 13 | 3 | 0 | 17 | DE "Mein Sommer mit Nonna", UK "The Hunger Games Sunrise of the Reaping" |
| **Spelling: "&" for "and", "SS" for "ß"** | 0 | 7 | 2 | 0 | 9 | DE "Sinn & Sinnlichkeit" ×5, "DER RHEIN FLIESST INS MITTELMEER" |
| A namesake runner-up as close | 4 | 4 | 0 | 0 | 8 | DE "ZUSAMMEN" |
| Venue typo | 1 | 3 | 4 | 0 | 8 | UK "Jak Zyc Zeby Nie Zwarjowac" |
| Local title TMDB lacks | 3 | 1 | 0 | 0 | 4 | PL "Kusama. Nieskończoność" |
| Festival catalogue-namesake veto | 0 | 3 | 0 | 0 | 3 | DE "Jüdische Filmwoche: Das siebte Kreuz" |
| Instalment numbered in words ("Część 3" against "Część trzecia") | 1 | 0 | 0 | 0 | 1 | PL "DIUNA: CZĘŚĆ 3 - 2D DUB" |
| Other | 7 | 6 | 1 | 0 | 14 | |

Only the two bold rows are single mechanisms. The rating-cut refusals are the calibration doing its job: in every
sampled case a published fact (a runtime gap, one venue billing it, no year) weighs against the film, and the owner's
rule keeps those unmatched. The banner row is 40 banners, each a title-rule row of its own.

## 3. Fix A: a companion event is no film of its own

**Cause.** A billing joining an event to its film ("The Assassin + Q&A") was split on the "+", and the event piece
was searched and read as a shape like any other. "Q&A" found Q & A (1990), which then:

- stood as the runner-up the film's own facts did not outweigh ("its own facts do not favour it over the runner-up":
  UK "The Assassin + Q&A", "My Daughter Is a Zombie + Q&A", "Oxide Ghosts: The Brass Eye Tapes + Q&A");
- or ranked first, at 46.1%, for documentaries ("Inside Gaza + Q&A", "Nina Roza + Q&A");
- and "Panel", "Interval", "reception", "Gäste" and "Filmgespräch" were missing from the side-event words, so "The
  Odyssey + Interval" and "Was haben wir gelacht + Gäste" were read as bills of two works.

**Change** (as landed; the first two attempts are below).

- `IdentityMeasures.namesOnlyAnEvent`: a piece a billing JOINS with a "+" that names nothing but an event (an event or
  side-event word, every other word one billed around a film): "Q&A", "Panel", "Interval", "Live Q&A".
- `IdentityMeasures.eventNamed`: a film the listing names ONLY by such a piece (Q & A, 1990, of "The Assassin + Q&A")
  stays in the pool as a rival but is untaken (`Scored.EventNamed`): no rule takes it, and the favoured rule skips it
  as the runner-up the film's own facts must outweigh. The listing's own titles are always shapes, so a film called
  "Q&A" is still billed by its title.
- `title-words.tsv` side-event rows: panel, interval, reception, ScreenTalk, Gäste, Filmgespräch.

Attempts that did not land:

1. Dropping the event piece from the shapes and searches, with a newer-namesake guard (measure 41348646489): lost Q & A
   (1990) as the namesake rival that kept a bare billing from a pooled pick on the ranking. "Amazing Grace + Q&A" took
   Apted's film, "Premature + Writer/Director Q&A" Dan Beers's 2014 comedy, both wrong.
2. Keeping the drop but refusing any pooled pick among namesakes for a bare billing joined to an event (measure
   41355106349): lost "Dracula + Q&A" (Fisher 1958), "The Birthday Party"'s model take and Amazing Grace's fill.

The pre-commit check of the final shape (41382479306) flagged "The Phantom of the Opera + live score" at The Garden
Cinema as a wrong take (2004 musical; the live score accompanies the 1925 silent). That take is main's too. The unified
rows (recording 38713274617) hold it, and on the pinned recording this change leaves it unmatched (a cure in measure
41360747818). It is judged known-wrong in scope `rows` until the rows are re-emitted. The same check found one more
recovery in the capture: "London Breeze: Silent Grace + Panel: …" → Silent Grace (Maeve Murphy, 2001), judged right.

**Slice (prod-latest recording, prod mirror).** 16 listings recovered: UK It's Nice Up North ×4, Premature, The Fall
of Otrar, Chess of the Wind, Inside Gaza, Who Are You Polly Maggoo, My Daughter Is a Zombie, The Odyssey ×2, Nina Roza;
DE Nur eine Frau, Was haben wir gelacht ×2. The remaining 68 companion listings stay unmatched: double bills, shorts
blocks, TV episodes, and documentaries TMDB lacks.

**Measure** (108e31dcc, run 41404505515, all five countries, pinned recording; base be5a6b181 with flip-unification
stage 2). LANDABLE:

- gained right: UK ×6 model / ×5 final (Flowers of Shanghai, Chess of the Wind, My Daughter Is a Zombie, Journey
  There, Silent Grace, Who Are You Polly Maggoo …), DE ×1 (Becoming Kim);
- 1 cure (UK "The Phantom of the Opera + live score" no longer takes the 2004 musical);
- 0 right lost, 0 right→wrong, 0 new wrong.

Drift rows (run 41404500946, both seeds, all five countries): ten clean. An earlier base's drift (41388705989) was
also clean.

## 4. Fix B: a billing's "&" and a record's "ß"

**Cause.** A title relation compares keys of letters and digits. "Sinn & Sinnlichkeit" keys to "sinnsinnlichkeit" and
TMDB's "Sinn und Sinnlichkeit" to "sinnundsinnlichkeit": only an `overlap`, which the learned cannot-link cut refused.
An upper-case billing writes "FLIESST" for "fließt", and `deburr` leaves "ß" alone.

**Change.** `TitleRelationMeasures.titleRelation` names a record exactly when:

- the LISTING writes an "&" and its key, with "&" and each language's "and" (and, und, y, i, et) folded alike, is the
  record's; or
- the RECORD writes an "ß" and the keys agree once it reads "ss".

The first measure (41347266388) folded conjunctions on the "ß" path too, and was NOT LANDABLE: Hufnagel's
"Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes & Amir" has an "ß", so "… Agnes und Amir" ×56 named it
exactly and was kept apart from the bare "Lieblingsmenschen" ×30, which then lost the film their feed link gave them.
The "ß" key now folds nothing else, and a billing spelling out a record's "&" ("Renoir and Love") is read as before.

**Slice.** DE "Sinn & Sinnlichkeit" ×5 (Georgia Oakley's 2026 film), "DER RHEIN FLIESST INS MITTELMEER", UK "Music &
Lyrics"; the first measure added UK "Anna & The Apocalypse".

**Measure** (e1111902242, run 41350255694, all five countries, pinned recording 573). LANDABLE:

- 8 gained right: DE ×6 (Sinn & Sinnlichkeit ×5, Der Rhein fließt ins Mittelmeer), UK ×2 (Music & Lyrics, Anna &
  The Apocalypse);
- 0 lost, 0 right→wrong, 0 new wrong.

Drift rows (run 41350289811, seeds 41 and 42, all five countries): nine clean. UK seed 42 shows one flip, "Scottish
Ballet: These Violent Delights" at Keswick Alhambra. Fix A's unrelated branch shows the same flip (run 41348680527),
so it is main's, not this change's (control run 41355445466 on main).

Cross-recording (recording 528, run 41355500669): the same 8 gains in DE and UK, nothing lost.

## 5. Fix C: a banner or strand around the film's title

**Cause.** About 40 banners, each its own, left text around the film's title that the searches carried: an edition
after a stop ("TOUCH OF EVIL: THE RESTORED CUT" searched only "THE RESTORED CUT"), a certificate note the billing runs
on past ("Wicker (15) 105 mins 05.01.27"), a strand in capitals ("Lalka.  DZIEŃ KINA POLSKIEGO"), a quoted film in an
announcement, and local banners with no stop ("Kino 60+", "On Screen", "Literaturverfilmung nach …", "zum 80. Geb.",
"Menü mit dem Film").

**Change.** `TitleShapeMeasures.shapesOf` gains four shapes: the title before an edition tail (`editionHead`), before
an inner certificate note (`beforeCertificate`), the halves at a case change (`caseSplit`: only where the half after
the change opens as a title, never inside an open bracket, never a half in capitals holding stops of its own, never a
half only of words billed around a film, no Roman numeral as a word in capitals), and the one title quoted in an
announcement of ten words or more (`quotedWithin`). Five `title-rules.tsv` rows: xtra-kino-60-prefix,
xtra-on-screen-bare-prefix, xtra-literaturverfilmung-suffix, xtra-geburtstag-suffix, xtra-mit-dem-film-prefix.

**Split.** The first form of this fix also dropped "screen" from a relay's work words (Lowther's "On Screen: The Royal
Ballet SWAN LAKE" took the 2024 relay). That opened four new DE seed-8 flips (Geister x4, drift run 41898476540); the
bisect put them on that one line (shapes only, run 41956587308, clean; with it, run 41958722825, red). It is parked
(`docs/identity-experiments/2026-11-05-stage-relays-screen.md`), so Lowther's Swan Lake 2024 take stands as a known-wrong
row until held takes land.

**Measure.** _pending_
