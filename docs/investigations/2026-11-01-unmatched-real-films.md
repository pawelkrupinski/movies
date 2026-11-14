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

## 6. Fix D: IMDb's only suggestion under a venue's slip

**Cause.** 40 listings reach their film only through IMDb's suggestion (`Scored.suggestedOnly`), and only the IMDb
rule may take such a film. In all eleven traced titles the rule's rungs refused: no year or director is stated, and the
title only overlaps the film's (`titleNamesIt` false). 34 of the 40 are IMDb's ONLY suggestion with a slipped title:

- UK "The Hunger Games Sunrise of the Reaping" ×25 ("of" for "on");
- UK "Ebeneezer" ×4;
- DE "LA VIE DE CHÂTEAU, MON ENFACE À VERSEILLES" ×2;
- DE "Caspar und Emmas wunderbare Weihnacht";
- a translation (Kardeş Türküler) and an ICA festival billing.

**Change.** `AcceptanceTitleRules.slippedTitle` adds a fifth rung: the only film IMDb suggests, not outranked, whose own
or original title one of the listing's titles is word for word but for a slip. A slip is:

- at most two words, never half;
- of four words or more;
- letters only, no numeral;
- at most one edit per four letters.

A billing so slipped numbers what the film's title numbers, so the instalment guard is not read on that path. TMDB's
numbered alias of "Sunrise on the Reaping" otherwise refused it as another instalment.

Kept out on purpose:

- one-word titles ("Ebeneezer");
- translations;
- several suggestions;
- "A Boy and His Dog" against "… Her Dog";
- "Part II" against "Part III".

**Slice** (recording 41379008920): Hunger Games (Cinebowl Uttoxeter, pinned as a slice case), La Vie de château and
Casper und Emma now taken. All three are judged right.

**Measure.** LANDABLE, run 42051692411 (all five countries, base f805949e5): right→wrong 0, no right take lost, no new
wrong. DE model 2 gained right (Caspar und Emma, La Vie de château); UK model lets go of Lowther's 2023 Nutcracker relay
(a cure, judged known-wrong in the rows); PL, US, ES unmoved; final takes unmoved. Drift 42051731189 clean on all ten
rows. The what-if over prod's mirror also moved UK Cinebowl Uttoxeter's Hunger Games and Gala Durham's "The MET
Opera: La Fancuilla del West" (the Met's 2026/27 relay), both judged right.

## 7. Fix E: a feed link overruled by a rival that could not be taken

**Cause.** DE "Lieblingsmenschen" (95 traces, 68 venues) ended Vetoed with no film. Cineamo links the bare title to
Hufnagel's 2026 release (1185542m, year 2026, original title "Agnes & Amir" copied). The bare title names a 65-minute
2021 "Lieblingsmenschen" (788385) exactly and the release's long TMDB title not at all, so `FeedLink.TitledRival`
overruled the link: the node was read on its titles alone and 1185542 was denied. The 2021 film was refused too (a
learned cannot-link, the namesake veto, 5.8% against a 37.5% cut), so nothing was taken. Webedia's 213 listings at
~40 venues took the release. The rule's origin, Yorck's "Jin-Roh - Die Wolfsbrigade" linked to the 2018 remake
"Illang", was right to be overruled, and it links an older film.

**Change.** `FamilyScope.titledRival` no longer overrules a link to a current release in favour of an older rival when
the venue's title is one edge of the release's title (relation `fragment`). A current release is dated at most
`IdentityResolver.CurrentRelease` years before the node's first screening. Releases are billed under the head of
their title, and an old namesake's record may carry that head whole. The rule reads only the node's titles and
screenings and the two films, so arrival order cannot move it.

The first draft used the listing's stated year instead of the title. It turned the pinned slice case Bundesplatz-Studio
"Der Diplomat" wrong: Cineamo links that listing to the 2025 Indian "The Diplomat" and copies its year, but the venue
screens the 1995 Hessel documentary. A year the feed copied says nothing. The "rival could not be taken anyway"
variant was not built, because it needs the node's own scoring inside `overrule`, and that scoring reads `overrule`.

**Tests.** `IdentityResolverCasesSpec`, "A feed's TMDB link the venue's own title contradicts": the link stands for the
2026 release and is still overruled when it links a 2018 record. Slice case
`de-albrecht-kino-waldshut-lieblingsmenschen-0da39147` took none before and takes 1185542 after. The Jin-Roh, Der
Diplomat and CineStar Lieblingsmenschen cases still pass.

**What-if** (DE mirror, snapshot 2026-11-14T20:07Z, model and final takes):

- 69 takes moved, all gained-right: 68 "Lieblingsmenschen" and Cineworld Lünen's "Komplizinnen" [2027]. The latter now
  takes Heiss's 2027 "Komplizinnen - Vernunft ist keine Lösung", its link, rather than none against Czenki's 1987 film.
- right-lost 0, right→wrong 0, new-wrong 0.
- No other country's mirror holds a link this rule overrules.

Astor Grand Cinema's "Arabeske" is unmoved. Cineamo links it to Bensmaïl's 2026 "The Arab", a title it does not
carry, and it is a review candidate.

## 8. Re-running the census: `scripts/unmatched_census.py`

The census is a script now: `scripts/unmatched_census.py` over the local mirror (after `scripts/local-mirror/freshness
<cc>`), about 5 seconds for all five countries. It prints the unmatched listings by trace basis (step 1), the
clusters the event pattern sets aside (step 2), the hand verdicts' buckets over the rest (step 3), and a watch list of
this notebook's examples (step 4). Its data files are in `scripts/unmatched-census/`: `event-patterns.txt`, `watch.tsv`,
and `verdicts.tsv`, which holds every hand verdict. A verdict is found by film id, else by title, so a later run
only asks for the clusters nobody has judged. `--emit-sample N --sample-out F` draws a listing-weighted sample of
them to judge next. Step 3 is exact where every cluster is judged and extrapolated where only a sample is.

**Round of 2026-11-14** (mirror fresh to the second). The verdicts were judged from the traces' candidates, with no
TMDB lookup.

| Country | Unmatched listings (11-01) | Clusters | Events by pattern | Judged |
|---|---:|---:|---:|---|
| PL | 762 (931) | 376 | 245 | 55 of 307 clusters, a sample |
| DE | 1,946 (2,157) | 1,366 | 271 | 81 of 1,224, a sample |
| UK | 1,620 (1,920) | 661 | 680 | 55 of 470, a sample |
| US | 744 (476) | 40 | 346 | all 19 |
| ES | 93 (51) | 19 | 5 | all 18 |

- The total fell from 5,535 to 5,165 listings. US grew on two events: a Paris Opera relay (186 listings) and the
  League of Legends finals (101).
- Every pattern event sits under a miss basis. The `Event` basis is projection-only (`Decision.scala`), so no
  stored trace shows it, and this census cannot tell which events the projection already handles.
- Real films come to about 300 listings (~6%), against ~230 (4%) on 11-01. Most of that is extrapolated, and one
  68-listing cluster moves DE by about 21. The biggest groups are a learned cannot-link veto (~125, DE "Hard Boiled",
  "Tabaluga und Leo"), banner text (~110, US "Dune: Part One Reissue (2026)", UK "Our House | Preview Screening",
  PL "FOKUS: GRANICE - Głupcy") and a namesake runner-up (DE "Lieblingsmenschen", 68 listings).
- Double bills of two real films: about 90 listings (UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea").
- Of section 2's examples, "The Assassin + Q&A", "The Odyssey + Interval", the Hunger Games slip, "Sinn &
  Sinnlichkeit" and "Touch of Evil" are matched now. "Here Lived", "Kult-Special: Blade Runner - The Final Cut" and
  the bare "Mein Sommer mit Nonna" and "Wielki żar" are still unmatched. The rest are no longer billed.

## 8. Fix F: banners Fix C missed

**Cause.** The 2026-11-14 census's banner group (section 7) left four shapes around a real film's title.

**Change.** Three `title-rules.tsv` rows were extended, with an example each. One canonical row was added. Every pattern
is anchored to its own words.

- `xtra-pp-cycle-banners` gains `FOKUS: GRANICE - `, Kino CK Lublin's feature strand. The festival's WSPÓLNOTA,
  TRANSGRESJE and ANIMACJA entries are shorts blocks with programme names ("Rodzina", "Bariery"), so they keep their
  banner. A generic `FOKUS: <strand> - ` row would have searched "Rodzina" as a film.
- `xtra-pp-us-uk-strands` gains `BIFA Presents `, Magic Lantern Tywyn's BIFA-nominee season. It does not cover
  "<host> presents" in general, since many of those billings are events (KFC Presents, Red Rock A Capella presents
  Christmas).
- A "Reissue [(YYYY)]" tail now comes off at both tiers. The query tier extends `xtra-rerelease-paren-suffix`, and the
  key tier adds `xtra-canonical-reissue-suffix`, so the reissue shows and merges as its film. The year goes with the
  word because it is the reissue's own year.

Two shapes were skipped:
- "The Host: 20th Anniversary" already searches "The Host" at HEAD (`structural-anniversary-suffix` plus the colon
  re-peel). The title names two films and the billing gives no year. A year hint (screening year − N) would need
  identity code, because `IdentityMeasures.anniversaryYear` only relaxes a published year. So the listing stays
  ambiguous.
- "Kino KILN: Born in Flames" and "Our House | Preview Screening" already search the bare title at HEAD.

**Measure.** The `--footprint` measure found nothing to replay: 0 of ~870k stored readings read otherwise, because
the pinned recording bills none of these titles. `scripts/identity-what-if` against prod's mirror (2026-11-14T20:05Z,
pl/uk/us) was CLEAN:
- UK gained 5 model takes, all of Magic Lantern Tywyn's BIFA season. 2 of them are new final takes (Tender Loving
  Care 1370191, The Incomer 1554956); prod already served the other 3.
- US gained "Dune: Part Two Reissue (2026)" (693134), model and final.
- PL moved nothing. Prod's trace for "FOKUS: GRANICE - Głupcy" (`identity_traces`) shows that identity already
  searched "Głupcy". It gave its weight to 908327, but the take is vetoed:
  `its-title-names-it-only-by-a-programme-tag-billed-beside-many-titles` reads the identity title, which the row does
  not change. The first hit is also a namesake, 577874 "Głupcy" (2022), and the billing gives no year. So the strip
  only fixes the lookup query; gaining the take needs identity work on that veto.

All 6 moved takes are judged right. "Dune: Part One Reissue" still takes nothing, because TMDB titles that film "Dune"
(438631) and `IdentityMeasures.shapes` deliberately does not search a bare "Dune".

## 9. Fix G: a feed's untrusted facts

**Cause.** A real film falls below the learned listing-film cannot-link cut because ONE fact a feed published is not the
film's, and the measure reads it as if it were:

- DE "Close" (Kommunales Kino im Künstlerhaus, Schaubühne Lindenfels): Cineamo's premiere rides as the year UPPER BOUND
  (`IdentityMeasures.yearBound`), but it is the premiere of the record the operator linked (148857m, 2004). Lukas
  Dhont's 2022 film read 18 years past it (`year.delta` -6.85), 16.5%. Passage Kinos Leipzig, from its own source,
  dates it 2022 at 104 minutes.
- DE "Die Blutgräfin - The Blood Countess" (Moviemento): the original-title field "… (OmeU)" carries the showing's
  English-subtitled version; read with it the original was a `segment` (-2.52) of Ottinger's film, 8.2%.
- DE "Hard Boiled" (Kino im Schafstall): the listing's 2018 is a re-release year (Filmstarts' separate entry 135766).

**Change.**

- (b) `IdentityMeasures.Listing.measuredOriginal`: the `originalTitle` measures (listing-film, listing-listing) read
  the original without the tags that are a showing's badge (`FormatTags.extractFormatTags`' tokens: OmU, OmeU, OV, 3D,
  IMAX, …), as the title is read. A note that is no badge stays: Viktoria's "DAS BOOT (DIRECTOR'S CUT)(WA:2023)", its
  re-release mark read off, left a copy of the title and the cut's stub record (1794076, judged wrong) took it. A field
  that is then the title again is MISSING beside a film the title names, never a match: ZOO PALAST's "OmU: In
  Memoriam" (booking slug "In Memoriam – Gegen ihren Willen") otherwise took Brasó's 1977 "In memoriam". Beside a film
  the title does not name it still measures, as a plain repeat does: made missing there, Metropolis Bochum's
  "… Live on Stage (OmeU)" no longer denied "The SpongeBob Musical: Live on Stage!", whose undenied rest
  (`ConstraintEdges.namesBeside`) then cut the segment link that gives Cineplex Baden-Baden's bare "Andrew Lloyd
  Webber's Starlight Express" its film (pinned as a slice case). `repeatsItsTitle` (whether a listing states a fact)
  and the searches still read the original as published, so no new lookups.
- (a) `yearBound`: the bound is MISSING against a film another venue's own facts back (`venues.corroborating` > 0:
  its exact year or its director). The bound is a copy of the link, the corroboration the venues' own claim. King
  Kong's 1933 bound, which no venue contradicts, still rules out the 2005 remake (slice case kept green).
- (c) left out: no principled signal. Schafstall's "Hard Boiled" bills no re-release mark and TMDB dates no 2018 DE
  release (its DE re-release is 2026). Lifting a stated later year because other venues back an older film would lift
  a reboot's own year just the same (Rocky beside its namesakes).

No weight changed; the weekly refit re-tunes them.

**Slices** (pinned as cases): Moviemento's Blutgräfin → 1251608 and Kommunales Kino's Close → 901563 (recording
43588197349), Cineplex Baden-Baden's Starlight Express → 1748001 (recording 573), all judged right.

**What-if** over prod's mirror (all five countries, base a7e0f57d9): CLEAN. DE model 46 moved: 30 off Lynch's "Dune" (841m,
bound 1984) onto Villeneuve's 438631, which the Traumpalast venues bill dated 2021 and credited -- 29 judged cures, the
Cineamo billings of the Dec 4-5 IMAX/OV run; Atelier Kino's Dec 19 OmU "DUNE" left unjudged (the hermetic fixture,
lacking the corroborating venues, keeps it on 841), 15 gained right (Close ×2, Blutgräfin,
Katyn ×3, All That Jazz, Moulin Rouge, Babel, Blade Runner Final Cut, Die drei ??? ×4, No Good Men), 1 gained unjudged
(Filmforum's "Les Misérables (OmU)" → Cavayé's 2026 film). PL, UK, ES, US unmoved.

**Measure.** LANDABLE, run 43647686064 (all five countries, base a7e0f57d9, head cd0c97112, recording 573): DE model 13
gained right (All That Jazz, Katyn, Blutgräfin, Close, Catching Fire ×2, Die drei ??? …), nothing lost, changed or
wrong; final takes and PL, UK, ES, US unmoved. An earlier head (run 43643114687) that read every tag off and every
tagged copy as missing lost Baden-Baden's Starlight Express and gained the Das Boot stub: the two narrowings above.
