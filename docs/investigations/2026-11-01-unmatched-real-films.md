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
  PL "Niesamowite przygody skarpetek: Do roboty! PREMIERA 2D dubbing") and a namesake runner-up (DE "Lieblingsmenschen", 68 listings).
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
  re-peel). The title names two films and the billing gives no year. A year hint (screening year − N) needed
  identity code, because `IdentityMeasures.anniversaryYear` only relaxes a published year: Fix I below.
- "Kino KILN: Born in Flames" and "Our House | Preview Screening" already search the bare title at HEAD.

**Measure.** The `--footprint` measure found nothing to replay: 0 of ~870k stored readings read otherwise, because
the pinned recording bills none of these titles. `scripts/identity-what-if` against prod's mirror (2026-11-14T20:05Z,
pl/uk/us) was CLEAN:
- UK gained 5 model takes, all of Magic Lantern Tywyn's BIFA season. 2 of them are new final takes (Tender Loving
  Care 1370191, The Incomer 1554956); prod already served the other 3.
- US gained "Dune: Part Two Reissue (2026)" (693134), model and final.
- PL moved nothing, and nothing was there to move. 908327 is "Frontières" (2023), titled "Granice" in Polish, so
  `CandidateScoring.namesOnlyATag` rightly denies a film named only by the strand word GRANICE; it can never deny
  a "Głupcy" candidate. The film Kino CK Lublin bills is most likely Wasilewski's "Głupcy" (577874, 2022), held
  back by the rating cut (4.9%: one venue, no year). An earlier reading here blamed the veto; a count on
  2026-11-15 (16 unmatched listings that veto denies, 14 of them rightly) showed it does not.

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

## 10. Fix I: an anniversary dates its film

**Cause.** US Alamo Drafthouse ×15 "The Host: 20th Anniversary" (BelowThreshold): the bare "The Host" names Bong
Joon-ho's 2006 film (1255) and Niccol's 2013 one (72710) alike, and the billing states no year, no director.
`favoured-calibrated` refused: "its own facts do not favour it over the runner-up". The anniversary was the one fact
telling them apart, and nothing read it without a published year.

**Precision check** (prod mirror, 2026-11-15, every country; `IdentityMeasures.AnniversaryYears` in every language
`title-words.tsv` lists): 1678 listings bill an anniversary, 1654 matched and 24 unmatched. Of the 1364 matched ones whose
first screening day could be read, 1360 (99.7%) took a film within a year of (first screening year − N). Of the 1239
stating no year, 1238 (99.9%); the one off is DE Kino Lahnstein's "SATURDAY NIGHT FEVER 40TH ANNIVERSARY" (1977, 9 years
off its implied 1986). Unmatched: The Host ×15 is the recoverable group; the other 9 are events (Snuff Box Q&A, a Trigger
Happy TV tour, an album playback, a rap evening) or unmatched for other causes (Yi Yi, La Haine, The Fast and the Furious
at one venue each).

**Change.** `Listing.anniversaryScreeningYear` (the model's Listing, kept in the evidence and its key only where the
title bills an anniversary, so no other listing's key moves) carries the year of its first screening.
`IdentityMeasures.Listing.anniversaryFilmYear` is that year less the anniversary's, where the listing states no year.
`anniversaryFilmDated`, the last step of `listingFilmTitled`, reads it as `year.delta` / `year.distance` against a film
within a year of it, and only where nothing else dated the listing. It is evidence FOR a film and never against one,
because a venue's anniversary may count from another release (the Saturday Night Fever above keeps its take). No new
rule and no weight changed: the existing year measure reads it.

**Slice** (pinned as a case): Alamo Drafthouse Yonkers' "The Host: 20th Anniversary" → 1255, judged right.

**What-if** over prod's mirror (all five countries, base 58bf39767): CLEAN. US model and final takes gained 15 right
(The Host ×15 → 1255). PL model gained 1 right (Kinoteka's "55 rocznica premiery „Skrzypka na dachu”" → 14811, 1971;
prod's final already served it). DE, ES and UK did not move.

**Measure.** LANDABLE, run 43665225491 (full, all five countries, base 58bf39767, head b95cb49ea): PL model gained 1 right
(Skrzypek na dachu 14811), nothing lost, changed or wrong; the pinned recording bills no "The Host: 20th Anniversary".
No weight was refit: only anniversary billings stating no year gain a `year.delta`, and the weekly refit re-tunes it.

## 11. The relay bucket: what a right take is (2026-11-15) — parked

**What a right take is.** A relay's right film is its house's TMDB record of that season's broadcast (the Met's
"The Metropolitan Opera: Carmen" (2024) for its 2023/24 Carmen), never a screen namesake of the work, another house's
record or another season's (`StageRelays.anotherProduction`, `ListingConstraints.seasonsApart`). Where TMDB holds no
record, the right answer is NO take, with the house's spellings joined into one film (the same-relay link, 2a3a2ddbe
and its fixes, the other relay session's work).

**The census bucket on the mirror (2026-11-15T05:13Z), by that rule:**
- No record, no take is right: the Paris Opera's 2026/27 relays (US "Opéra National de Paris: La fanciulla del West"
  ×186, UK Bayadère ×19 and Notre-Dame ×19, PL "Balet z Opery Paryskiej 2026-2027: …" ×49, DE "La Bayadere (Opera
  national de Paris)", ES "La Bayadère (Cine)"), ES "Hamlet - Ópera (Cine)", DE "Poreia Theatre: Sophokles' Elektra",
  "NOSFERATU – … Ballett Extra". About 90% of the bucket.
- A record exists: UK "Royal Ballet & Opera 2024-25: Romeo and Juliet" ×16 (+1 US), a Flicks page kept from 2024/25
  screening on the 2026/27 relay's air day (25 May 2027) and its encore. Tried as
  [2026-11-15-relay-stale-season-airday](../identity-experiments/2026-11-15-relay-stale-season-airday.md): what-if UK
  model and final 12 gained (all → 1702789), US/DE/PL/ES 0 moved, but the 2026-10-08 review page judges 1702789 wrong
  for this title, so all 12 count as new-wrong. **Parked**, filed as a review candidate for the owner.
- Small, not tried: UK "MetOpera: Carmen (2023-24)" ×3 (`relay-record` refused: the venue credits Bizet, the composer, as
  director; the catalogue holds no composers), UK "The Met: La Fanciulla del West 27" ×1 (a two-digit trailing season
  not read as one), UK "The Royal Ballet Romeo and Juliet (Filmed live 2025)" ×2 (the 2019 record outranks the 2025 one),
  US "MetOpera: Medea (2022–23)" ×1 (no Met record found by any search), PL "Carmen | metropolitan opera: live in hd
  2026/27" (no 2026/27 Carmen record found).
  Followed up in §15: the Met's "… 27" landed (Fix M); the others stay unmatched, with the reasons.
## 12. Fix K: a bare billing beside a unanimous title family

**Cause.** A billing that publishes nothing but its title stays unmatched while every decorated sibling of its family
takes one film: DE Kino im Filmhaus' and Méliès Göttingen's "Mein Sommer mit Nonna" beside eleven venues' "Gioia mia -
Mein Sommer mit Nonna" (Milani's 1510325, TMDB's German title "Mein Sommer bei Nonna", so the bare search finds
nothing). `ClusterVoting.familyMajority`, the only lending path, needed siblings split across two films or more; and the
round's segment must-link is cut (`ConstraintEdges.namesBeside`) when the rest of the decorated title names its film.
A census (2026-11-15, read-only) found 123 such listings / 38 titles: ~20 right by eye, ~11 season relays, 5 clearly
wrong (DE "Musik", "Bruder", "Die andere", UK "Rocky", ES "Cecilia"), 4-5 namesake-risky ("Glaube", "Les Misérables",
"Blood Money", ES "Dune").

**Change.** `ClusterVoting.unanimous`, beside the majority: a facts-free cluster takes the one film when (1) every sibling
that took a film alone -- the round's title-linked members and those whose titles CARRY its title as a whole delimited
piece -- took it, seasons apart; (2) the carrying siblings, a stage house's relay apart, bill it at two venues or more;
(3) the majority's bound (one-sided 95% Wilson of their venue share, the bare venues counted against) clears the
show-ratings cut; (4) no member of the cluster nor sibling denies it; (5) the bare title names no OTHER film its own
searches found (Komplizinnen, Rocky, Lalka: namesake-ambiguous by the bare title alone); (6) a one-word title only as the
film's whole title. Every input is the family's content or the bare listing's own searches; nothing reads arrival order.
Typo siblings (UK "Ebeneezer" beside "Ebenezer") do not fall out of it and are left out.

**Slices** (pinned): Kino im Filmhaus' "Mein Sommer mit Nonna" → 1510325, Lumière's "Immer verkehrt" → 1520507
(recording 43654865568), both judged right; their population walks and 12 arrival orders take one film.

**What-if** over prod's mirror (all five countries, snapshot 2026-11-15 ~02:2xZ): CLEAN. DE model and final +3 gained
right (Nonna ×2, Immer verkehrt); PL, UK, US, ES unmoved. None of the census' wrong or risky titles, nor UK Pershore's
"Royal Ballet: The Nutcracker", moves. Precision 3/3; recall is low by design: Drei Abschiede (4 carrying venues of 6)
misses the bound, Komplizinnen and Godzilla Minus Zero (Quadroscope, its own evidence vetoes) the namesake and denial
guards.
## 13. The learned cannot-link bucket, counted (2026-11-15)

**Question.** Section 8's round extrapolated ~125 listings of real films refused by a LEARNED cannot-link
(`LearnedCannotLinks`: a rule of `identity-weights.json`, or the listing-film probability below its certified cut). The
sample's clusters were drawn listing-weighted, and a 68-listing cluster or a three-listing one moved a country's estimate
by tens, so the bucket was counted exactly instead.

**Count.** Every unmatched listing (mirror 2026-11-15 05:19Z, screenings from 11-01) whose stored trace denies a
candidate `Learned(...)`: 891 listings, 336 clusters, nearly all events and their stray candidates. Those whose denied
candidate shares the listing's title (event pattern set aside): 96 listings, 82 clusters, each judged from the trace's
candidates and evidence. **18 listings (18 clusters, one listing each) are real films the right record of which a
learned rule refused**: DE 9, PL 2, UK 6, US 1. Judged into `scripts/unmatched-census/verdicts.tsv`. Re-judged on the
way: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" (3) and "MISERICORDIA" are refused by the feed-title veto, no
learned rule; PL Kino PCKul's "Szczęściarze" credits a scenographer, a stage show more likely than Presque (unclear).
The census still extrapolates this bucket from the judged clusters (they are not a random sample now), so read its
step 3 row for it as the judged counts, not the `~` estimate.

| What weighs against the right film | Listings | Examples |
|---|---:|---|
| A stated year that is the screening's or the local release's | 5 | UK "Glastonbury The Movie" [2026], "Rocky" [2026], DE "Hard Boiled" [2018], PL "Wszystko jest iluminacją" [2007], "„Zaklęte Rewiry” z autorską narracją" [2026] |
| The venue's runtime (ads, an introduction, a different cut) | 4 | DE "Helter Skelter" (91+ off), UK "The Warriors (1978)", "Great Expectations (1947)", "Daisies" |
| A director credit the record lacks (a producer, a feed's copied credit) | 3 | DE "I, Pastafari …" {Eden Shaw}, "Der Polarexpress" {Michael Stocks}, US "Great Bear Rainforest 3D" |
| A bare title and nothing else, below the probability cut | 3 | DE "Silent Night - Leise rieselt das Blut", "Dead Snow", "Happy Together" |
| An anniversary dating the film a year early | 2 | DE "LA HAINE (30TH ANNIVERSARY)", "YI YI (25TH ANNIVERSARY)" |
| An English title TMDB lacks | 1 | UK "My Movie: The Miner" (Hirnyk) |

**Cause.** No common one. Each refusal is a published fact weighing against the film, the calibration doing its job by
the owner's rule, except the anniversary pair: Fix I's anniversary date is meant as evidence for a film, never against
it, yet a film a year before (first screening year − N) read as `year.delta` 1, which weighs -1.24 (a stated year a
year after the film is rare: 19 of 522 such pairs are the film). In prod's matched anniversary billings stating no year,
1199 took a film of that date, 45 one a year before, 1 one a year after: a re-release tours into the year after its
anniversary. The stated-year group is not one mechanism: `screeningYearAbsent` already drops a screening's year where
the title is the film's and the director is credited, and each of the five misses one of those two guards for a reason
the guard exists for (no director, or a fragment title).

**Fix J.** `IdentityMeasures.anniversaryFilmDated` reads a film released the year before the anniversary's date as of
that year (delta 0). The window stays "within a year of the date"; a film two years off is still not read.
Test: `IdentityMeasuresSpec`, "date a listing stating no year by the year it screens in less the anniversary's"
(fails before: `Number(1.0) was not equal to Number(0.0)`).

**What-if** (`scripts/identity-what-if`, all five countries, base b50bcb58c, head b850a6203, every family re-resolved):
CLEAN. DE model gained 1 right (Filmpalette "LA HAINE (30TH ANNIVERSARY)" → 406), DE final gained 2 right (also
Filmrauschpalast "YI YI (25TH ANNIVERSARY)" → 25538, judged right); PL, UK, US, ES moved nothing. right-lost 0,
right→wrong 0, new-wrong 0. The other 16 listings of the bucket stay unmatched by design: each has a published fact
against its film.

**Heads-up (not done).** UK "Daisies" is refused partly as a country mismatch: TMDB codes Chytilová's film XC
(Czechoslovakia), which the JDK-backed `YearRuntimeMeasures.isoByName` knows no successor of, so "Czech Republic"
mismatches it. A dissolved state's successors (XC → CZ, SK; SU → RU, UA, …; YU; DD/XG → DE) would be a measure
change of its own, measured on its own: done as Fix N (section 16).
## 14. Fix L: the banners Fix F left (2026-11-15)

**Cases.** The census's "real/banner" bucket on the 2026-11-15 mirror: DE "STUMMFILM MIT TOBIAS RANK:NOSFERATU (Freie
Platzwahl)", "LEICA - EIN JAHRHUNDERT IN BILDERN - am 15.11. mit Regie-Besuch!", "Feels like Home | Osteuropäische
Filmtage 2026"; PL "ZAKLĘTE REWIRY | NARRACJA: MALCOLM XD", "Niesamowite przygody skarpetek: Do roboty! PREMIERA 2D
dubbing"; UK "Our House | Preview Screening", "Kino KILN: Born in Flames"; US "Dune: Part One Reissue (2026)". A sweep for
the same shapes in all five countries found 6 "| Preview Screening" (5 matched), 7 director-visit tails in the golden
corpus, 4 "(Freie Platzwahl)", 4 "| NARRACJA:" (Pionier: Rejs, Zaklęte rewiry, Zemsta, Ślesicki), 12 "- Osteuropa" and
13 "| Osteuropäische Filmtage" (most matched).

**Finding.** The banner was no longer what held most of them. Each search already asked for the bare film. They stop at
scoring:
- Our House: four TMDB films of that title, no year. Kino KILN Born in Flames: 3.1%, no corroborating venue.
- Feels like Home, Father, Fantasy, Home Sweet Home (Osteuropa): bare titles that name a better-known film. Feels
  like Home is the Hungarian "Itt érzem magam otthon" (1395661), and stripping the festival banner would only invite
  the 2018 namesake. No row was added.
- Zaklęte rewiry (Rialto, Luna): learned year and runtime vetoes, which another agent owns. Skarpetki: the billing
  drops the "4." its record carries.
- Nosferatu: the silent-film denial and the programme-tag denial. A "STUMMFILM MIT <x>:" banner row moved nothing and
  sent "CHARLIE CHAPLIN" to search, so it was dropped.
- Leica: the slice over the pinned recording takes 1689091 at base and head alike. Prod's miss is the family's state,
  not its title.
- Dune: Part One: prod runs b81577148, which holds 6db813fa2's Reissue rule, so this is no deploy lag. A "Dune: Part
  One" → "Dune" rename took Dune: Part Three (1170608) in the what-if, a new wrong take, and was dropped. The instalment
  guard needs the "Part One".

**Change.** Three `title-rules.tsv` query rows. Each one's search title is now the bare film, which the IMDb
suggestions and the venues corroborating by search group read:
- `xtra-de-regie-guest-suffix`: "- [LETsDOK|Premiere] [am DD.MM.] mit Regie-Besuch / Regiegespräch / in Anwesenheit
  des Regisseurs …" (7 golden billings).
- `xtra-de-freie-platzwahl-suffix`.
- `xtra-pl-pipe-narracja-suffix`.

**Measure.** what-if over prod's mirror (05:13-05:17Z, all five countries, base b50bcb58c): CLEAN. PL model gained 4
"Zemsta" takes (Pafawag, Kika, Spektrum, MOK Głogów → 19721), all judged right, and prod's final already served them.
DE, UK, US and ES did not move. Footprint (pl): 57,557 readings, 0 read otherwise. DE has no stored base trace.
## 15. Fix M: the small relay groups §11 left (2026-11-15)

**Cases** (UK/US mirror 2026-11-15, the four "small, not tried" groups of §11):
- UK "MetOpera: Carmen (2023-24)" ×3 (+ US Lark ×1, prod-taken): `relay-record` refuses the Met's 2023/24 record
  (1143037, Cracknell) because the venue credits Georges Bizet, the composer. **No rule.** A sweep of every listing
  in all five countries for a composer's name as its director (60 composers' surnames) finds this one Flicks page
  (flicks:20812) alone, 4 listings, screening once each on 15-18 November. TMDB credits no composer on the record
  (its crew holds directors only), so the exception would need a composer catalogue for one page.
- UK The Beehive Honiton's "The Met: La Fanciulla del West 27" (26 January 2027) and "The Met: Samson and Delilah 26"
  (5 December 2026): **landed, Fix M below.**
- UK Ilkley's and Wetherby's "The Royal Ballet Romeo and Juliet (Filmed live 2025)" (25 May 2027, "AS LIVE", encore
  30 May): **no change, the owner's call.** The synopsis says "Filmed live 20th March 2025": 1451897's release day.
  The screenings are on 1702789's air day, the 2026/27 season's "as live" broadcast of that same capture, which 24
  judged rows take for the 25 May screenings (e.g. "Royal Ballet Recorded Live: Romeo and Juliet"). TMDB holds two
  records of one performance. Which one is right is the question the parked
  [relay-stale-season-airday](../identity-experiments/2026-11-15-relay-stale-season-airday.md) review candidate puts
  to the owner. Dating it by "Filmed live 2025" would take 1451897 and go against those rows. Taking 1702789 needs
  `namesAnotherWork` to stop reading "Filmed live" as the work. Neither is done until that call is made.
- US Cinema Art Theater's "MetOpera: Medea (2022–23)" (June 2027): **stays unmatched.** TMDB does hold the Met's
  record, 987694 "The Metropolitan Opera: Medea" (aired 22 October 2022, McVicar / Halvorson). But only a search for
  "(The) Metropolitan Opera: Medea" returns it. "Medea" has 74 results and it is not on the first page; "Met Opera:
  Medea", "MetOpera: Medea" and "Medea 2022" return nothing. Reached, `relay-record` would still refuse it: the venue
  credits Jo Meredith. That leaves one listing, and its fix (the `xtra-canonical-met-opera` row read as `Met\s*Opera`)
  would re-key ~540 US/UK Flicks "MetOpera:" listings (Tosca (2024-25) ×506). Not tried.

**Fix M: the cause.** The Beehive bills the Met's relays "The Met: <work> <yy>". Its yy is the year the relay screens
in. Its Otello and Parsifal took their records because those records carry the season in their titles ("The Metropolitan
Opera 2026/27: Otello"), which is what `season-record` reads. TMDB now titles Fanciulla's and Samson's records with no
season, and `relay-record` never read The Beehive's billings for three reasons:
- "Met" alone is no stage-house word (it is a relay series' banner word, `RelayStage.searchedInItsSeason`).
- "27" was no year: `closingYear` read four digits only.
- Fanciulla's record was reached only by IMDb's suggestion. TMDB finds it by "La Fanciulla del West" and by nothing
  carrying the "27" or a season year.

**Change.** In `RelayStage`:
- `of` reads a relay series' banner beside its work as it reads a house word.
- `ClosingYear` reads a year shortened to two digits after a space ("Tosca 27", "Tosca '27"), never a season's end
  ("26/27"). It is still held to the season its screenings fall in or later.
- `shortYearCut` gives a relay's titles with that year cut off, which `SearchMeasures.searchQueries` searches as shapes.

Tests: `AcceptanceSpec` "a relay billing its house and no season" (Fanciulla 27 → 1703629 over the 2018 staging,
Samson 26 → 1703624, "… 18" → none). `IdentityMeasuresSpec` (searched "La Fanciulla del West"; "Apollo 13" keeps its
digits).

**What-if** over prod's mirror (all five countries, base 892c88ffa, snapshots 09:46-10:13Z): CLEAN.
- UK model and final gained 2 (The Beehive's Fanciulla → 1703629, Samson → 1703624, both by `relay-record`, judged
  right).
- DE model gained 8 right: "MET 2026/27 Giacomo Puccini: LA FANCIULLA DEL WEST" → 1703629, which prod's final already
  serves through agreement. DE final did not move.
- US, PL and ES did not move.

Not covered: UK "MET: La Fanciulle del West" (a misspelt work) and "MET: Silent Night" ×2 (below the cut, no year).

## 16. Fix N: a dissolved state is one country with its successors (2026-11-15)

**Cause.** TMDB codes a film by the state it was made in: Chytilová's "Daisies" (46919) is XC, Czechoslovakia. A venue
writes the country it knows today (MAC Birmingham: "Czechia"), or the old state under a name the JDK's country table
has no code for (Rio Dalston: "Czechoslovakia"; PL "Czechosłowacja", DE "DDR", ES "Unión Soviética (URSS)", one listing
each on the 11-15 mirror). The `country` measure read the first as a `mismatch` and the second as `unmapped`. TMDB's
records on the mirror carry such codes often: per country 190-750 XC, 60-730 SU, 20-230 YU, 10-890 XG (East Germany),
plus CS and AN.

**Change.** `identity-country-lineage.tsv` (hand-kept, read by `CountryLineage`) names each dissolved state (XC, SU,
YU, CS, XG, DD, AN), its successors and the spellings venues write it in. The live `countryRelation` reads a country
and its predecessor or successor as a `match`; two successors of one state stay apart (CZ is not SK, RU is not UA, HR
is not RS). `countryCode` reads a defunct state's spelling as its code after the JDK's table. The pinned gate's frozen
measure (`PinnedGateMeasures`) reads no lineage. No weight changed; the weekly refit re-tunes them.
Test: `IdentityMeasuresSpec`, "a dissolved state" (fails before: `scripts/fail-before`), and
`CountryLineageSpec` for the table.

**What-if** (all five countries, base 892c88ffa, head 3d8dbc2ff, every family re-resolved since a resource changed):
CLEAN. UK model and final gained 1 right (MAC's "Daisies" → 46919: its page credits Chytilová, 1966, Czechia; its 105
minutes hold an introduction). PL, DE, US, ES moved nothing. right-lost 0, right→wrong 0, new-wrong 0. Rio Dalston's
"Rio Film Feminists: DAISIES" still takes nothing (not traced): its country now matches, so another fact or its banner
holds it.
