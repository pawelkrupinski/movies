# Identity resolver: film identity as a function of the listing set

Status: design, 2026-09-25. Prerequisites (a) to (d) below are on branch `identity-prereqs`.
The shadow prototype is on `identity-resolver-proof` (test code only). The proof report is the
source for every number marked *(proof)*.

Related: `docs/stable-film-id.md` (FilmId, phases 1–4), `docs/read-model-convergence.md`,
`docs/loop-a-settle-oscillation.md`, `docs/promotion.md`.

---

## 1. The problem

Today no single place decides which film a listing belongs to. Identity is decided a little at a
time by stages that each mutate stored state, and each stage reads what the previous ones wrote:

| stage | code | what it decides |
|---|---|---|
| decoration stripping | `TitleNormalizer`, `ScrapeListing.cleanTitle`, `FormatTags` | the title a slot and a row are keyed by |
| landing | `ScrapeLanding`, `ListingLanding`, `CorpusIndex`, `MovieCache.recordCinemaScrape` | which row a scraped listing is written to, or whether it is diverted to staging |
| staging fold | `StagingFold.planGroup`, `MongoStagingFolder`, `FoldOnStagingEnriched` | which newcomer rows become which film, and which retire |
| settle | `FilmCanonicalizer.groupByFilm` / `clusterByFilm`, `MixedFilmSplitter`, `SettleReaper`, `MovieCache.collapseCluster` / `settleResolved` / `backfillEmbeddedYears` | merges and splits after the fact |
| re-key / backfill | `MovieCache.rekey`, `RekeyReason.*`, `SideCollectionMove`, `ReadModelProjector.healMissingCards` | moving a row, with its slots, screenings and card, to a new key |
| TMDB resolution | `MovieService.resolveStagingRecord` / `lookupTmdb`, `TmdbCandidateSearch`, `ResolutionCache` | which film a row *is* |
| reapers | `UnresolvedTmdbReaper`, `StagingReaper`, `EnrichmentReaper` | re-asking, and concluding "no match" |
| vetoes | now `ListingConstraints` (prerequisite b): the venue-denied candidate, the decoration veto, the bare-listing home, the Faust containment refusal, the same-title slot split | refusing a merge one of the above would make |

State is keyed by derived strings. `CacheKey` is `sanitize(displayTitle)|year`. A slot is
`CinemaShowing(cinema, sanitize(cleanTitle))`. `FilmId` is minted from the key a row was first
created under. Each of those strings is a function of mutable state (the dominant spelling, TMDB's
year), so the output of the pipeline depends on the order listings arrive in and on the stored
history. Every bug below is one of four shapes: **order-dependence**, **churn** (a fixpoint that
is never reached), **data loss on re-key**, and **wrong merges**.

### 1.1 Incidents

From the proof report's historical replay and `git log origin/main --since=2026-09-23`: 584
commits, of which about 50 are identity fixes. A selection:

| incident | shape | fix on main |
|---|---|---|
| "A Star Is Born" 1954/1976/2018, three US venues, no published year, unioned into one row | wrong merge, order | `StagingFold` files differently-resolved rows at TMDB's year (5572eb461, b51a0194d) |
| Met "Samson i Dalila" 2026 folded onto DeMille 1949 because a bare listing resolved the title | wrong merge | `MixedFilmDetector.deniesFilm` in the fold, stitched evidence (c155fd983) |
| Kino Amok's bare "Samson i Dalila" moved between two rows every tick | churn | the bare listing's one home (c155fd983) |
| Kinoteka's Wong Kar Wai "Happy Together" bound to Kim Jeong-hwan's 2018 film by a yearless IMDb rung | wrong resolve | venue-denied candidate in `lookupTmdb` (4bfe094b2) |
| "It Ends with Us" landed on "It Ends" when it arrived second, but resolved alone when both arrived together | order | `listingDeniesFilm` on decoration landing (95f17ad40) |
| "Zärtlich kreist die Faust" (1990) adopted onto Murnau's "Faust" (1926) by containment | wrong merge | venue-denied containment fold (b2d308432) |
| Belle 2013/2021 (Arc Blackpool), Sinn und Sinnlichkeit 1995/2026 (Cinema-Arthouse) shared one slot, so one film showed the other's showtimes | data corruption | same-title slot split by year (338af3edf) |
| Marion Theatre Ocala's "Planet of the Apes" (Schaffner, no year) and "Planet of the Apes (2001)" (Burton) still shared one slot: one year among two is no year *disagreement* | data corruption | the same-venue fold also splits on directors crediting no common person, `ListingConstraints.venueCreditsApart` (branch `slot-key-collisions`) |
| "It" 1990/2017: yearless rows folded by title | wrong merge | unresolved groups filed at their bracketed years (e96dbf496) |
| Avengers rerelease and new arrival orders | order | 3ddc1a100 |
| A late-resolving row's showtimes lost when TMDB's year re-keyed or folded it | data loss | ce90509fd |
| A re-scraped slot's landing depended on the row's other venues | order | 617681c45 |
| A fold's retired rows collided with the survivor (E11000); duplicates deleted before the survivor landed | data loss | bc640f6ea, 4eb3ec7dd, 8983eced0 |
| A forced re-resolve re-keyed off the stored title key | churn | 5e1667746 |
| A doubted TMDB match stripped and rebuilt by every sweep | churn | 0719191cd |
| Vue showtimes deleted on re-key (UK); read model 4.5 h short after an id-scheme change; BackfillRM wiped screenings | data loss | earlier, see `docs/stable-film-id.md` |
| 9 days of logs, 1,211 key moves, 122 ping-pong pairs (`99seasonthematrix` ↔ `cineworld30thematrix`, 12× each way) | churn | FilmId phase 1 (743dd9f37) |

Each fix is local: it adds one more veto, window or tie-break to one stage. The hard-cluster
ratchet (`HardClusters`, `expected-hard-clusters-*.txt`) keeps them fixed, but three merge
bugs found in the 2026-09-16 audit are still open, and each new cluster has so far exposed a new
order. The fixes also compose badly. The same predicate was asked five different ways (see §4),
and at the time of writing another branch is narrowing those vetoes because they over-fire:
Poland lost 8 TMDB matches.

### 1.2 What the proof established

The proof checked each assumption on 5 hard-cluster corpora, 5 full recorded corpora (recorder run
36153174348) and 150 generated corpora, and gave each check a mutation that shows it can fail.

| claim | held? |
|---|---|
| A1: the lookup multiset is a function of the listing set | yes: 21 presentations of every corpus were identical, and the lazy-lookup mutant was caught |
| A1: the recorded trees answer every resolver query | **no**: gaps of PL 200, UK 140, US 18, DE 2 (→ prerequisite d) |
| A2: constraint resolution is order-independent | yes: 5,000 random graphs × 20 orders; the first-wins mutant was caught |
| A3: a title-derived family key is complete | **no**: every one leaks edges; only the block closure is complete (→ prerequisite c) |
| A4: assignment by overlap gives stable IDs | the rule held; its input (unique listing IDs) did **not** on DE (→ prerequisite a) |
| P1: order independence, P3: no cannot-linked pair in a cluster | yes |
| P2: fixpoint | 9 of 10 corpora; failed on DE through the listing-identity collision |
| P4: no key derived from title or year | yes for *derived* years; a page-less venue's own published year must be in its listing key |

---

## 2. Target architecture

```
  scrapes ──► EVIDENCE STORE ──► RESOLVE ──► ASSIGN IDS ──► PROJECTION
              listings, keyed     clusters =   stable FilmId   movies / slots /
              by ListingKey,      resolve(E),  by overlap      screenings / read model,
              + cached lookups    pure         with previous   all keyed by FilmId
                                                               and ListingKey
```

1. **Evidence store.** Append/replace per venue scrape. One document per listing
   (`ListingKey` → the listing exactly as the venue published it, and its showtimes), plus the
   lookup cache: the venue's detail answer per `(venue, page)` and the TMDB answer per
   `Evidence.key`. `cinema_scrapes` (`ScrapeArchiveRepository`) already holds the raw rows; the
   store is that archive with a listing-level key, plus the answers `EnrichmentCache` and
   `ResolutionCache` already persist. Nothing in it is derived from another listing.
2. **Resolve.** `clusters = resolve(E)`. It is a pure function of the listing set and the cached
   lookups, per family (§5). The steps: enumerate the lookups in sorted order, collapse identical
   evidence into nodes, draw edges only within blocks, and solve (§3). No stored film is read. It
   replaces the identity decisions in `ScrapeLanding` / `ListingLanding`, `StagingFold`, the settle
   (`FilmCanonicalizer`, `MixedFilmSplitter`, `collapseCluster`, `settleResolved`,
   `backfillEmbeddedYears`) and the "no match" concluding in `UnresolvedTmdbReaper`.
3. **Assign IDs.** `IdAssigner` (§6) maps each new cluster to the previous FilmId it overlaps
   most. It never reads a title or a year.
4. **Projection.** A film document is a *view*: displayTitle, year, TMDB/IMDb ids, ratings and
   the merged fields, computed from its cluster's listings and lookups (today's
   `MovieRecordMerge` / display-title picker, moved). `movie_slots` and `screenings` are keyed by
   `(FilmId, ListingKey)`. `ReadModelProjector` reads films and slots exactly as today. A cluster
   change moves slot rows between FilmIds by ListingKey, so it never re-derives a slot key.

Enrichment (ratings, posters, synopsis) stays keyed by FilmId and hangs off the projection,
unchanged. Rating lookups are per-film, not per-listing, and have nothing to do with identity.

### 2.1 What gets deleted at the end

- `CacheKey` as an identity (it stays a display/lookup string, as `key` already is on `movies`);
- `ScrapeLanding`'s divert / redirect / variant-rekey logic, `ListingLanding`, and `CorpusIndex`'s
  decoration and search-key lookups;
- `StagingFold`, `MongoStagingFolder`, the staging collection and `StagingReaper`;
- `FilmCanonicalizer`'s clustering and `MixedFilmSplitter`, together with `SettleReaper`,
  `MovieCache.collapseCluster` / `settleResolved` / `backfillEmbeddedYears` / `rekey`, every
  `RekeyReason`, and every `MergeReason` except the reporting of `IdAssigner`'s merges;
- `SideCollectionMove` (slots move by ListingKey inside the projection write);
- `UnresolvedTmdbReaper`'s concluding (a no-match is simply a cached lookup answer, re-asked on TTL);
- `ScrapeListing.prepare`'s per-title fold, which erases the very rows listing identity needs;
- the per-film re-resolve paths `MovieService.resolveTmdbOnce` and `retryUnresolvedTmdb`, in
  favour of the resolver's per-evidence resolve (`resolveStagingRecord`'s body, without a stored
  row).

---

## 3. The constraint model

There is one module, `services.movies.ListingConstraints` (prerequisite b), and one solver.

**Edges** are drawn only between two nodes that share a block key (§5):

| kind | tier | rule | source today |
|---|---|---|---|
| must | 1 | same TMDB id | the write-time tmdbId fold, `groupByFilm` |
| must | 2 | same sanitised title | the landing's same-titled rows, the sanitize group |
| must | 3 | same search form (decoration / programme banner stripped), or one side's original title is the other's title | `FilmCanonicalizer.searchKey`, `TitleContainment`, `keysForAlias` |
| cannot | – | different TMDB ids | – |
| cannot | – | `VenueDeniesFilm`: a venue's own year AND director contradict the other's film | `ListingConstraints.slotDeniesFilm` / `rowDeniesFilms` |
| cannot | – | `ListingDeniesFilm`: a shape-matched listing's director and runtime/year deny the film | `ListingConstraints.landingRefused` |
| cannot | – | `OriginalTitleNamesAnotherFilm` | `ListingConstraints.originalTitleNamesAnotherFilm` |
| cannot | – | `CinemasDescribeDifferentFilms` | `ListingConstraints.cinemasDescribeDifferentFilms` |
| cannot | – | different instalments | `SequelMarker.differentInstalments` (to be routed through `ListingConstraints`) |
| cannot | – | different bracketed years, with no shared film | `EmbeddedYear` (to be routed through `ListingConstraints`) |

**Solving** (`ConstraintSolver` in the prototype). Cannot-link wins: two components never unite
while a cannot-link joins them, so P3 holds by construction. Must-links are applied tier by tier,
and within a tier in the order `(min key, max key)`. A node whose tier edges reach two components
that are cannot-linked to each other is **ambiguous** and is left alone for that tier. That is the
bare "A Star Is Born" beside 1954 and 2018. Because the solver reads its input only through sorts
under a total order, the partition is a function of the set.

**Must-links that are not edges.** Today's bare-listing rule
(`ListingConstraints.keepsIncumbentHome`) exists only because the incremental pipeline has an
incumbent. In the resolver a bare listing is a node whose only edges are title must-links: it
joins its title's film when that film is unambiguous, and otherwise it stays alone.

**Rule discipline.** A new rule goes into `ListingConstraints` with its reason, and must come
with a block key (§5), or the family check fails. `ListingConstraintsRoutingSpec` fails any
production file that asks a `MixedFilmDetector` veto directly.

---

## 4. Listing identity

A **listing** is one row of one venue's scrape. Its key is `ListingKey`
(`common/src/main/scala/services/movies/ListingKey.scala`, prerequisite a):

- `Native(venue, page, rawTitle)` when the venue publishes a page (`filmUrl`). The page alone is
  not unique: KinoPort, Kino Studio Opole and DK Łapy link a whole month's films to one programme
  page.
- `Published(venue, rawTitle, ownYear, sortedDirectors)` when it does not. The raw title alone is
  not unique: Cinema-Arthouse and Schauburg Karlsruhe each list "Sinn und Sinnlichkeit" twice
  (Ang Lee 1995 and Georgia Oakley 2026), and Club Manufaktur lists "Bad Apples" twice (2018 and
  2025).

`ListingKeyCorpusSpec` checks uniqueness per distinct listing over 15 corpora (185,439 listings,
all five full recorded corpora included when `KINOWO_IDENTITY_CORPUS_DIR` is set), and shows that
every naive key fails somewhere: venue + raw title, venue + page, the prototype's `ListingId`, and
the production slot key.

Consequences:

- A year in a `Published` key is the venue's own published year, never a derived one. When a
  page-less venue corrects its year or director, the listing gets a new key, and its film keeps
  its FilmId only through overlap with its other listings (§6). A page-bearing listing survives
  any such correction.
- Slots and screenings keyed by ListingKey make the Belle class structurally impossible.
- **The slot-key finding.** The proof counted production slot keys `(venue, sanitize(title))`
  holding listings of two resolver films: 13 in UK and 234 in US *(proof)* (Candyman
  1992/2021 at the Regals, Halloween 1978/2007 at the Prince Charles, Resident Evil 2002/2026 at
  Cannock). That count reads *raw* rows, and the slot key alone is indeed not unique. What
  decides whether two films share a slot is `ScrapeListing.prepare`'s same-title fold, and after
  338af3edf it keeps every one of those bracketed-year pairs on its own slot. Re-checked over
  all five full corpora (`ListingKeyCorpusSpec`, "the production slot fold"), exactly one pair
  was still folded: Marion Theatre Ocala's yearless "Planet of the Apes" (Schaffner) with
  "Planet of the Apes (2001)" (Burton), because one year among two is no year disagreement.
  The fold now also splits on the venue's own directors crediting no common person
  (`ListingConstraints.venueCreditsApart`; the landing's `ownCopy` reads the same rule), which
  is `ListingKey.Published`'s discriminator. On the five corpora every same-venue, same-title
  pair with disjoint directors also differs in year or runtime, so the rule splits no film.
  The fold's discriminator is therefore the listing key's, and a (film, slot key) pair holds
  one published film. Keying storage by `ListingKey` itself stays phase 2's job: it re-keys
  every stored slot, which is the migration phase 2 plans.
- The proof's shadow diff mapped each listing to a pipeline film through a `Map` keyed by that
  same non-unique slot key, so where two films share it the listing lands on whichever the map
  kept. Its UK category-2 "pipeline merged two films" cases (Halloween, Poltergeist, Belle, …)
  and the category-4 Belle / Sinn und Sinnlichkeit artifacts are that mapping, not evidence of
  a merge. The next shadow diff maps by `(film, ListingKey)`.
- `ScrapeListing.prepare` folds a venue's rows per sanitised title before anything sees them,
  which hides every collision. The evidence store must keep raw rows, and that fold moves into
  the projection's display merge.

---

## 5. Families: the block closure

A resolve is scoped to a **family**: the union-find closure of every listing's block keys. The
block keys are the sanitised title, the search form, the original title (both forms) and the
TMDB id once resolved. `services.identity.FamilyClosure` (prerequisite c) implements
`blockKeys`, `families`, `crossings`, `merges` and the `check` hook (`FamilyClosureMetrics`, a
logging implementation, not yet wired to Prometheus).

- The closure is **complete by construction**: every edge is drawn between nodes that share a
  block key, so a family-by-family resolve equals the global one. Checked on 10 corpora and 150
  generated ones *(proof)*.
- No title-derived key is complete. First-token families leak 293 / 45 / 7 / 2 must-links on
  PL / UK / DE / US *(proof)*. A programme banner puts the real title anywhere in the string, and
  a shared TMDB id joins any two strings.
- The closure is only complete *relative to the rules*. A rule whose endpoints share no block key
  (raw-title containment, which is the Faust bug) breaks it silently. `FamilyClosure.check`
  returns the crossing edges, and the resolver must then **fail rather than scope**.
- The cost is family size: the largest are 3,179 listings (US) and 657 (UK). But a family is one
  wide release's venues, and its node count is small.
- Routing a new listing to its family needs only its own keys, so an incremental resolve
  recomputes only the families it touches.

---

## 6. ID assignment

`IdAssigner` in the prototype works over every (previous cluster, next cluster) pair that shares
a listing. It sorts the pairs by `(previous id asc, overlap desc, next cluster's smallest
ListingKey asc)` and matches greedily. The result:

- **Merge**: the older (smaller) id survives, even when the newer side is 6× larger.
- **Split**: the id stays on the larger half. On an equal split it stays on the half holding the
  smallest ListingKey, a property of the data rather than of input order.
- **Fresh**: every unmatched cluster gets a new id, in order of its smallest ListingKey.
- A previous id that matches no cluster is **retired**. Its card is removed by the projection, as
  the prune does today.

FilmIds stay opaque. New ones come from a counter (smaller is older). Existing `FilmId`s,
including legacy `title|year` strings, are carried over by the migration (§8, phase 2). This is
the property `docs/stable-film-id.md` wanted and the incremental fold could not give: two replays
in different orders assign the same ids.

---

## 7. What is guaranteed, and what is not

**Guaranteed, by construction and by property tests:**

- *Determinism*: clusters, and therefore FilmIds, are a function of the listing set and the
  cached answers (P1). Neither arrival order, chunking, a restart nor a replay can change them.
- *Fixpoint*: re-resolving an unchanged set changes nothing (P2). The churn class is gone.
- *No cannot-linked pair in one film* (P3).
- *Data safety*: showtimes are keyed by ListingKey, so a resolution change moves a slot between
  FilmIds and never re-keys, rewrites or deletes it (P4). Only a venue withdrawing a listing
  removes its showtimes.

**Not guaranteed: correctness.** The resolver is *wrong but stable* where the evidence is thin.
From the shadow diff:

- *Under-merge* of decorated and programme-bannered spellings that have no must-link: the 18
  decorated "Lalka" spellings, "Casino Royale (20th Anniversary)" vs "Throwback: Casino Royale",
  "100 dni: Misja Zeus 2D PL", "Matilda (30th Anniversary)", and "Mockingjay – Part 2 (2026)"
  cannot-linked to the 2015 film by bracketed years. The pipeline joins these through containment
  and group-level resolution, which the resolver does not yet have (§7b).
- *Undecidable from the evidence*: a listing that publishes only a title naming two or more
  films. Examples are Kino 1410's "Opętanie | klasyka w 4k", Amok's bare "Samson i Dalila",
  "Throwback: The Hunger Games", Alamo's "IT (2017)" with no sibling bracketing a year, the bare
  "Resident Evil" / "Sense and Sensibility" / "The Omen", and every yearless, directorless listing
  of a remade title (Crash, Renoir, Mira, Kura, Digger). Every rule that places such a listing is
  guessing. The resolver makes the guess stable (P1/P2) and visible: the ambiguity rule leaves a
  truly two-sided node alone. A prior such as "at a first-run venue, a bare title is the current
  release" would be a policy, and is an open question.
- *Opera and event broadcasts*: the Met's and RBO's "Macbeth" are joined by a venue's
  `originalTitle`, and "Così fan tutte" is fragmented. These belong to the undecidable class
  until a broadcast has an identity of its own.

---

## 7a. What the shadow diff found

The proof booted the real pipeline (`PipelineReplay`: every venue arrives in sorted order,
staging advanced between venues, then the full settle and projection) over each recorded corpus
and the same recorded answers, and compared films with resolver clusters by listing set
*(proof)*:

| corpus | pipeline films | resolver clusters | identical | identical share | boot |
|---|---|---|---|---|---|
| PL | 1,141 | 1,288 | 1,011 | 88.6% | 532 s |
| UK | 1,539 | 1,590 | 1,468 | 95.4% | 949 s |
| DE | 1,694 | 1,695 | 1,684 | 99.4% | 589 s |
| ES | 236 | 236 | 236 | 100% | 107 s |
| US | – | 2,320 | – | – | stopped after 65 min at ~1,500 of 4,452 venues |

No listing lacked a pipeline slot in any corpus. The disagreements, by category:

| category | PL | UK | DE | who is right |
|---|---|---|---|---|
| 1. resolver splits a pipeline film: no must-link joins the parts | 90 | 37 | 2 | the **pipeline**, bar DE's Faust |
| 2. resolver splits on a cannot-link | 7 | 13 | 2 | mostly the resolver; UK's 13 are the slot-mapping artifact (§4) |
| 3. resolver leaves an ambiguous node alone | 1 | 5 | 0 | undecidable from the listing |
| 4. resolver merges pipeline films | 16 | 13 | 4 | mostly the **resolver** (Crash, Digger, Renoir); RBO/Met "Macbeth" is its error |
| 5. same members, different film (resolver unresolved) | 83 | 71 | 38 | the pipeline; fixture gaps (§9) and thinner per-listing evidence |

ES has 5 of category 5 and nothing else.

**Reading.** Where the two disagree on a *merge*, the resolver is right more often: it has no
path that merges on title shape against a resolved film's evidence. Where they disagree on a
*split*, the pipeline is right: the prototype has neither containment nor group-level
resolution, which are how the pipeline joins "100 dni: Misja Zeus 2D PL", "Throwback: Casino
Royale", "Coraline - Sensory Friendly Screening" and the 18 decorated "Lalka" spellings to their
films. §7b designs both back in.

**US is unfinished.** The resolver side of US is complete (99,774 listings, 2,320 clusters, P1–P4
held), but the pipeline boot scales with venues through `PipelineReplay`'s staging advance per
venue and was stopped. Only the hard-cluster US diff (29 of 31 pipeline films identical) exists.
See open question 8.

**Historical replay verdicts** *(proof, full corpora)*:

| case | verdict |
|---|---|
| A Star Is Born 1954/1976/2018 (US) | correct: three clusters |
| Faust 1990 vs 1926 (DE) | correct: no containment edge exists |
| It Ends with Us vs It Ends (US) | correct |
| Belle 2013/2021 (UK) | correct: two clusters, two listing keys |
| Avengers: Endgame rerelease (PL) | correct: joins 299534 with 143 venues |
| UK "(2026)" rereleases (Hope, Resident Evil, Moana, …) | mostly correct; "Mockingjay – Part 2 (2026)" ×63 is split off (wrong but stable) |
| Hunger Games siblings | correct per instalment, bar the Part 2 rerelease |
| Lalka / Überleben / Matilda / Samson churn | correct by construction (P2, 0 id changes) |
| Vue showtimes deleted on re-key (UK) | correct by construction (P4); not replayable on today's corpus |
| Happy Together (PL) | no wrong binding; the right film is not found either |
| Skarpetek instalments (PL) | order-stable, mostly correct |
| It 1990/2017 (US) | order-stable; Alamo's "IT (2017)" left alone by the too-coarse ambiguity rule |
| Met "Samson i Dalila" (PL) | wrong but stable: the Met decoration is the only evidence and no edge reads it |
| Così fan tutte (PL) | wrong but stable: the broadcast is fragmented (under-merge) |
| Lalka decorated spellings (PL) | wrong but stable: 18 spellings stay out (no containment) |
| Opętanie (PL) | undecidable: the title names two films and the listing publishes nothing else |

---

## 7b. Containment and group-level resolution, order-independent

Both are how the pipeline gets category 1 right, and both are today *incremental*: containment
reads the rows already landed, group resolution resolves the group the fold happened to build.
Each is added back as a deterministic function of the listing set.

**Containment as a tier-4 must-link, by segment.** A listing's title is split at programme
delimiters (`|`, `:`, ` - `, `–`, a bracket pair, "presented in", "sensory friendly") into
segments, each normalised to its search form. A containment edge joins A and B when A's whole
search form **equals one whole segment** of B's. The block key is each segment: B emits all its
segments, A emits its search form, so every edge joins two listings that share a block key and
the family closure stays complete (§5). Whole segments, not token n-grams, because the Faust and
It Ends failures are both *intra*-segment: "Zärtlich kreist die Faust" has no delimiter before
"Faust", "It Ends with Us" none before "with". The edge is guarded by `VenueDeniesFilm`,
`ListingDeniesFilm` and a new `BroadcastSeries` cannot-link (a Met / RBO / NT Live marker on one
side and not the other), and the ambiguity rule applies unchanged: "Throwback: The Hunger
Games" reaches two cannot-linked instalments and stays alone. Format suffixes ("2D PL") belong in
`FormatTags`, not here.

**Group-level resolution as a fixpoint over partitions.** The pipeline resolves a merged group's
pooled evidence: one venue's director or year resolves every venue's bare listing. The resolver
does it in rounds, each a function of the previous round's partition:

1. Round 0 is today's resolve: per-listing lookups, solve, partition P0.
2. Round k+1: for each cluster of Pk that holds an unresolved node, pool its members' evidence
   canonically (the modal own year, smallest on a tie; the union of directors, sorted; the
   runtimes that agree with the majority) and resolve the pooled evidence. The answer becomes an
   attribute of every member with no answer of its own, drawn as a tier-1 must-link to that
   TMDB id's component. A member's own answer always wins, and cannot-links still win over
   everything.
3. Stop when Pk+1 = Pk.

Order-independence: P0 is a function of the set (A2); a cluster's pooled evidence is a function
of its members, so round k+1's lookups and edges are a function of Pk; by induction every round
is a function of the set. Termination: a round only adds must-links, so partitions coarsen
monotonically and there are at most (nodes − 1) rounds, in practice one or two. A1 weakens from
"no lookup depends on another's answer" to "every lookup is a function of the set", which the
permutation checks still test, and the lazy-lookup mutant stays detectable. The recording pass
(§9) gains the pooled queries as a second round of `IdentityLookupSweep`.

Each must be proven the way the rest was: a hard cluster, and a mutation that the checks catch
(dropping the whole-segment rule merges Faust; pooling in arrival order fails P1).

---

## 8. Migration plan

Each phase is independently shippable and revertible. No phase deletes old code before the
following phase has held its acceptance for a week.

### Phase 0: prerequisites (done on `identity-prereqs`)

(a) `ListingKey` and its corpus proof. (b) `ListingConstraints`, the one constraint model, with
every veto routed through it. (c) `FamilyClosure` and its runtime check. (d) The recording pass
for the resolver's query set (§9).

### Phase 1: shadow mode

- Move the prototype (`IdentityModel`, `IdentityResolver`, `ConstraintSolver`, `IdAssigner`) from
  test code into `worker/src/main/scala/services/identity`. The prototype's `Listing` becomes
  `ListingKey` plus the raw row. Its `Evidence` becomes `IdentityLookupSweep.Evidence`, lifted to
  main. Its edges are drawn from `ListingConstraints`.
- A `ShadowIdentityReaper` runs `resolve` per family, after each settle tick, over the live
  corpus and the lookup caches. It issues **no** new lookups in prod (a gap is an "unknown"
  node), writes nothing but a `identity_shadow` report collection, and exports gauges:
  `kinowo_identity_shadow_films{relation="identical|split|merged|moved"}`,
  `kinowo_identity_family_crossings`, and `kinowo_identity_resolve_seconds`.
- CI: every convergence leg also runs the resolver on its corpus and writes the shadow diff as an
  artifact. `IdentityResolverCorpusIntegrationSpec` moves to itAll.

### Phase 2: stable IDs and data migration

- Evidence store: a `listings` collection keyed by `ListingKey`, written by the scrape path
  *beside* the current landing (dual write), and backfilled from `cinema_scrapes`.
- ID seeding: run `IdAssigner` with the **current** films as the previous assignment. Each
  current film is the set of its listings, mapped through its slots. So every resolver cluster
  inherits the FilmId it overlaps most, and every surviving film keeps its URL, card id, ratings
  and share card. The table of films that map to no cluster, or to two, is the migration's
  review list.
- `movie_slots` / `screenings` gain a `listingKey` field, backfilled. Nothing reads it yet.

### Phase 3: cutover, per country

- Behind a per-country switch that is chosen at the composition root, not tested in business
  code, `AppLoader` wires an `IdentityProjection` in place of `ScrapeLanding` + staging + settle.
  The scrape path writes listings. The resolver runs per touched family, and the projection
  writes films and slots keyed `(FilmId, ListingKey)`.
- The order is ES → DE → UK → US → PL, smallest and cleanest shadow diff first. PL goes last
  because it has the most decorated spellings and is where under-merge costs most.

### Phase 4: delete the old stages

After a country has held phase-3 acceptance for a week, delete from that country's wiring
everything listed in §2.1. The code itself goes once the last country has cut over.

---

## 9. Recording the resolver's queries (prerequisite d)

The resolver resolves each listing's own evidence. The pipeline resolves merged groups, so the
recorded trees lack the resolver's query shapes. `tools.IdentityLookupSweep` (worker fixtures)
issues exactly the resolver's query set, with a memo-free `MovieService`:

- each raw listing, in its total order;
- the venue's detail page, once per `(venue, page)`;
- `resolveStagingRecord` of each distinct evidence, on a slot of one fixed catalogue venue.

`CountryConvergenceBehaviour` runs the sweep after the boot, before the boot-complete marker, when
`KINOWO_IDENTITY_LOOKUPS=true`:

- In a **recording** leg, every miss is fetched live and written into `enrichment-<cc>`, which
  the leg then publishes and pins as the hermetic pair.
- In a **hermetic** leg, a miss fails the leg by name, which makes the flag the coverage check.

Dispatch it (it records live, so it is not run from a laptop):

```
gh workflow run "Record scrape fixtures" --ref main -f identity-lookups=true
```

The cron run leaves the flag off, so nothing changes for the nightly recording. Once one run
with the flag has pinned its pair, the proof's full-corpus replay should report 0 gaps on all five
countries; that is phase 1's first acceptance check.

---

## 10. Acceptance criteria

### Phase 0

- `ListingKeyCorpusSpec` is green on all 15 corpora, and red with any naive key; its slot-fold check is green.
- `ListingConstraintsRoutingSpec` is green. The veto specs (`DeniedCandidateSpec`,
  `DecorationVetoSpec`, `ContainmentDeniedByVenueSpec`, `StagingFoldSpec`,
  `BareListingOneHomeSpec`), `testUnit`, `itAll` (including `HardClusterConvergenceIntegrationSpec`)
  and the sample legs are unchanged.
- `FamilyClosureSpec` is green, including the Faust crossing.
- One recording run with `identity-lookups=true` is pinned, and the proof replays with 0 gaps.

### Phase 1 (shadow), per country, over 7 consecutive days

- P1/P2/P3 hold on every live resolve. Any crossing edge fails the resolve and pages. The shadow
  diff is identical run to run on an unchanged corpus.
- The shadow diff against production maps listings to films by `(film, ListingKey)`, never by
  slot key (§4), and is measured per category rather than as one share, because the categories
  have opposite owners (§7a):
  - **identical ≥ 97% of films in every country**, once §7b's edges land. Measured without them
    (by films): ES 100%, DE 99.4%, UK 95.4%, PL 88.6%.
  - **category 1, resolver under-merges: ≤ 1% of films**, and 0 of the named cases (the Lalka
    spellings, Casino Royale, 100 dni, Coraline, Digger). Today PL 90 (7.9%), UK 37 (2.4%), DE 2.
  - **categories 2 and 4, merge disagreements**: every case reviewed and on the `known-issues`
    list with a verdict. No threshold, since the resolver is usually the right side.
  - **category 3, ambiguous**: every case on the list as *undecidable*.
  - **category 5, resolver unresolved where the pipeline resolved: ≤ 1% of listings**, with 0
    caused by fixture gaps. Today PL 83, UK 71, DE 38, ES 5 films, mostly gaps.
  - **US** enters phase 1 only once its diff exists at all (open question 8), with the same
    thresholds.
- Each §7b edge is proven by a hard cluster and a mutation, and every one keeps "Zärtlich kreist
  die Faust" and "It Ends with Us" apart.

### The known-issues list is the acceptance test

- The proof's historical-replay table becomes `expected-identity-<cc>.txt`, one line per case
  with its verdict: *correct*, *wrong but stable (accepted)*, or *undecidable*.
- It is checked like `expected-hard-clusters-*.txt`: a case may move from wrong to correct and
  never back.
- The three open merges from the 2026-09-16 audit, and the `ServedCorpusInvariants.wrongMerges`
  output, are entries on it.
- A resolver change that flips any line fails CI until the file is regenerated and reviewed.

### Phase 2 (migration)

- Every current FilmId maps to exactly one cluster or is on the reviewed list. No surviving film
  changes URL.
- For every listing, the slot and screenings rows found by `listingKey` equal those found by the
  current slot key (0 differences), except the known Belle-class collisions, which gain a slot.

### Phase 3 (cutover), per country

- The convergence legs (full and sample) and the hard clusters are green on the new wiring,
  including the order-independence and next-day legs.
- The churn counters are zero: `kinowo_worker_rekeys_total` and
  `kinowo_worker_merges_total{reason!="imdb-identity"}` both at 0 over 7 days.
- Read-model drift gauges hold at baseline. `ReadModelServingDiffers` does not fire.
- Film count per country is within ±2% of the pre-cutover count, and served showtimes within
  ±0.5%.

### Phase 4

- The deleted-code list in §2.1 is gone. `testUnit`, `itAll` and every convergence leg are green.

---

## 11. Rollout and rollback in production

- **Per country**: the switch is per country and chosen in `AppLoader`. Workers are per country
  already (`worker-pl`, …), so a cutover is a config change and a worker restart. Worker downtime
  is acceptable.
- **The read model is the safety net.** The web tier reads `web_movies` / `web_screenings`
  only. In phases 1 and 2 nothing the resolver computes reaches them. In phase 3 the first
  projection is diffed against the live read model before it is written: if more than 2% of
  cards or 0.5% of showtimes would disappear, the worker refuses and stays on the old wiring.
  This is the same guard as the scrape breadth guard.
- **Rollback in phase 3** is to flip the country back. The old path's collections (`movies`,
  staging) keep being written in dual-write for the first week, and their FilmIds are the ones the
  resolver inherited, so switching back neither re-keys nor loses a card. After the week, rollback
  means reseeding the old path from the projection. That is a documented runbook step, not an
  automatic one.
- **Rollback in phase 4** is a revert of the deletion commit. Deletion happens only after a
  week's green.
- The first cutover goes to a single worker pod, and web is never restarted for it.

---

## 12. Open questions

1. **Containment and group-level evidence.** §7b proposes both. Open: the delimiter list, and
   the family-size cost of segment block keys, which needs measuring on PL before it ships.
2. **The ambiguity rule is too coarse** where a bracketed year matches one side (Alamo's
   "IT (2017)"). Should an ambiguous node be allowed to join the side its own stated year agrees
   with?
3. **Policy for the undecidable.** Should there be a "current release at a first-run venue"
   prior for bare titles of remade films? That would resolve the Resident Evil and Sense and
   Sensibility class, but it is a guess made explicit.
4. **Broadcast identity.** Met / RBO / NT Live productions share titles with films and with each
   other. Should a broadcast be its own entity type, keyed by the series plus the production?
5. **Lookup TTLs.** Should a cached TMDB no-match expire like today's resolution memo, and if it
   does, how is the resulting change in `resolve(E)` reported so that it does not look like churn?
6. **`SequelMarker` false positives.** It reads "Auta (re-release) – 20. rocznica" as an
   instalment. Fix the rule, or demote it from cannot-link to a tier penalty?
7. **Family-size ceiling.** The US wide releases reach 3,179 listings. The resolve is O(nodes)
   per family, but a pathological bridge could join two blockbusters. Should there be a size alarm
   on `FamilyClosure.merges`?
8. **The US shadow diff** is still missing; ES has landed (236 of 236 identical). The replay boot
   (`PipelineReplay`, one staging advance per venue) did not finish US's 4,452 venues in 65
   minutes. Options: advance staging per *batch* of venues, or diff against the pipeline output
   the US convergence leg already produces rather than booting a second time. Until one lands,
   US rests on its hard cluster alone and cannot pass phase 1.

---

## 13. Curation: pins, confidence-gated ratings, the admin view

Program phase 3 (the program brief numbers its phases 1 observations, 2 shadow resolver,
3 curation, 4 stable ids, 5 cutover, 6 deletion; §8 above predates that numbering). Everything
here is data or pure code, and nothing reaches what production serves: the rating gate is off at
the composition root and the shadow source is empty until the resolver's shadow run lands.

Design constraints: minimum human involvement and no hardcoding. Pins are a rarely used escape
hatch, not a workflow; no pin is seeded in production; the rating threshold is derived from data;
there is no per-title, per-venue or per-franchise rule anywhere.

### 13.1 Pins

`services.identity.Pin` (common): a claim over a set of `ListingKey`s, with author, reason and
time. Three claims:

| claim | meaning | what the resolver gets |
|---|---|---|
| `IsFilm(tmdbId)` | these listings are this film | the film overrides each listing's own lookup answer; the listings are must-linked to each other and to every other listing pinned to that film |
| `SameFilm` | these listings are one film | must-links between them |
| `NeverFilm(tmdbId)` | these listings are never this film | that answer is dropped from their lookups; a cannot-link to every listing whose film it is |

A pin's id is its content (SHA-256 of the claim over the sorted listing set), so re-asserting it
is refused rather than duplicated, and the id is the same everywhere.

**Into the constraint model.** `ListingConstraints.pinned(pins)` returns `PinConstraints`, the
adapter the resolver consumes:

- `resolvedFilm(key, looked)`: the pinned film, else the listing's own answer unless denied;
- `blockKeys(key)`: a `pin:<group>` key per pinned group plus `id:<tmdbId>` for pinned and denied
  films, added to `FamilyClosure.blockKeys`. Every pin edge therefore joins two listings of one
  family, and the closure stays complete (§5);
- `mustLinks`: each pinned group as a star from its smallest key, reason `MustLink.Pinned`, to
  be solved as the strongest tier (tier 0);
- `cannotLinks(keys, filmOf)`: `CannotLink.PinnedNotFilm` edges, given the resolver's per-listing
  films;
- `admits(edge)`: pins beat derived rules. A derived cannot-link inside a pinned group is
  dropped, and so is a derived must-link between two differently pinned films or onto a denied
  film.

All of it is a function of the pin set (`PinConstraintsSpec` checks order independence).

**Consistency.** `Pins` (the rules above the `PinStore` seam) refuses a malformed pin (no
listing, a one-listing `SameFilm`, a non-positive TMDB id, no author or reason), a duplicate, and
any pin that makes the set contradict itself: a group pinned to two films, or to a film one of its
listings is pinned never to be. Stores: `MongoPinStore` (collection `identity_pins`, written only
from the admin page) and `InMemoryPinStore`.

`KnownCasePins` (common test sources only) holds realistic pins for the undecidable and
wrong-but-stable cases of §7 (Opętanie "klasyka w 4k", the Met's Samson broadcast, the
Mockingjay – Part 2 (2026) rerelease, the decorated Lalka spellings). It is a test fixture for the
constraint shape and is never written to production.

### 13.2 Confidence-gated ratings

A film whose identity decision is below a threshold is served with no ratings, no IMDb link, the
search page of each other rating site, and the unrated sort key (`RatingGate.withheld`), rather
than ratings that may belong to another film. Title, showtimes and everything the venues
published are untouched.

- **Pure core.** `RatingGate.gate(movie, confidence, calibration)`. No decision or no calibration
  withholds nothing. A film's confidence is the minimum over the decisions its listings belong
  to (`confidenceOf`): a film spanning two clusters is only as sure as the weaker one. The
  listings of a stored row come from its venue slots (`ListingKey.ofSlot`).
- **Calibration, not a constant.** `ConfidenceCalibration.calibrate` takes the labelled shadow
  diff: every decision whose correctness is known, either because the pipeline agrees or because a
  reviewed verdict on the known-issues list (§10) says right or wrong. It picks the cut that
  misclassifies the fewest of them (wrong shown plus right withheld). On a tie it takes the lower
  cut, which withholds less, so with no evidence either way a film keeps its ratings. Without
  labelled data there is no threshold and nothing is withheld. Only the order of confidences
  matters, so the resolver's score does not have to be a probability, and a change in how the
  resolver scores is re-calibrated by the next labelled run. The labelled set comes from the
  shadow diff's categories (§7a): identical films are correct, category-1 splits the pipeline
  got right are wrong, and category 2–4 cases carry their reviewed verdict. The admin view shows
  the resulting cut and its confusion counts.
- **Wiring, off.** `ReadModelProjector(ratingGate = …)` applies the gate to every card it
  projects. The gate's `version` is part of the metadata-reuse key, so a new gate re-projects
  instead of reusing cards gated under the old one. `ReadModelContentAudit` projects through the
  same gate, so a withheld card is not reported as drift. The worker's `ReadModelWiring` passes
  `RatingGate.off` unless `KINOWO_IDENTITY_RATING_GATE=true`
  (`ProcessConfiguration.identityRatingGate`, a typed `IdentityRatingGateEnabled`). When on, it snapshots
  `shadowDecisions` at boot, and that is `ShadowDecisions.none` until the resolver's shadow run
  lands, so even a switched-on gate withholds nothing today. This is a staged-migration switch
  (allowed here per the program brief), to be removed at cutover.
- **Open.** Posters, synopsis and credits also come from the matched film and are equally
  suspect below the threshold. The gate covers ratings and rating links only, as specified.
  Whether to extend it is a question for the cutover phase.

### 13.3 The admin view

`/admin/identity` (web, `IdentityAdminController`): `AdminAction`-gated like `/admin/config`. Its
two pin POSTs are declared `Auth.Admin` / `CrossSiteFilter` in `RouteProtectionMatrixSpec`. It is
a read-only diagnostic of the latest shadow decisions:

- **Contradicted**: decisions with constraint pressure (`Decision.contradictions`: a must-link a
  cannot-link refused, an ambiguous node left alone);
- **Below the rating threshold**: the decisions the calibrated gate would withhold;
- each with the resolver's own `explanation`, plus the current calibration.

Pins can be created (tick listings, or paste listing JSON) and removed there, for emergencies.
There is no queue and nothing expects routine review. The page is covered by
`IdentityAdminControllerSpec` and by `IdentityAdminPageSpec`, which drives the real page script in
Chrome: it pins the ticked listing, removes the pin, and shows a refusal.

### 13.4 Reconciliation with the other phases

- `Decision` and `ShadowDecisions` (common, `services.identity`) are minimal stand-ins. The
  resolver branch owns `Decision`: when it lands, its type replaces or extends this trait (fields
  used: `listings`, `tmdbId`, `confidence`, `explanation`, `contradictions`), and a
  `ShadowDecisions` reading its shadow report replaces `ShadowDecisions.none` in both wirings
  (web `AdminWiring`, worker `ReadModelWiring`).
- The resolver must read pins through `ListingConstraints.pinned` (§13.1): add `blockKeys` to the
  family keys, solve `mustLinks` as tier 0, apply `admits` to its derived edges, and resolve each
  listing through `resolvedFilm`.
