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
  node), writes nothing but its shadow collections, and exports gauges. *Built — see §17*: the
  collections are `identity_shadow_decisions` and `identity_shadow_diff`, and the gauges follow the
  worker's naming (`kinowo_worker_identity_shadow_films{country,relation}`,
  `kinowo_worker_identity_family_crossings{country}`, `kinowo_worker_identity_resolve_seconds{country}`).
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
- Landed (as programme phase 4): the dual write, the backfill tool and the measured ID-seeding
  review. The evidence store is phase 1's observation store. See §16.

### Phase 3: cutover, per country

- Behind a per-country switch that is chosen at the composition root, not tested in business
  code, `AppLoader` wires an `IdentityProjection` in place of `ScrapeLanding` + staging + settle.
  The scrape path writes listings. The resolver runs per touched family, and the projection
  writes films and slots keyed `(FilmId, ListingKey)`.
- The order is ES → DE → UK → US → PL, smallest and cleanest shadow diff first. PL goes last
  because it has the most decorated spellings and is where under-merge costs most.
- *Built, off everywhere — see §18 and `docs/design/identity-cutover-runbook.md`.*

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

The flag is on by default for a dispatch and always on for the nightly cron, and a hermetic leg
replaying a tree recorded with it runs the sweep too (§9a). Once one run with it has pinned its
pair, the phase-1 gate (`IdentityQueryCoverageIntegrationSpec`, §9a) should report 0 gaps on all
five countries.

---

## 9a. Phase 1 (programme): observations

The programme's phase 1 (the brief's numbering; §8's "Phase 1: shadow mode" is the resolver's
own shadow run, which reads what this phase stores) turns the evidence into data.

### What is stored

`services.observations` (worker). Two kinds of observation, both immutable and timestamped:

| kind | key | content | collection |
|---|---|---|---|
| listing | `ListingKey` (§4) | the listing as the venue published it, WITHOUT showtimes | `obs_listings` |
| lookup | `LookupQuery`: method, credential-masked URL, POST-body fingerprint | the answer: a body, bytes, or a failure with its status | `obs_lookups` |

- `LookupQuery` is the same key the recorded trees' verdict cache and `RecordedResponses` use
  (`CachingEnrichmentFetch.keyOf` now delegates to it, and the cache's `CachedResponse` IS
  `LookupAnswer`), so an observation, a fixture and a remembered verdict of one request are one
  key.
- A venue's per-film detail is a lookup keyed `DETAIL <page> <venue>`, holding the parsed
  `FilmDetail` (some venues assemble a detail from several requests).
- Showtimes are not evidence of which film a listing is and change daily: they stay in
  `cinema_scrapes`, and phase 4 keys them by `ListingKey`.

`ObservationStore` owns every rule, above a storage seam with a Mongo and an in-memory backend
(`ObservationStoreBehaviour` runs the same cases over both):

- a new observation is written only when the content differs from the key's current one; the
  same answer seen again restamps `lastSeenAt`. Content is never rewritten.
- A transient failure (timeout, 5xx, 429) never supersedes a definitive answer — a failed read
  is not data. It is kept when nothing better is known, so the question is on record.

### Capture

One decorator per seam, generic over everything that passes it — no per-source or per-venue
code: `ObservingHttpFetch` on `identityLookupFetch` (the enrich-phase chain under the TMDB client, the
one external client the resolver's `TmdbIdentityLookups` asks), `ObservingDetailEnricher` on every `DetailEnricher`, and
`ObservingScrapeArchive` on the runner's archive. Each returns or rethrows exactly what it wraps,
and a store failure never fails the observed call.

Scoped by WIRING, not by host: every rating, metadata and id-crosswalk client (Metacritic, Rotten
Tomatoes, IMDb, Filmweb, OMDb, Letterboxd, Wikidata, Cinemeta) draws from the unobserved
`enrichmentFetch`. Rating pages are per-film enrichment (§2) and were ~80% of the bytes the
unscoped capture wrote (kinowo_de, first hour: Metacritic 44 KB and RT 25 KB per page, against
TMDB's 3 KB). `tools.PurgeNonIdentityObservations` (dry run unless `--apply`) removes what the unscoped capture
filed.

`KINOWO_OBSERVATION_CAPTURE=true` turns it on (a staged-migration switch at the composition root,
off by default). `ObservationCaptureEndToEndSpec` boots the recorded corpus with capture on and
requires `expected-schedules.txt` and the read-model snapshot to come out exactly as with it off;
a capture that changes one exception type fails it.

### Retention

One rule for every observation, with one number derived from the pipeline, not chosen:

- `LongestReaskPeriod` = the longest of the freshness windows (`Freshness.ttlFor`) and the
  rating cadence's ceiling (`RatingCadence.MaxInterval`): 4 days today.
- `Window` = 2 × that = 8 days, so one missed cycle (a deploy, an open breaker) never expires a
  live key.
- The current observation of a key expires a window after it was last OBSERVED or last READ.
  A read renews: the shadow resolver re-reads each live family's lookups every tick, so a TMDB
  answer the pipeline never re-asks (the resolution memo is permanent) stays as long as a live
  listing needs it, and ages out a window after the last listing that needed it left.
- A superseded observation expires a window after its replacement, so every change can be
  compared with what it replaced for a full re-ask cycle.
- The store stamps `expireAt`; Mongo's TTL index (`expireAfterSeconds = 0`, reconciled by
  `MongoTtlIndex`) deletes. The window lives in the data, so changing the rule never rebuilds
  the index. No job, no manual step. Reads filter by the same stamp, so TTL lag is invisible.

Volume, estimated from the recorded trees, which are one full lookup set per country: 0.3–2.5 GB
uncompressed, about 4.5× smaller gzipped (ES: 320 MB, 5,410 responses → 71 MB). So the current
lookups are roughly 70–550 MB per country, plus superseded versions for one window. Capture
should be turned on one worker at a time, ES first, watching `obs_lookups`' storage size.

### The gate

`IdentityQueryCoverage` measures, per corpus, how much of the resolver's query set the recorded
answers serve. The query set is `IdentityLookupSweep`'s, a function of the listing set alone:
every listing's own detail page, and the TMDB resolve of every distinct evidence.

- A logical lookup is answerable when every HTTP request it made was served; a remembered 404
  counts.
- The gate is met at 100% of lookups on every corpus.
- `IdentityQueryCoverageIntegrationSpec` (itAll) runs it on the five hard-cluster corpora
  always, and on the five full corpora when `KINOWO_IDENTITY_FULL` names them. `KINOWO_IDENTITY_GATE=strict`
  fails on any gap.

On 2026-09-26, against recorder run 36153174348's trees:

| corpus | lookups answerable | requests served | gaps |
|---|---|---|---|
| hc-pl | 174 / 183 (95.1%) | 280 / 289 | 9, all TMDB searches |
| hc-uk, hc-de, hc-us, hc-es | 100% | 100% | 0 |
| full-pl | 6,031 / 6,242 (96.6%) | 8,188 / 8,388 | 200: TMDB 185, kinonh.pl 13, kinopodbaranami.pl 2 |
| full-uk | 6,781 / 6,793 (99.8%) | 8,518 / 8,535 | 17, all TMDB |
| full-de | 1,695 / 1,697 (99.9%) | 9,915 / 9,917 | 2: TMDB person searches (Bryan Coyne, SABU) |
| full-us | 2,507 / 2,524 (99.3%) | 11,839 / 11,857 | 18, all TMDB |
| full-es | 237 / 237 (100%) | 1,451 / 1,451 | 0: gate met |

Every gap is a query the resolver makes and the pipeline never did. PL, DE and US match the
proof's counts. UK does not: the proof counted 123 Cineworld box-office detail requests as gaps,
but they are held. The recording answered them 403 (the API refuses a CI runner), and the recorded
chain files that verdict under the request's BYTES key. The proof's chain had no recorder layer,
so it never read them. Being held is not the same as being evidence: a remembered failed read
replays deterministically and still says nothing about the film. The gate prints those
separately ("served by a remembered failed read"). UK's 123 can only be closed by recording
Cineworld from an IP it serves. No recording from a runner can close them.

### Keeping it met, automatically

- `Record scrape fixtures` runs the sweep in every recording leg: always on the nightly cron,
  and by default on a dispatch. A recording files each gap's answer into the tree it pins.
  `IdentityQueryCoverageIntegrationSpec` checks, with `KINOWO_IDENTITY_RECORD_CHECK=<cc>` over a
  scratch copy of a tree, that the recording leg's own chain asks the service for every gap and
  for nothing already recorded, and that the gate is then met. On a clone of PL's tree it asked
  the 200 gaps plus 3 follow-ons (two TMDB person credits and one search, reachable only once a
  gap is answered), and the gate then read 6,242 / 6,242.
- A recording that ran the sweep leaves `.identity-lookups` at its tree's root. A hermetic
  verdict leg replaying such a tree runs the sweep too, and fails on any gap by name. So every
  verdict leg enforces the gate from the first pinned recording on, and no leg is failed for
  replaying a tree recorded before the sweep existed.
- The hard-cluster responses are recorded from the trees (`scripts/hard-clusters.sh record`),
  and that record mode now asks the sweep's queries as well. Once a tree recorded with the sweep
  is pinned, one re-record closes hc-pl's 9.
- The resolver's round-2 pooled queries (§7b) join the gate by joining `IdentityLookupSweep`.

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
8. **The US shadow diff** — *answered in §15: booted the convergence-leg way (`bootCorpus`), US takes 342–583 s.* Originally: the diff was missing; ES has landed (236 of 236 identical). The replay boot
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

- **Scored from the row's own evidence.** `RatingGate.fromEvidence(IdentityCalibration.default)`
  reads each stored row through `StoredIdentityConfidence`: the TMDB slot is the film, each venue
  slot (with its detail-page facts) a listing, measured by the calibration's own
  `IdentityMeasures.listingFilm` — title, original title, year, director, runtime, country, and
  `venues.corroborating` over the row's venues sharing the title key — and the film's standing in
  each listing title's own yearless TMDB search (`search.rank`, `rivals`), which the resolution
  records on the TMDB slot (`SourceData.titleSearches`, `IdentityMeasures.titleSearch`, in TMDB's
  own result order as the calibration measured it; `backfill-title-searches.js` fills rows resolved
  before). A title with no stored search leaves both out; popularity is never used. The search
  standing can lend a listing confidence, never withdraw it (the larger of the probability with and
  without it — the resolver's own rule for vetoes, `factsProbability`, §14.6). The film's
  confidence is its best-evidenced listing's `IdentityCalibration.probability`; below the
  artefact's `showRatings` threshold (fitted, §14) the card is withheld. `tmdbBasis` is never read — most rows predate it. A row with no TMDB slot
  or no venue slot has nothing to measure and keeps its card. So a title-only match stays a match,
  and keeps its ratings exactly when some venue's facts back it: Kino 1410's "Samson i dalila |
  metropolitan opera: live in hd 2026/27" (no year, director or runtime, against DeMille's 1949
  film) is withheld; a Back to the Future III whose venues state 1990, 118 min and Zemeckis is
  shown; an exact title with no other fact scores ≈0.18 and is withheld unless its own search
  returned it first with no same-titled rival (then ≈0.5, shown), as a namesake would otherwise
  have the same evidence (`StoredIdentityConfidenceSpec`).
- **Why the search standing, measured** (`scripts.IdentitySearchEvidence`, labelled units replayed
  through run 36224654409's recorded searches, the listing stripped to its title): rank 1 with no
  rival under an exact title is 2,697 corroborated units against 1 contradicted; the same under a
  banner segment 387 against 18. Shown on a bare title: 61.7% of corroborated units and 2.1% of
  contradicted with rank and rivals, 9.4% and 0% without. Stable where §14.6 said the pooled
  weights are not: k = 3 labels 69.6%, labels not using other venues 69.0%, held-out 63.4%
  (contradicted held-out 1 of 12). Popularity adds nothing (60.5%). With full listing evidence the
  lend-never-withdraw rule shows 99.7% of corroborated units (99.5% without the search) and the
  same 1 of 47 contradicted.
- **Why not the shadow decisions.** The first cut gated on the shadow resolver's persisted
  decisions, with a threshold `ConfidenceCalibration` fitted from its labelled diff. Those
  decisions were not persisted then (`ShadowDecisions.none`), so a switched-on gate withheld nothing.
  `ConfidenceCalibration` now only draws the admin view's low-confidence line (§13.3).
- **Measure before switching.** `scripts.IdentityGateImpact` replays a read-only export of
  production rows through the same gate (with `--trees`, measuring rows that store no search from
  the recorded answers — the backfill's plan) and reports per country what it would hide, judged
  against the labelled set. 2026-09-26, listed rated films, hidden (false / true by label,
  unlabelled), then a hand spot-check of the unlabelled hides (right / wrong / unsure):
  PL 64/709 (0/1/63; 17/10/3 of 30), UK 21/1375 (0/2/19; 11/5/3 of 19), DE 8/1545 (0/1/7; 4/3/0),
  US 40/2136 (0/1/39; 19/3/8 of 30), ES 2/209 (0/0/2; 1/1/0). Without the search standing:
  PL 199, UK 33, DE 9, US 86, ES 2. Estimated false hides: PL ≈5% of rated films, US ≈1.2–1.7%,
  UK ≈0.9–1.0%, DE 0.3%, ES 0.5% — PL and US stay above today's 0.97% wrong rate. What remains
  hidden and right: PL series banners whose segment keeps the bracketed year ("Akademia Kina
  Polskiego: Psy (1992)" measures `contains` and is searched with the year inside, so TMDB returns
  nothing); US repertory titles with same-titled rivals (Free Willy, Frailty), where the labels put
  the top hit right about 91% of the time — hiding them is what the 0.97% target asks; re-release
  years ("25 Aniversario", "20 Jähriges Jubiläum") measured as a year 20+ off. Rows with a tmdbId
  but no stored TMDB slot (PL 15, UK 26, DE 52) are not scored.
- **Wiring, off.** `ReadModelProjector(ratingGate = …)` applies the gate to every card it
  projects. The gate's `version` is part of the metadata-reuse key, so a new gate re-projects
  instead of reusing cards gated under the old one. `ReadModelContentAudit` projects through the
  same gate, so a withheld card is not reported as drift. The worker's `ReadModelWiring` passes
  `RatingGate.off` unless `KINOWO_IDENTITY_RATING_GATE=true`
  (`ProcessConfiguration.identityRatingGate`, a typed `IdentityRatingGateEnabled`), and
  `RatingGate.fromEvidence` when it is. The switch is read per worker process, so it is flipped
  per country in that country's worker env. This is a staged-migration switch (allowed here per
  the program brief), to be removed at cutover.
- **Open.** Posters, synopsis and credits also come from the matched film and are equally
  suspect below the threshold. The gate covers ratings and rating links only, as specified.
  Whether to extend it is a question for the cutover phase.

### 13.3 The admin view

`/admin/identity` (web, `IdentityAdminController`): `AdminAction`-gated like `/admin/config`. Its
two pin POSTs are declared `Auth.Admin` / `CrossSiteFilter` in `RouteProtectionMatrixSpec`. It is
a read-only diagnostic of the latest shadow decisions:

- **Contradicted**: decisions with constraint pressure (`Decision.contradictions`: a must-link a
  cannot-link refused, an ambiguous node left alone);
- **Below the rating threshold**: the decisions below the cut `ConfidenceCalibration` fits from the labelled shadow diff;
- each with the resolver's own `explanation`, plus the current calibration.

Pins can be created (tick listings, or paste listing JSON) and removed there, for emergencies.
There is no queue and nothing expects routine review. The page is covered by
`IdentityAdminControllerSpec` and by `IdentityAdminPageSpec`, which drives the real page script in
Chrome: it pins the ticked listing, removes the pin, and shows a refusal.

### 13.4 Reconciliation with the other phases

- `Decision` and `ShadowDecisions` (common, `services.identity`) are minimal stand-ins. The
  resolver branch owns `Decision`: when it lands, its type replaces or extends this trait (fields
  used: `listings`, `tmdbId`, `confidence`, `explanation`, `contradictions`), and a
  `ShadowDecisions` reading its shadow report replaces `ShadowDecisions.none` in the web
  `AdminWiring` (the worker's rating gate scores stored evidence instead, §13.2).
- The resolver must read pins through `ListingConstraints.pinned` (§13.1): add `blockKeys` to the
  family keys, solve `mustLinks` as tier 0, apply `admits` to its derived edges, and resolve each
  listing through `resolvedFilm`.
  *Done on `identity-resolver` (§15):* `ResolverDecision` implements `Decision`, and
  `IdentityResolver.resolve(…, pins = ListingConstraints.pinned(pins))` does all four (a pinned
  listing's decision has basis `Pinned` and confidence 1).
  *Done since (§17):* the shadow run persists each run's decisions (`ShadowRunStore`), the web
  `AdminWiring` reads them back through `ShadowDecisions`, and `ShadowDecisions.none` is gone.

---

## 14. Calibration

Status: 2026-09-26, branch `identity-calibration`. Every weight, bin edge, probability map,
threshold and cannot-link rule the resolver scores with is DATA, fitted from existing films by a
reproducible script. Nothing in it is hand-tuned; §14.7 lists the constants that remain.

### 14.1 What ships

| artefact | what |
|---|---|
| `common/src/main/resources/identity-weights.json` | per scope (`listing-film`, `listing-listing`): a prior and one log-likelihood-ratio weight per signal value (numeric signals in data-derived bins), an isotonic calibration map, the show-ratings and cannot-link thresholds with their held-out errors; plus the learned cannot-link rules with their measured false-veto rates; plus provenance |
| `test/resources/fixtures/identity/identity-labels.json.gz` | the labelled set (142,503 listings: corroborated or contradicted, with split and family) and the held-out listing pairs; the resolver's benchmark reads only `split == "test"` |
| `services.identity.IdentityMeasures` | the measurements (pure, primitives in): title relation, original-title relation, signed year delta, director relation, runtime delta, country, search rank, popularity, rivals, corroborating venues; listing-listing adds venue and chain id. Both the calibration and the resolver call it, so the fitted weights and the scored values are one definition |
| `services.identity.IdentityCalibration` | loads the artefact and evaluates it: `logOdds`, `probability`, `explain`, `showsRatings`, `forbidsLink`, `cannotLink` (a generic evaluator of the rules as data) |
| `scripts/identity-calibrate.sh` → `worker/Test/runMain scripts.IdentityCalibrate` | regenerates all of the above, the report below, and the healing lists |
| `IdentityCalibrationSpec` | the artefact loads, is internally sound, and puts the historical cases on the right side |

Regenerate (≈ 7–20 min, 12 GB heap):

```
CORPORA=<dir of cinema-scrapes-<cc>.json.gz> FIXTURES=<dir of enrichment-<cc>/> PROD=<snapshot dir> \
  scripts/identity-calibrate.sh [--extract-prod]
```

`--extract-prod` first re-reads production (read-only `find()` over `movies` and `movie_slots` of
every country database, `scripts/identity-calibrate/extract-prod.js`, through the local prod tunnel).
The numbers below used recorder run 36153174348 (five full corpora and their enrichment trees),
the hard-cluster corpora and responses, and a production snapshot of 2026-09-26.

### 14.2 Labels: production is a proposal, never ground truth

A listing's production tmdbId becomes a **positive** only when the listing's own evidence
corroborates it on at least k = 2 of:

- the venue's year within ±1 of TMDB's;
- a director TMDB credits (same person, name order ignored);
- the venue's original title naming the film;
- two or more OTHER venues listing the same title whose own year or director back the film and
  deny it on nothing (same title only: a production film that already merged two remakes would
  otherwise vouch for its own mistake);

and nothing denies it (a year off by 2+, a director nobody in the credits, an original title
sharing no word, or a runtime whose own likelihood ratio says "different film" at 19:1 or more).
Two or more denials mark the filing **contradicted**: listed for healing, never used. Everything
else is unlabelled and unused. **Negatives** are the other films the listing's own title search
returned (hard negatives: remakes, namesakes, sequels), plus films the recorded answers know under
one of the listing's title shapes, for listings whose positive is corroborated.

The IMDb cross-link is **not** a corroborator: production's imdbId equals TMDB's own external id
for 99.9% of films (5,973 of 5,978 checked), so it is derived, not independent. The 5 that differ
are in the cross-check list.

**No circularity.** A signal's table is fitted on labels that do not use that signal (leave one
corroborator out): the year table is fitted on films corroborated by director, original title and
venues only. That is naive Bayes' own assumption (signals independent given the film). A
cannot-link rule's false-veto rate is measured on labels that use none of its signals, with one
remaining corroborator enough (weaker labels can only overstate false vetoes). Historical cases
are test labels only; the script never reads them.

**Units.** A wide release lists one film thousands of times with the same evidence. Counts, bin
tests, calibration, false-veto bounds and the wrong rate all count distinct **units** (family and
candidate film), not listings.

**Splits.** A family (listings joined by title key or by production film) goes wholly to one
split by `murmur3(smallest ListingKey) mod 10`: 0–4 train (weights, bins, rules), 5–6 calibration
(isotonic map, thresholds), 7–9 held out (every number reported).

### 14.3 Model

Naive Bayes: `logOdds = prior + Σ LLR(signal value)`, with Jeffreys smoothing. A numeric signal's
bins start at every observed integer and merge adjacent bins while a G-test (5%) cannot tell their
ratios apart. Missing evidence is its own value per side (`missing:listing`, `missing:film`); four
missing values are forced to 0 because they mark how the recorded data was SAMPLED, not the film
(credits, runtime and country of candidates whose details were never fetched; a positive absent
from the title search), and the artefact says so beside each. The summed log-odds go through an
isotonic map fitted on the calibration split.

### 14.4 Thresholds, from data

- **Today's wrong rate**: 60 contradicted of 6,217 decisive production filings per unit =
  **0.97%** (1,471 of 142,503 listings; the opera relays dominate by listing count).
- **Show ratings**: p ≥ **0.3575**, the lowest cut whose wrong share on the calibration split has
  a one-sided 95% upper bound within today's 0.97%. Held out: 49 wrong of 4,361 shown units
  (1.12%, within noise of the target), recall 99.9%, 1 of 18 contradicted production filings still
  shown; per decision (the top candidate per family and film) 1 wrong of 4,313. Stricter cuts are
  tabulated below (p ≥ 0.9: 4 wrong of 4,276 shown, 0.09%).
- **Cannot-link by score** (certified: below every same-film unit of the calibration split):
  listing-film p < 0.0848 (held out 0 false vetoes, 95.5% of different-film units vetoed);
  listing-listing p < 0.0184 (held out 0.05%, 62.7% vetoed).
- **Cannot-link rules**: a conjunction of one or two signal conditions becomes a cannot-link only
  if it fired on **no** same-film unit of the fitting splits and fires significantly more on
  different films. Every shape searched is a hypothesis, so each is bounded at a
  Bonferroni-corrected confidence; the resulting certified bound (≈ 0.19–0.22%) is the tightest
  the ~4,300 independent same-film units can prove. Everything that does not pass stays an
  ordinary negative weight. 12 listing-film and 14 listing-listing rules pass; held-out false
  vetoes 0–0.17% each.

### 14.5 Today's vetoes, re-measured

| veto | false vetoes (same film) | true vetoes | verdict |
|---|---|---|---|
| VenueDeniesFilm / DeniedCandidate (year > 5 AND director apart) | 4 of 6,159 units (all DE) | 6.1% | not certified; within today's wrong rate → a negative weight, or the learned `director in {different} AND year.distance >= 32` |
| ListingDeniesFilm / DecorationVeto (director apart AND runtime > 2 or year > 5) | **44 of 6,159 (0.71%)**, PL 5/725, DE 19/1,629 | 7.5% | the loosest of today's vetoes: blocks true matches at 0.7%; the runtime > 2 arm is what over-fires (venues print runtimes with trailers or cuts). Replace with its certified cousins (`director different AND runtime >= 41`-class rules, or leave to the score) |
| OriginalTitleNamesAnotherFilm | 1 of 4,396 | 13.1% | nearly certified; the learned `originalTitle disjoint AND year.distance >= 2` covers it |
| Faust fold refusal (containment + venue denial) | 0 of 6,159 | 2.7% | **certified** |
| CinemasDescribeDifferentFilms | 1 of 4,187 | 47.5% | nearly certified |
| VenueCreditsApart | 2 of 5,849 | 16.6% | not certified alone; `director in {different} AND venue in {same}` and `runtime.delta >= 1 AND venue in {same}` are |
| Bare-listing home (must-link) | 459,916 of 460,191 exact-title pairs with a bare side are one film (99.94%) | – | supported as a strong positive, not a must-link: 0.06% are different films |

Conditions none of today's vetoes cover, all certified: a title sharing no word with the film
beside a year off by 2+ (`title in {none,overlap} AND year.distance >= 3`: 33% of different-film
units; today's vetoes catch about 11% of those pairs); a year 49+ years off on its own; two listings with
different chain film ids and differing runtimes or titles; two listings at the SAME venue whose
runtime or year differs at all (`runtime.delta >= 1 AND venue in {same}`: 26% held out, 0 false).

### 14.6 What the data says is useless or misleading

- **IMDb cross-link**: not independent (§14.2); dropped.
- **Pooled and search signals** (`venues.corroborating`, `rivals`, `search.rank`, `popularity`)
  are label-selection sensitive: with a stricter bar (k = 3) several of their bins change sign,
  within a single country too, while the evidence signals do not (title, director, original title
  and country keep every sign; see the sensitivity table). Their weights are real on the k = 2
  labels but should not be trusted beyond them; the resolver should cap their influence or refit
  them on its own shadow decisions.
- **Director "different"** is weaker than today's rules assume across a listing and a film
  (LLR −3.4: venues print the writer), but strong between two listings (−5.2).
- **Runtime** is the single strongest listing-film signal (Δ ≤ 1 min: +5.7) and **year agreement**
  (±1: +3.1), then **same director** (+5.3); an **exact title** is worth only +1.3, because the hard
  negatives share it.
- **Sensitivity** (k = 3 against k = 2): listing-film correlation 0.80 (41 of 51 weights keep their
  sign), listing-listing 0.93. The movement is concentrated in the pooled/search signals and in
  sparse year/runtime bins; the k = 3 set is 51% DE/ES (against 20% at k = 2), so part of it is composition. The evidence
  signals are stable, the pooled ones are not: labels are sound for the former and
  selection-biased for the latter.

### 14.7 Remaining constants, with a general replacement each

| constant | where | why it is there | general replacement |
|---|---|---|---|
| corroboration bar k = 2, year ±1 | label definition | the user-specified bar; k = 3 reported as sensitivity | iterate: relabel with the fitted model's own posterior (EM / Fellegi–Sunter) |
| runtime denial at LR ≤ 1:19 | label definition | the 95% convention as a likelihood ratio | same as above |
| G-test 5%, Wilson 95%, Bonferroni 5% | bins, bounds, rules | statistical conventions, not domain constants | none needed |
| Jeffreys α = 0.5 | smoothing | standard non-informative prior | none needed |
| name words ≥ 3 letters (shared name), original-title words ≥ 4 | `IdentityMeasures` | what counts as a shared word | learn a word-weight table (IDF over the corpus) |
| delimiter set of banner segments | `SearchTitles.candidates` | reused, not new | learn segments from co-occurring spellings |
| ≤ 20 listing pairs per member in blocks over 41 | pair sampling | cost | none needed: sampling does not bias a unit-counted estimate |
| 4 neutral missing values | fitting | sampling artefacts of the recorded trees | fetch every candidate's details in the recording pass (§9), then fit them |

### 14.8 Findings for healing (production, read-only)

- `contradicted-prod-resolutions.tsv` (scratchpad): 1,471 listings on 59 films production files on a
  film their own evidence denies. The largest: the Met's and a German MET relay of *Macbeth* on
  NT Live's / a 2025 *Macbeth*, the RBO's *Carmen* on Millepied's 2023 film, *Candyman (1992)* and
  *(2021)* on each other, *Psycho (1998)*, *Halloween (2018/2007)*, *Ghostbusters (2016)* on the
  originals, *Avengers: Koniec gry* on Falk's 1992 *Koniec gry*, *Zärtlich kreist die Faust* on
  Murnau's *Faust*, *Sense and Sensibility (1995)* on the 2026 film.
- `prod-cross-check-mismatches.tsv`: 5 imdbIds that are not TMDB's and 54 rating pages whose slug
  year disagrees with the film (e.g. *Bogaci i martwi* 2026 → Metacritic `sacrifice-2021`).
- Not caught by the contradiction rule but visible in the held-out errors: production files
  *Lalka (ale to horror)* (2025, 82 min) under *Lalka* (2026, 162 min).

### 14.9 The generated report

The script writes this section's numbers to `calibration-report.md` (plus `.json` with the
reliability data and `held-out-errors.tsv`); the copy below is the run the artefact came from.

#### Label counts

| country | listings | filed on a production tmdbId | with TMDB details | corroborated (k=2) | corroborated (k=3) | contradicted | unlabelled |
|---|---|---|---|---|---|---|---|
| pl | 11811 | 11056 | 10862 | 6382 | 2605 | 9 | 5420 |
| uk | 33456 | 28488 | 28064 | 18436 | 5272 | 13 | 15007 |
| de | 23540 | 23077 | 22755 | 22390 | 21890 | 175 | 975 |
| us | 105126 | 97226 | 96954 | 88719 | 17866 | 273 | 16134 |
| es | 5312 | 5229 | 5186 | 5123 | 4889 | 2 | 187 |
| all | 179245 | 165076 | 163821 | 141032 | 52520 | 1471 | 36742 |

Today's measured wrong rate: 60 contradicted of 6217 decisive production filings, counted per family and film = 0.97% (per listing: 1471 of 142503).

#### Listing ↔ film signal tables

**title** (fitted on 4216 same / 3235 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| original | 306 | 10 | +3.11 |
| alternative | 420 | 33 | +2.27 |
| exact | 3100 | 630 | +1.33 |
| segment | 329 | 264 | -0.04 |
| overlap | 37 | 932 | -3.48 |
| none | 5 | 248 | -4.08 |
| contains | 19 | 1118 | -4.31 |

**originalTitle** (fitted on 5352 same / 4488 different pairs, labels without `originalTitle`)

| value | same | different | log-LR |
|---|---|---|---|
| match | 3082 | 551 | +1.54 |
| disjoint | 32 | 607 | -3.10 |
| overlap | 62 | 1634 | -3.44 |
| missing:listing | 2176 | 1696 | +0.07 |

**year.delta** (fitted on 4860 same / 4306 different pairs, labels without `year`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ -8 | 0 | 866 | -7.58 |
| -8 … -3 | 2 | 141 | -4.16 |
| -3 … -2 | 19 | 35 | -0.72 |
| -2 … -1 | 294 | 58 | +1.50 |
| -1 … 1 | 3073 | 128 | +3.05 |
| 1 … 2 | 19 | 51 | -1.09 |
| 2 … 8 | 2 | 319 | -4.97 |
| ≥ 8 | 0 | 1114 | -7.83 |
| missing:film | 3 | 154 | -3.91 |
| missing:listing | 1448 | 1440 | -0.12 |

**director** (fitted on 3283 same / 2994 different pairs, labels without `director`)

| value | same | different | log-LR |
|---|---|---|---|
| same_person | 3079 | 14 | +5.27 |
| shared_name | 63 | 2 | +3.14 |
| incomparable | 1 | 2 | -0.60 |
| different | 7 | 204 | -3.40 |
| missing:film | 18 | 2429 | +0.00 (neutral) |
| missing:listing | 115 | 343 | -1.18 |

**runtime.delta** (fitted on 5045 same / 3398 different pairs, labels without `runtime`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 1 | 2863 | 6 | +5.69 |
| 1 … 2 | 906 | 15 | +3.67 |
| 2 … 7 | 747 | 62 | +2.09 |
| 7 … 12 | 125 | 37 | +0.81 |
| 12 … 36 | 98 | 145 | -0.78 |
| 36 … 37 | 0 | 10 | -3.44 |
| 37 … 51 | 18 | 32 | -0.96 |
| 51 … 56 | 0 | 16 | -3.89 |
| 56 … 64 | 6 | 14 | -1.20 |
| 64 … 75 | 0 | 8 | -3.23 |
| 75 … 78 | 1 | 0 | +0.70 |
| ≥ 78 | 0 | 33 | -4.60 |
| missing:film | 40 | 2433 | +0.00 (neutral) |
| missing:listing | 241 | 587 | -1.28 |

**country** (fitted on 5737 same / 4465 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| match | 3047 | 156 | +2.72 |
| mismatch | 20 | 76 | -1.57 |
| missing:film | 10 | 2391 | +0.00 (neutral) |
| missing:listing | 2604 | 1808 | +0.11 |
| missing:unmapped | 56 | 34 | +0.24 |

**search.rank** (fitted on 3240 same / 2730 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 2 | 327 | 66 | +1.42 |
| 2 … 3 | 28 | 194 | -2.09 |
| 3 … 9 | 14 | 938 | -4.34 |
| ≥ 9 | 4 | 1410 | -5.92 |
| missing:not-returned | 2867 | 122 | +0.00 (neutral) |

**popularity.log2** (fitted on 3115 same / 2720 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ -3 | 7 | 2 | +0.96 |
| -3 … -2 | 9 | 130 | -2.76 |
| -2 … -1 | 108 | 653 | -1.93 |
| -1 … 1 | 471 | 941 | -0.83 |
| 1 … 2 | 666 | 486 | +0.18 |
| 2 … 4 | 1310 | 332 | +1.24 |
| 4 … 7 | 517 | 175 | +0.95 |
| ≥ 7 | 27 | 1 | +2.77 |

**rivals** (fitted on 3229 same / 3405 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 1 | 2992 | 531 | +1.78 |
| 1 … 3 | 157 | 1297 | -2.06 |
| ≥ 3 | 80 | 1577 | -2.92 |

**venues.corroborating** (fitted on 3904 same / 2811 different pairs, labels without `venues`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 1 | 919 | 2587 | -1.36 |
| 1 … 2 | 857 | 170 | +1.29 |
| 2 … 3 | 1072 | 14 | +3.98 |
| 3 … 16 | 743 | 20 | +3.26 |
| 16 … 24 | 69 | 8 | +1.77 |
| 24 … 34 | 30 | 0 | +3.78 |
| 34 … 35 | 2 | 2 | -0.33 |
| 35 … 46 | 47 | 0 | +4.23 |
| 46 … 48 | 1 | 2 | -0.84 |
| 48 … 186 | 76 | 1 | +3.60 |
| 186 … 212 | 12 | 3 | +0.95 |
| 212 … 618 | 51 | 0 | +4.31 |
| 618 … 631 | 1 | 1 | -0.33 |
| 631 … 1222 | 14 | 0 | +3.04 |
| 1222 … 1406 | 1 | 3 | -1.17 |
| ≥ 1406 | 9 | 0 | +2.62 |

#### Listing ↔ listing signal tables

**title** (fitted on 3601 same / 249 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| exact | 2809 | 31 | +1.83 |
| segment | 748 | 36 | +0.36 |
| none | 4 | 1 | -1.56 |
| contains | 26 | 16 | -2.19 |
| overlap | 14 | 165 | -5.10 |

**originalTitle** (fitted on 5158 same / 188 different pairs, labels without `originalTitle`)

| value | same | different | log-LR |
|---|---|---|---|
| match | 2548 | 22 | +1.43 |
| disjoint | 251 | 25 | -1.01 |
| overlap | 63 | 44 | -2.94 |
| missing:film | 158 | 13 | -0.84 |
| missing:listing | 2138 | 84 | -0.07 |

**year.delta** (fitted on 4486 same / 251 different pairs, labels without `year`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 1 | 2627 | 14 | +2.33 |
| 1 … 2 | 292 | 9 | +0.56 |
| 2 … 3 | 16 | 5 | -1.77 |
| 3 … 9 | 3 | 25 | -4.86 |
| ≥ 9 | 0 | 61 | -7.68 |
| missing:film | 110 | 36 | -1.76 |
| missing:listing | 1438 | 101 | -0.22 |

**director** (fitted on 2998 same / 211 different pairs, labels without `director`)

| value | same | different | log-LR |
|---|---|---|---|
| same_person | 2759 | 12 | +2.76 |
| shared_name | 63 | 0 | +2.20 |
| incomparable | 19 | 0 | +1.02 |
| different | 7 | 94 | -5.17 |
| missing:film | 44 | 33 | +0.00 (neutral) |
| missing:listing | 106 | 72 | -2.26 |

**runtime.delta** (fitted on 4759 same / 337 different pairs, labels without `runtime`)

| value | same | different | log-LR |
|---|---|---|---|
| ≤ 1 | 2590 | 8 | +3.09 |
| 1 … 2 | 810 | 10 | +1.72 |
| 2 … 7 | 778 | 49 | +0.13 |
| 7 … 12 | 124 | 35 | -1.37 |
| 12 … 19 | 59 | 54 | -2.54 |
| 19 … 21 | 25 | 3 | -0.64 |
| 21 … 25 | 17 | 17 | -2.63 |
| 25 … 29 | 1 | 20 | -5.24 |
| 29 … 52 | 32 | 46 | -2.99 |
| 52 … 56 | 0 | 18 | -6.24 |
| 56 … 66 | 8 | 11 | -2.93 |
| ≥ 66 | 1 | 35 | -5.79 |
| missing:film | 121 | 12 | +0.00 (neutral) |
| missing:listing | 193 | 19 | -0.33 |

**venue** (fitted on 2929 same / 285 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| different | 2925 | 198 | +0.36 |
| same | 4 | 87 | -5.29 |

**chainId** (fitted on 4037 same / 260 different pairs, labels without `-`)

| value | same | different | log-LR |
|---|---|---|---|
| same | 1116 | 0 | +4.97 |
| different | 2 | 62 | -5.96 |
| missing:no-shared-namespace | 2919 | 198 | -0.05 |

#### Calibration, listing ↔ film (held-out split: 7608 units, 4492 same)

AUC 0.9999, Brier 0.0041, log-loss 0.0206.

| predicted bin | units | mean predicted | observed same |
|---|---|---|---|
| 0.0–0.1 | 3021 | 0.018 | 0.000 |
| 0.1–0.2 | 18 | 0.148 | 0.056 |
| 0.2–0.3 | 21 | 0.245 | 0.143 |
| 0.3–0.4 | 14 | 0.337 | 0.000 |
| 0.4–0.5 | 4 | 0.450 | 0.250 |
| 0.5–0.6 | 34 | 0.531 | 0.147 |
| 0.6–0.7 | 9 | 0.664 | 0.556 |
| 0.7–0.8 | 6 | 0.752 | 0.833 |
| 0.8–0.9 | 30 | 0.870 | 0.800 |
| 0.9–1.0 | 4451 | 0.996 | 0.999 |

| threshold | precision | recall | wrong among accepted |
|---|---|---|---|
| 0.50 | 0.9903 | 0.9987 | 44 of 4530 |
| 0.80 | 0.9978 | 0.9953 | 10 of 4481 |
| 0.90 | 0.9991 | 0.9900 | 4 of 4451 |
| 0.95 | 0.9998 | 0.9722 | 1 of 4368 |
| 0.98 | 1.0000 | 0.9570 | 0 of 4299 |
| 0.99 | 1.0000 | 0.9132 | 0 of 4102 |

#### Calibration, listing ↔ listing (held-out split: 5348 units, 4959 same)

AUC 0.9995, Brier 0.0037, log-loss 0.0164.

| predicted bin | units | mean predicted | observed same |
|---|---|---|---|
| 0.0–0.1 | 348 | 0.014 | 0.011 |
| 0.1–0.2 | 19 | 0.154 | 0.158 |
| 0.2–0.3 | 16 | 0.253 | 0.063 |
| 0.3–0.4 | 6 | 0.331 | 0.833 |
| 0.4–0.5 | 7 | 0.437 | 0.571 |
| 0.5–0.6 | 9 | 0.554 | 0.333 |
| 0.6–0.7 | 5 | 0.658 | 0.800 |
| 0.7–0.8 | 8 | 0.727 | 1.000 |
| 0.8–0.9 | 48 | 0.882 | 0.979 |
| 0.9–1.0 | 4882 | 0.996 | 1.000 |

| threshold | precision | recall | wrong among accepted |
|---|---|---|---|
| 0.50 | 0.9980 | 0.9966 | 10 of 4952 |
| 0.80 | 0.9994 | 0.9935 | 3 of 4930 |
| 0.90 | 0.9996 | 0.9841 | 2 of 4882 |
| 0.95 | 1.0000 | 0.9706 | 0 of 4813 |
| 0.98 | 1.0000 | 0.9478 | 0 of 4700 |
| 0.99 | 1.0000 | 0.9383 | 0 of 4653 |

#### Show-ratings threshold

Target: among pairs whose ratings would be shown, the wrong share must not exceed today's measured wrong rate, 0.97%.
Derived on the calibration split: p ≥ 0.3575. Held out: 49 wrong of 4361 shown (1.124%); recall of same-film units 99.9%; production's contradicted filings shown: 1 of 18.

| p ≥ | held-out units shown | wrong | wrong rate | per-decision: shown / wrong |
|---|---|---|---|---|
| 0.3575 | 4361 | 49 | 1.124% | 4313 / 1 |
| 0.5000 | 4356 | 45 | 1.033% | 4312 / 1 |
| 0.8000 | 4306 | 10 | 0.232% | 4296 / 0 |
| 0.9000 | 4276 | 4 | 0.094% | 4272 / 0 |
| 0.9500 | 4193 | 1 | 0.024% | 4192 / 0 |
| 0.9900 | 3927 | 0 | 0.000% | 3927 / 0 |

#### Cannot-link score thresholds

Certified: each cut sits below every same-film unit of the calibration split.
- listing ↔ film: p < 0.08482 — held-out false veto 0.000% (≤ 0.063%), vetoes 95.5% of different-film units.
- listing ↔ listing: p < 0.01844 — held-out false veto 0.047% (≤ 0.143%), vetoes 62.7% of different-film units.

#### Today's vetoes, measured (listing ↔ film)

| veto | fires on same-film (false veto) | upper 95% | fires on different-film (true veto) | per country false veto | verdict |
|---|---|---|---|---|---|
| VenueDeniesFilm (DeniedCandidate) | 4 of 6159 (0.065%) | 0.145% | 313 of 5150 (6.08%) | de 4/1629, es 0/228, pl 0/725, uk 0/1429, us 0/2156 | within today's wrong rate (4 false vetoes), an ordinary negative weight rather than a cannot-link |
| ListingDeniesFilm (DecorationVeto) | 44 of 6159 (0.714%) | 0.914% | 386 of 5150 (7.50%) | de 19/1629, es 3/228, pl 5/725, uk 6/1429, us 11/2156 | within today's wrong rate (44 false vetoes), an ordinary negative weight rather than a cannot-link |
| OriginalTitleNamesAnotherFilm | 1 of 4396 (0.023%) | 0.102% | 543 of 4148 (13.09%) | de 1/977, es 0/137, pl 0/240, uk 0/1181, us 0/1862 | within today's wrong rate (1 false vetoes), an ordinary negative weight rather than a cannot-link |
| Faust fold refusal (containment + venue denial) | 0 of 6159 (0.000%) | 0.044% | 137 of 5150 (2.66%) | de 0/1629, es 0/228, pl 0/725, uk 0/1429, us 0/2156 | certified: never fired on a same-film unit |

#### Today's vetoes, measured (listing ↔ listing)

| veto | fires on same-film (false veto) | upper 95% | fires on different-film (true veto) | per country false veto | verdict |
|---|---|---|---|---|---|
| CinemasDescribeDifferentFilms | 1 of 4187 (0.024%) | 0.107% | 222 of 467 (47.54%) | de 1/952, es 0/135, pl 0/177, uk 0/1152, us 0/1772 | within today's wrong rate (1 false vetoes), an ordinary negative weight rather than a cannot-link |
| VenueCreditsApart | 2 of 5849 (0.034%) | 0.103% | 113 of 682 (16.57%) | de 0/1568, es 0/225, pl 1/622, uk 0/1352, us 1/2090 | within today's wrong rate (2 false vetoes), an ordinary negative weight rather than a cannot-link |

Bare-listing home (must-link): of 460191 labelled exact-title pairs where a side publishes neither year nor runtime, 459916 are one film (99.94%).

#### Derived cannot-link rules (listing-film)

| rule | fit: false veto (Bonferroni bound) | fit: true veto | held out: false veto | held out: true veto | covered by today's vetoes |
|---|---|---|---|---|---|
| title in {none,overlap} AND year.distance >= 3 | 0/4336 (0.194%) | 1280/3867 (33.10%) | 0/1828 (0.000%) | 461/1390 (33.17%) | 3929 of 36333 |
| director in {different} AND year.distance >= 32 | 0/4335 (0.194%) | 104/3781 (2.75%) | 1/1824 (0.055%) | 43/1369 (3.14%) | 5199 of 5199 |
| year.distance >= 49 | 0/4336 (0.194%) | 466/3867 (12.05%) | 0/1828 (0.000%) | 118/1390 (8.49%) | 2548 of 4291 |
| runtime.delta >= 18 AND year.distance >= 16 | 0/4336 (0.194%) | 100/3867 (2.59%) | 0/1828 (0.000%) | 27/1390 (1.94%) | 2244 of 2280 |
| originalTitle in {disjoint} AND year.distance >= 2 | 0/4314 (0.195%) | 730/3770 (19.36%) | 0/1821 (0.000%) | 332/1377 (24.11%) | 1584 of 1593 |
| runtime.delta >= 38 AND title in {none,overlap} | 0/4337 (0.194%) | 32/3875 (0.83%) | 1/1827 (0.055%) | 2/1390 (0.14%) | 1643 of 1643 |
| originalTitle in {disjoint} AND runtime.delta >= 11 | 0/4333 (0.194%) | 39/3878 (1.01%) | 0/1825 (0.000%) | 16/1391 (1.15%) | 149 of 151 |
| title in {none} AND year.distance >= 2 | 0/4336 (0.194%) | 295/3867 (7.63%) | 0/1828 (0.000%) | 126/1390 (9.06%) | 557 of 1345 |
| country in {mismatch} AND year.distance >= 2 | 0/4336 (0.194%) | 85/3867 (2.20%) | 0/1828 (0.000%) | 32/1390 (2.30%) | 223 of 235 |
| runtime.delta >= 82 | 0/4337 (0.194%) | 27/3875 (0.70%) | 2/1827 (0.109%) | 6/1390 (0.43%) | 204 of 220 |
| country in {mismatch} AND runtime.delta >= 21 | 0/4337 (0.194%) | 52/3875 (1.34%) | 0/1827 (0.000%) | 22/1390 (1.58%) | 150 of 151 |
| country in {mismatch} AND originalTitle in {disjoint} | 0/4333 (0.194%) | 20/3878 (0.52%) | 1/1825 (0.055%) | 13/1391 (0.93%) | 27 of 32 |

#### Derived cannot-link rules (listing-listing)

| rule | fit: false veto (Bonferroni bound) | fit: true veto | held out: false veto | held out: true veto | covered by today's vetoes |
|---|---|---|---|---|---|
| chainId in {different} AND title in {none,overlap} | 0/4142 (0.214%) | 63/408 (15.44%) | 3/1748 (0.172%) | 28/271 (10.33%) | 54 of 5029 |
| runtime.delta >= 2 AND year.delta >= 16 | 0/4151 (0.214%) | 64/407 (15.72%) | 1/1754 (0.057%) | 17/271 (6.27%) | 100 of 1787 |
| chainId in {different} AND runtime.delta >= 1 | 0/4142 (0.214%) | 76/408 (18.63%) | 2/1750 (0.114%) | 34/271 (12.55%) | 53 of 3922 |
| runtime.delta >= 51 AND title in {none,overlap} | 0/4142 (0.214%) | 58/408 (14.22%) | 1/1750 (0.057%) | 50/271 (18.45%) | 153 of 1449 |
| director in {different} AND year.delta >= 32 | 0/4078 (0.218%) | 27/399 (6.77%) | 1/1721 (0.058%) | 12/265 (4.53%) | 54 of 1248 |
| title in {none,overlap} AND year.delta >= 16 | 0/4151 (0.214%) | 32/407 (7.86%) | 0/1752 (0.000%) | 8/271 (2.95%) | 20 of 1723 |
| director in {different} AND originalTitle in {disjoint} | 0/4124 (0.215%) | 36/357 (10.08%) | 1/1735 (0.058%) | 22/228 (9.65%) | 195 of 219 |
| title in {none,overlap} AND venue in {same} | 0/4142 (0.214%) | 96/408 (23.53%) | 2/1748 (0.114%) | 59/271 (21.77%) | 137 of 228 |
| year.delta >= 47 | 0/4151 (0.214%) | 19/407 (4.67%) | 0/1752 (0.000%) | 7/271 (2.58%) | 37 of 858 |
| runtime.delta >= 1 AND venue in {same} | 0/4142 (0.214%) | 138/408 (33.82%) | 0/1750 (0.000%) | 71/271 (26.20%) | 226 of 264 |
| runtime.delta >= 81 | 0/4142 (0.214%) | 20/408 (4.90%) | 3/1750 (0.171%) | 9/271 (3.32%) | 54 of 1130 |
| originalTitle in {disjoint} AND year.delta >= 3 | 0/4135 (0.215%) | 22/346 (6.36%) | 0/1744 (0.000%) | 10/230 (4.35%) | 64 of 77 |
| chainId in {different} AND year.delta >= 16 | 0/4151 (0.214%) | 38/407 (9.34%) | 0/1752 (0.000%) | 8/271 (2.95%) | 5 of 2084 |
| director in {different} AND venue in {same} | 0/4115 (0.216%) | 79/413 (19.13%) | 1/1734 (0.058%) | 33/269 (12.27%) | 172 of 172 |

#### Sensitivity: corroboration bar k=3 against k=2

- listing ↔ film: 51 weights with ≥30 units; same sign 41/51; mean |Δ| 1.583, max |Δ| 6.481 (year.delta[-2.5..-1.5]), correlation 0.8014.
  - per signal: country 2/2 same sign, mean |Δ| 0.16; director 2/2 same sign, mean |Δ| 0.65; originalTitle 3/3 same sign, mean |Δ| 0.37; popularity.log2 4/6 same sign, mean |Δ| 1.27; rivals 2/3 same sign, mean |Δ| 1.70; runtime.delta 5/7 same sign, mean |Δ| 1.92; search.rank 3/4 same sign, mean |Δ| 1.83; title 7/7 same sign, mean |Δ| 0.81; venues.corroborating 7/9 same sign, mean |Δ| 1.96; year.delta 6/8 same sign, mean |Δ| 2.65
  - year.delta[-2.5..-1.5]: k=2 -0.72, k=3 -7.20 (54 units)
  - runtime.delta[78.0..]: k=2 -4.60, k=3 +0.70 (33 units)
  - venues.corroborating[34.5..45.5]: k=2 +4.23, k=3 -0.34 (47 units)
- listing ↔ listing: 22 weights with ≥30 units; same sign 20/22; mean |Δ| 1.032, max |Δ| 2.479 (runtime.delta[11.5..18.5]), correlation 0.9337.
  - per signal: chainId 2/2 same sign, mean |Δ| 1.39; director 1/1 same sign, mean |Δ| 0.80; originalTitle 3/3 same sign, mean |Δ| 0.38; runtime.delta 7/8 same sign, mean |Δ| 1.63; title 2/3 same sign, mean |Δ| 0.53; venue 2/2 same sign, mean |Δ| 0.71; year.delta 3/3 same sign, mean |Δ| 0.65
  - runtime.delta[11.5..18.5]: k=2 -2.54, k=3 -0.06 (113 units)
  - runtime.delta[28.5..51.5]: k=2 -2.99, k=3 -5.35 (78 units)
  - runtime.delta[65.5..]: k=2 -5.79, k=3 -3.55 (36 units)
- listing ↔ film, de only: 21 weights with ≥30 units; same sign 18/21; mean |Δ| 0.991, max |Δ| 5.735 (rivals[0.5..]), correlation 0.4296.
  - per signal: country 1/1 same sign, mean |Δ| 0.01; director 1/1 same sign, mean |Δ| 0.04; originalTitle 1/1 same sign, mean |Δ| 0.34; popularity.log2 1/1 same sign, mean |Δ| 0.00; rivals 1/2 same sign, mean |Δ| 2.88; runtime.delta 3/4 same sign, mean |Δ| 1.33; title 4/4 same sign, mean |Δ| 0.04; venues.corroborating 4/5 same sign, mean |Δ| 1.04; year.delta 2/2 same sign, mean |Δ| 2.00
  - rivals[0.5..]: k=2 -3.43, k=3 +2.30 (57 units)
  - year.delta[-3.0..-0.5]: k=2 +0.35, k=3 +3.70 (131 units)
  - runtime.delta[7.5..11.5]: k=2 -1.33, k=3 +1.46 (33 units)
- listing ↔ film, us only: 39 weights with ≥30 units; same sign 32/39; mean |Δ| 1.324, max |Δ| 4.628 (venues.corroborating[0.5..1.5]), correlation 0.8608.
  - per signal: country 2/2 same sign, mean |Δ| 0.18; director 2/2 same sign, mean |Δ| 0.36; originalTitle 3/3 same sign, mean |Δ| 0.39; popularity.log2 4/5 same sign, mean |Δ| 1.05; rivals 2/3 same sign, mean |Δ| 1.58; runtime.delta 4/4 same sign, mean |Δ| 1.47; search.rank 3/4 same sign, mean |Δ| 2.03; title 5/6 same sign, mean |Δ| 0.59; venues.corroborating 3/5 same sign, mean |Δ| 2.76; year.delta 4/5 same sign, mean |Δ| 1.61
  - venues.corroborating[0.5..1.5]: k=2 +1.07, k=3 -3.56 (328 units)
  - venues.corroborating[214.0..1222.0]: k=2 +4.44, k=3 -0.01 (43 units)
  - rivals[0.5..3.5]: k=2 -1.97, k=3 +2.12 (860 units)
- listing ↔ film, uk only: 35 weights with ≥30 units; same sign 30/35; mean |Δ| 1.305, max |Δ| 4.736 (venues.corroborating[1.5..15.5]), correlation 0.8608.
  - per signal: country 1/1 same sign, mean |Δ| 0.14; director 2/2 same sign, mean |Δ| 0.78; originalTitle 3/3 same sign, mean |Δ| 0.15; popularity.log2 3/4 same sign, mean |Δ| 0.99; rivals 1/2 same sign, mean |Δ| 1.98; runtime.delta 5/5 same sign, mean |Δ| 1.25; search.rank 3/4 same sign, mean |Δ| 1.77; title 5/6 same sign, mean |Δ| 0.74; venues.corroborating 3/4 same sign, mean |Δ| 2.68; year.delta 4/4 same sign, mean |Δ| 1.77
  - venues.corroborating[1.5..15.5]: k=2 +5.90, k=3 +1.17 (514 units)
  - year.delta[1.5..]: k=2 -6.82, k=3 -2.18 (516 units)
  - venues.corroborating[0.5..1.5]: k=2 +1.17, k=3 -2.73 (221 units)

Contradicted production filings: 1471 listings on 59 tmdbIds (`contradicted-prod-resolutions.tsv`); 59 imdb / rating-page cross-check mismatches (`prod-cross-check-mismatches.tsv`).

---

## 15. Phase 2: the shadow resolver — results (2026-09-26, branch `identity-resolver`)

### 15.1 What was built

Production code that serves nothing (no wiring reads it):

- `common/.../services/identity/`:
  - `IdentityModel`: `Listing` (a `ListingKey` plus the raw row), `Evidence`, `Hit`, `FilmFacts`, `Candidate`, `Answer` (`Known` / `Unknown`), `CandidateQuery`, and the `IdentityLookups` trait. The trait is minimal and exists for reconciliation with phase 1's observation types.
  - `CandidateQueries`: the query set. For every title shape (the title, the original title, and each delimited segment from `SearchTitles`) it issues a search with the stated year and one without, plus every credited director's filmography.
  - Scoring (since round 2, §15.7): the calibration's own `IdentityMeasures` and `IdentityCalibration` over `identity-weights.json`. One loader and one format, no interim weights.
  - `IdentityResolver`:
    - families: the block closure, plus segment keys and matched ids;
    - candidates are scored only along an evidence path;
    - a node accepts a match on its **confidence**, p(best) × Π(1 − p(rival));
    - venue agreement is a confidence-weighted vote;
    - `ConstraintSolver` (A2), then group-level voting over pooled evidence, then a re-solve;
    - output is `ResolverDecision`s (implementing curation's `Decision`), each with a confidence, a basis, an explanation and its contradictions; curation pins are read through `ListingConstraints.pinned`.
  - `IdAssigner` (A4), and `ListingConstraints.learnedCannotLink`: a generic evaluator for data-driven cannot-links. When the artefact carries learned cannot-links the resolver uses them; the old pipeline never reads them. `ListingConstraints.statedYearsApart` is new.
- `worker/.../services/identity/TmdbIdentityLookups` implements the trait over raw TMDB primitives (`TmdbClient.search` is new) and the venues' detail pages. A replay gap becomes `Unknown`, not "no film".
- Tests:
  - `common/src/test/.../identity/`: the properties on generated corpora, each beside its mutation (A1 lazy lookups, P1 first-wins, A3 narrowed families, A4 input-order tie-break, NoVoting), plus the hand-built incident cases. 37 tests, about 45 s.
  - `worker/src/it/scala/IdentityShadowIntegrationSpec` compares the resolver with the pipeline, head to head. The hard clusters always run (about 90 s on itAll). The full corpora are opt-in.

Reproduce:

```
# the five hard clusters (itAll):
MONGODB_URI=mongodb://localhost:28017 sbt "worker/IntegrationTest/testOnly integration.IdentityShadowIntegrationSpec"
# + the five full corpora (recorder run 36153174348):
KINOWO_IDENTITY_FULL=es,de,pl,uk,us KINOWO_IDENTITY_CORPUS_DIR=<cinema-scrapes-<cc>.json.gz dir> \
KINOWO_FIXTURE_ROOT=<real enrichment-<cc> dirs> KINOWO_IDENTITY_OUT=<out> …same… (sbt -J-Xmx16g)
```

The pipeline side is booted the way the convergence legs boot it (`bootCorpus`, then the settle pair, staging, conclusion and projection), so the US diff now exists. The US boot took 342–583 s, against more than 65 min for the proof's per-venue replay. Listings are mapped to pipeline films by slot, with the year and director discriminator the slot fold uses.

### 15.2 Head to head (full corpora; hard clusters in the spec output)

*Round 1 — superseded by §15.7.* Weights: `interim-pipeline-6ad0b89d`, a logistic fit on train families against the pipeline's answers. Ground truth for accuracy is the **corroborated** label: the node's one candidate its own evidence backs with at least two independent signals and nothing against it. Accuracy is measured on held-out families only (one fold in five).

| | PL old / new | UK old / new | DE old / new | US old / new | ES old / new |
|---|---|---|---|---|---|
| listings | 10,138 | 29,400 | 19,636 | 99,774 | 4,491 |
| films (with tmdb) | 1,138 (751) / 1,265 (598) | 1,546 (1,484) / 1,577 (1,352) | 1,693 (1,653) / 1,695 (1,572) | 2,275 (2,195) / 2,304 (2,031) | 236 (232) / 236 (215) |
| identical clusters | 1,000 | 1,497 | 1,691 | 2,239 | 233 |
| cannot-linked pairs in a cluster (P3) | – / **0** | – / **0** | – / **0** | – / **0** | – / **0** |
| order variants (permutations + split arrivals) | – / 0 of 3 | – / 0 of 3 | – / 0 of 3 | – / 0 of 3 | – / 0 of 3 |
| listings matched (coverage) | 94.2% / 86.8% | 90.4% / 72.0% | 99.0% / 97.2% | 92.9% / 86.8% | 93.9% / 93.3% |
| accuracy of matched, held-out corroborated | 100% / 100% (224) | 98.2% / 99.8% (1,995) | 100% / 99.9% (3,101) | 99.9% / 99.9% (16,245) | 100% / 100% (543) |
| matched films a listing's own evidence contradicts | 74/751 (9.9%) / 37/598 (6.2%) | 84/1,484 (5.7%) / 49/1,352 (3.6%) | 88/1,653 (5.3%) / 60/1,572 (3.8%) | 95/2,195 (4.3%) / 59/2,031 (2.9%) | 16/232 (6.9%) / 10/215 (4.7%) |
| historical checks, pass–fail | 4–2 / 4–2 | 4–0 / 3–1 | 3–0 / 3–0 | 6–1 / 7–0 | – |
| seconds (boot / resolve) | 70 / 20 | 117 / 8 | 68 / 6 | 342 / 11 | 16 / 1 |

Hard clusters: P3 was 0 and order variance was 0 of 21 on all five corpora. The historical checks regress once on hc-pl (Avengers "Dogrywka") and once on hc-uk (Mockingjay Part 2 (2026)).

On matched films, the pipeline and the resolver are about equally accurate: 98.2–100% against 99.8–100%. What the resolver loses is coverage, 1 to 18 points.

### 15.3 Label-free scorecard

- **Contradiction rate** (the venue's own year, director or runtime against the matched film): lower for the resolver on all five corpora, by 1.4–3.7 points. Wrong ratings are shown exactly on these contradicted matches, so fewer of them means fewer wrong ratings.
- **Cross-country agreement.** 955 director-credited titles appear in two or more corpora. Titles resolved to different films across countries: pipeline 9, resolver 5.
- **Perturbation recovery** (decorated, re-dated, re-cased and deburred copies of a confident match): PL 119/136, UK 152/156, DE 160/160, US 147/152, ES 160/160. Measured for the resolver only; the pipeline side needs a re-boot per perturbation.
- **Simulated 30% TMDB outage.** No listing moved to a *different* film (0 on all five corpora). Affected listings became unmatched instead: PL 356, UK 3,197, DE 3,685, US 18,228, ES 785. With best-probability acceptance, 1,090 UK listings moved to another film; the confidence rule removed that.
- **Stability over time and churn.** P2 holds by construction: a pure function, 0 id changes on identical input (property-tested). Consecutive recorded days were not replayed, because only one recorded day per corpus exists locally.
- **Cross-source consistency** (IMDb, Wikidata, RT/MC/Filmweb) and **rating outliers** are not measurable offline. The rating pages were recorded only for the pipeline's own films, so the resolver's other films have none to compare.
- **Disagreements adjudicated on evidence alone.** Each cell (the listings one pipeline film and one resolver cluster share) is judged by the listings' corroboration of each side's film:

  | | resolver right | pipeline right | both wrong | undecidable |
  |---|---|---|---|---|
  | PL | 9 | 99 | 0 | 140 |
  | UK | 16 | 84 | 0 | 69 |
  | DE | 7 | 62 | 0 | 26 |
  | US | 10 | 94 | 0 | 93 |
  | ES | 2 | 12 | 0 | 5 |

  "Pipeline right" is almost always a listing the resolver left **unmatched** while the pipeline's film is corroborated: a coverage loss, not a wrong match. The undecidable cases are listings that publish nothing but a title. The full lists are in `<out>/<corpus>-report.txt`.

The label-free measures agree with the labelled verdict. The resolver is at least as precise and more stable (lower contradiction rate, no moves under an outage, better cross-country agreement), but it covers less.

### 15.4 Historical checks (test labels only; never resolver inputs)

- **Resolver right, pipeline wrong:**
  - Così fan tutte is not Brass 1992 (PL).
  - Schaffner's "Planet of the Apes" is apart from Burton's (US).
- **Both pass:**
  - Samson/Met (PL);
  - Happy Together (PL);
  - Skarpetek 3 vs 4 (PL);
  - A Star Is Born 1954/1976/2018 (US);
  - It Ends with Us (US);
  - It 1990/2017 (US; fixed on this branch by `statedYearsApart`);
  - the Hunger Games instalments (UK/US);
  - Belle 2013/2021 (UK);
  - Sinn und Sinnlichkeit (DE);
  - Bad Apples (DE);
  - "Zärtlich kreist die Faust" is not 10728 (DE).
- **Both fail:** the decorated Lalka spellings (PL). The resolver puts Kawalski-credited listings on 1321666 and leaves the bare ones unmatched.
- **Regressions:**
  - "Avengers: Koniec gry. Dogrywka (re-release)" (PL): no delimiter separates the title from "Dogrywka", so there is no evidence path.
  - "Mockingjay – Part 2 (2026)" (UK): its yearless search is a replay gap, and `statedYearsApart` keeps it apart from 2015.
- **Undecidable:** Opętanie. The resolver answers 21484 (Żuławski, 1981); the pipeline answers 958160 (1973).

### 15.5 Phase-2 gate

| criterion | status |
|---|---|
| 0 cannot-link violations | **met**: 0 on 10 corpora and on 40 generated × 21 presentations |
| order independence | **met**: 0 variants (21 per hard cluster, 3 per full corpus, 21 × 40 generated) |
| no regressions vs today except known-wrong | **not met**: 2 historical regressions, and 351 evidence-adjudicated "pipeline right" disagreements, mostly coverage |
| cutover gate: accuracy ≥, determinism >, coverage within margin | accuracy of matched ≥ (UK +1.6 pts); determinism better; **coverage fails**: −0.6 (ES) to −18.4 (UK) points |

**Why coverage fails, and what closes it:**

1. **Recording gaps (phase 1).** The trees cannot answer 45–78% of the resolver's own queries, and 88–93% of its candidates' film records. On the full corpora:

   | | queries unanswerable | film records unanswerable |
   |---|---|---|
   | PL | 2,638 / 5,553 | 9,201 / 9,976 |
   | UK | 2,508 / 4,072 | 19,137 / 20,631 |
   | DE | 6,417 / 8,173 | 19,249 / 20,957 |
   | US | 3,368 / 5,440 | 24,092 / 26,355 |
   | ES | 1,112 / 1,370 | 3,473 / 3,704 |

   The pipeline's own queries are answered because they were recorded.
2. **Recording bias in the signals.** Director, runtime and country facts exist only for the film the pipeline picked, so an unmasked fit learned "director known ⇒ pick it", and gave director **mismatch** a positive weight. The interim fit masks those signals. With a phase-1 recording of every candidate's record, they can be fitted honestly. They are what separates same-titled films (the Lalka 1321666 vs 1309396 class).
3. The interim threshold is the Bayes boundary, 0.5. The calibration artefact's derived threshold replaces it.

Re-run this section's commands once phase 1's recording pass is pinned and `identity-weights.json` has landed. The spec reads the artefact automatically.

### 15.6 Hardcoding that remains in the identity decision

| what | where | why it is still there | general replacement |
|---|---|---|---|
| Title rule tables: 29 base rules (16 per-cinema) and 214 extra rules (77 programme-prefix banners, 124 strip patterns, 13 unifications) | `TitleNormalizer` / `titlerules`, via `ScrapeListing.cleanTitle`, `sanitize`, `searchQuery`, `apiQuery` | query strings must match what was recorded, and block keys use the same forms; changing them now only turns answers into gaps | the segment decomposition already in place, plus banner detection from data (a segment attached to many distinct films at a venue), then re-record the raw-title queries |
| `FormatTags` format vocabulary (8 tokens) | `ScrapeListing.cleanTitle` | as above | the same banner-by-co-occurrence signal |
| `SequelMarker` franchise table (Hunger Games, Bring It On) and ordinal word lists | inside `describeDifferentFilms`, the listing-listing cannot-link when no learned rules exist | the calibration's learned listing-listing rules have not landed | learned cannot-links (the evaluator exists); different instalments already split through "different-films", because their candidates differ |
| `YearWindow` constants | the incremental pipeline's predicates only; since round 2 the resolver's vetoes are the artefact's learned rules and cuts | — | done (§15.7) |
| The prior/fact split of the measures (`search.rank`, `popularity.log2`, `rivals`, `venues.corroborating` are priors) | `IdentityResolver.Priors` | a classification of the calibration's measure names, not a weight or threshold | the calibration could publish it in the artefact |
| The evidence-path rule (a candidate is scored only when the listing's own query named it or its title relates) | `IdentityResolver.reachable` | structural pruning, with no constant | none needed |

The resolver has no per-title, per-venue, per-chain or per-franchise rule of its own. Every constant above is inherited from the constraint model or the normaliser, and a learned counterpart for it is pending.

### 15.7 Round 2 (2026-09-26): calibrated weights, the recorded query set, re-measured

**What changed:**

- The resolver scores with `IdentityCalibration` over `IdentityMeasures`, which is the artefact and the measurement definitions the weights were fitted on. `Signals`, `IdentityWeights`, the interim weights and `scripts.IdentityInterimFit` are deleted.
- The calibration and the resolver now share one definition each of:
  - the search shapes (`IdentityMeasures.searchQueries`);
  - the venue co-occurrence count (`corroboratingVenues`);
  - own-evidence agreement (`ownAgreement`);
  - the TMDB record parser (`TmdbFilmRecord`).
- `IdentityLookupSweep` **is** a resolve over the recording chains. It asks exactly the resolver's candidate searches, filmographies, detail pages and candidate identity records (`TmdbClient.identityRecord`). Its tree marker is versioned as `.identity-lookups-v2`.
- Cannot-links are the artefact's learned rules and certified cuts, reached through `ListingConstraints.learned`. The probability cut reads the listing's **own facts** only, not the ranking priors. A candidate is not vetoed for ranking second in TMDB's search.
- A node accepts a film **alone** only when its own facts favour it over the runner-up. Otherwise it follows the film its cluster's credited members chose, or the pooled vote. The confidence is the calibrated probability.
- A decorated spelling joins its plain sibling through a whole-segment must-link (tier 4).
- An unmatched decision now says why: `NoCandidate`, `NoEvidence`, `Vetoed` or `BelowThreshold`.

**Measured locally before the resolver-query recording.** Inputs: corpora and trees of recorder run 36212620541. Labels: the calibration's `split == test` corroborated labels. The trees still lack most of the resolver's own queries: the phase-1 gate on the new query set is 11–37% of lookups answerable, and 91–94% of candidate records are unanswerable.

| | PL | UK | DE | US | ES |
|---|---|---|---|---|---|
| held-out labelled listings | 1,615 | 4,025 | 6,379 | 22,026 | 1,501 |
| accuracy of matched, old / new | 100 / 94.5% | 100 / 99.8% | 100 / 100% | 100 / 98.9% | 83.7 / 83.7% |
| labelled recall, old / new | 100 / 94.4% | 93.9 / **98.0%** | 100 / 99.2% | 100 / 97.2% | 83.7 / 83.6% |
| listings on a *contradicted* production filing, old / new | 1 / 0 | 0 / 0 | **154 / 0** | 0 / 0 | 0 / 0 |
| matched films a listing's facts contradict, old / new | 8 / 1 | 0 / 0 | 4 / 0 | 0 / 0 | 0 / 0 |
| unmatched: NoEvidence / NoCandidate / Vetoed / BelowThreshold | 346 / 345 / 392 / 150 | 1,049 / 383 / 3,910 / 955 | 599 / 1 / 235 / 10 | 7,041 / 211 / 1,513 / 2,914 | 56 / 0 / 9 / 45 |

- Cannot-link violations are 0 on all 10 corpora. Order variants are 0 of 21 on the hard clusters and 0 of 3 on the full corpora.
- Cross-country disagreement: pipeline 9, resolver 13, out of 957 director-credited titles.

Old accuracy is 100% partly by construction, because the labels are production filings corroborated twice.

**Remaining wrong matches, from the evidence:**

| case | listings | what went wrong | whose fix |
|---|---|---|---|
| PL "Lalka" | 88 | Bare listings still pool to 1309396 over Kawalski's 1321666. The rival's record was not answerable, so no director tells them apart. | coverage |
| US "Ken Russell's The Devils" | 245 | Matched to *Tommy* (11326) through the director walk (same director, same 111-minute runtime). The artefact weighs `title=contains` (−4.31) *below* `title=none` (−4.08). | calibration |
| ES "Vengadores: Endgame (Reestreno)" | 244 | The label says 1003596, *Avengers: Doomsday*. The resolver's 299534, *Endgame*, is right, so this is a label error. | calibration |

**The historical regressions:**

- **Avengers "Dogrywka" (PL)**: coverage. The decision is `NoEvidence`: both of its searches are unanswerable in the trees.
- **Mockingjay Part 1 and Part 2 "(2026)" (UK)**: weights plus coverage.
  - The bracketed re-release year 2026 reads as the film's year (`year.delta=0`, +3.05), and Francis Lawrence directed both films (+5.27).
  - Together these outweigh `title=overlap` (−3.48), so both rereleases land on *Sunrise on the Reaping* (1300968, 2026).
  - The 2015 film's candidates for these title shapes are unanswered gaps.
  - General fix, for the calibration: measure a bracket year separately from a published year field. The calibration may confirm that re-release brackets carry little weight.
- **Skarpetek 3 vs 4 (PL)**, new in this round: coverage. "…skarpetek 4. Do roboty! – zestaw" has a detail page crediting the shared crew, so it lands on film 3, whose record is known. Film 4's record (1735319) is a gap, so its director cannot compete.

The learned `title in {none,overlap} AND venue in {same}` rule fired on several PL bundle listings. It is certified at a 0.11% false-veto rate, so it stays.

**Next:** re-run this section once the `identity-lookups=true` recording of the resolver's query set is pinned:

```
KINOWO_IDENTITY_FULL=es,de,pl,uk,us KINOWO_IDENTITY_CORPUS_DIR=… KINOWO_FIXTURE_ROOT=… sbt "worker/IntegrationTest/testOnly integration.IdentityShadowIntegrationSpec"
```

---

## 16. Programme phase 4: stable IDs, dual write (2026-09-26, branch `identity-phase4`)

This is the programme's phase 4 and §8's "Phase 2: stable IDs and data migration". Everything
here is additive. Nothing reads the new field, and no FilmId changes.

### 16.1 What landed

- **The stored form.** `ListingKey.serialised` / `ListingKey.parse` are NUL-joined, total and
  injective. It is the same string the observation store keys listings by (it replaces
  `ListingObservation.keyString`), so a slot and its listing observations join on it.
- **One derivation.** `ListingKey.ofSlotRow(slotKey, slot)` computes the key from the stored row.
  `None` means the row is not a venue listing:
  - an enrichment slot;
  - a retired venue;
  - a chain's network-level detail slot (`CinemaCityChain`, `CineworldChain`, `RegalChain`: a
    `Cinema` that is not in `Cinema.all`).

  On prod those detail slots carry no title, so as "listings" they collapsed every film onto
  one key per chain and director: 24 false collisions (UK 14, PL 2, US 8).
- **The dual write.**
  - `movie_slots`: `StoredSlotDto.of` stamps `listingKey` from the row itself, so every slot
    write carries it, including the merge move.
  - `screenings`: a row is now `ListedShowtimes(showtimes, listingKey)`.
    `ScreeningsSplit.screeningsOf` / `slotOps` give each row its slot's key. A merge
    (`SideCollectionMove`) carries the key it had. The write compares keys too, so a listing
    whose key moves under unchanged showtimes (a page-less venue correcting its year) is
    rewritten on the next whole-record write.
  - Serving reads (`findForFilmChecked`, `findAll`) are unchanged.
- **Guards.**
  - `ListingKeyDualWriteIntegrationSpec` (itAll, raw documents) checks each repository write
    path: the upsert (the landing, the fold's completion, a re-key's retitle), the per-slot
    patch, a key change, and the merge move. It then boots the PL hard-cluster corpus through
    the whole pipeline and checks that every slot and screenings row carries its slot's key.
    With the stamp removed, both tests fail.
  - `ListingKeyWritePathLintSpec` keeps new write paths on the factories. Rows are built only
    by `StoredSlotDto.of` / `StoredScreeningsDto.of`. Screenings payloads are built only in the
    split. The side collections are written only by their repositories. Run against the
    pre-change `SlotsRepository`, the lint fails.
- **Backfill.** `scripts.ListingKeyBackfill` (worker Test).
  - The plan is pure and tested (`ListingKeyBackfillSpec`).
  - It is a DRY RUN by default. `--apply` makes conditional `$set` writes, each only if the row
    still holds the key the scan read. `--export <dir>` writes each film's key set and the
    collision list.
  - Run it per country, only **after** the dual write is deployed. A pre-phase-4 worker
    rewrites rows whole and would drop the field. Each stamped row re-projects its film once.
- **ID seeding.** `services.identity.IdSeeding` runs `IdAssigner` with today's films as the
  previous assignment, then names the review list (`IdSeedingSpec`, including order
  independence).
  - Today's FilmIds are strings, so they are ranked onto the counter largest film first: on a
    contested cluster, the film more showtimes hang off keeps its id.
  - `IdentitySeedingIntegrationSpec` (opt-in: `KINOWO_IDENTITY_SEED_FILMS` plus the full-corpus
    settings) measures it.

### 16.2 Backfill, measured read-only on prod (2026-09-26 12:25)

| | movie_slots rows | to stamp | no venue listing | screenings rows | to stamp | no keyed slot | listing-key collisions |
|---|---|---|---|---|---|---|---|
| PL | 12,458 | 10,049 | 2,409 | 10,046 | 10,046 | 0 | **0** |
| UK | 33,370 | 30,244 | 3,126 | 30,244 | 30,244 | 0 | **0** |
| DE | 23,286 | 19,862 | 3,424 | 19,849 | 19,844 | 5 | **0** |
| US | 106,354 | 101,860 | 4,494 | 101,860 | 101,860 | 0 | **0** |
| ES | 5,388 | 4,937 | 451 | 4,937 | 4,937 | 0 | **0** |

- No stored slot shares a listing key with another, in any country.
- The Belle / Planet of the Apes class (two listings folded into one slot) cannot be seen from
  stored slots. `ListingKeyCorpusSpec` covers it on the corpora, and the fold hides no film
  pair since `venueCreditsApart`.
- The 5 DE rows are July leftovers under the pre-rename wire key `Cineworld␟…` (`backrooms`,
  `rose`, `spaceballs`, `supergirl`, `vaiana`). They now resolve to `CineworldChain`, and
  their showtimes are past. They are cleanup candidates, and are left unstamped.

### 16.3 ID seeding review (today's prod films × resolver clusters, recording run 36224654409)

| | films | clusters | keep id | no cluster | merged away | split | fresh (film went elsewhere) | fresh (no prod film) | today's keys in corpus | listings the fold hides |
|---|---|---|---|---|---|---|---|---|---|---|
| PL | 1,110 | 1,314 | 1,085 | 3 | 22 | 102 | 221 | 8 | 99.5% | 73 |
| UK | 1,515 | 1,563 | 1,503 | 1 | 11 | 43 | 45 | 15 | 99.6% | 0 |
| DE | 1,685 | 1,680 | 1,647 | 31 | 7 | 13 | 13 | 20 | 98.6% | 0 |
| US | 2,280 | 2,339 | 2,262 | 11 | 7 | 57 | 73 | 4 | 99.4% | 0 |
| ES | 233 | 236 | 230 | 2 | 1 | 4 | 4 | 2 | 99.3% | 0 |

The lists, with 20 examples each, are in `docs/design/identity-seeding/seeding-<cc>.txt`.

- The gate check passes on all five: every film keeps its id or is on the list.
- "No cluster" films are listings the corpus (recorded at 06:50) no longer holds. By 12:25 they
  were scraped (for example, a one-off PL event).
- The "split" and "fresh (film went elsewhere)" rows are mostly the resolver's under-merge that
  §15.5 measured as coverage. The largest PL example is `lalka|2026` (608 listings), cut into
  393 + 51 ("Kino Kobiet") + 50 (Sa…) + decorated spellings "LALKA 2D PL". Also: the Ukrainian
  dubs of "Resident Evil", and every "DKF: …" / "Klub Konesera: …" banner listing.
- "Merged away" is mostly the resolver joining a decorated one-listing row to its film. That is
  right: "Carmen | metropolitan opera", "Camille i kameleon - zestaw", and "Potyag Chervona ruta
  - UA" (18 listings) onto `pociagczerwonaruta|2026`.

Seeding this assignment today would move listings off their current URL for PL 221 clusters and
retire 22 PL film ids. The other countries: UK 45 / 11, DE 13 / 7, US 73 / 7, ES 4 / 1.

### 16.4 What remains before dual READS

1. Deploy the dual write, then run `ListingKeyBackfill --apply` per country (ES → DE → UK → US
   → PL). Re-run the dry run: "to stamp" must be 0.
2. A shadow read, which is §10's phase-2 acceptance. For every listing, the rows found by
   `listingKey` must equal the rows found by the slot key. It is not built. It needs a
   `listingKey` index on both collections, and a gauge or alert for unstamped rows.
3. **The slot fold hides listings.** `ScrapeListing.prepare` folds a venue's same-film rows into
   one slot, which stores only the representative listing's key. In PL, 73 raw listings sit
   behind another listing's slot. A read by their key finds nothing until slots are one per
   listing: the fold moves into the projection's display merge (§4).
4. Resolver coverage (§15.5). Seeding now would re-id 221 PL clusters split off `lalka`,
   `obcy`, `tony` and similar. The split/fresh columns are the measure to drive to near zero
   before any FilmId is assigned by overlap.
5. A persistent FilmId map. `IdAssigner` works over `Long` counters, and today's ids are
   strings. The seeding needs a stored string ↔ counter map, or `IdAssigner` over opaque ids,
   before it can write.
6. `web_screenings` is not keyed. A `CityScreening` merges a venue's slots, so phase 5's
   projection writes it keyed `(FilmId, ListingKey)`.
7. Clean up the 5 stale DE `Cineworld␟…` rows.

Hardcoding added: none. The chain-slot exclusion reads `Cinema.all` membership, the model's own
venue roster. The seeding's largest-film-first rank is a rule over the data, not a constant.

### 16.5 Dual-read prerequisites (2026-09-26, branch `identity-phase4-dualread`)

Still invisible to what production serves: no serving path reads `listingKey`, the read-model
field or the FilmId map, and no FilmId changes. Status of §16.4:

- **Index (item 2).** `SlotKeyed.ensureIndexes` builds `filmId` and `listingKey` on `movie_slots`
  and `screenings` at the repositories' first use. `ListingKeyDualWriteIntegrationSpec` asserts a
  read by one listing's key is an index scan (it reads COLLSCAN with the index removed). The read
  itself is `ListingKeyedRows.rowIdsForListingKeyChecked`, beside `rowListingKeysChecked`
  (every row id with its stamp), on both Mongo and in-memory stores (`SlotsRepositoryContractSpec`).
- **Unstamped rows (item 1's gate).** `UnstampedListingCensus` publishes
  `kinowo_worker_listing_key_unstamped_rows{country,collection}` hourly. It counts venue rows with
  no `listingKey`. The exemptions are `ListingKey.isVenueRow`: enrichment slots, chain network
  detail slots, retired venues. It must read 0 per country after the backfill. It is charted on
  the worker-diagnostics dashboard. No alert yet: before the backfill it is ~10k per country by
  design. Add one (above 0 for a day) once every country has been backfilled.
- **Shadow read (item 2).** `services.identity.ListingKeyShadowRead` samples
  `KINOWO_LISTING_KEY_SHADOW_SAMPLE` (default 500) venue slot rows an hour. It resolves each one
  by slot key and by `listingKey` in both collections. It publishes
  `kinowo_worker_listing_key_shadow_read_rows{country,outcome=agree|slots_disagree|screenings_disagree|unread}`
  and logs the first 10 disagreements with their keys. It is **off by default**
  (`KINOWO_LISTING_KEY_SHADOW_READ`, wired in `MetricsWiring`). Over the PL hard clusters after
  the whole pipeline, every venue slot row agrees (the teeth: one unset stamp is reported by both
  the shadow read and the census).
- **`web_screenings` (item 6).** `CityScreening.listingKeys` holds the sorted keys of every slot a
  row unions, derived as the side rows are stamped. Nothing reads it. It is derivation
  `e90186e6b0473c75` (Full), so every worker re-projects its corpus once after the deploy. On the
  fixture corpus all 4,784 rows carry a key, and none unions two.
- **FilmId map (item 5).** `FilmIdCounters` holds the rules: injective both ways, append-only,
  unmapped films numbered after the largest counter, largest film first. The stores are
  `MongoFilmIdCounterStore` (`identity_film_ids`, unique `counter`) and
  `InMemoryFilmIdCounterStore`, both held to `FilmIdCounterStoreContractSpec`.
  `IdSeeding.review` takes the stored map, so a film seeded earlier keeps its counter, and with it
  any contested cluster. `scripts.FilmIdCounterSeed` is a dry run by default and builds its index
  only on `--apply`. The dry run against prod (read-only, 2026-09-26) found an empty map, with
  1,203 PL / 1,598 UK / 1,861 DE / 2,275 US / 239 ES films to add.
- **The listings the fold hides (item 3): a phase-5 item, not fixed here.** `ListingKeyCorpusSpec`
  measures them on the recorded full PL corpus: 73 listings at 42 venues, and 0 in the other
  countries' checked-in corpora.
  - What differs: page and raw title for 39, the page alone for 31, and raw title plus
    year/directors for 3 (an uncredited row beside a credited one).
  - Every one is a format edition with its own page (2D/3D, dubbing/napisy, LEKTOR, Helios
    RePlay editions). `ScrapeListing.prepare` unions it into one slot whose representative
    carries another edition's page and title.
  - The spec asserts each one is the same film as its slot. So hiding it loses a key, never
    showtimes or identity.
  - A generic fix exists without a per-title rule: the slot carries the set of listing keys it
    folded, as a multikey field. It is not done here. It changes `CinemaMovie` / `SourceData` and
    every landing path, and phase 5 removes the need: one slot per listing, with the fold moved
    into the projection's display merge (§4).
  - Until then a dual read must treat "not found by key" for these 73 as expected, or read the
    representative's slot.

Still blocking dual reads:

1. Deploy, then run `ListingKeyBackfill --apply` per country, until the unstamped gauge reads 0.
2. Turn the shadow read on per country, until `agree` equals the sample for a week.
3. Phase 5's one slot per listing, for the fold-hidden listings.
4. Apply `FilmIdCounterSeed` (parent), and resolver coverage (§15.5) before any id is assigned by
   overlap.
5. Remove the 5 stale DE rows.

---

## 17. Phase 1 (§8): the shadow run in production

### 17.1 What runs

`ShadowIdentityReaper` (worker, `services.identity`) rides the settle tick (`ResolutionWiring.settleTick`:
the whole-corpus settle, then the shadow run, on the same cluster-claimed 30-minute window). Each tick:

- **Listings**: every listing of the scrape archive's latest scrape per live venue (`Listing.corpus`,
  the same function the offline harness and the recording sweep use).
- **Lookups**: `ObservedIdentityLookups` — the very `TmdbIdentityLookups`, over a TMDB client built by
  the deployment's own factory (`tmdbClientOver`) on an `ObservedHttpFetch`, and the venues' details
  from `DETAIL <page> <venue>` observations. There is no network beneath it. A question with no live
  definitive observation (never asked, or answered only by a failed read) is a gap: `Unknown`, counted.
  Reads renew what they read (§9a).
- **Pins**: `identity_pins`, through `ListingConstraints.pinned`.
- **Diff** (`ShadowDiff`): each cluster against the pipeline's films of the same listings (`PipelineFilms`,
  by slot — the rule the offline harness uses): `identical`, `split`, `merged`, `moved`; a family whose
  clusters are not all identical is itemised.
- **Writes**: `identity_shadow_decisions` (a document per cluster) and `identity_shadow_diff` (per
  differing family), one run replacing the previous whole, each document stamped `expireAt` = run +
  `ObservationRetention.Window` (8 days), deleted by a TTL index the worker reconciles. `ShadowRunStore`
  owns every rule over a Mongo and an in-memory backend (`ShadowRunStoreBehaviour` runs both).
- **Reads**: `/admin/identity` reads the latest run through `ShadowDecisions`. Its verdicts (the
  low-confidence cut's input, §13.3) are the clusters the pipeline settles: identical = right; the same
  listings on another film = wrong; splits, merges and "no film" are not verdicts.
- **Gauges**: `kinowo_worker_identity_shadow_films{country,relation}`,
  `kinowo_worker_identity_family_crossings{country}` (a crossing refuses the resolve; the previous run
  stays), `kinowo_worker_identity_resolve_seconds{country}`; charted on worker-diagnostics. No series
  while a country's run is off.

**Switches** (staged-migration, read per worker process, off by default):
`KINOWO_IDENTITY_SHADOW` (`ProcessConfiguration.identityShadow`) wires the run;
`KINOWO_OBSERVATION_CAPTURE` fills what it reads. With the shadow on and capture off it reads whatever
the store holds.

### 17.2 Proof

- `ShadowIdentityReaperIntegrationSpec` (itAll; full corpora with `KINOWO_IDENTITY_FULL`): the offline
  resolve over the recorded answers, every answer filed by the production capture, then one reaper tick
  over that store alone. The persisted decisions equal the offline resolver's; the corpus fetch sees zero
  requests during the tick; no crossing.
- `ObservationCaptureEndToEndSpec`: the recorded Poznań corpus booted with capture and the shadow run on,
  a tick after the boot: `expected-schedules.txt` and the read-model snapshot unchanged, zero requests.
- Found on the way: the offline harness counted each replay gap once per distinct request, so a gap met
  a second time read as the replay's empty answer (`Known(Nil)`); it now counts every gap met.

### 17.3 Measured (2026-09-26)

| corpus | listings | clusters (identical / split / merged / moved) | tick wall | CPU | allocated | Mongo (decisions + diff) |
|---|---|---|---|---|---|---|
| hc-pl | 147 | 49 (12 / 33 / 2 / 2) | 1.1 s | 0.6 s | 579 MB | 0.13 MB |
| hc-uk | 78 | 24 (16 / 0 / 1 / 7) | 0.3 s | 0.2 s | 191 MB | 0.05 MB |
| hc-de | 45 | 11 (8 / 0 / 0 / 3) | 0.2 s | 0.0 s | 33 MB | 0.01 MB |
| hc-us | 107 | 36 (17 / 8 / 0 / 11) | 0.3 s | 0.1 s | 121 MB | 0.06 MB |
| hc-es | 44 | 9 (7 / 0 / 0 / 2) | 0.2 s | 0.0 s | 43 MB | 0.01 MB |
| full-pl | 10,135 | 1,314 (1,086 / 76 / 152 / 0) | 2.5 s | 2.2 s | 1,887 MB | 5.7 MB |
| full-uk | 30,302 | 1,563 (1,481 / 7 / 75 / 0) | 1.0 s | 0.7 s | 1,007 MB | 8.8 MB |
| full-de | 19,674 | 1,680 (1,673 / 0 / 7 / 0) | 0.6 s | 0.4 s | 768 MB | 3.3 MB |
| full-es | 4,909 | 236 (235 / 0 / 1 / 0) | 0.4 s | 0.1 s | 155 MB | 0.8 MB |

Decisions equal the offline resolver's and zero requests on all nine (recorder run 36153174348's
trees for the full corpora). "Allocated" is garbage, not retained heap: the run keeps nothing between
ticks but the Mongo documents. Mongo is the uncompressed BSON `size` of one run (a run replaces the
last). The tick above excludes the Mongo reads of the observations, one per lookup (full-es: ~1,100
reads and as many `expireAt` renewals per 30-minute tick).

**What production will see**: over the capture of the pipeline's OWN lookups (the e2e Poznań corpus),
4,800 listings gave 723 clusters (527 identical, 96 split, 55 merged, 45 moved), 337 matched, and 4,559
lookups the capture never observed. The resolver's yearless searches, director walks and candidate
records are mostly questions the pipeline never asks, and the shadow run asks no service, so in
production most nodes stay `Unknown` until those questions are observed. The diff then measures the
resolver on the pipeline's evidence, not its own.

---

## 18. Programme phase 5: per-country cutover (2026-09-26, branch `identity-phase5`)

Built and tested; **off for every country**. The runbook (preconditions, the gitops line, reading
it, rollback, per-country blockers, the phase-6 deletion list) is
`docs/design/identity-cutover-runbook.md`.

### 18.1 What landed

- **The switch.** `KINOWO_IDENTITY_CUTOVER` (`ProcessConfiguration.identityCutover`, a typed
  `IdentityCutoverCountries`), read only by `modules.wiring.IdentityCutoverWiring`. Off, every
  country is wired as before; on, the scrape runner's sink, the settle tick, the task handlers and
  the reapers started are the projection's.
- **The evidence.** `ListingIntake` (common, pure): the landing's scrape-health rules (empty,
  fallback, rewire, depth, breadth) decide a venue's ACCEPTED listing instead of which slots to
  write. `IdentityListingIntake` keeps it in `identity_listings` (the archive's shape and rules),
  reading `cinema_scrapes`' last listing for a venue it has none for — how a cutover starts.
- **The projection.** `IdentityProjectionPlan` (common, pure) and `IdentityProjection` (worker):
  1. stored films → listing sets (the slot's own `ListingKey`, else `PipelineFilms`), numbered
     through `identity_film_ids` (`FilmIdCounters.covering`: a legacy `title|year` id keeps its
     counter);
  2. the resolver's clusters, one per TMDB film (`movies`' unique `tmdbId`), get ids by overlap
     (`IdAssigner`); a fresh id is minted as today (`FilmId.fresh`) and appended to the map;
  3. each film's slots are its listings' rows unioned per slot (`ScrapeListing.prepare`, the
     extracted `CinemaSlotBuilder`), every showtime kept (P4), over the previous film's
     enrichment when the TMDB film is unchanged and over none when it changed; a cluster matching
     no film is concluded (`tmdbAttempt` "identity-resolver");
  4. `ProjectionGuard` (§11): refused if > 2% of cards or > 0.5% of upcoming showtimes would go,
     for 3 projections running;
  5. a film new to its TMDB id gets its details BY ID (`MovieService.withFilmDetails`, shared with
     `refillTmdbSlot`), then title / year / key; two films of one title and year are
     `title|year` (older) and `title~<counter>|year`;
  6. only changed films are written, through `MovieCache.writeProjected` / `retireProjected` (no
     identity gate; the unique indexes decide write order), and a film whose TMDB answer changed
     is announced to IMDb-id recovery and ratings (`announceResolvedNewMovie`).
- **Lookups.** `CutoverIdentityLookups`: `TmdbIdentityLookups` over the observation store first
  and the pipeline's own observed fetch / detail enrichers for a gap.
- **Metrics** (`IdentityCutoverMetrics`, charted on worker-diagnostics):
  `kinowo_worker_identity_cutover_films|listings{country,path}`,
  `kinowo_worker_identity_regroupings_total{country,kind}`,
  `kinowo_worker_identity_cutover_canary{country,relation}` (the shadow diff's relations, against
  the films stored before each projection), `kinowo_worker_identity_projection_refusals_total`,
  `kinowo_worker_identity_projection_seconds`.

### 18.2 Proof

- Switch off: `FilmScheduleEndToEndSpec` (`expected-schedules.txt`, the read-model snapshot) and the
  page snapshots unchanged.
- `IdentityCutoverEndToEndSpec` (e2e): the recorded Poznań corpus with Poland cut over — 4,802
  listings → 720 films (342 matched), 955 cards served, ratings fetched; P3, P4; a projection over
  its own output writes nothing (P2). It found two P2 breaks on the way (the details builder
  dropped `searchTitle`; the canary compared resolver clusters instead of stored films).
- `IdentityCutoverIntegrationSpec` (itAll), all five hard-cluster corpora cut over: P1 (two orders,
  ids included; half-then-full arrival, same films), P2, P3, P4, the switch-over keeping every id
  `IdAssigner` gives the old path's films, and switching back serving every showtime.

| hard clusters | listings | films (matched) | old path → projection: ids kept, regroupings (merge, split, move, fresh, retire) | canary identical / split / merged / moved |
|---|---|---|---|---|
| ES | 44 | 9 (9) | 9 → 9: 9, (0, 0, 0, 0, 0) | 9 / 0 / 0 / 0 |
| DE | 45 | 11 (10) | 11 → 11: 11, (0, 0, 0, 0, 0) | 11 / 0 / 0 / 0 |
| UK | 78 | 24 (13) | 25 → 24: 24, (1, 0, 5, 0, 1) | 21 / 0 / 1 / 2 |
| US | 107 | 36 (25) | 31 → 36: 31, (0, 3, 6, 5, 0) | 26 / 8 / 0 / 2 |
| PL | 147 | 49 (19) | 24 → 49: 23, (2, 7, 32, 26, 1) | 13 / 33 / 2 / 1 |

Hardcoding added: none in the identity decision. `ProjectionGuard`'s 2% / 0.5% are §11's rollout
thresholds, its grace the scrape guards' own constant.

