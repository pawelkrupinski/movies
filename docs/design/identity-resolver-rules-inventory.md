# Identity resolver: rules inventory (2026-10-06)

Every rule the identity resolver and its agreement stage decide by — guard, veto, denial, fill, take, correction,
signal — with where it lives, what it reads, what pins it, how often it fires, and what else implements the same
concept. Written before the consolidation of `identity-resolver.md` §21; line numbers are at `984f4857b` (the
equivalence harness's commit, on `c1cc08eb9`).

## How it was measured

- **Fires** on the five recorded corpora come from two runs, both deterministic and both the equivalence proof of
  every later phase:
  - the FULL corpora (recording `seed-20261002`: PL 9,484 listings, UK 29,755, DE 19,553, US 101,514, ES 4,855) through
    `IdentityResolveDumpIntegrationSpec` — every listing's decision, explanation and trace rules (`accept:`, `pooled:`,
    `veto:`, `join:`, `apart:`, `refused:`) and its candidates' denials. Counted **per listing**. The recording answers
    part of the corpus only (unanswered queries PL 4,135, UK 1,979, DE 3,345, US 2,612, ES 469), so the counts are the
    rules' reach on what it answers, not prod's;
  - the five checked-in UNMATCHED-cluster captures (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`, the
    ratchet's) through `scripts.IdentityEquivalence` — the agreement stage over the model's no-matches (`agreed`), and
    the resolver over the captures' listings with the agreement over that (`resolved`, `resolved-agreed`). Agreement
    rules are counted **per cluster** (527 clusters).
- A rule that fires on neither is **dead on the corpora**; it is dead in fact only if no caller can reach it.
- "Pinned by" names the spec(s) that fail when the rule goes (common = `common/src/test/.../identity/`, worker =
  `worker/src/test/.../identity/`). AS = AcceptanceSpec, IRC = IdentityResolverCasesSpec, ITS = IdentityTraceSpec.

## Headline

- **About 130 named rules and predicates**: 21 acceptance rules and helpers, 12 scoring denials and vetoes (the 28
  learned cannot-links counted once), 12 clustering and voting rules, ~40 title-relation and fact predicates the rules
  read, 22 agreement-stage guards, takes and corrections, 16 unified fill guards and rules (beside ~25 signals).
- **12 duplicated concepts** (list (a) below): the same idea implemented 2–7 times, most of all *year/director
  contradiction* (7 copies, two of them with different semantics), *bill / several works* (7), *stage relay / house*
  (6), *edition* (6), *title names the film* (4), *runtime tolerance* (5), *catalogue provenance* (4).
- **Dead**: 4 functions with no production caller (`FilmCuts.billedCut`, both `FilmCuts.nearestRuntime`,
  `IdentityMeasures.Film.releasedIn`); `UnifiedRules.correct` is never read; the unified fill's poster signals can never
  be set in production (the fill is built with no posters). On the corpora: the `cast.venue` fill, the
  `same-catalogue-id` must-link, `Correction` (no model take is in the captures), and 6 more rules never fire but each is
  pinned by a spec — kept.
- **Iterative processes**: 25 loops; every one is bounded. Measured: `Families.grow` takes exactly 2 rounds on every
  input (the second only confirms), `IncrementalResolver.update` ≤ 2 rounds, `IdentityMeasures.shapes` ≤ 6
  (p99 3), `CaptureReplay.spliced` 1. See §4.

## 1. The resolver (model)

### 1.1 Acceptance — what a node or a cluster takes (`Acceptance.scala`)

Alone rules are tried in list order (`Acceptance.scala:71`), first accept wins; `season-production` pre-empts them and
`billsSeveralWorks` gates them all. Fires = listings whose decision names the rule (`accept:` alone, `pooled:` pooled).

| rule | line | evidence | fires (full corpora, listings) | pinned by |
|---|---|---|---|---|
| billsSeveralWorks (gate) | 90 | "+" bills, MultiFilmBill marker, `billedAlone` | veto `bills-both-works` 167; refused `alone` 340 | IRC double bill, marathon cases; AS |
| billedAlone / takesNoBill (filter) | 100, 117 | `billedOne`, candidate title keys | in the 340 above | IRC marathon case |
| season-production | 417 | `seasonProduction`, `own()` tie-break | 9,466 (UK 4,589, US 4,838) | IRC broadcast-season cases |
| sole-work | 459 | rank 1, `titleIsWorkOf`, ≥2 words | 2,337 | AS, ITS |
| favoured-calibrated | 344 | probability vs `showsRatings`, `favours`, `closerThan` | 145,584 | ITS |
| exact-top-hit | 174 | exact title + rank 1, `speaksAgainst`, `fitsBetter`, class | 383 | ITS, IRC |
| segment-top-hit | 198 | segment relation, rank 1, `standsForTheWhole`, titleYear | 59 | IRC, ITS, AS |
| sole-result | 302 | only hit of a title search, `cutOnly` | 278 | AS, IRC |
| imdb-suggested | 232 | IMDb place, AKA, director, year | 294 | AS, IRC |
| directors-work | 484 | same director, shared work, `BilledRuntime`=15 | 360 | AS, ITS, IRC |
| directors-title | 508 | exact title, same director, `withoutHollow` | 8 | AS, UnifiedEvidenceSpec |
| dated-title | 518 | title's year, `namedButForItsYear` | 92 | AS, ITS, IRC |
| house-production | 554 | `houseProduction`, titleYear | 908 | AS (5), IRC |
| stage-production | 575 | listing year, stage works, season record | 4 (DE) | IRC (indirect) |
| season-record | 594 | season-stripped title equality | 128 | AS, IRC |
| unrivalled-calibrated (pooled) | 396 | calibrated, `closerThan`, naming pieces | 130 | IRC pooled cases |
| editionNamed (filter) | 625 | `namedAs`, `editionOf` | filter | IRC edition cases |
| leaning (no-match lean) | 361 | `LeanMargin`=1.25 | display | AS, IRC |
| fallback (no-match source) | 434 | rivalling title, corroborated | display | AS, IRC |
| contradicted (helper, 8 rules) | 447 | year > `PublishedAdjacency`, runtime ≥ 30 | helper | indirect |
| classAccepted (helper, 4 rules) | 54 | class probability, `showsRatings` | helper | IRC |

Refusals: every listing no alone rule took carries one `refused:<rule>:<why>` per rule (4,685–4,920 listings each).

### 1.2 Scoring — denials and vetoes (`CandidateScoring`, `FamilyScope`, `ReleaseVeto`, `EvidenceWeights`)

| rule | file:line | evidence | fires (candidate × listing) | pinned by |
|---|---|---|---|---|
| director's other film | FamilyScope:92 | same director, title not naming it | 102,502 | IRC |
| learned listing-film cannot-links (28, data) | CandidateScoring:31 via identity-weights.json | measures | 11,350 + 9,655 + … (14 kinds seen) | IRC learned cases |
| seasonsApart | CandidateScoring:31 (ListingConstraints) | season years | 4,925 | IRC |
| another house's season production | CandidateScoring:33 | `Houses` | 2,123 | IRC |
| another instalment | FamilyScope:94 | numeral relation | 768 | AS, IRC |
| ReleaseVeto (not released in CC) | ReleaseVeto:24 | country releases, namesake | 753 | IRC |
| namesOnlyItsVenue | CandidateScoring:42 | naming pieces vs venue/city | 25 | IRC |
| namesOnlyATag | CandidateScoring:57 | capitalised recurring tag | 2 | IRC |
| pinned never | FamilyScope:138 | pins | 0 (no pins in recordings) | IRC, PinConstraintsSpec |
| pooled member denial | FamilyScope:168 | members' denials | in pooled | IRC |
| titledBy (IMDb AKA) | FamilyScope:40 | `soleImdbTitled`, `takesImdbTitle` | pre-step | IRC |
| speaksAgainst / fitsBetter / priorsLent / factsProbability | EvidenceWeights:48/73/102/33 | weights | helpers | IRC, ResolverAllocationSpec |

### 1.3 Clustering and voting (`Families`, `ConstraintEdges`, `ConstraintSolver`, `ClusterVoting`, `PinConstraints`)

| rule | file:line | fires (full corpora, listings) | pinned by |
|---|---|---|---|
| must-link same-film (tier 1) | ConstraintEdges:91 | 110,504 | ConstraintSolverSpec, IRC |
| must-link same-title / same-search-form / original-title / title-segment | ConstraintEdges | 14,470 / 1,983 / 682 / 2,151 | IRC |
| must-link same-catalogue-id (tier 2) | ConstraintEdges:80 | **0** | IRC chain-id cases |
| cannot-link denies-film / different-films | ConstraintEdges | 23,417 / 22,309 | IRC |
| cannot-link learned listing-listing (data) | ConstraintEdges:33 | 15,296 + 3,719 + 3,204 + … | IRC |
| cannot-link seasonsApart | ConstraintEdges:45 | 2,371 | IRC |
| withoutSiblingDenials / deniedBySibling | Families:30/41 | in `accept` withdrawals (refused `withdrawn` 379) | IRC |
| namesBeside | ConstraintEdges:68 | in edges | IRC |
| ConstraintSolver tiers, cannot wins | ConstraintSolver:53 | every cluster | ConstraintSolverSpec, IRP |
| ClusterVoting.votedFor / vote / familyMajority | ClusterVoting:25/40/68 | pooled 130 | IRC |
| contested catalogue ids | IdentityResolver:328 | 0 | none found |
| PinConstraints admits / must / cannot | PinConstraints:85–111 | 0 | PinConstraintsSpec, IRC |

### 1.4 Measures the rules read (`IdentityMeasures`, `TitleLinks`, `MultiFilmBill`, `StageWorks`, `FilmCuts`, `CastEvidence`)

Title relation (`titleRelation` :837: exact via articleless / one-typo / Latin key, original, alternative, segment,
decorated, fragment, overlap, none); `numeralRelation` :1097; `namesSeasonProduction` :131; `billing`/`Houses` :183–292;
`Qualifiers` :325; `editionOf` :373; `namingPieces` :887; `titleIsWorkOf`/`sharesWork` :788–811; `standsForTheWhole` :1226;
`takesImdbTitle` :1364; `originalFormRelation` :1032; `venueTitles` :941; `titlesByFacts` :967; `shapes` :698;
`directorRelation`/`creditRelation` :647/667 (+ `creditedBySearch`); `countryRelation` :1140; `runtimeDelta` :1170
(+ `FilmCuts`, `billsAnEdition`); `runtimeContradicts` :1461 (30 min); `corroboratingVenues` :1395; `billsStageWork`
:1284; `billsTwoWorks`/`billsTwoWholeWorks` :1242/1247; `MultiFilmBill.marker`/`billedOne`/`namedBy`/`billsBeside`;
`TitleLinks.titleLinked`/`titlesBeside`/`Cut`; `CastEvidence.take` (≥2 names). All pinned by IdentityMeasuresSpec /
IRC / TitleDecorationsSpec / MultiFilmBillSpec / FilmCutsSpec / CastEvidenceSpec.

## 2. The agreement stage (`agreement/`)

### 2.1 The take order, per model decision (`AgreementStage.applied`)

1. a model take (`OwnMatch`/`PooledMatch`) not an event → **correction** (§2.3);
2. a decision with a film, a fallback, unanswered questions or no members passes;
3. an **event** no film database holds → `Event` (NonFilmEvents);
4. the families' **verdict** (stored, else resolved; the **fill** is computed with it when nothing agreed);
5. the **agreed** film → taken, pending, vetoed by a poster, or untaken; agreed for a listing billing a stage work →
   the **broadcast** take first, else the agreed film;
6. vetoed or untaken → **poster vote** → **broadcast** → **fill** → **catalogue** → as the model left it.

Fires, per cluster of the five captures (527 clusters): event 57 (marathon 28, mystery screening 10, live event 8,
no screening 5, esports 3, pass 3), agreed 55, fill 22, poster vote 16, broadcast 12 (9 on the day, 3 after),
catalogue 1. Corroboration among the agreed: tmdb 18 (+ with listing/feed), feed 9, listing 12, runtime 8, venues 6,
catalogue 1. Fill rules: venues.current 7, families.current 5, family.filmweb.took&!model.unscored 5,
model.lean&count.leaning&title.namesIt 2, family.facts 1, count.takers2&venues.current 1,
and.leanTakers&!family.dissent 1, **cast.venue 0**.

### 2.2 Agreement guards (`Agreement.agreed`) and takes

| rule | file:line | evidence | pinned by |
|---|---|---|---|
| Quorum (3) + dissent + turnedDown + catalogue dissent | Agreement:162, 245, 293 | picks, leans, corroboration | AgreementSpec |
| namesIt / namedPieceByPiece | Agreement:431 / 452 | listing titles vs record titles | AgreementSpec |
| anothersOwnTitle (when completed) | Agreement:381 | original titles weighed | AgreementSpec |
| contradictedByTheListing / ByTheCatalogue / ByAnyListing | Agreement:359–369 | year, director | AgreementSpec, AgreementStageSpec |
| listingVotes / factVotes (listing, runtime, feed) | Agreement:317, 336 | year, director, runtime ±5 | AgreementSpec, AgreementStageSpec |
| Venues corroboration (≥3 venues, this year or last) | Agreement:278 | venues | AgreementSpec |
| ModelVote / Catalogue corroboration | Agreement:269, 276 | lean, catalogue ids | AgreementSpec |
| billsSeveralBeside | Agreement:519 | MultiFilmBill, zestaw, quoted, two whole works | AgreementSpec |
| stagesAWork / screenAdaptation / billsAHouse | Agreement:485, 493, 501 | StageWorks, HouseWords | AgreementSpec |
| namesFilms (review sites alone take nothing) | AgreementStage:648 | family | AgreementStageSpec |
| poster veto / poster vote (4 / 8 / 10 bits) | AgreementStage:642, 685; Posters:144 | hashes | AgreementPosterSpec, PosterEvidenceSpec |
| PosterEvidence.shows / editionsApart | Posters:133, 171 | provenance, stage, bill, numbers | PosterEvidenceSpec |
| broadcast take (takeOrWait, fits, credits, spells; EncoreDays 60) | Broadcast:37, 131–184 | days, release, banner | AgreementBroadcastSpec |
| fill (unified rules) | AgreementStage:555, 579; UnifiedEvidence:308 | contenders, signals | AgreementStageSpec, UnifiedEvidenceSpec |
| catalogue take (oneFilm, fedOnly, uncorroborable, corroborated) | agreement/Catalogue:72–136 | ids, page links, title picks | AgreementCatalogueSpec |
| NonFilmEvents (Markers, FilmAttached, relays, ConcertFilm) | NonFilmEvents:61, 70 | raw title | NonFilmEventsSpec, NonFilmEventsFixtureSpec |

### 2.3 Corrections (`correctionOf`, `Correction.decide`)

superseded relay (Broadcast:72, `RelayRunDays` 365) → posters (own first, then the far ones) → the venues' Filmweb
programmes (only when a poster questions the take) → the families (only then) → `decide` (two kinds name one film:
switch; Filmweb, or posters and families on one film sharing no director: withdraw). **0 fires on the captures** (they
hold no model take); pinned by AgreementCorrectionSpec (12), VenueListingsSpec, MatchedTakesLabelSpec's
`noLongerServed`.

### 2.4 The unified fill's signals and guards (`UnifiedEvidence`, `identity-unified-rules.json`)

Guards: `bill.several`, `stage.work`, `title.anothersOwn`, `listing.contradicts`, `poster.otherMatches`,
`family.turnedDown`, `title.namesNone`, `edition.apart`. Signals recompute the agreement's concepts
(`agreement.quorum` runs `Agreement.agreed`; `venues.current`; dissent; turnedDown; namesIt; poster vote/match/veto;
catalogue match; `broadcast.take` via `Broadcast.take`). Rules: the fill `venues.current` and seven pinned rules (§2.1).

## 3. Flags

### (a) Duplicates and near-duplicates

1. **Year and director contradiction — 7 copies.** `Agreement.contradicts` (year > 1, `different` director, no shared
   stem), `Correction.contradicts`/`otherPeople` (the same, but `different_script` counts too), `Agreement.equivalent`
   (year > 1), `Agreement.listingVotes` (year ≤ 1, same person), `Broadcast.fitsButTheHouse`/`credits`/`superseded`
   (`PublishedAdjacency`, literal 1), `Posters.editionsApart` (year > 1), `IdentityMeasures` :972/:988/:1009/:1233/:1365/
   :1522 and `Acceptance` :212/:520/:560 (`PublishedAdjacency`, literal 1). **Consolidated** (phase 1:
   `FactRelations`). The two contradiction semantics disagreed on names in two scripts — **finding F1**, unified.
2. **Bill / several works — 7.** `Acceptance.billsSeveralWorks`, `Agreement.billsSeveral`/`billsSeveralBeside`/
   `billsSeveralBySigns` (zestaw, quoted), `IdentityMeasures.billsTwoWorks`/`billsTwoWholeWorks`, `MultiFilmBill`,
   `DecorationSegments.billsSeveral`, `NonFilmEvents` "marathon" marker, `ConstraintEdges`:40.
3. **Stage relay / house — 6.** `Agreement.stagesAWork`, `IdentityMeasures.billsStageWork`/`stageWorks`,
   `Agreement.billsAHouse` (hard-coded `HouseWords`), `Broadcast.billsAHouse(title)` (stage work + banner),
   `NonFilmEvents.relays`, `IdentityMeasures.billsUnderItsHouse` (learned `Houses`), `UnifiedEvidence` `stage.work`.
4. **Banner spelling — 2.** `Broadcast.Billing.spells` (subset, or ≥2 shared words) vs `IdentityMeasures.spellsItsHouse`
   (≥2 shared words, no subset case) — **finding F2**.
5. **Edition — 6.** `Agreement.editionOf(SourceRecord)` (same director, year no earlier, title tokens),
   `IdentityMeasures.editionOf(Film)` (segment/decorated relation, later year), `PosterEvidence.editionsApart` (numbers),
   `IdentityMeasures.billsAnEdition`, `DecorationSegments.billsAnEdition` (`EditionWords`), `FilmCuts`.
6. **Catalogue provenance — 4.** `CatalogueSources.feedStated`/`catalogueEntry`, `Listing.factsFromCatalogue`,
   `Broadcast.superseded`'s `ownFacts` (re-derived, broader: any linked catalogue page), the catalogue-id-equals-pick test
   in `Agreement.supported` and `UnifiedEvidence` `listing.catalogue`.
7. **Title names the film — 4.** `IdentityMeasures.names`/`NamingRelations`, `Agreement.namesIt` (own leading
   articles), `IdentityMeasures.backsFilm`, `Agreement.namedWithinAnother`. Two leading-article lists were one each —
   **finding F3**, unified into `IdentityMeasures.LeadingArticles` (15, five languages).
8. **Runtime tolerance — 5.** `runtimeDelta`, `runtimeContradicts` (30), `Agreement.runsAsTheListing` (±5, own lookup
   incl. `FilmCuts`), `Agreement.equivalent` (±2), `Acceptance.BilledRuntime` (15).
9. **Director same-ness — 3 in Acceptance.** `Acceptance.imdbSuggestedWhy`'s local `sameDirector` repeated
   `IdentityMeasures.sameDirector` — **consolidated** (phase 1).
10. **The "titled top hit" override** `title=exact, search.rank=1, rivals=0` built twice in Acceptance (:271, :327) —
    **consolidated** (phase 1, `asItsTitlesTopHit`).
11. **The broadcast join twice** — **finding F4**, unified: one `Broadcast.take` (productions credited, the
    undated-record wait as a `Left`) for the stage's take, a correction's switch and the fill's `broadcast.take` signal.
12. **The agreement's concepts twice.** `UnifiedEvidence` recomputes quorum, venues, dissent, turnedDown, namesIt, poster
    vote/veto, catalogue match and broadcast as fill signals; the stage's hand-written takes apply the same concepts.

### (b) Dead

- No production caller: `FilmCuts.billedCut` (spec only), both `FilmCuts.nearestRuntime` (no caller at all),
  `IdentityMeasures.Film.releasedIn` (spec only; `ReleaseVeto` reads `knownReleasedIn`).
- Never read: `UnifiedRules.correct` (always empty).
- Unreachable in production: the fill's `poster.vote`/`poster.match`/`poster.near`/`poster.otherMatches`/
  `and.posterTakers` and the IMDb-find join of a family pick (`AgreementStage.filledOf` builds the evidence with no
  posters and `tmdbOf = _ => None`); the `poster.otherMatches` guard is always false there (the veto is applied after,
  in `filledTake`; kept on purpose, §5 F5). `title.namesIt` and `title.namesNone` are exact complements.
- Offline only (not dead, not production): `UnifiedWeights`, `DecorationScore`/`DecorationTokens`/`DecorationSegments`
  fits (`DecorationScore.cut` = 1 accepts nothing).
- Zero fires on the corpora, pinned by specs (kept): `cast.venue`, `same-catalogue-id`, contested catalogue ids, pins,
  `Correction` (no model take in the captures), the agreed-for-a-stage-work → broadcast branch (no stage-level spec —
  **untested**, see (c)).

### (c) Order

Load-bearing: the alone rules' list order and `season-production` pre-empting; `billsSeveralWorks` before them;
`editionNamed` before `takesNoBill`; the must-link tiers (`take(1)`); cannot-wins; event before verdict (cost); agreed
before the fall-through; the stage-work branch letting a broadcast override an agreed screen adaptation; `superseded`
first among corrections (it saves every read); posters → programmes → families (each gated on the last).

Accidental or fragile:
- `catalogued` runs LAST, after the fill, though an exact id is the stronger evidence — where both take, the fill
  wins. On the captures no cluster has both, so the order decides nothing there; kept (the registry records it).
- `voted` before `broadcast`: `PosterEvidence.shows` already drops relay listings, so it decides only mixed clusters.
- `correctable` is checked before `eventOf`, and `eventOf` is read again in the else branch.
- `Acceptance.confidenceOf` reads top-hit → calibrated → pooled whatever rule took the film; pooled lists
  `unrivalled-calibrated` before `exact-top-hit`, alone the reverse.
- `FamilyScope.score`: the director-other-film denial wins over the instalment denial by `if/else` order.
- `imdb-suggested` filters `denied` only; the other rules use `eligibleOf` (also drops `suggestedOnly`).

### (d) One-off parameters that belong in the config

`Agreement.Quorum` 3, `WidelyBilled` 3, `RuntimeSlack` 5, `equivalent`'s ±2 minutes and 4-letter words, the
`thisYear - 1` currency window (Agreement, UnifiedEvidence ×2), `Acceptance.LeanMargin` 1.25 and `BilledRuntime` 15,
`IdentityMeasures.RuntimeContradiction` 30, `PosterEvidence.VoteBits` 4 / `VetoMatchBits` 8 / `VetoBits` 10,
`Broadcast.EncoreDays` 60 / `RelayRunDays` 365, `CastEvidence.Names` 2, `FamilyLookups.Records` 6, the VoterFamily prior
spreads, `UnifiedEvidence.Guards` (duplicated by the JSON's `guards`), and the word lists (`HouseWords`,
`LeadingArticles`, `EventWords`, `EditionWords`, `NonFilmEvents.Markers`, `MultiFilmBill.Markers`). Not moved
(behaviour-preserving brief; listed for the refit).

## 4. Iterative and recursive processes

Iterations measured with a temporary probe over the five captures (resolver + agreement, 6,005 resolves), the full
corpora (5 resolves), and the identity/hard-cluster integration specs (1,210 resolves, 230 incremental updates).

| process | file | iterates to | bound | measured max / p99 | simpler form |
|---|---|---|---|---|---|
| Families.grow | Families:66 | matched films stable | monotone on a finite set | **2 / 2** everywhere: the 2nd round only confirms | stop when the films merged no family (phase 2: 6,004 of 6,005 small resolves end after 1) |
| IncrementalResolver.update | IncrementalResolver:214 | no family joins another | monotone (`replaced`, `settled`) | 2 / 2 (0: 32, 1: 192, 2: 6) | a genuine fixed point (resolve makes new block keys); kept |
| IdentityMeasures.shapes | IdentityMeasures:698 | no new shape | finite substrings | 6 / 3 (full), 5 / 3 (captures) | already a worklist |
| FamilyClosure.families | FamilyClosure:51 | union-find | one pass | — | already union-find (path compression) |
| ConstraintSolver.solveAs | ConstraintSolver:53 | tiers | one pass per tier | — | union-find; `cannot` sets merged without small-to-large |
| PinConstraints.groupOf / IncrementalResolver.pack | | union-find | one pass | — | already |
| ProjectionScope.close | ProjectionScope:86 | closure | each node once | — | already a worklist |
| AgreementStage.applied | | one pass of `orElse` per decision | `CorrectionsPerApply` 200, posters 64 | — | not a fixed point: answers filed → next apply |
| SearchTitles.candidates, TitleDecorations.strip | | single pass | — | — | — |
| CaptureReplay.spliced (refit) | CaptureReplay:93 | closure over shared listings | monotone | **1** | union-find, no rounds (phase 2) |
| CaptureReplay.answering | CaptureReplay:27 | questions answered live | 5 runs | — | kept |
| LogisticFit.fit / fitSigned / newton | LogisticFit | fixed Newton steps; active set ≤ 4n+4 | explicit | — | fixed count is deliberate (artefact equality) |
| IdentityRefit.search | IdentityRefit:329 | greedy coordinate ascent | `maxChanges` 5 | — | kept (order-sensitive by design, deterministic) |
| retries/backoff (EventTrigger, TmdbGapMemory, …) | | capped doubling | explicit | — | — |

CPU: the resolve-perf work of 09-29/30 put the resolver's cost in scoring and title relations, not in the loops; the
loops' own overhead is the confirming `grow` round (grouping, `takenAlone` memo reads, sibling denials).

## 5. Findings: two copies disagreeing (F1, F3, F4 unified and F5 kept, each pinned in `IdentityRuleFindingsSpec`; F2 apart on purpose)

- **F1 — a credit in another script. UNIFIED.** The agreement's listing contradiction (`Agreement.contradicts`) read
  only names in one script as other people; a correction's (`Correction.contradicts`, `shareDirector`) read
  `different_script` too. Now one reading, `FactRelations.otherPeople`: other people in either script, sharing no
  name's stem — the stems taken in Latin letters (`IdentityMeasures.latinized`), so "Andrei Tarkovsky" and
  "Андрей Тарковский" share "andr"/"tark" and contradict nothing (before, a Cyrillic name had no stem at all, and the
  correction's across-script read could not tell one person transliterated from another). The agreement's guard is the
  stricter of the two now (it rules out more, takes nothing more). Ratchet 507 right / 0 wrong before and after;
  FilmScheduleEndToEndSpec unchanged.
- **F2 — spelling a house.** `Broadcast.Billing.spells` accepts a banner that is a subset of the house's words ("Opera"
  of "The Metropolitan Opera"), `IdentityMeasures.spellsItsHouse` only two shared words. Not a bug: the resolver's
  rule takes a season record on the banner alone, where a subset would teach the Paris Opera's banner to be the Met;
  the broadcast join has the screening day beside it. Kept apart on purpose.
- **F3 — leading articles. UNIFIED.** `IdentityMeasures`' article-less exact title knew "the", "a", "an";
  `Agreement.namesIt` fifteen in five languages. One list now, `IdentityMeasures.LeadingArticles` (the fifteen): the
  measures read DE "Camp der Verlorenen" as TMDB's "Das Camp der Verlorenen" `exact`, as the agreement already named it.
  The other direction (the agreement down to English) would lose that agreed take. The measures' own thresholds stay
  (three words after the article, no banner, never the listing's own article dropped). Ratchet 507 right / 0 wrong
  before and after; FilmScheduleEndToEndSpec unchanged.
- **F4 — the broadcast join twice. UNIFIED.** The stage took by `Broadcast.takeOrWait` (a credited production; waiting
  on undated records), the fill's `broadcast.take` signal by `Broadcast.take` (neither). Now one `Broadcast.take`
  returning `Either` (a `Left` while it waits); the fill's evidence carries the stage's productions read and its
  undated-record wait (`ClusterEvidence.productions`/`undated`, wired in `AgreementStage.filledOf`), and a reader that
  cannot wait reads a `Left` as no take. A correction's switch to a relay (`relayed`) reads it with no productions and
  no record wait, as before — except that a cluster whose screening days could not be read is now no take there either
  (fewer switches, never a new one). No selected fill rule or guard reads `broadcast.take` (it is a fitted feature
  only), so no production take moves: ratchet 507 right / 0 wrong before and after; FilmScheduleEndToEndSpec unchanged.
- **F5 — the fill's poster guard. KEPT (investigated).** `AgreementStage.filledOf` builds the fill's evidence with
  no posters, so `poster.otherMatches` never rules a contender out before a rule picks; `filledTake` vetoes the picked
  film after and then takes nothing, where the offline fit picked the other contender (or, a rule firing on two, the
  one the poster leaves). WHY prod never has them: the fill is computed with the verdict (`resolved`) and stored in it,
  while a cluster's posters are asked only by the fall-through takes (`voted`, `filledTake`), which run after a verdict
  exists — and a stored verdict is re-read only when a family answer it read is filed, never when its posters are
  hashed. Measured: re-reading the fill at take time with the cluster's posters (every family verdict recomputed) moved
  no fill on the ratchet's captures (40 poster-bearing clusters reached it, 0 fills changed; FilmScheduleEndToEndSpec's
  corpus reaches it with none). Wiring it for real needs the stored verdict to carry its poster read (re-resolved when
  the hashes land) or every untaken poster cluster's family verdicts recomputed per apply (the 2026-10-04 cost shape) —
  for no measured take. Kept as the safer read (fewer takes than measured, never a wrong one); pinned by
  `IdentityRuleFindingsSpec` F5.

## 6. What the consolidation did (branch commits)

| phase | commit | what |
|---|---|---|
| 0 | `984f4857b`, `8815a78d5` | the equivalence harness (`scripts.IdentityEquivalence`); this inventory |
| 1 | `c7d29bcb3` | `FactRelations`: one year / director comparison for 7 copies; Acceptance's local `sameDirector` and twice-built titled-top-hit measures |
| 2 | `defd77079` | `Families.grow` stops when its films merge no family; `CaptureReplay.spliced` by union-find |
| 3 | `68a43ae29` | `ListingShape`: stage relay, house, several works, whose facts — out of Agreement, NonFilmEvents, Broadcast |
| 4 | `8efc54c13` | the agreement stage's fall-through takes as an ordered registry; the rules table (§21.2) held to the code by a spec |
| 5 | `309f7c85e` | dead `FilmCuts` helpers; the UFF tag's works pinned as searched by their own pieces |
| 6 | the slot commit | a festival's programme slot ("…Film Festival 2026: Opening Night") is an event and names no film; findings F1–F5 pending |
| 7 | the findings commits | F1 one director-contradiction reading across scripts, stems latinized; F3 one leading-article list; F4 one broadcast join; F5 kept, investigated (ratchet 507 right / 0 wrong throughout) |

Deferred: unifying the edition detectors (§3 (a) 5: their semantics differ — a family record's edition, a title's
qualifier, a poster's number — so one would change decisions), `UnifiedEvidence` re-reading the agreement's concepts
as signals (§3 (a) 12: a fill-model redesign, not a refactor), the one-off parameters (§3 (d): left for the refit),
`ConstraintSolver`'s cannot-set merge (small-to-large saves only the `++=`, the back-reference loop stays), the
`catalogue`-after-`fill` order (no capture cluster has both; kept, now named in the registry).
