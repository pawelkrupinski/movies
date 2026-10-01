# Identity cutover runbook (programme phase 5, one country at a time)

What the switch does, when a country may take it, how to flip it, how to read it, and how to
flip it back. The design is `docs/design/identity-resolver.md` §8 ("cutover, per country"), §10,
§11 and §18 (as landed). Nothing here is enabled anywhere yet: `KINOWO_IDENTITY_CUTOVER` is unset
in every overlay.

## 1. What the switch changes

`KINOWO_IDENTITY_CUTOVER` is a comma-separated list of country codes, read once by the worker's
composition root (`modules.wiring.IdentityCutoverWiring`). For a named country:

| | old path (switch off) | cut over (switch on) |
|---|---|---|
| a finished scrape | `MovieCache.recordCinemaScrape` → `ScrapeLanding` (divert, redirect, re-key, prune) | `IdentityListingIntake`: the scrape-health guards decide the venue's ACCEPTED listing (`identity_listings`) |
| identity | staging fold, settle (`FilmCanonicalizer`, `MixedFilmSplitter`, `collapseCluster`, `settleResolved`), `UnresolvedTmdbReaper` concluding | `IdentityProjection` every `KINOWO_IDENTITY_PROJECTION_SECONDS` (300): resolve all families, ids by overlap through `identity_film_ids`, write the films that changed, retire the ids nothing overlaps |
| TMDB details of a film | the resolve writes them | fetched BY ID (`MovieService.withFilmDetails`) for a film new to its record |
| queued `ResolveTmdb` / `RefreshAllTmdb` / `Staging*` tasks | run | completed unrun (`CutoverTaskHandlers`) |
| `EnrichDetails` (a venue's detail page) | runs, then resolves on `MovieDetailsComplete` | runs — the page is source data the model reads — without that resolve |
| unresolved-TMDB and staging reapers | started | not started (the detail reaper runs on both paths) |
| TMDB / IMDb lookups the resolver asks | — | the normalized store first (`StoredTmdbLookups` over `TmdbStore`), the live service only for what it lacks (`StoredFirstLookups`), filed back into the store — so a take-up reads the store rather than asking every question again |
| a listing's venue details (director, runtime, original title) | — | the pipeline's enriched slots (`VenueDetailLookups` over `VenueDetailSlots`); a chain's shared slot answers a page only when its row names one |

Unchanged either way: `movies` / `movie_slots` / `screenings` keep their shape (slots still carry
`listingKey`), `ReadModelProjector`, every rating / IMDb-id / share-card enrichment keyed by the
film, the web tier.

## 2. Preconditions, per country (all must hold)

1. **Dual write deployed and backfilled** (§16.4): `kinowo_worker_listing_key_unstamped_rows{country}`
   reads 0 for both collections.
2. **A week of `ListingKeyShadowRead` agreement**: `KINOWO_LISTING_KEY_SHADOW_READ=true` in the
   country's overlay for 7 consecutive days, and
   `kinowo_worker_listing_key_shadow_read_rows{outcome!="agree"}` = 0 over that week (the 73 PL
   fold-hidden listings of §16.5 excepted; they are the projection's to fix, one slot per listing).
3. **The FilmId map is seeded** (`scripts.FilmIdCounterSeed --apply`; done 2026-09-26 for all five).
4. **The model's TMDB store is filled**: `KINOWO_IDENTITY_SHADOW_LOOKUPS=true` with the shadow run
   until its gaps settle — `kinowo_worker_identity_shadow_lookups{outcome="deferred"}` at 0 and the
   round's `asked` small — so the cut-over projection's lookups are answered from the normalized store
   (`TmdbStore`) and it asks TMDB live only on a gap. Venue details need no gate of their own: the
   detail reaper fills `VenueDetailSlots` on both paths, and the shadow run already reads them.
5. **The no-worse gate** from the combined measurement (§15.5, §15.7, §17.3), re-run on a recording
   no older than a week, on the country's full corpus:
   - 0 cannot-link violations, 0 order variants;
   - accuracy of matched ≥ the pipeline's, labelled recall ≥ the pipeline's − 0.5 points;
   - the shadow run's `identical` ≥ 97% of films (`kinowo_worker_identity_shadow_films`) for 7 days;
   - `IdentityCutoverIntegrationSpec` green on the country's hard clusters (P1–P4, ids, rollback);
   - the country's cut-over convergence leg green: judged against production's SHADOW run
     (`ShadowCoverage` in `prod-coverage-<cc>.json`, since 88052dca8), the pipeline's coverage band
     reported only — a faithful new model is not failed for differing from the old one;
   - the seeding review's "split" + "fresh (film went elsewhere)" ≤ 1% of films (§16.3).

Order: **ES → DE → UK → US → PL**, each only after the previous one has held §10's phase-3
acceptance for a week. What blocks each today is in §6.

## 3. Flipping it on

In `movies-gitops`, `worker/overlays/<cc>/patch.yaml`, `ConfigMap worker-env`, `data:` — one
line (Reloader restarts the worker; the web tier is untouched):

```yaml
  # Identity phase 5: this country's films are the identity projection's
  # (docs/design/identity-cutover-runbook.md). Remove the line to switch back.
  KINOWO_IDENTITY_CUTOVER: "<cc>"
```

The value is the country's OWN code — each worker runs one country (`KINOWO_COUNTRIES`), and a
worker only reads its own. Optional: `KINOWO_IDENTITY_PROJECTION_SECONDS` (default 300).

**The first projection** (≈5 minutes after the worker starts) seeds every id from today's films
(`IdAssigner` over the FilmId map): a film keeps its id, URL, ratings and share card wherever its
listings stay together. It is guarded: if it would take more than 2% of the cards or 0.5% of the
upcoming showtimes off the site, it is refused (`kinowo_worker_identity_projection_refusals_total{reason="shrink"}`)
and the old path's films keep serving; after 3 refusals running the shrink is taken as real and
written. On any refusal of the FIRST projection, switch back (§5) and investigate — do not wait
out the grace.

## 4. Reading it (worker-diagnostics dashboard, "Identity cutover" panels)

- `kinowo_worker_identity_cutover_canary{country,relation}` on the first projection: the old path's
  films against the projection's. `identical` should match the shadow run's figure from the gate.
- `kinowo_worker_identity_regroupings_total{kind=merge|split|move}`: 0 after the first projection on
  an unchanged corpus (P2) — §10's churn acceptance, together with `kinowo_worker_rekeys_total` and
  `kinowo_worker_merges_total{reason!="imdb-identity"}` at 0 for 7 days.
- `kinowo_worker_identity_cutover_films` within ±2% of the pre-cutover `kinowo_worker_corpus_movies`;
  served showtimes (`kinowo_worker_*` city showtime census) within ±0.5%.
- `ReadModelServingDiffers` must not fire; read-model drift gauges at baseline.
- `kinowo_worker_identity_projection_refusals_total` flat.

## 5. Switching back

Remove the `KINOWO_IDENTITY_CUTOVER` line (or the country from it). On restart the country is on the
old path again, over the projection's rows:

- the rows are ordinary `movies` / `movie_slots` / `screenings` rows, keyed and shaped as the old path
  writes them, so the landing re-lands every listing onto them (proven per country by
  `IdentityCutoverIntegrationSpec` "switching BACK": every published showtime served, keys unique);
- films the resolver kept apart and the old path would have joined are joined again by the next
  settle — ids retire the old path's way (`MergeReason`), not lost;
- a pair of films the projection stored under one title and year (the older plain `title|year`, the
  other `title~<counter>|year`) is re-landed by title onto the plain one; the `~` row loses its slots
  and is retired by `UnscreenedCleanup`;
- `identity_listings` and `identity_film_ids` are left as they are: the map is append-only and a
  later re-cutover numbers from it.

Rollback is data-safe at any time: showtimes are re-derived from the scrapes, never stored only in
the projection's own state. No manual reseed is needed in either direction.

## 6. What blocks each country today (2026-10-01)

Offline: CI measurement `Identity measure` run 36759100370 (main c90f2363e + 78b6f082c, recording
36691290304), the full corpora; labelled coverage and accuracy as each corpus's report gives them
for every accepted decision. Production: the workers' metrics at 2026-09-30 ~23:00 UTC.

| | ES | DE | UK | US | PL |
|---|---|---|---|---|---|
| labelled accuracy, resolver / pipeline | 100.0 / 100.0% | 100.0 / 100.0% | 100.0 / 100.0% | 100.0 / 99.6% | 100.0 / 100.0% |
| labelled coverage, resolver / pipeline | 99.6 / 99.3% | 99.3 / 98.0% | 97.5 / 87.9% | 98.0 / 93.6% | 91.1 / 92.8% |
| gate 5 accuracy + coverage (−0.5 pt) | met | met | met | met | **not met** (−1.7 pt) |
| shadow read (1–2) | 500/500 agree | 500/500 | 500/500 | 500/500 | 500/500 |
| TMDB store filled (4): fill `deferred` | 0 — met | 0 — met | 0 — met | 0 — met | 0 — met |
| shadow `identical` (prod) | 226/241 = 93.8% | 1,725/1,797 = 96.0% | 1,429/1,542 = 92.7% | 2,117/2,255 = 93.9% | 889/1,252 = 71.0% |

The earliest 7-day shadow-read windows close ~Oct 3 (ES/DE) and ~Oct 4 (PL/UK), ~Oct 6 (US). Every
country's lookup fill has caught up (`deferred` 0, the last round asking 0–278), so `identical` now
moves with the resolver's rules and the corpus, not with the fill. PL's coverage gap is mostly
listings the referee cannot verify either way — double bills (54, unmatched by decision), titles TMDB
holds no Polish title for (Lalka/Dolly, Superfutrzak, Róża) — itemised in the identity progress notes.

**PROPOSED, awaiting a decision — the `identical ≥ 97%` gate.** It compares against the OLD
pipeline, so the resolver's corrections count as failures. ES has no lookups left to fill and
holds at 94.6%: of its 13 non-identical films, the resolver is right on at least four (*The Dark
Knight Rises* and *All We Imagine as Light*, which the pipeline filed as *The Dark Knight* and
*La luz*; *Dune: Part Three* and *Cars* listings the pipeline left unmatched), and the pipeline on
four coverage losses (Leonas, Carmen (OPERA LIVE), Manon (BALLET LIVE), Lumière). Proposal: count
a film as agreeing when it is `identical` OR the offline adjudication (§17.3) rules it
resolver-right; keep 97%. Until decided, the gate stands as written in §2.

Relearning the weights on today's code is NOT yet a win: the listing↔listing cannot-link cut
falls from 0.268 to 0.034 (it is certified below the single lowest same-film unit), merging
e.g. Odeon's "The Gruffalo + The Gruffalo's Child" double bill (28 new wrong in the UK) for PL's
Lalka ×9 fixed and ~80 right lost net. Under investigation; r5 stays.

## 7. Phase 6: what becomes dead once all five are on

Delete only after the LAST country has held phase-3 acceptance for a week (§8 "Phase 4: delete").
Then no wiring reaches any of the following, and each goes with its specs:

- **Landing**: `ScrapeLanding` (divert / redirect / variant re-key / prune; its guard logic now lives
  in `ListingIntake`), `ListingLanding`, `LandingStore`, `MovieCache.recordCinemaScrape` and
  `ScrapeSink`'s cache implementation, `CorpusIndex`'s decoration and search-key lookups,
  `ScrapeLandingMetrics`.
- **Staging**: `StagingFold`, `MongoStagingFolder`, `StagingFolder`, `FoldOnStagingEnriched`,
  `StagingSteps`, the four `Staging*Handler`s, `StagingReaper`, `StagingStuckAlerter`,
  `StagingRepository` and the `pending_movies` collection, `StagingNewcomerDiverted`.
- **Settle**: `FilmCanonicalizer`'s clustering, `MixedFilmSplitter`, `SettleReaper`'s settle (the
  reaper itself stays as the projection's scheduler, renamed), `MovieService.settle`,
  `MovieCache.collapseCluster` / `settleResolved` / `backfillEmbeddedYears` / `canonicalizeBySanitize`
  / `rekey`, every `RekeyReason`, every `MergeReason` but `IdAssigner`'s merge reporting, the
  write-time tmdbId / imdbId fold in `CaffeineMovieCache.put`, `SideCollectionMove` /
  `MovieRepository.moveFilm`.
- **Resolution**: `UnresolvedTmdbReaper`'s concluding and re-try, `MovieService.resolveTmdbOnce`,
  `retryUnresolvedTmdb`, `retryResolve`, `forceResolve`, `reexamineResolution`, `resolveStagingRecord`,
  `ResolveTmdbHandler`, `TmdbCandidateSearch` (the resolver's `CandidateQueries` replaces it; its
  `ImdbDisambiguatorSuffix`, which `TmdbIdentityLookups` reads, moves there),
  `TmdbAttempt`'s fingerprints (a no-match is the TMDB store's answer), `ResolveDispatcher`s.
- **Deferred detail as an identity trigger**: `EnrichDetailsHandler`'s TMDB trigger,
  `MovieDetailsComplete` and `detailPending` (the resolver reads venue details as lookups). NOT the
  `DetailReaper` / `DetailTaskEnqueuer` / `EnrichDetails` fetch itself: it fills `VenueDetailSlots`,
  the model's venue-detail input, and stays.
- **The shadow run**: `ShadowIdentityReaper`, `ShadowRunStore`, `identity_shadow_*` (the canary
  replaces it), and `ListingKeyShadowRead` / `UnstampedListingCensus` once every row is stamped by
  construction.
- **The switch itself**: `KINOWO_IDENTITY_CUTOVER`, `IdentityCutoverCountries`, `CutoverTaskHandlers`,
  the `landing = None` default of `CinemaScrapeRunner`: the projection becomes the only path.
- **Keys**: `CacheKey` as an identity (it stays the display/lookup string on `movies.key`), and
  the cache's key-collision and cold-mirror guards that exist only because two paths could key one film.
