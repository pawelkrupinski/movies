# Stable film id

**Goal (Paweł, 2026-09-06):** the periodic settle must in effect never merge or
split anything. Onboarding puts a screening in the right place; the settle
exists to self-heal after a bug fix, not to finish the scrape path's work.

## Why the key had to stop being the identity

A film's storage identity was the string `sanitize(displayTitle)|year`
(`CacheKey`), used as the `_id` of `movies` and embedded in the ids of
`movie_slots`, `screenings`, `web_movies` and `web_screenings`. Both halves of
that string are functions of mutable state: the year is TMDB's once the film
resolves (measured: key year == TMDB year for 100% of resolved rows), and the
spelling is `displayTitle`, the dominant cinema-reported form — which changes
as venues' slots come and go. A primary key that is a pure function of mutable
state re-keys by construction, and every re-key was a delete-and-upsert across
three collections plus a read-model reprojection.

Nine days of production logs (2026-08-28 → 09-06): 1,211 key moves.

| shape | share | example |
|---|---|---|
| decorated listing folded onto its film | 63% | `gbfallenangelsbynoelcoward\|` → `fallenangelsbynoelcoward\|2026`, 92× |
| same title, year changed | 18% | `99seasonthematrix\|` → `99seasonthematrix\|1999` |
| other cross-title | 19% | `nct1275thtour…` → `nct127…` (a search-tier strip) |

122 pairs ping-pong (both directions occur). `99seasonthematrix|1999` ↔
`cineworld30thematrix|1999` flipped 12 times each way in three days: the same
film, two decorated Cineworld spellings, and a key that follows whichever
spelling the slot set makes dominant that tick.

The first 63% was closed at landing time (`TitleContainment`, 6c37ed48b). The
rest is the key itself.

## The design

- `FilmId` is an opaque string, the `_id` of a `movies` document, assigned when
  the row is created and never changed. New rows get `f<15 hex>`, derived from
  the key they were created under (so the fixture corpus is deterministic) and
  bumped on the rare collision with a live id. **Legacy rows keep their current
  `_id` as their `FilmId`** — `persepolis|2007` is just an opaque string now.
  Nothing parses it. No data migration.
- `key` is a field on the document: the current `sanitize(title)|year` lookup
  key, indexed, backfilled from `_id` at worker boot for documents that predate
  it. The in-memory `MovieCache` still keys its map by `CacheKey`; `CorpusIndex`
  maps each key to its `FilmId`.
- A **re-key is now a retitle**: the Caffeine entry moves to the new `CacheKey`
  and the same document is upserted with a new `key`. `movie_slots`,
  `screenings` and the read model never see it.
- A **merge** (two documents that turn out to be one film — the tmdbId fold, an
  imdbId fold, the staging fold's retirements) is the only thing that still
  moves side-collection rows, through `SideCollectionMove`, exactly as before.
- The staging fold plans by id: a yearless newcomer that concludes its year
  keeps its `FilmId` and changes its `key`; a genuine merge retires the loser's
  id into the winner's. The cache's two fold paths (`collapseCluster`, the
  write-time tmdbId fold) follow the same rule: the survivor is an EXISTING
  member's id — the row at the canonical key, else the best-ranked member —
  and a canonical key no member holds is a retitle of that member.
- Ids are minted from the key a row is first created under, so a replay of the
  same arrivals assigns the same ids, and two replays in different orders may
  id one film differently. Opaque means nothing may care; the determinism specs
  compare everything but the id.

## Phases

1. `FilmId` + `key` field; repository addressed by id; `rekey` stops moving
   side rows; staging fold plans by id. Legacy ids preserved, boot backfill of
   `key`. (this document's commit series)
2. `tmdbId` unique sparse index on `movies` — the write-time fold already
   merges a duplicate; the index is the safety net.
3. Read model `_id` = `FilmId` (`ReadModelProjection.filmId` today re-derives
   `sanitize(title)|resolvedYear`). Insert-then-prune on the reconcile, and a
   whole-corpus reprojection on deploy.
4. Delete what this displaced: the hydrate-time orphan reap, `applyDelete`'s
   id→key search, the determinism specs that pin key moves, `RekeyReason`
   metrics become retitle counts.

## Measuring

`kinowo_worker_merges_total` / `kinowo_worker_rekeys_total` by reason, and the
`re-key <old> -> <new>` lines at logs.kinowo.net. A retitle no longer logs a
side-collection move, so the log line count itself is the before/after.
