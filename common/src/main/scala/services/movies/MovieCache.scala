package services.movies

import models.{Cinema, CinemaMovie, MovieRecord, Source, SourceData}

/**
 * Read surface of the enrichment store — pure reads, no side effects.
 *
 * Split out (ISP) so consumers that only resolve a `CacheKey` or walk the rows
 * (the reapers/enqueuers) depend on just this, and are compile-time barred from
 * mutating the cache. `MovieCache` adds the mutating surface; `CaffeineMovieCache`
 * implements both. Per CLAUDE.md DIP guidance, consumers depend on these traits,
 * not the concrete implementation.
 */
trait MovieCacheReader {
  /** The country whose title rules key this cache's rows. Exposed on the READ
   *  surface because the enrichers and the read-model projection hold a cache and
   *  need to fold titles the same way it did — a resolution hint key or a
   *  projected `_id` built under different rules addresses a row that isn't
   *  there. ABSTRACT: an implementation that could silently fall back to a
   *  process default is how a German title came to be keyed `minionsimonster`. */
  def normalizer: TitleNormalizer

  /** Stable snapshot for debug tooling — sorted by title (case-insensitive). */
  def snapshot(): Seq[StoredMovieRecord]

  /** Wall-clock instant of the most recent data mutation. */
  def lastModified: java.time.Instant

  // ── Internal read surface (services.* only) ──────────────────────────────
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey
  private[services] def get(key: CacheKey): Option[MovieRecord]
  private[services] def entries: Seq[(CacheKey, MovieRecord)]
}

/**
 * Where a venue's fresh scrape goes: `IdentityListingIntake` takes it as the venue's published listing
 * for the identity projection (docs/design/identity-resolver.md §8).
 *
 * `listingIsComplete = false` means the caller KNOWS this listing is short — a chunked scrape reduced
 * from only some of its date-chunks — so a film missing from it is not evidence that it stopped
 * screening. Every scrape DECORATOR must forward it ([[services.cinemas.common.DelegatingCinemaScraper]]).
 * `sourceKey` names the upstream listing this scrape read (`CinemaScraper.sourceKey`); `viaFallback`
 * says a FALLBACK served it because the venue's own source is down ([[ScrapeHealth]]).
 */
trait ScrapeSink {
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie],
                         listingIsComplete: Boolean = true,
                         sourceKey: Option[String] = None,
                         viaFallback: Boolean = false): Unit
}

/**
 * In-memory enrichment store with write-through to the underlying `MovieRepository`.
 * Adds the mutating surface on top of `MovieCacheReader`.
 *
 * Per CLAUDE.md DIP guidance: consumers depend on this trait (or the narrower
 * `MovieCacheReader`), not the concrete implementation. `CaffeineMovieCache` is
 * the production implementation; tests can swap in any other implementation if
 * they ever need to.
 */
trait MovieCache extends MovieCacheReader {
  /** Reload the positive cache from the repository: drop every in-memory positive
   *  entry, then `repository.findAll()` and put each row. Returns the number of
   *  rows loaded. Used at construction and by the admin rehydrate endpoint. */
  def rehydrate(): Int

  /** Call `listener` with each film another writer than the identity projection changed here — a write through this
   *  cache that moved a film, or another process's change the change stream brought that differs from the film held —
   *  never with the projection's own (`writeProjected`, `patchProjected`, `retireProjected`) or their echo. What the
   *  projection reads of a film another writer moved is projected on that, not on a period. */
  def onChanged(listener: FilmId => Unit): Unit

  /** Call `listener` with each key whose held film changed here, by ANY path — every writer's, the identity
   *  projection's included, the change stream's and a rehydrate's — with the film as now held, or `None` once it is gone.
   *  Called first with every film held when it registers, so a listener starts from the whole cache. Each key's calls
   *  come in order, under that key's lock: a listener must be quick, and must not touch this cache. */
  def onResident(listener: (CacheKey, Option[StoredMovieRecord]) => Unit): Unit

  /** Whether a corpus read has ever been answered whole, so the films held here are the whole corpus rather than the
   *  rows written since boot. */
  def hydrated: Boolean

  // ── Internal write surface (services.* only) ─────────────────────────────
  private[services] def put(key: CacheKey, e: MovieRecord): WriteOutcome
  /** The identity projection's write (docs/design/identity-resolver.md §8, phase 5): film `id` AS
   *  the resolver made it, stored under `key`. No identity gate — no tmdbId or imdbId fold — because
   *  the projection, not the write, decides identity; the store's unique `key` and `tmdbId` indexes
   *  still refuse a second holder (`WriteOutcome.IdentityHeld`). A write under a new key retitles
   *  the film: its old key leaves the cache with it. */
  private[services] def writeProjected(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome
  /** [[writeProjected]] of a film the projection rewrites under the id, key and TMDB id it is held under, as only what
   *  differs from `before` — the record this cache holds for it, as the projection read it: the changed fields, venue
   *  slot rows and screenings (`MovieRepository.updateIfPresent`), never the film whole, so a film at hundreds of
   *  venues that moved at one is not read back and rewritten whole. Anything else — the row no longer resident as
   *  `before`, another id, key or TMDB id, a patch that fails, a store without the showtimes and slots split — is
   *  written whole, by [[writeProjected]]. */
  private[services] def patchProjected(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord,
                                       only: Option[Set[Source]] = None): WriteOutcome
  /** Whether the films held here keep every slot lean — the showtimes in `screenings`, the slots in `movie_slots` — so a
   *  slot written lean is one the store already holds the showtimes of. */
  private[services] def holdsSlotsLean: Boolean
  /** Remove film `id` — one the projection retired — with its side rows, from the store and the cache. */
  private[services] def retireProjected(id: FilmId): WriteOutcome
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
  /** [[putIfPresent]] of an update to `sources`' slots alone, each set to `slotOf` what it held — in work independent of
   *  the row's other slots. Like `putIfPresent`, true says the row is resident and holds the update, so with no
   *  `sources` it is true of any resident row: it does not say a slot took anything. */
  private[services] def putSlotsIfPresent(key: CacheKey, sources: Seq[Source])(slotOf: (Source, Option[SourceData]) => SourceData): Boolean
  /** Like [[get]], but falls back to a direct `movies` read when the cache doesn't
   *  hold `key`. The TMDB resolve's carry-forward reads this rather than the
   *  Caffeine-only `get`, so a cold / evicted / re-keyed entry can't make the
   *  rebuild read EMPTY and null a persisted rating (or cinema slot). */
  private[services] def stored(key: CacheKey): Option[MovieRecord] = storedChecked(key).toOptionOrThrow

  /** [[stored]] as the read it is — absent ("this film is new") told apart from failed. A failed
   *  read taken for absent makes a caller rebuild a live film from scratch, carrying only what
   *  the current scrape saw. See [[MovieRepository.findByIdChecked]]. */
  private[services] def storedChecked(key: CacheKey): tools.ReadOutcome[MovieRecord]

  private[services] def invalidate(key: CacheKey): Unit
  /** Run `body` under the per-normalised-title lock. Any read-modify-write
   *  across the cache's surface for keys sharing this `cleanTitle` must
   *  happen inside this block to be serialised against `recordCinemaScrape`,
   *  `rekey`, and other concurrent operations on the same title. */
  private[services] def withTitleLock[A](cleanTitle: String)(body: => A): A
  /** Re-kick the enrichments whose INPUT fields an enrichment write (not a merge)
   *  changed — e.g. Filmweb writing a director / originalTitle onto its slot
   *  re-attempts the TMDB / IMDb resolution its new hint might now crack, and the
   *  title-ratings when the row is resolved. Same pure decision + retrigger port as
   *  the merge path ([[MergeRetrigger.changedEnrichments]]); `before`/`after` are
   *  the row at `key` around the write, so they share the key. No-op when nothing an
   *  enrichment reads changed. */
  private[services] def retriggerAfterEnrichment(key: CacheKey, before: MovieRecord, after: MovieRecord): Unit
}
