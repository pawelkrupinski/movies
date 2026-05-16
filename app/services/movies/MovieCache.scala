package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, CinemaScrape, CinemaShowings, MovieRecord}
import play.api.Logging

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

/**
 * Normalised `(title, year)` lookup key. Diacritics stripped, lowercased,
 * whitespace squashed — so "Drzewo Magii" and "drzewo magii" don't end up as
 * two cache entries. Equality uses the normalised form; `cleanTitle` retains
 * the original casing for display in snapshots.
 */
private[services] case class CacheKey(cleanTitle: String, year: Option[Int]) {
  private val normalized = MovieService.normalize(cleanTitle)
  override def hashCode(): Int = (normalized, year).hashCode()
  override def equals(other: Any): Boolean = other match {
    case k: CacheKey => k.normalized == normalized && k.year == year
    case _           => false
  }
}

/**
 * In-memory enrichment store with write-through to the underlying `MovieRepo`.
 *
 * Per CLAUDE.md DIP guidance: consumers depend on this trait, not the concrete
 * implementation. `CaffeineMovieCache` is the production implementation;
 * tests can swap in any other implementation if they ever need to.
 */
trait MovieCache {
  // ── Public read surface (controllers, ShowtimeCache) ─────────────────────

  /** Apply one cinema's fresh scrape to the cache. Returns one
   *  `(CinemaMovie, CacheKey, isNew)` triple per input movie. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)]

  /** True when some existing cache row's cleanTitle normalises to the same
   *  form as `rawTitle` AND has been TMDB-resolved (tmdbId set). */
  def hasResolvedSiblingByTitle(rawTitle: String): Boolean

  /** Stable snapshot for debug tooling — sorted by title (case-insensitive). */
  def snapshot(): Seq[StoredMovieRecord]

  /** Reload the positive cache from the repo: drop every in-memory positive
   *  entry, then `repo.findAll()` and put each row. Returns the number of
   *  rows loaded. Leaves the negative cache (24h-TTL TMDB miss markers)
   *  alone — negatives aren't persisted in Mongo, so they're orthogonal to
   *  this reload. Used at construction and by the admin rehydrate endpoint. */
  def rehydrate(): Int

  // ── Internal surface (services.* only) ───────────────────────────────────
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey
  private[services] def get(key: CacheKey): Option[MovieRecord]
  private[services] def isNegative(key: CacheKey): Boolean
  private[services] def put(key: CacheKey, e: MovieRecord): Unit
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
  private[services] def markMissing(key: CacheKey): Unit
  private[services] def clearNegatives(): Unit
  private[services] def invalidate(key: CacheKey): Unit
  private[services] def entries: Seq[(CacheKey, MovieRecord)]
}

/**
 * Caffeine-backed `MovieCache` with write-through to `MovieRepo`.
 *
 * Two caches:
 *   - **Positive**: successful enrichments, never expire in-process (they
 *     change slowly; restarts re-warm via `rehydrate`).
 *   - **Negative**: known misses (events, festivals, retrospectives that
 *     don't match a real film), 24h TTL — failed TMDB lookups get retried
 *     about once a day. The daily TMDB-retry scheduler can clear the whole
 *     negative cache explicitly via `clearNegatives` so the next refresh tick
 *     gets a fresh shot at every previously-failed key.
 *
 * Pure reads (`get`, `isNegative`, `snapshot`, `entries`) have no side
 * effects — callers that want to *trigger* a lookup on miss go through
 * `MovieService` (which owns the worker pool + dedup).
 */
class CaffeineMovieCache(repo: MovieRepo) extends MovieCache with Logging {

  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

  // Per-normalised-title locks for `recordCinemaScrape`. Two cinemas
  // first-scraping the same brand-new film concurrently used to each see an
  // empty cache in their redirect check and each created its own row at a
  // different `(title, year)` key. Serialising the redirect-then-put step
  // per normalised title eliminates that race; films with different
  // normalised titles still scrape in parallel.
  private val titleLocks = new ConcurrentHashMap[String, AnyRef]()
  private def lockFor(rawTitle: String): AnyRef =
    titleLocks.computeIfAbsent(MovieService.normalize(rawTitle), _ => new Object())

  // Per-tmdbId locks for the `put` identity gate (see below). Serialises the
  // "is there already a row with this tmdbId?" check + the resulting fold,
  // so two threads writing the same freshly-resolved tmdbId to different
  // CacheKeys can't both pass the no-sibling check and produce duplicates.
  // Always acquired *inside* a titleLock when both apply, so the lock order
  // is stable (title → tmdb).
  private val tmdbLocks = new ConcurrentHashMap[Int, AnyRef]()
  private def tmdbLockFor(tmdbId: Int): AnyRef =
    tmdbLocks.computeIfAbsent(tmdbId, _ => new Object())

  rehydrate()

  private[services] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(MovieService.searchTitle(title), year)

  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

  private[services] def isNegative(key: CacheKey): Boolean =
    negative.getIfPresent(key) != null

  /** Persist a row at `key`. **Identity gate**: when `e` carries a `tmdbId`
   *  AND a different cache key already holds that same tmdbId, the write is
   *  folded onto that canonical row instead of creating a duplicate — the
   *  victim's cinema-side data is unioned in via `MovieRecordMerge.union`,
   *  the source key is dropped from both cache and repo. tmdbId is the
   *  identity signal: two CacheKeys with the same tmdbId are the same film
   *  regardless of any year-divergence or title-spelling differences.
   *
   *  This is the only persist path in the codebase — `MovieRepo.upsert` is
   *  called from nowhere else — so the gate is the chokepoint that prevents
   *  new tmdbId-duplicates from ever being written. */
  private[services] def put(key: CacheKey, e: MovieRecord): Unit = e.tmdbId match {
    case Some(tid) =>
      tmdbLockFor(tid).synchronized {
        siblingKeyByTmdb(tid, excluding = key) match {
          case Some(canonical) => foldOnto(canonical, key, e)
          case None            => persist(key, e)
        }
      }
    case None =>
      persist(key, e)
  }

  private def persist(key: CacheKey, e: MovieRecord): Unit = {
    positive.put(key, e)
    repo.upsert(key.cleanTitle, key.year, e)
  }

  private def siblingKeyByTmdb(tid: Int, excluding: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .find { case (k, v) => k != excluding && v.tmdbId.contains(tid) }
      .map(_._1)
  }

  private def foldOnto(canonical: CacheKey, source: CacheKey, victim: MovieRecord): Unit = {
    val current = Option(positive.getIfPresent(canonical)).getOrElse(victim)
    val merged  = MovieRecordMerge.union(current, victim)
    persist(canonical, merged)
    if (source != canonical) {
      positive.invalidate(source)
      repo.delete(source.cleanTitle, source.year)
      logger.info(s"Folded duplicate '${source.cleanTitle}' (${source.year.getOrElse("—")}) " +
                  s"into '${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}) — same tmdbId=${victim.tmdbId.get}.")
    }
  }

  /** Conditional write — applies `updater` to the row if it currently exists
   *  in the cache, otherwise a no-op. Returns true if the write landed.
   *
   *  Used by the rating listeners (`ImdbRatings`, `FilmwebRatings`,
   *  `MetascoreRatings`, `RottenTomatoesRatings`) so a rating fetch that
   *  finishes after a concurrent `cache.invalidate` doesn't resurrect the row.
   *  The updater runs inside Caffeine's per-key compute lock so two
   *  concurrent rating updates on the same key serialize cleanly. The Mongo
   *  side uses `replaceOne(upsert=false)` for the same no-resurrect
   *  guarantee against a concurrent `repo.delete`.
   *
   *  The updater takes the CURRENT value (the freshest cached row) rather
   *  than a captured snapshot — so a listener that read the row, made a
   *  slow network call, and now wants to update one field doesn't clobber
   *  concurrent updates to other fields. */
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean = {
    val updated = positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = updater(current)
    })
    if (updated != null) {
      repo.updateIfPresent(key.cleanTitle, key.year, updated)
      true
    } else false
  }

  private[services] def markMissing(key: CacheKey): Unit =
    negative.put(key, java.lang.Boolean.TRUE)

  /** Drop all negative entries — used by the daily TMDB retry to give every
   *  previously-failed key one fresh shot. New misses re-populate the cache
   *  organically as they happen. */
  private[services] def clearNegatives(): Unit = negative.invalidateAll()

  /** Drop a row from positive cache + Mongo — used by `reEnrich` to clear the
   *  row before re-fetching every upstream source. */
  private[services] def invalidate(key: CacheKey): Unit = {
    positive.invalidate(key)
    repo.delete(key.cleanTitle, key.year)
  }

  /** Apply one cinema's fresh scrape to the cache: for every CinemaMovie in
   *  `movies`, find-or-create the matching record and replace that cinema's
   *  slot in `cinemaShowings`. After processing, prune the cinema's slot
   *  from any record that previously held it but didn't appear this tick.
   *
   *  Variant redirect: when the cinema-reported `(title, year)` doesn't have
   *  a row at its primary key, but exactly one existing row already knows
   *  this raw title (via `cinemaTitles`), we write to THAT row's key instead
   *  of creating a fresh one. Returns one `(CinemaMovie, CacheKey, isNew)`
   *  triple per input movie:
   *
   *    - `CacheKey` is the *canonical* key the slot actually landed on
   *      (post-redirect). `ShowtimeCache` publishes `MovieRecordCreated`
   *      against this so two cinemas reporting different `year` values for
   *      the same film land on a single TMDB-stage event, no phantom row.
   *    - `isNew` is true when the `(cinema, raw title, raw year)` tuple is
   *      landing on this row for the first time; false on repeat ticks.
   *      `ShowtimeCache` skips the bus publish for `isNew == false` so
   *      already-enriched rows don't churn event dispatches every 5 min.
   *
   *  Doesn't touch enrichment-side fields (imdbId, ratings, URLs, …) — the
   *  TMDB / IMDb / MC / RT / Filmweb stages own those and run independently.
   *  Records are kept even when `cinemaShowings` becomes empty (per the
   *  "keep forever" policy): a film that returns next month finds its
   *  prior enrichment data still in place. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, ScrapingAnt 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

    val resolved = movies.map { cm =>
      // Lock per normalised title so a concurrent first-scrape from another
      // cinema can't create a duplicate row at a different `(title, year)`
      // key while we're between the redirect check and the put. Different
      // films don't contend.
      lockFor(cm.movie.title).synchronized {
        val primary = keyOf(cm.movie.title, cm.movie.releaseYear)
        val key     = redirectToExistingVariant(primary).getOrElse(primary)
        val existing = Option(positive.getIfPresent(key))
          .getOrElse(MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None))
        val slot = CinemaShowings(
          filmUrl        = cm.filmUrl,
          posterUrl      = cm.posterUrl,
          synopsis       = cm.synopsis,
          cast           = cm.cast,
          director       = cm.director,
          runtimeMinutes = cm.movie.runtimeMinutes,
          releaseYear    = cm.movie.releaseYear,
          originalTitle  = cm.movie.originalTitle,
          country        = cm.movie.country,
          showtimes      = cm.showtimes
        )
        val scrape = CinemaScrape(cinema, cm.movie.title, cm.movie.releaseYear)
        val isNew  = !existing.cinemaScrapes.contains(scrape)
        // The raw cinema-reported title goes into `cinemaScrapes`; the
        // derived `cinemaTitles` view picks it up automatically.
        put(key, existing.copy(
          cinemaScrapes  = existing.cinemaScrapes + scrape,
          cinemaShowings = existing.cinemaShowings + (cinema -> slot)
        ))
        (cm, key, isNew)
      }
    }

    // Prune: any cache entry that previously had this cinema's slot but
    // wasn't touched this tick → drop the slot. The record itself stays.
    val touched = resolved.iterator.map(_._2).toSet
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .filter { case (k, e) => e.cinemaShowings.contains(cinema) && !touched.contains(k) }
      .foreach { case (k, e) =>
        put(k, e.copy(cinemaShowings = e.cinemaShowings - cinema))
      }

    resolved
  }

  /** If `primary` doesn't currently exist in the cache, look for an existing
   *  row that already knows `primary.cleanTitle` (via its `cinemaTitles`
   *  set). Returns that row's key when there's exactly one such match —
   *  ambiguous (cross-film collision) and zero-match cases fall through to
   *  None and the caller creates a fresh row.
   *
   *  Match uses `MovieService.normalize` so an Arabic/Roman /
   *  punctuation / case variant ("Mortal Kombat 2") redirects onto a row
   *  that only knows the canonical form ("Mortal Kombat II") — without
   *  this, the FIRST cinema's raw spelling would pin the row's
   *  `cinemaTitles` and every other cinema's variant of the same film
   *  would create a separate row at its own key. */
  private def redirectToExistingVariant(primary: CacheKey): Option[CacheKey] = {
    if (positive.getIfPresent(primary) != null) return None
    import scala.jdk.CollectionConverters._
    val normalizedRaw = MovieService.normalize(primary.cleanTitle)
    // Match by cleanTitle normalisation. Cross-script titles produce
    // different normalised forms (sanitize keeps Unicode letters), so a
    // Cyrillic row can't be matched by a Latin scrape and vice versa.
    val candidates = positive.asMap().asScala.iterator
      .filter { case (k, _) => MovieService.normalize(k.cleanTitle) == normalizedRaw }
      .map(_._1)
      .toSet  // unique by CacheKey (which dedups by normalized form)
    if (candidates.size == 1) Some(candidates.head) else None
  }

  def hasResolvedSiblingByTitle(rawTitle: String): Boolean = {
    import scala.jdk.CollectionConverters._
    val normalizedRaw = MovieService.normalize(rawTitle)
    positive.asMap().asScala.iterator.exists { case (k, e) =>
      e.tmdbId.isDefined && MovieService.normalize(k.cleanTitle) == normalizedRaw
    }
  }

  def snapshot(): Seq[StoredMovieRecord] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => StoredMovieRecord(k.cleanTitle, k.year, e) }
      .toSeq
      .sortBy(_.title.toLowerCase)
  }

  /** Snapshot of (key, enrichment) pairs for the IMDb refresh loop. Copy so a
   *  concurrent `put` mid-iteration doesn't surprise the caller. */
  private[services] def entries: Seq[(CacheKey, MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }

  def rehydrate(): Int = {
    positive.invalidateAll()
    val rows = repo.findAll()
    rows.foreach(r => positive.put(CacheKey(r.title, r.year), r.record))
    if (rows.nonEmpty) logger.info(s"Hydrated ${rows.size} enrichment(s) from Mongo.")
    rows.size
  }
}
