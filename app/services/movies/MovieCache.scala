package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, CinemaShowings, MovieRecord}
import play.api.Logging

import java.util.concurrent.TimeUnit

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
 * In-memory enrichment store with write-through to Mongo via `MovieRepo`.
 *
 * Two caches:
 *   - **Positive**: successful enrichments, never expire in-process (they
 *     change slowly; restarts re-warm via `hydrateFromRepo`).
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
class MovieCache(repo: MovieRepo) extends Logging {

  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

  hydrateFromRepo()

  private[services] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(MovieService.searchTitle(title), year)

  /** Pure read — never blocks, never schedules. */
  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

  private[services] def isNegative(key: CacheKey): Boolean =
    negative.getIfPresent(key) != null

  /** Write-through: positive cache + Mongo upsert.
   *
   *  Folds the row's current `cleanTitle` AND the existing cached row's
   *  `cinemaTitles` into the written set, so every raw cinema-reported title
   *  that has ever resolved here is recorded. Cross-script entries are
   *  filtered out so a row's `cinemaTitles` never accumulates spellings in
   *  a script other than the row's own (the `cleanTitle`'s) — legacy
   *  Polish+Cyrillic mixes get cleaned up on the next write. */
  private[services] def put(key: CacheKey, e: MovieRecord): Unit = {
    val priorTitles  = Option(positive.getIfPresent(key)).map(_.cinemaTitles).getOrElse(Set.empty)
    val allVariants  = e.cinemaTitles ++ priorTitles + key.cleanTitle
    val sameScript   = allVariants.filter(t => controllers.TitleNormalizer.sameScript(t, key.cleanTitle))
    val merged = e.copy(cinemaTitles = sameScript)
    positive.put(key, merged)
    repo.upsert(key.cleanTitle, key.year, merged)
  }

  /** Conditional write — applies `updater` to the row if it currently exists
   *  in the cache, otherwise a no-op. Returns true if the write landed.
   *
   *  Used by the rating listeners (`ImdbRatings`, `FilmwebRatings`,
   *  `MetascoreRatings`, `RottenTomatoesRatings`) so a rating fetch that
   *  finishes after `IdentityMerger` deleted the row doesn't resurrect it.
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
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = {
        val next         = updater(current)
        // Same fold + cross-script filter as `put` — keeps cinemaTitles in
        // sync across both write paths.
        val allVariants  = next.cinemaTitles ++ current.cinemaTitles + k.cleanTitle
        val sameScript   = allVariants.filter(t => controllers.TitleNormalizer.sameScript(t, k.cleanTitle))
        next.copy(cinemaTitles = sameScript)
      }
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
   *  of creating a fresh one. This stops the flap loop where a year=None
   *  cinema title got re-created every 5 min only to be deleted by the
   *  next IdentityMerger run.
   *
   *  Doesn't touch enrichment-side fields (imdbId, ratings, URLs, …) — the
   *  TMDB / IMDb / MC / RT / Filmweb stages own those and run independently.
   *  Records are kept even when `cinemaShowings` becomes empty (per the
   *  "keep forever" policy): a film that returns next month finds its
   *  prior enrichment data still in place. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Unit = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, ScrapingAnt 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return

    val touched = movies.map { cm =>
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
        showtimes      = cm.showtimes
      )
      // Always record the incoming raw cinema title in `cinemaTitles` — when
      // a redirect sends us to a row with a different `cleanTitle` ("Mortal
      // Kombat 2" landing on a row whose cleanTitle is "Mortal Kombat II"),
      // we still want this variant tracked so the NEXT scrape of "Mortal
      // Kombat 2" redirects directly via the `cinemaTitles` index, without
      // having to wait for an IdentityMerger pass to fold it in.
      put(key, existing.copy(
        cinemaTitles   = existing.cinemaTitles + cm.movie.title,
        cinemaShowings = existing.cinemaShowings + (cinema -> slot)
      ))
      key
    }.toSet

    // Prune: any cache entry that previously had this cinema's slot but
    // wasn't touched this tick → drop the slot. The record itself stays.
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .filter { case (k, e) => e.cinemaShowings.contains(cinema) && !touched.contains(k) }
      .foreach { case (k, e) =>
        put(k, e.copy(cinemaShowings = e.cinemaShowings - cinema))
      }
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
   *  this, the FIRST cinema's raw spelling pins the row's `cinemaTitles`
   *  and every other cinema's variant of the same film creates a separate
   *  row that IdentityMerger then has to async-clean up. During that
   *  window each row carries a cinema slot and renders as its own card.
   *
   *  Without this redirect, every scrape tick re-creates "year=None" rows
   *  that the IdentityMerger then has to delete again: a wasted TMDB API
   *  call per row per tick, plus a brief window where /debug/enrichment
   *  shows the orphan as a "pending" duplicate. */
  private def redirectToExistingVariant(primary: CacheKey): Option[CacheKey] = {
    if (positive.getIfPresent(primary) != null) return None
    import scala.jdk.CollectionConverters._
    val normalizedRaw = MovieService.normalize(primary.cleanTitle)
    // Require the candidate row's cleanTitle to normalise to the SAME form
    // as the incoming title, in addition to having a matching variant in
    // its cinemaTitles. Without this, a Cyrillic-titled row whose
    // cinemaTitles incidentally includes the Latin spelling (from an
    // earlier cross-script merge) would absorb Latin scrapes and the two
    // scripts would re-merge on every tick — exactly the user-visible
    // collapse `IdentityMerger.isSibling` now refuses.
    val candidates = positive.asMap().asScala.iterator
      .filter { case (k, e) =>
        MovieService.normalize(k.cleanTitle) == normalizedRaw &&
        e.cinemaTitles.exists(t => MovieService.normalize(t) == normalizedRaw)
      }
      .map(_._1)
      .toSet  // unique by CacheKey (which dedups by normalized form)
    if (candidates.size == 1) Some(candidates.head) else None
  }

  /** True when some existing cache row has a `cinemaTitles` entry that
   *  normalises to `rawTitle`'s normalised form AND has been TMDB-resolved
   *  (tmdbId set). The `MovieService` TMDB stage uses this to short-
   *  circuit: if the cinema's title already maps to an existing resolved
   *  row via `recordCinemaScrape`'s redirect, there's no point running
   *  another TMDB lookup for the raw `(title, year)` key and creating a
   *  phantom row that the `IdentityMerger` would have to delete async —
   *  during which window each row carries a cinema slot and renders as
   *  its own card.
   *
   *  Match uses `MovieService.normalize` so cross-spelling variants
   *  ("Mortal Kombat 2" vs "Mortal Kombat II") still short-circuit even
   *  before the merger has had a chance to fold the variants together. */
  def hasResolvedSiblingByTitle(rawTitle: String): Boolean = {
    import scala.jdk.CollectionConverters._
    val normalizedRaw = MovieService.normalize(rawTitle)
    // Require the candidate row's cleanTitle to normalise the same way as
    // `rawTitle`. Without this, a Cyrillic-titled row whose cinemaTitles
    // happens to include the Latin spelling would short-circuit TMDB for
    // the Latin variant — leaving the Latin row uncreated even though
    // it's now meant to be a separate record from the Cyrillic one.
    positive.asMap().asScala.iterator.exists { case (k, e) =>
      e.tmdbId.isDefined &&
      MovieService.normalize(k.cleanTitle) == normalizedRaw &&
      e.cinemaTitles.exists(t => MovieService.normalize(t) == normalizedRaw)
    }
  }

  /** Stable snapshot for debug tooling — sorted by title (case-insensitive). */
  def snapshot(): Seq[(String, Option[Int], MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => (k.cleanTitle, k.year, e) }
      .toSeq
      .sortBy { case (t, _, _) => t.toLowerCase }
  }

  /** Snapshot of (key, enrichment) pairs for the IMDb refresh loop. Copy so a
   *  concurrent `put` mid-iteration doesn't surprise the caller. */
  private[services] def entries: Seq[(CacheKey, MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }

  private def hydrateFromRepo(): Unit = {
    val rows = repo.findAll()
    rows.foreach { case (title, year, e) => positive.put(CacheKey(title, year), e) }
    if (rows.nonEmpty) logger.info(s"Hydrated ${rows.size} enrichment(s) from Mongo.")
  }
}
