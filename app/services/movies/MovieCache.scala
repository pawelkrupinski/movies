package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, MovieRecord, Source, SourceData}
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{CinemaMovieAdded, EventBus, InProcessEventBus}
import tools.{DaemonExecutors, Env, TextNormalization}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.util.Try

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

  /** Wall-clock instant of the most recent data mutation. */
  def lastModified: java.time.Instant

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
  /** Run `body` under the per-normalised-title lock. Any read-modify-write
   *  across the cache's surface for keys sharing this `cleanTitle` must
   *  happen inside this block to be serialised against `recordCinemaScrape`,
   *  `rekey`, and other concurrent operations on the same title. */
  private[services] def withTitleLock[A](cleanTitle: String)(body: => A): A
  /** Move a row from `oldKey` to `newKey`. The `update` function receives
   *  the CURRENT state at `oldKey` (under the per-title lock) and returns
   *  the record to write at `newKey` — so a concurrent cinema-slot write
   *  that landed before `update` runs is visible to it. */
  private[services] def rekey(oldKey: CacheKey, newKey: CacheKey, update: MovieRecord => MovieRecord): Unit
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
class CaffeineMovieCache(
  repo: MovieRepo,
  bus:  EventBus = new InProcessEventBus()
) extends MovieCache with Stoppable with Logging {

  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

  @volatile private var _lastModified: java.time.Instant = java.time.Instant.now()
  def lastModified: java.time.Instant = _lastModified
  private def touch(): Unit = { _lastModified = java.time.Instant.now() }

  // Per-normalised-title locks for `recordCinemaScrape`. Two cinemas
  // first-scraping the same brand-new film concurrently used to each see an
  // empty cache in their redirect check and each created its own row at a
  // different `(title, year)` key. Serialising the redirect-then-put step
  // per normalised title eliminates that race; films with different
  // normalised titles still scrape in parallel.
  private val titleLocks = new ConcurrentHashMap[String, AnyRef]()
  private def lockFor(rawTitle: String): AnyRef =
    titleLocks.computeIfAbsent(MovieService.normalize(rawTitle), _ => new Object())

  /** The lock that serialises every read-modify-write on rows whose
   *  normalised cleanTitle matches `cleanTitle`. Reentrant (JVM
   *  `synchronized`), so a body that itself calls `rekey` or
   *  `recordCinemaScrape` for the same title doesn't self-deadlock. */
  private[services] def withTitleLock[A](cleanTitle: String)(body: => A): A =
    lockFor(cleanTitle).synchronized(body)

  // Per-tmdbId locks for the `put` identity gate (see below). Serialises the
  // "is there already a row with this tmdbId?" check + the resulting fold,
  // so two threads writing the same freshly-resolved tmdbId to different
  // CacheKeys can't both pass the no-sibling check and produce duplicates.
  // Always acquired *inside* a titleLock when both apply, so the lock order
  // is stable (title → tmdb).
  private val tmdbLocks = new ConcurrentHashMap[Int, AnyRef]()
  private def tmdbLockFor(tmdbId: Int): AnyRef =
    tmdbLocks.computeIfAbsent(tmdbId, _ => new Object())

  // Hydrate from Mongo on construction. Synchronous: Wiring builds the cache
  // during `start()`, so the first HTTP request only lands after the initial
  // findAll has completed. Pages render against a fully-populated cache; no
  // first-request flicker, no scrape-vs-hydrate race.
  rehydrate()

  private[services] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(MovieService.searchTitle(title), year)

  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

  private[services] def isNegative(key: CacheKey): Boolean =
    negative.getIfPresent(key) != null

  /** Persist a row at `key`. **Identity gate**: when `e` carries a `tmdbId`
   *  AND a cache key with the SAME normalised cleanTitle already holds that
   *  same tmdbId, the write is folded onto that canonical row instead of
   *  creating a duplicate — the victim's cinema-side data is unioned in via
   *  `MovieRecordMerge.union`, the source key is dropped from both cache and
   *  repo.
   *
   *  Identity check: **same `tmdbId` AND same normalised `cleanTitle`**.
   *  This narrows the gate to the *year-divergence* case ("Viridiana"
   *  Some(1961) vs Some(1962), "Diabeł ubiera się u Prady 2" None vs
   *  Some(2026)) — the films TMDB tells us are one row but cinemas have
   *  reported with different years. Rows that share a tmdbId but have a
   *  genuinely different cleanTitle — Polish/Latin "Diabeł ubiera się u
   *  Prady 2" vs Cyrillic "ДИЯВОЛ НОСИТЬ ПРАДА 2" vs the explicit
   *  Ukrainian-dubbed listing "Diabeł ubiera się u Prady 2 ukraiński
   *  dubbing" — are intentionally kept as separate cards. They target
   *  separate audiences and folding them would force one variant's display
   *  title to be hidden.
   *
   *  The rest of the cache (`redirectToExistingVariant`,
   *  `hasResolvedSiblingByTitle`, `MovieService.sisterRowMatch`) already
   *  enforces the same per-cleanTitle separation via normalised-cleanTitle
   *  comparison — this gate is the one remaining bridge that needed an
   *  explicit guard.
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
    val clean = withoutZeroRatings(e)
    positive.put(key, clean)
    repo.upsert(key.cleanTitle, key.year, clean)
    touch()
  }

  // Rating sources occasionally hand us a literal zero — MC/RT search pages
  // that surface 0% for an unrated title, Filmweb's API for a film with no
  // votes yet, IMDb GraphQL for a brand-new entry. Zero isn't a real rating;
  // persisting it would render a misleading "0/10" badge. Squash to None at
  // the single write boundary so neither Caffeine nor Mongo holds the
  // phantom score. Applied to every write (`persist` and `putIfPresent`),
  // so any future caller automatically inherits the rule.
  private def withoutZeroRatings(e: MovieRecord): MovieRecord = e.copy(
    imdbRating     = e.imdbRating.filter(_ > 0.0),
    metascore      = e.metascore.filter(_ > 0),
    filmwebRating  = e.filmwebRating.filter(_ > 0.0),
    rottenTomatoes = e.rottenTomatoes.filter(_ > 0)
  )

  /** Find an existing cache key carrying the same tmdbId as `excluding`,
   *  filtered to candidates whose normalised cleanTitle matches
   *  `excluding`'s. Different-cleanTitle candidates are intentionally
   *  invisible here — see the `put` docstring above. */
  private def siblingKeyByTmdb(tid: Int, excluding: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    val target = MovieService.normalize(excluding.cleanTitle)
    positive.asMap().asScala.iterator
      .find { case (k, v) =>
        k != excluding &&
        v.tmdbId.contains(tid) &&
        MovieService.normalize(k.cleanTitle) == target
      }
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
    // Capture both `before` and `after` inside the Caffeine compute lock so
    // the pair is atomic. The repo write below uses the pair to compute a
    // per-field diff — out-of-band Mongo edits to fields the updater didn't
    // touch (e.g. `FilmwebUrlAudit` clearing `filmwebUrl` while we're
    // bumping `filmwebRating`) survive the write.
    val before  = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
    val updated = positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = {
        before.set(current)
        withoutZeroRatings(updater(current))
      }
    })
    if (updated != null) {
      repo.updateIfPresent(key.cleanTitle, key.year, before.get(), updated)
      touch()
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
    touch()
  }

  /** Atomically rename a row from `oldKey` to `newKey`, computing the new
   *  value from the row's CURRENT state at `oldKey`. Both the read and
   *  the write happen under the per-cleanTitle lock that
   *  `recordCinemaScrape` also acquires — so:
   *
   *    1. A concurrent scrape can never observe the cache in the empty
   *       window between the invalidate and the put. Without this, a
   *       year=2026 scrape that lands mid-rekey would see no sibling
   *       for "Straszny film" and create a phantom row at (Some(2026))
   *       while the rekey settles at (Some(2000)) — two rows for the
   *       same Polish title.
   *    2. A cinema slot that was written under the same title-lock
   *       just before the rekey acquired it is visible to `update`,
   *       so the new record carries it forward. Without this, the
   *       rekey would overwrite the just-written slot with stale data
   *       the caller captured before the lock — losing the slot
   *       entirely.
   *
   *  Both keys must share the same cleanTitle (same lock). Used by the
   *  TMDB stage when a no-year scrape's resolved year promotes the row
   *  to a year-keyed identity. */
  private[services] def rekey(oldKey: CacheKey, newKey: CacheKey, update: MovieRecord => MovieRecord): Unit = {
    require(MovieService.normalize(oldKey.cleanTitle) == MovieService.normalize(newKey.cleanTitle),
      s"rekey requires same normalised cleanTitle: ${oldKey.cleanTitle} vs ${newKey.cleanTitle}")
    withTitleLock(oldKey.cleanTitle) {
      val current = Option(positive.getIfPresent(oldKey)).getOrElse(MovieRecord())
      val updated = update(current)
      if (oldKey != newKey) invalidate(oldKey)
      put(newKey, updated)
    }
  }

  /** Apply one cinema's fresh scrape to the cache: for every CinemaMovie in
   *  `movies`, find-or-create the matching record and replace that cinema's
   *  slot in `data`. After processing, prune the cinema's slot
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
   *  Records whose cinema-side slots become empty are pruned daily by
   *  `UnscreenedCleanup`; a film that returns after the prune ran will
   *  re-pay the full enrichment cost on its next scrape. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, proxy 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

    // Carry the freshly-written `SourceData` slot alongside the public
    // tuple so the prune below can identify "newly written this tick" by
    // slot reference rather than by cache key — keys can shift mid-tick
    // when a TMDB-stage rekey moves a row out from under us.
    val resolved: Seq[((CinemaMovie, CacheKey, Boolean), SourceData)] = movies.map { cm =>
      // Lock per normalised title so a concurrent first-scrape from another
      // cinema can't create a duplicate row at a different `(title, year)`
      // key while we're between the redirect check and the put. Different
      // films don't contend.
      withTitleLock(cm.movie.title) {
        val primary = keyOf(cm.movie.title, cm.movie.releaseYear)
        val key     = redirectToExistingVariant(primary).getOrElse(primary)
        val existingOpt = Option(positive.getIfPresent(key))
        val existing    = existingOpt.getOrElse(MovieRecord())
        // Two-stage scrapers (today: Kino Muza) leave `cm.posterUrl` /
        // `cm.synopsis` / `cm.trailerUrl` as `None` from the 5-min listing
        // tick — the detail-page refresher owns those three fields. The
        // listing tick mustn't undo that. Rule: the cinema's freshly-
        // scraped value wins when it's `Some`; when the cinema reports
        // `None`, preserve whatever's on the existing slot. Single-stage
        // cinemas (Multikino, Helios, CC, Apollo, …) ship `Some` every
        // tick, so this is a no-op for them — they keep their authoritative
        // per-tick refresh.
        val priorSlot = existing.data.get(cinema)
        // A cinema's freshly-scraped year wins when it reports one; when this
        // tick reports None, preserve the year we already had. Helios sources
        // `releaseYear` solely from a best-effort REST lookup with no fallback
        // (its NUXT listing carries no year), so a transiently-absent REST body
        // makes the year oscillate Some(year)↔None across passes. Without this
        // fallback that drop would (a) wipe the year from the merged view and
        // (b) flip `isNew` to true every pass — the recurring "(N new)" on
        // Helios. mergeDuplicateFilms collapses same-pass duplicate spellings,
        // but it can't see a year that flakes only on a *later* pass; this
        // fallback covers that orthogonal case. Treat a dropped year as data
        // loss, not a correction; a genuine year change (None→Some, or one Some
        // to another) still flows.
        val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
        val slot = SourceData(
          title          = Some(cm.movie.title),
          originalTitle  = cm.movie.originalTitle,
          synopsis       = cm.synopsis.orElse(priorSlot.flatMap(_.synopsis)),
          // Normalise cast at the write boundary: Cinema City returns
          // ALL CAPS comma-lists hard-capped at ~232 chars (the trailing
          // name is sliced mid-word by the upstream JSON field). Drop
          // the truncated tail first, then Title-Case the result.
          // Applied here rather than per-cinema so any future source
          // that ships ALL CAPS or truncates inherits the fix.
          cast           = cm.cast.map(TextNormalization.titleCaseIfAllCaps),
          director       = cm.director.map(TextNormalization.titleCaseIfAllCaps),
          // Some cinema feeds surface runtime as a raw integer that lands as
          // 0 when the upstream field is empty (CC's `length`, Multikino's
          // `runningTime`) or when a parser regex matches "0 min" in
          // unrelated text. Squash to None so a real reading from another
          // cinema isn't outranked by a phantom zero in the merged view.
          runtimeMinutes = cm.movie.runtimeMinutes.filter(_ > 0),
          releaseYear    = effectiveYear,
          // Fold every spelling/alias into a single canonical name per
          // CountryNames — "Stany Zjednoczone" / "USA" / "U.S.A." all
          // become "USA", "UK" / "Wielka Brytania" both become
          // "Wielka Brytania". Stored data is canonical so the merged
          // MovieRecord.countries union/dedup operates on consistent
          // strings.
          countries      = cm.movie.countries.map(CountryNames.canonical).distinct,
          genres         = cm.movie.genres,
          posterUrl      = cm.posterUrl.orElse(priorSlot.flatMap(_.posterUrl)),
          filmUrl        = cm.filmUrl,
          trailerUrl     = cm.trailerUrl.orElse(priorSlot.flatMap(_.trailerUrl)),
          showtimes      = cm.showtimes
        )
        // `isNew` controls whether to publish `MovieRecordCreated` to the bus.
        // We dedup against the prior slot for this cinema — the same `(title,
        // year)` reported tick after tick suppresses the event so downstream
        // listeners don't churn. A pruned-then-re-listed scrape will re-emit
        // (no prior slot to compare against), which is harmless since the
        // listeners early-exit on already-resolved rows.
        val isNew = !priorSlot.exists(s =>
          s.title.contains(cm.movie.title) && s.releaseYear == effectiveYear
        )
        // For existing rows, route through `putIfPresent` so the Mongo write
        // is a `$set`-diff against the (before, after) pair. The diff only
        // touches the cinema slot we just rewrote; any out-of-band edit to a
        // different field of the same record (FilmwebUrlAudit, a per-row
        // backfill, a manual Mongo update) survives — the running cache's
        // stale view of that field is irrelevant because the diff doesn't
        // surface it. For first-time scrapes the row doesn't exist yet, so
        // a full `put` is the right call (also keeps the `tmdbId` identity
        // gate live for the only case it can fire).
        existingOpt match {
          case Some(_) =>
            putIfPresent(key, current => current.copy(data = current.data + ((cinema: Source) -> slot)))
          case None =>
            put(key, existing.copy(data = existing.data + ((cinema: Source) -> slot)))
        }
        ((cm, key, isNew), slot)
      }
    }

    // Prune: any cache entry that previously had this cinema's slot but
    // wasn't touched this tick → drop the slot. The record itself stays.
    //
    // Identify "touched" by the slot's `SourceData` reference rather than
    // by cache key. The prune runs OUTSIDE the per-title lock, so between
    // the per-movie write block above and this iteration a concurrent
    // `cache.rekey` (typically from a TMDB-stage re-key for another
    // film's cinema-event-triggered resolution) can move the row from
    // its original key to a year-keyed sibling — carrying our just-
    // written slot along. A key-based `touched` set would miss the
    // moved row and erroneously prune our slot off it. Slot-identity
    // tracking works because `cache.rekey` preserves the SourceData
    // reference verbatim through the move.
    val touchedSlots: Set[SourceData] = resolved.iterator.map(_._2).toSet
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .filter { case (_, e) =>
        e.data.get(cinema).exists(slot => !touchedSlots.contains(slot))
      }
      .foreach { case (k, e) =>
        put(k, e.copy(data = e.data - cinema))
      }

    // Publish CinemaMovieAdded for each row we just first-scraped onto. This
    // runs AFTER both the slot put and the prune step so a handler reading
    // the cache for `(title, year)` immediately sees the freshly-written
    // slot — eliminating the race ShowtimeCache's caller-side publish would
    // otherwise leave between persist and notify.
    //
    // Gated on `isNew` (same gate as `ShowtimeCache`'s MovieRecordCreated)
    // so steady-state ticks where the same cinema reports the same film
    // again don't refire — the periodic safety net in each detail-page
    // enricher picks up rows whose first event was missed.
    resolved.foreach { case ((cm, key, isNew), _) =>
      if (isNew) bus.publish(CinemaMovieAdded(cinema, key.cleanTitle, key.year, cm.filmUrl))
    }

    resolved.map(_._1)
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
    // Additive sync — never blank the cache mid-rehydrate. The periodic
    // 30-s tick (see `start()` below) runs while page loads are flying
    // through `snapshot()`; an `invalidateAll()` window would briefly
    // render an empty repertoire. Instead: put every Mongo row (cache's
    // copy gets overwritten if it changed), then evict only the keys
    // that disappeared from Mongo since the last sync.
    import scala.jdk.CollectionConverters._
    val tFindAllStart = System.nanoTime()
    val rows          = repo.findAll()
    val tFindAllMs    = (System.nanoTime() - tFindAllStart) / 1000000
    // `repo.findAll()` swallows every Mongo failure into `Seq.empty` — a
    // TLS-selector race, a connection-pool churn, an Atlas-side reset all
    // surface as "no rows". Treating that as "Mongo is genuinely empty,
    // evict every cached row" wipes the live cache on every transient
    // hiccup. Skip the eviction step when the result is empty AND the
    // cache currently has rows: a real Mongo wipe is a degenerate manual
    // operation that's acceptable to handle only on app restart.
    val cachedSize = positive.estimatedSize()
    if (rows.isEmpty && cachedSize > 0) {
      logger.warn(s"MovieCache rehydrate: findAll() returned empty while cache holds $cachedSize row(s) — " +
                  "treating as a transient Mongo failure; cache left intact.")
      return 0
    }
    // Cold-boot empty result — `findAll()` returned nothing AND the cache was
    // already empty. Don't silently start serving an empty repertoire; surface
    // it explicitly so a Mongo timeout / disabled connection / projection bug
    // is obvious in the boot log instead of hiding behind a 200 response with
    // zero films on it. The rest of `rehydrate` is a no-op in this case
    // (`put` over an empty Seq, `invalidate` over an empty set), so the
    // surrounding flow stays the same.
    if (rows.isEmpty && cachedSize == 0) {
      logger.warn(s"MovieCache rehydrate: findAll() returned empty on a cold cache (findAll=${tFindAllMs}ms) — " +
                  "Mongo connection disabled, query timed out, or repo genuinely empty. " +
                  "Pages will render with no films until the next successful tick.")
    }
    val tPostFetch = System.nanoTime()
    val nextKeys = rows.iterator.map(r => CacheKey(r.title, r.year)).toSet
    rows.foreach(r => positive.put(CacheKey(r.title, r.year), r.record))
    positive.asMap().keySet().asScala.toSeq
      .filterNot(nextKeys.contains)
      .foreach(positive.invalidate)
    val tPopulateMs = (System.nanoTime() - tPostFetch) / 1000000
    if (rows.nonEmpty)
      logger.info(s"Hydrated ${rows.size} enrichment(s) from Mongo — findAll=${tFindAllMs}ms populate=${tPopulateMs}ms.")
    touch()
    rows.size
  }

  // ── Mongo → cache sync ─────────────────────────────────────────────────────
  //
  // Out-of-band Mongo edits — `FilmwebUrlAudit` clearing a stale URL, a manual
  // `db.movies.update(...)` to fix one row — bypass the in-memory cache. Two
  // mechanisms keep the cache current:
  //
  //  1. INCREMENTAL (primary): a change stream (`repo.watchUpserts`) applies
  //     each inserted/updated/replaced row to the cache the moment it lands in
  //     Mongo — O(changes), near-instant, and costs nothing when nothing
  //     changes. This replaced a periodic full `findAll()` that re-read the
  //     ENTIRE collection on a timer: at ~200 rows that was ~150 ms (so the old
  //     30-s cadence felt free), but at 500+ rows it climbed to 6–9 s and, run
  //     twice a minute, burned 20–30% of the single shared vCPU continuously and
  //     starved page renders. The change stream makes the cost proportional to
  //     real edits instead of corpus size.
  //
  //  2. BACKSTOP (safety net): an INFREQUENT full `rehydrate()` still runs to
  //     catch out-of-band DELETES (change-stream delete events aren't applied)
  //     and to close any gap if the stream dropped, or to be the only mechanism
  //     on a standalone Mongo that can't stream. The expensive findAll now fires
  //     every `BackstopIntervalSeconds` (default 30 min) instead of every 30 s.
  //     Tunable via KINOWO_CACHE_REHYDRATE_SECONDS.
  //
  // The scheduler + watch only start when `start()` is called (Wiring does,
  // tests don't), so unit tests still get a single one-shot hydrate at
  // construction unless they opt into the live sync.
  private val refreshScheduler        = DaemonExecutors.scheduler("movie-cache-refresh")
  private val BackstopIntervalSeconds = Env.positiveLong("KINOWO_CACHE_REHYDRATE_SECONDS", 1800L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Apply one out-of-band upsert from the change stream to the in-memory cache.
   *  Mirrors a single row of `rehydrate` — a direct `positive.put`, bypassing
   *  the identity-gate `put` (Mongo is already the source of truth here, no
   *  re-folding needed). Deletes are not streamed; the backstop reconciles them. */
  private def applyUpsert(r: StoredMovieRecord): Unit = {
    positive.put(CacheKey(r.title, r.year), r.record)
    touch()
  }

  def start(): Unit = {
    watchHandle = repo.watchUpserts(applyUpsert)
    logger.info(
      s"MovieCache incremental change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
      s"backstop rehydrate every ${BackstopIntervalSeconds}s.")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(rehydrate()).recover {
        case ex => logger.warn(s"MovieCache rehydrate tick failed: ${ex.getMessage}")
      },
      BackstopIntervalSeconds, BackstopIntervalSeconds, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    refreshScheduler.shutdown()
  }
}
