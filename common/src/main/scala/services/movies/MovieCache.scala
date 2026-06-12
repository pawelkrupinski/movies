package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, MovieRecord, Source, SourceData}
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{CinemaMovieAdded, EventBus, InProcessEventBus}
import services.titlerules.TitleRuleKey
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
  private val normalized = TitleNormalizer.sanitize(cleanTitle)
  override def hashCode(): Int = (normalized, year).hashCode()
  override def equals(other: Any): Boolean = other match {
    case k: CacheKey => k.normalized == normalized && k.year == year
    case _           => false
  }
}

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
  /** True when some existing cache row's cleanTitle normalises to the same
   *  form as `rawTitle` AND has been TMDB-resolved (tmdbId set). */
  def hasResolvedSiblingByTitle(rawTitle: String): Boolean

  /** Stable snapshot for debug tooling — sorted by title (case-insensitive). */
  def snapshot(): Seq[StoredMovieRecord]

  /** Wall-clock instant of the most recent data mutation. */
  def lastModified: java.time.Instant

  // ── Internal read surface (services.* only) ──────────────────────────────
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey
  /** The key of the existing row whose normalised cleanTitle matches `key`'s,
   *  regardless of year. An event can carry a pre-canonicalisation `(title,
   *  year)` that no longer addresses the row after `recordCinemaScrape`
   *  promoted it; every enrichment stage resolves through this so its read /
   *  write hits the live row, never a stale or phantom key. None when no row
   *  exists yet. */
  private[services] def canonicalKeyFor(key: CacheKey): Option[CacheKey]
  private[services] def get(key: CacheKey): Option[MovieRecord]
  private[services] def isNegative(key: CacheKey): Boolean
  private[services] def entries: Seq[(CacheKey, MovieRecord)]
}

/**
 * In-memory enrichment store with write-through to the underlying `MovieRepo`.
 * Adds the mutating surface on top of `MovieCacheReader`.
 *
 * Per CLAUDE.md DIP guidance: consumers depend on this trait (or the narrower
 * `MovieCacheReader`), not the concrete implementation. `CaffeineMovieCache` is
 * the production implementation; tests can swap in any other implementation if
 * they ever need to.
 */
trait MovieCache extends MovieCacheReader {
  /** Apply one cinema's fresh scrape to the cache. Returns one
   *  `(CinemaMovie, CacheKey, isNew)` triple per input movie. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)]

  /** Reload the positive cache from the repo: drop every in-memory positive
   *  entry, then `repo.findAll()` and put each row. Returns the number of
   *  rows loaded. Leaves the negative cache (24h-TTL TMDB miss markers)
   *  alone — negatives aren't persisted in Mongo, so they're orthogonal to
   *  this reload. Used at construction and by the admin rehydrate endpoint. */
  def rehydrate(): Int

  // ── Internal write surface (services.* only) ─────────────────────────────
  private[services] def put(key: CacheKey, e: MovieRecord): Unit
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
  private[services] def markMissing(key: CacheKey): Unit
  private[services] def clearNegatives(): Unit
  private[services] def clearNegative(key: CacheKey): Unit
  def canonicalizeBySanitize(): Unit
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
    titleLocks.computeIfAbsent(TitleNormalizer.sanitize(rawTitle), _ => new Object())

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
    CacheKey(TitleNormalizer.searchTitle(title), year)

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
          case Some(siblingKey) => foldDeterministically(key, e, siblingKey)
          case None             => persist(key, e)
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
    val target = TitleNormalizer.sanitize(excluding.cleanTitle)
    // `minByOption`, not `find`: a same-tmdbId row can have more than one
    // sibling (year + yearless + a dub variant), and `asMap` iteration order
    // is not stable across JVM builds / platforms. Pick the canonical-rank
    // minimum so the chosen sibling — and therefore the fold result — is a
    // pure function of the cache contents, not iteration order.
    positive.asMap().asScala.iterator
      .collect { case (k, v)
        if k != excluding &&
           v.tmdbId.contains(tid) &&
           TitleNormalizer.sanitize(k.cleanTitle) == target => k }
      .minByOption(canonicalRank)
  }

  /** Total order picking the canonical (surviving) key among same-tmdbId,
   *  same-normalised-title rows: prefer a row that carries a release year over
   *  a yearless one, then the lower year, then the cleanTitle. A pure function
   *  of the key, so the canonical never depends on write order. */
  private def canonicalRank(k: CacheKey): (Boolean, Int, String) =
    (k.year.isEmpty, k.year.getOrElse(Int.MaxValue), k.cleanTitle)

  private[services] def canonicalKeyFor(key: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    val target = TitleNormalizer.sanitize(key.cleanTitle)
    positive.asMap().asScala.keysIterator
      .filter(k => TitleNormalizer.sanitize(k.cleanTitle) == target)
      .minByOption(canonicalRank)
  }

  /** Collapse every set of rows that share a normalised cleanTitle into ONE row
   *  under the canonical (min `canonicalRank`) key, unioning their records. A
   *  concurrent scrape/enrichment can transiently split a film across two
   *  spellings — a stale-keyed TMDB write seeds a phantom ("Nowa fala" beside
   *  the canonical "Nowa Fala"), and once two same-sanitize rows exist
   *  `redirectToExistingVariant` stops merging (it only redirects on a UNIQUE
   *  match), so the split persists and which spelling a film ends under depends
   *  on order. This re-asserts the invariant deterministically: a pure function
   *  of the current row set, run after a pass settles. Rows with DIFFERENT
   *  resolved tmdbIds are genuinely distinct films that merely share a
   *  normalised form (a real cross-film collision) — those are left untouched. */
  def canonicalizeBySanitize(): Unit = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
      .groupBy { case (k, _) => TitleNormalizer.sanitize(k.cleanTitle) }
      .valuesIterator
      .foreach { group =>
        val distinctTmdb = group.flatMap(_._2.tmdbId).distinct
        if (distinctTmdb.sizeIs > 1) () // genuine cross-film collision — leave alone
        else {
          // Every reported variant: each cinema slot's derived key (cleaned
          // title + year) plus the rows' current keys.
          val slotKeys = group.flatMap { case (_, e) => e.data.values.flatMap(d => d.title.map(t => keyOf(t, d.releaseYear))) }
          val keys     = group.map(_._1)
          val allKeys  = slotKeys ++ keys
          // Choose the canonical key as a pure function of the variant SET, with
          // SPELLING decoupled from YEAR:
          //   - year:    TMDB's resolved year is authoritative when the group is
          //     resolved — it overrides cinema-reported years, which often carry
          //     the production year and disagree (the "Dzień objawienia" 2025 vs
          //     2026 split). Only when nothing in the group is TMDB-resolved do we
          //     fall back to the variant keys: a present year beats yearless, then
          //     the lowest.
          //   - spelling: the min cleanTitle across ALL variants regardless of
          //     year. Without decoupling, a yearless all-caps variant ("SAVAGE
          //     HOUSE" reported with no year) can't win the spelling at the
          //     resolved year — the only year-bearing spellings are the
          //     enrichment title and the (order-dependent) surviving key — so
          //     which casing stuck depended on scrape order.
          // `minOption` (not `.iterator…nextOption`): a pure function of the
          // group, independent of the cache's (non-deterministic) map iteration
          // order. Resolved rows of one film share a tmdbId hence a tmdbYear, so
          // min just picks that shared year; the determinism guard requires this.
          val canonicalYear  = group.flatMap { case (_, e) => e.tmdbYear }.minOption
            .orElse(allKeys.iterator.map(_.year).minBy(y => (y.isEmpty, y.getOrElse(Int.MaxValue))))
          // Prefer a normally-cased spelling over a SHOUTING one ("Savage House"
          // over "SAVAGE HOUSE") — a cinema's all-caps styling shouldn't become
          // the canonical display — then break ties by string order so it stays
          // a pure function of the variant set.
          def isAllCaps(t: String): Boolean = t.exists(_.isLetter) && t == t.toUpperCase(java.util.Locale.ROOT)
          val canonicalTitle = allKeys.map(_.cleanTitle).minBy(t => (isAllCaps(t), t))
          val canonical      = CacheKey(canonicalTitle, canonicalYear)
          // Re-assert the canonical spelling whenever ANY variant differs from
          // it. `CacheKey` equality is by NORMALISED title (sanitize) + year, so
          // a case/separator variant compares EQUAL to the canonical even though
          // its stored string differs — and `put` over an equal key keeps the
          // original. And the cache (Caffeine, first-inserted key) can disagree
          // with the repo (last-written title). So `invalidate` (remove) every
          // key, then `put` under the canonical string, rewriting BOTH stores.
          val needsFix = keys.sizeIs > 1 ||
            allKeys.exists(k => k.cleanTitle != canonical.cleanTitle || k.year != canonical.year)
          if (needsFix) {
            withTitleLock(canonical.cleanTitle) {
              val merged = group.sortBy { case (k, _) => canonicalRank(k) }.map(_._2).reduce(MovieRecordMerge.union)
              keys.foreach(invalidate)
              put(canonical, merged)
            }
          }
        }
      }
  }

  /** Collapse two same-tmdbId rows into one. The surviving key is chosen by
   *  `canonicalRank` (NOT arrival order), and the record is the union of every
   *  per-source slot — so no scraped data is lost whichever key wins, and the
   *  displayed title/year/ratings are derived from that union at read time.
   *  The stored result is therefore identical no matter which row was written
   *  first; enrichment-thread arrival order (which varies across machines, and
   *  used to flip the canonical here, drifting the whole-corpus snapshot
   *  between arm64 dev boxes and amd64 CI) no longer matters. */
  private def foldDeterministically(newKey: CacheKey, newRec: MovieRecord, siblingKey: CacheKey): Unit = {
    val siblingRec = Option(positive.getIfPresent(siblingKey)).getOrElse(newRec)
    val newWins    = Ordering[(Boolean, Int, String)].lt(canonicalRank(newKey), canonicalRank(siblingKey))
    val canonical  = if (newWins) newKey else siblingKey
    val victim     = if (newWins) siblingKey else newKey
    // union(canonical, victim): per-source slots are unioned (no loss); the
    // shared top-level enrichment fields are identical between same-tmdbId
    // siblings, so the merged record doesn't depend on the union direction.
    val merged = if (newWins) MovieRecordMerge.union(newRec, siblingRec)
                 else         MovieRecordMerge.union(siblingRec, newRec)
    persist(canonical, merged)
    if (victim != canonical) {
      positive.invalidate(victim)
      repo.delete(victim.cleanTitle, victim.year)
      logger.info(s"Folded duplicate '${victim.cleanTitle}' (${victim.year.getOrElse("—")}) " +
                  s"into '${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}) — same tmdbId=${newRec.tmdbId.get}.")
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
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean =
    // Serialise under the per-title lock that `recordCinemaScrape` and `rekey`
    // also hold. Caffeine's `computeIfPresent` is atomic for a single KEY, but
    // a rating update racing a concurrent `rekey` (which invalidates the old
    // key and re-puts under a new one) could land on a key being torn out from
    // under it — the write silently lost. Sharing the title lock (reentrant;
    // always acquired before Caffeine's compute lock, so the order stays
    // title→Caffeine and can't deadlock) makes every read-modify-write on a
    // row — scrape, rekey, rating — mutually exclusive. The slow rating HTTP
    // fetch already happened in the caller; only the cache write is held here.
    withTitleLock(key.cleanTitle) {
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
    if (updated == null) false
    else if (updated == before.get()) {
      // No real change — the common case: an unchanged cinema re-scrape tick
      // re-asserts the same slot. Skip the write entirely. Otherwise it issues
      // an `updateOne` that bumps only `updatedAt` (see `patchToUpdate`), and
      // each such no-op write is an oplog entry plus a change-stream
      // `updateLookup` full-document read per row, per pass — the dominant load
      // on the shared-CPU Mongo. The row is already in the desired state, so
      // report success without touching the repo (or firing the change stream).
      true
    } else {
      repo.updateIfPresent(key.cleanTitle, key.year, before.get(), updated)
      touch()
      true
    }
  }

  private[services] def markMissing(key: CacheKey): Unit =
    negative.put(key, java.lang.Boolean.TRUE)

  /** Drop all negative entries — used by the daily TMDB retry to give every
   *  previously-failed key one fresh shot. New misses re-populate the cache
   *  organically as they happen. */
  private[services] def clearNegatives(): Unit = negative.invalidateAll()

  /** Drop a single key's "missing" verdict. Called when a fresh cinema slot
   *  changes a row's resolution inputs (a new title/director the TMDB stage
   *  hadn't seen), so a `markMissing` recorded against the older, partial row
   *  can't block the re-resolve the new data warrants. Mirrors what the daily
   *  `clearNegatives` retry does, but scoped to the one row that just grew —
   *  the difference between a film resolving this pass vs. staying blank until
   *  the next daily sweep, when its first scraped cinema happened to fire the
   *  TMDB stage before later cinemas (with the verifying director) arrived. */
  private[services] def clearNegative(key: CacheKey): Unit = negative.invalidate(key)

  /** Drop a row from positive cache + Mongo — used by the TMDB stage to clear
   *  a stale row before re-keying it under a corrected (title, year). */
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
    require(TitleNormalizer.sanitize(oldKey.cleanTitle) == TitleNormalizer.sanitize(newKey.cleanTitle),
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
    // Record in a deterministic (title, year) order regardless of the order the
    // scraper handed movies over — several clients build their result from a
    // `mutable.Map.toSeq` or a parallel detail-fetch whose iteration order
    // varies run to run. That order leaks into which of two colliding title
    // variants is recorded first and how cinema slots/showtimes merge, which
    // made FilmScheduleEndToEndSpec's per-cinema counts + dub-slot assertions
    // flaky. Sorting here makes the scrape→merge reproducible at no cost to
    // production correctness.
    // Per-cinema title cleanup, rule-driven and keyed by the cinema. A migrated
    // client already applies these rules to `title` (carrying the pre-strip
    // string in `rawTitle`), so this re-application is idempotent insurance; a
    // client that emits a raw title with no inline cleanup gets cleaned here.
    // Either way `displayTitle` is a pure function of the scraped title + the
    // active rules. A key with no rules is an identity no-op.
    val ruleKey = TitleRuleKey.of(cinema)
    val cleaned: CinemaMovie => String = cm => TitleNormalizer.cinemaClean(ruleKey, cm.movie.title)

    // A single cinema can report one film as several rows — one per screening
    // page (Kino Nowe Horyzonty's per-event `op.s?id=…` URLs), which all
    // normalise to the same `(cleanTitle, year)` slot. Recording them one by
    // one let the LAST win, keeping only its filmUrl + showtimes and silently
    // dropping every other screening — and which row won depended on the order
    // the scraper emitted them. Fold each cinema's same-key rows into one:
    // union every screening's showtimes (deduped, ordered) so none is lost, and
    // keep a deterministic representative for the scalar film fields.
    val deduped: Seq[CinemaMovie] =
      movies.groupBy(cm => (cleaned(cm), cm.movie.releaseYear)).toSeq
        .sortBy { case ((t, y), _) => (t, y.getOrElse(Int.MinValue)) }
        .map { case (_, group) =>
          if (group.lengthCompare(1) == 0) group.head
          else {
            val rep = group.minBy(cm => (cm.filmUrl.getOrElse(""), cm.movie.title))
            // Dedup by *physical* screening identity (dateTime/room/format), not
            // by the whole Showtime: a cinema that lists one film under several
            // event pages (Kino Nowe Horyzonty's `op.s?id=…`) reports the same
            // session with different per-event bookingUrls, and a plain
            // `.distinct` kept each as a phantom duplicate whose order flipped
            // with the scraper's emit order. See MovieRecordMerge.dedupShowtimes.
            rep.copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
          }
        }

    val resolved: Seq[((CinemaMovie, CacheKey, Boolean), SourceData)] =
      deduped.sortBy(cm => (cleaned(cm), cm.movie.releaseYear.getOrElse(Int.MinValue))).map { cm =>
      val displayTitle = cleaned(cm)
      val primary      = keyOf(displayTitle, cm.movie.releaseYear)
      // Lock on the row's NORMALISED cleanTitle — the SAME key the TMDB stage
      // and `rekey` acquire — not the raw display title. `searchTitle` (the
      // cleanTitle derivation) can collapse a subtitle / programme-prefix /
      // anniversary variant that `displayTitle` keeps, so locking on
      // `displayTitle` put scrape-merge and the enrichment-stage rekey on
      // DIFFERENT locks for the SAME row — they then raced into lost cinema
      // slots under a concurrent (production-style) scrape. Locking on
      // `primary.cleanTitle` serialises every read-modify-write on the row
      // (scrape, rekey, TMDB put) against each other. Different films
      // (different normalised cleanTitle) still don't contend.
      withTitleLock(primary.cleanTitle) {
        // Land the slot on the *canonical* key for this film, chosen by
        // `canonicalRank` (NOT arrival order): when this cinema's primary key
        // out-ranks the existing variant's (a year where the row was created
        // yearless, a lower year, or a casing that sorts first), promote the
        // whole row onto it before writing. Keeps the stored `(cleanTitle,
        // year)` a pure function of the reported variants — so every stage
        // addresses the film by the same deterministic key and no slot or
        // rating write lands on a stale key. Display title is unaffected.
        val key = redirectToExistingVariant(primary) match {
          case Some(existingKey) =>
            val canonical = Seq(primary, existingKey).minBy(canonicalRank)
            if (canonical != existingKey) rekey(existingKey, canonical, identity)
            canonical
          case None => primary
        }
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
          title          = Some(displayTitle),
          // Verbatim upstream title, kept so the merge key is re-derivable when
          // the per-cinema rules change (backfill / un-merge). A rule-driven
          // client carries the pre-strip string in `movie.rawTitle`; clients that
          // did no cleanup leave it None and `title` is already raw.
          rawTitle       = cm.movie.rawTitle.orElse(Some(cm.movie.title)),
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
          s.title.contains(displayTitle) && s.releaseYear == effectiveYear
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
        // A brand-new cinema observation for this row changes what the TMDB
        // stage has to work with (a new title/director the resolver hadn't
        // seen). Drop any stale "missing" verdict so the `MovieRecordCreated`
        // we're about to publish re-resolves against the grown row instead of
        // being short-circuited by an earlier, partial-row failure — the race
        // that left films blank under a concurrent (production-style) scrape.
        if (isNew) clearNegative(key)
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
      .map(_._1)
      .toList
      .foreach { k =>
        // Remove the stale slot atomically under the per-title lock — NOT via a
        // `put` of the snapshot `e` captured during iteration. Under a concurrent
        // (production-style) parallel scrape, another cinema may have just added
        // ITS slot to this same row between our read of `e` and the write; a
        // stale `put` would clobber that sibling slot (the Cinema City venues
        // dropping each other's slot run-to-run). `putIfPresent` reads the
        // CURRENT row, drops only our cinema's slot, and is a no-op if a rekey
        // moved the row out from under us.
        putIfPresent(k, cur => cur.copy(data = cur.data - cinema))
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
   *  Match uses `TitleNormalizer.sanitize` so an Arabic/Roman /
   *  punctuation / case variant ("Mortal Kombat 2") redirects onto a row
   *  that only knows the canonical form ("Mortal Kombat II") — without
   *  this, the FIRST cinema's raw spelling would pin the row's
   *  `cinemaTitles` and every other cinema's variant of the same film
   *  would create a separate row at its own key. */
  private def redirectToExistingVariant(primary: CacheKey): Option[CacheKey] = {
    if (positive.getIfPresent(primary) != null) return None
    import scala.jdk.CollectionConverters._
    val normalizedRaw = TitleNormalizer.sanitize(primary.cleanTitle)
    // Match by cleanTitle normalisation. Cross-script titles produce
    // different normalised forms (sanitize keeps Unicode letters), so a
    // Cyrillic row can't be matched by a Latin scrape and vice versa.
    val candidates = positive.asMap().asScala.iterator
      .filter { case (k, _) => TitleNormalizer.sanitize(k.cleanTitle) == normalizedRaw }
      .map(_._1)
      .toSet  // unique by CacheKey (which dedups by normalized form)
    if (candidates.size == 1) Some(candidates.head) else None
  }

  def hasResolvedSiblingByTitle(rawTitle: String): Boolean = {
    import scala.jdk.CollectionConverters._
    val normalizedRaw = TitleNormalizer.sanitize(rawTitle)
    positive.asMap().asScala.iterator.exists { case (k, e) =>
      e.tmdbId.isDefined && TitleNormalizer.sanitize(k.cleanTitle) == normalizedRaw
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
    // Group by key BEFORE putting: a merge-key rule added after these docs were
    // written (a new GlobalStructural strip) makes two stored titles collide on
    // `CacheKey`, and a bare `put`-per-row is last-write-wins — it would silently
    // drop one doc's showtimes until the next scrape. Union the colliding rows
    // instead, so the cache is lossless the moment the rule lands (the orphaned
    // Mongo `_id` is reconciled by a later scrape / the reaper).
    val byKey: Map[CacheKey, MovieRecord] =
      rows.groupBy(r => CacheKey(r.title, r.year))
        .map { case (k, rs) => k -> MovieRecordMerge.unionAll(rs.map(_.record)) }
    byKey.foreach { case (k, rec) => positive.put(k, rec) }
    positive.asMap().keySet().asScala.toSeq
      .filterNot(byKey.keySet.contains)
      .foreach(positive.invalidate)
    // The raw `positive.put` above bypasses the `put` identity-fold, so same-film
    // rows that different cinemas reported under different years (`Kumotry|2025`
    // + `Kumotry|2026`, both resolved to one tmdbId) land as separate entries.
    // Collapse them right here on every load — at boot and on the 30-s sync tick.
    // The periodic `settle` is supposed to, but the worker's restart loop keeps
    // resetting its cadence, so duplicates persist for hours, each independently
    // re-enriched (double rating runs) and projected as a second card. Folding on
    // hydrate (which demonstrably runs) makes convergence independent of settle.
    // No-op on an already-canonical corpus — it only writes when a group needs
    // collapsing.
    if (rows.nonEmpty) canonicalizeBySanitize()
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
