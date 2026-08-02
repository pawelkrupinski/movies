package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{CinemaMovieAdded, EventBus, InProcessEventBus, StagingNewcomerDiverted}
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
private[services] case class CacheKey private (cleanTitle: String, year: Option[Int], normalized: String) {
  override def hashCode(): Int = (normalized, year).hashCode()
  override def equals(other: Any): Boolean = other match {
    case k: CacheKey => k.normalized == normalized && k.year == year
    case _           => false
  }
}

private[services] object CacheKey {
  /** Build a key under ONE country's rules. `normalized` is computed here and
   *  carried, rather than derived in the constructor body as it used to be:
   *  identity depends on the rule set, so a key that normalises itself silently
   *  adopts whatever set happened to be global on the constructing thread. That
   *  is the mechanism that made a shared `TitleNormalizer` impossible to scope —
   *  keys are built on Mongo change-stream driver threads and inside
   *  `BoundedParallel`'s executor, neither of which any caller-side scope
   *  reaches. Taking the normalizer as a context parameter puts the choice back
   *  where the country is known, and leaves existing call sites unchanged
   *  wherever a `given` is already in scope. */
  def apply(cleanTitle: String, year: Option[Int])(using normalizer: TitleNormalizer): CacheKey =
    new CacheKey(cleanTitle, year, normalizer.sanitize(cleanTitle))
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
  /** The country whose title rules key this cache's rows. Exposed on the READ
   *  surface because the enrichers and the read-model projection hold a cache and
   *  need to fold titles the same way it did — a resolution hint key or a
   *  projected `_id` built under different rules addresses a row that isn't
   *  there. Defaulted like `MovieRepository.normalizer` so in-memory and inline
   *  test caches need not supply one. */
  def normalizer: TitleNormalizer = TitleNormalizer.deployment

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
 * In-memory enrichment store with write-through to the underlying `MovieRepository`.
 * Adds the mutating surface on top of `MovieCacheReader`.
 *
 * Per CLAUDE.md DIP guidance: consumers depend on this trait (or the narrower
 * `MovieCacheReader`), not the concrete implementation. `CaffeineMovieCache` is
 * the production implementation; tests can swap in any other implementation if
 * they ever need to.
 */
trait MovieCache extends MovieCacheReader {
  /** Apply one cinema's fresh scrape to the cache. Returns one
   *  `(CinemaMovie, CacheKey, isNew)` triple per input movie.
   *
   *  `listingIsComplete = false` means the caller KNOWS this listing is short — a chunked
   *  scrape reduced from only some of its date-chunks. The prune is then skipped outright,
   *  because a film missing from a listing nobody finished is not evidence that it stopped
   *  screening. Every scrape DECORATOR must forward this
   *  ([[services.cinemas.common.DelegatingCinemaScraper]]); one that answers the default
   *  instead silently turns the guard off. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie],
                         listingIsComplete: Boolean = true): Seq[(CinemaMovie, CacheKey, Boolean)]

  /** Reload the positive cache from the repository: drop every in-memory positive
   *  entry, then `repository.findAll()` and put each row. Returns the number of
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
  /** Settle-path persistence for the title-embedded year: re-key every yearless
   *  row whose cinema-slot titles carry an unambiguous delimited `EmbeddedYear`
   *  ("Konwicki: Lawa (1989)", "Following (1998)") onto that year, and stamp the
   *  year onto its yearless slots — so a scrape that reported no year still keys,
   *  resolves and displays as if it had. Runs OFF the async scrape/resolve path
   *  (over the already-settled corpus), so — unlike a scrape-time re-key — it can't
   *  race `canonicalRank`: TMDB-resolved rows are already keyed on their TMDB year,
   *  so this only ever moves rows a year source hasn't otherwise claimed. Reuses the
   *  vetted `rekey` (merging into any existing occupant so a sister row isn't
   *  clobbered) and always ends with a `canonicalizeBySanitize`; returns the count
   *  re-keyed. Driven by `MovieService.settle` (the periodic `SettleReaper`) and the
   *  one-shot `scripts.EmbeddedYearBackfill`. */
  def backfillEmbeddedYears(): Int
  def canonicalizeBySanitize(): Unit
  /** Conclusion-time scoped settle: the just-resolved record `resolved` is the
   *  new state of the row at `oldKey`. Write it (re-keyed onto its TMDB year if
   *  `oldKey` was yearless) AND fold any YEARLESS + IDLESS same-title stray onto
   *  it in ONE write — the unambiguous `clusterByFilm` rule-(4) rows a concurrent
   *  scrape can strand beside the resolved one. The single write means the
   *  resolved row's first `readyToProject` upsert already carries every cinema,
   *  so the read model is never shown a partial, single-cinema split. The broader
   *  ±1-year / distinct-tmdbId clustering needs the FULL corpus and so stays in
   *  `canonicalizeBySanitize` (order-independent); this stays on the resolved
   *  row's own key for the same reason. Returns that key. */
  private[services] def settleResolved(oldKey: CacheKey, resolved: MovieRecord): CacheKey
  /** Like [[get]], but falls back to a direct `movies` read when the cache doesn't
   *  hold `key`. The TMDB resolve's carry-forward reads this rather than the
   *  Caffeine-only `get`, so a cold / evicted / re-keyed entry can't make the
   *  rebuild read EMPTY and null a persisted rating (or cinema slot). */
  private[services] def stored(key: CacheKey): Option[MovieRecord] = storedChecked(key)._1

  /** Like [[stored]], but says whether the underlying read SUCCEEDED — `(row, readOk)`.
   *  A caller that would treat `None` as "this film is new" MUST use this instead: a
   *  failed read otherwise makes it rebuild a live film from scratch, carrying only what
   *  the current scrape saw. See [[MovieRepository.findByIdChecked]]. */
  private[services] def storedChecked(key: CacheKey): (Option[MovieRecord], Boolean)

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

  /** Re-kick the enrichments whose INPUT fields an enrichment write (not a merge)
   *  changed — e.g. Filmweb writing a director / originalTitle onto its slot
   *  re-attempts the TMDB / IMDb resolution its new hint might now crack, and the
   *  title-ratings when the row is resolved. Same pure decision + retrigger port as
   *  the merge path ([[MergeRetrigger.changedEnrichments]]); `before`/`after` are
   *  the row at `key` around the write, so they share the key. No-op when nothing an
   *  enrichment reads changed. */
  private[services] def retriggerAfterEnrichment(key: CacheKey, before: MovieRecord, after: MovieRecord): Unit
}

/**
 * Caffeine-backed `MovieCache` with write-through to `MovieRepository`.
 *
 * Two caches:
 *   - **Positive**: successful enrichments, never expire in-process (they
 *     change slowly; restarts re-warm via `rehydrate`).
 *   - **Negative**: known misses (events, festivals, retrospectives that
 *     don't match a real film), 24h TTL — failed TMDB lookups get retried
 *     about once a day (the phase-spread `UnresolvedTmdbReaper`, which clears
 *     each due row's marker via `clearNegative`). The operator bulk retry can
 *     also clear the whole negative cache explicitly via `clearNegatives` so
 *     every previously-failed key gets a fresh shot at once.
 *
 * Pure reads (`get`, `isNegative`, `snapshot`, `entries`) have no side
 * effects — callers that want to *trigger* a lookup on miss go through
 * `MovieService` (which owns the worker pool + dedup).
 */
class CaffeineMovieCache(
  repository: MovieRepository,
  bus:  EventBus = new InProcessEventBus(),
  // Boot-hydrate retry — OFF by default (0 attempts) so tests and a genuine
  // cold start pay nothing. Prod turns it on via the Fly env
  // `KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS` so a not-ready Mongo at boot can't leave
  // the cache empty (see `bootHydrate`).
  bootHydrateMaxAttempts: Int  = Env.get("KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS").flatMap(_.toIntOption).getOrElse(0),
  bootHydrateRetryMillis: Long = Env.positiveLong("KINOWO_BOOT_HYDRATE_RETRY_MS", 1000L),
  // A genuinely-NEW film (one whose `sanitize(title)` group isn't already in
  // `movies`) is diverted to this staging sink — one row per `cinema|title|year`
  // — to incubate until TMDB concludes, instead of landing in the merged
  // `movies` cache; known films keep the direct path. The worker wires this
  // `Some(stagingRepository)`. `None` (the default, used by unit tests that exercise the
  // cache directly) disables diversion — every scrape lands in `movies`.
  staging: Option[services.staging.StagingRepository] = None,
  // Called after a merge whose inputs changed an enrichment's resolution, to
  // re-kick that enrichment as a worker task (per case). Default no-op for unit
  // tests + non-worker builds; the worker wires `QueueEnrichmentRetrigger`.
  retrigger: EnrichmentRetrigger = EnrichmentRetrigger.noop,
  // Counts each movie-row fold by reason (canonicalize / resolved-settle /
  // tmdb-identity) so the worker can chart the merge rate that drives re-key
  // re-enrichment load. No-op for web + unit tests; the worker wires
  // `WorkerTaskMetrics`.
  mergeMetrics: MergeMetrics = MergeMetrics.noop,
  // Measures what the periodic backstop rehydrate catches that the incremental change
  // stream missed — the redundancy signal for retiring the rehydrate. No-op for web/tests.
  cacheMetrics: CacheSyncMetrics = CacheSyncMetrics.noop,
  // The deployment's language, used to canonicalise cinema-reported production
  // countries into the deployment's own (Polish "USA"/"Wielka Brytania" on
  // `kinowo`, the source's already-localised name elsewhere — see
  // `CountryNames.canonical`). The worker wires `country.language`; defaults to
  // Polish so every existing single-country construction is unchanged.
  enrichmentLanguage: java.util.Locale = CountryNames.DefaultLanguage,
  // The country's title rules. Sibling of `enrichmentLanguage` above and wired
  // from the same `country` by the worker; both default to Poland so existing
  // single-country constructions are unchanged. Every `CacheKey` this cache
  // builds — including the ones built on the change-stream driver thread and in
  // the rehydrate scheduler — keys through THIS instance, which is what makes
  // the cache's identity country-correct rather than process-global.
  override val normalizer: TitleNormalizer = TitleNormalizer.deployment
) extends MovieCache with Stoppable with Logging {

  // Supplies `CacheKey.apply` throughout this class, so a key can never be built
  // here under another country's rules.
  private given TitleNormalizer = normalizer

  // Films skipped this process's lifetime because their stored row could not be read.
  // Exposed for tests + diagnostics: a non-zero value means scrapes are landing against
  // an unreadable corpus, which is the state that used to silently prune boards.
  private[services] val skippedUnreadable = new java.util.concurrent.atomic.AtomicLong(0)

  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

  // Partial-scrape guard thresholds (see `recordCinemaScrape`'s prune): a tick
  // that returns fewer than `PruneFloorRatio` of the cinema's currently-held
  // slots — and only when it already holds at least `MinSlotsForShrinkGuard`, so a
  // small venue's ±1 swings never trip it — is treated as a degraded fetch and
  // skips the prune. Half is a wide margin: cinemas don't shed most of their
  // catalogue between two ticks, but a partial Cloudflare/session response can
  // return a handful of a full board's rows.
  private val MinSlotsForShrinkGuard = 8
  private val PruneFloorRatio        = 0.5

  // The DEPTH guard's engage-floor (its ratio is `PruneFloorRatio` above — one
  // half-floor, measured valid on both axes, rather than two constants that mean
  // the same thing). A cinema holding fewer upcoming showtimes than this is small
  // enough that halving its board between ticks is a real schedule change, not a
  // degraded fetch — the depth analogue of `MinSlotsForShrinkGuard`, and set to
  // roughly that many slots' worth of screenings.
  private val MinShowtimesForDepthGuard = 24

  // The depth guard DISCARDS a tick, so it must not be able to freeze a venue
  // forever. A board that genuinely halves — a screen closes, a summer schedule
  // starts — would otherwise be rejected on every subsequent tick too, since each
  // one is compared against the same stale stored total. Leaving stale FUTURE
  // showtimes up is worse than showing too few: a user can act on a screening that
  // is not happening. After this many CONSECUTIVE rejections the reduction is
  // treated as real and allowed through. This does not reopen the incident: those
  // degraded ticks were interspersed with healthy ones, each of which resets the
  // count and restores the full board.
  private val MaxConsecutiveDepthRejections = 3

  /** Consecutive depth-guard rejections per cinema, reset by any tick that passes. */
  private val depthRejections = scala.collection.concurrent.TrieMap.empty[String, Int]

  // Fires the cold-mirror sync at most once, on the FIRST scrape (see
  // `recordCinemaScrape`). One-shot so the sync can't re-trigger at an
  // arrival-order-dependent later scrape — the mirror only goes cold at boot, and a
  // later mirror-empty-while-repo-non-empty state is reachable only in a harness
  // with no change stream (its mirror lags the repo), where re-checking would make
  // the staging path order-dependent (StagingOrderDeterminismSpec).
  private val coldMirrorSyncArmed = new java.util.concurrent.atomic.AtomicBoolean(true)

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
    titleLocks.computeIfAbsent(normalizer.sanitize(rawTitle), _ => new Object())

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
  //
  // RETRY an empty result (prod only): the worker boots alongside its Mongo, so
  // an empty findAll at boot is almost always "Mongo not ready yet" (findAll
  // swallows errors to Seq.empty). Without retry the cache starts empty and the
  // change stream only ever delivers rows written AFTER boot — leaving every
  // quiescent row (one not re-scraped since) Mongo-only and invisible to the
  // in-memory fold / settle, so its duplicate sits stranded forever. Bounded, so
  // a genuinely empty corpus still proceeds after the attempts. Default 0
  // attempts = one plain hydrate (tests, cold start); prod sets the env.
  bootHydrate()

  private def bootHydrate(): Unit = {
    var attempt = 0
    while (rehydrate() == 0 && attempt < bootHydrateMaxAttempts) {
      attempt += 1
      Try(Thread.sleep(bootHydrateRetryMillis))
    }
  }

  // Key by the title's OWN form — the same input the display vote
  // (`MovieRecord.displayTitle`) sanitizes — so a record's identity always
  // matches what it shows, and two listings are merged only when they resolve
  // to the same title key on their own. `CacheKey.normalized` still applies
  // `sanitize` (= `normalize` Arabic→Roman + `canonical` & → i / "Gwiezdne
  // Wojny:" + deburr), so the GLOBAL canonical folds stay in the key; only the
  // GlobalStructural decoration strip (anniversary / "- wersja X" / slash
  // suffix / Cykl prefix / restored) is left out — that tier now only feeds
  // `apiQuery` for external lookups, not identity.
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(title, year)

  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

  private[services] def isNegative(key: CacheKey): Boolean =
    negative.getIfPresent(key) != null

  /** Persist a row at `key`. **Identity gate**: when `e` carries a `tmdbId`
   *  AND any other cache key already holds that same tmdbId, the write is
   *  folded onto that canonical row instead of creating a duplicate — the
   *  victim's cinema-side data is unioned in via `MovieRecordMerge.union`, the
   *  source key is dropped from both cache and repository.
   *
   *  Identity check: **same `tmdbId`**, regardless of how the two rows spell
   *  their cleanTitle. A film TMDB resolves to one id is ONE record — the
   *  year-divergence case ("Viridiana" 1961 vs 1962), the cross-language case
   *  (Polish "Diabeł ubiera się u Prady 2" vs Cyrillic "ДИЯВОЛ НОСИТЬ ПРАДА 2"),
   *  and the decorated/dubbed edition ("…ukraiński dubbing") all fold onto it.
   *  Each shown title is split back into its own CARD by the read-model
   *  projection (`ReadModelProjection.projectAll`), so keeping one storage
   *  record per film no longer hides any variant's display title — the split
   *  moved from storage to display.
   *
   *  This is the only persist path in the codebase — `MovieRepository.upsert` is
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

  // Strip only when the read-split is active (showtimes live in `screenings`); without it
  // the cache must keep showtimes — there's nowhere else to hold them.
  private def forCache(r: MovieRecord): MovieRecord =
    if (repository.hasScreenings) ShowtimesDigest.stripForCache(r) else r

  private def persist(key: CacheKey, e: MovieRecord): Unit = {
    val clean = withoutZeroRatings(e)
    positive.put(key, forCache(clean))
    // `clean` may carry stripped slots (folds/canonicalize read from the stripped cache);
    // `upsert` re-stitches those from the film's screenings so a full write never deletes them.
    repository.upsert(key.cleanTitle, key.year, clean)
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

  /** Find an existing cache key carrying the same tmdbId as `excluding` — the
   *  same film, whatever its cleanTitle spelling (the display split into a card
   *  per shown title now lives in the read-model projection, see the `put`
   *  docstring above).
   *
   *  `minByOption`, not `find`: a same-tmdbId row can have more than one sibling
   *  (year + yearless + a dub variant), and `asMap` iteration order is not
   *  stable across JVM builds / platforms. Pick the canonical-rank minimum so
   *  the chosen sibling — and therefore the fold result — is a pure function of
   *  the cache contents, not iteration order. */
  private def siblingKeyByTmdb(tid: Int, excluding: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .collect { case (k, v) if k != excluding && v.tmdbId.contains(tid) => k }
      .minByOption(canonicalRank)
  }

  /** Total order picking the canonical (surviving) key among same-tmdbId,
   *  same-normalised-title rows — see `FilmCanonicalizer.canonicalRank` for the
   *  rule. Delegates so there is ONE definition shared with the pure
   *  canonicaliser. */
  private def canonicalRank(k: CacheKey): (Boolean, Int, String) =
    FilmCanonicalizer.canonicalRank(k)

  private[services] def canonicalKeyFor(key: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    val target = normalizer.sanitize(key.cleanTitle)
    val sameTitle = positive.asMap().asScala.keysIterator
      .filter(k => normalizer.sanitize(k.cleanTitle) == target).toSeq
    // Prefer a row at this EXACT year. A same-title row at a DIFFERENT year is a
    // distinct film — a remake or re-release carrying the original's name
    // ("Zaproszenie" 2022 "The Invitation" vs 2026 "The Invite", "Diuna" 1984 vs
    // 2021) — not a stale-key alias of this one, so a resolve/read must never
    // redirect onto it (year-blind `minByOption` clobbered the lowest-year row).
    // The year-blind fallback still fires when NO exact-year row exists, which is
    // the only shape the genuine redirect needs: `recordCinemaScrape`'s rekeys
    // change spelling at the SAME year (case/separator) and a yearless key whose
    // row gained a resolved year both reach their row through it.
    sameTitle.filter(_.year == key.year).minByOption(canonicalRank)
      .orElse(sameTitle.minByOption(canonicalRank))
  }

  /** Collapse every set of rows that are the SAME FILM into ONE row under the
   *  canonical key, unioning their records. Film identity is `groupByFilm`: rows
   *  sharing a normalised cleanTitle, OR (both bare film titles) a tmdbId — so a
   *  film keyed under two languages ("Tangled" + "Zaplątani", same tmdbId) folds
   *  to one row, while a decorated edition that merely carries the base tmdbId
   *  stays separate. A concurrent scrape/enrichment can transiently split a film
   *  across two spellings — a stale-keyed TMDB write seeds a phantom ("Nowa fala"
   *  beside the canonical "Nowa Fala"), and once two same-sanitize rows exist
   *  `redirectToExistingVariant` stops merging (it only redirects on a UNIQUE
   *  match), so the split persists and which spelling a film ends under depends
   *  on order. This re-asserts the invariant deterministically: a pure function
   *  of the current row set, run after a pass settles.
   *
   *  A single normalised title can legitimately cover SEVERAL films — a remake
   *  carrying the original's name ("Diuna" 1984 vs 2021), or adjacent-year
   *  variants of one film where cinemas disagree on the year (production vs
   *  theatrical). So the group is sub-clustered into per-film clusters
   *  (`clusterByFilm`), each of which then collapses on its own. Two DISTINCT
   *  resolved tmdbIds are never merged; year-bearing unresolved rows attach to a
   *  resolved cluster within ±1 of its TMDB year, otherwise pack into greedy
   *  2-year windows; yearless+idless rows fold into the group's canonical
   *  cluster. */
  def canonicalizeBySanitize(): Unit = {
    import scala.jdk.CollectionConverters._
    canonicalizeGroups(positive.asMap().asScala.toSeq)
  }

  def backfillEmbeddedYears(): Int = {
    import scala.jdk.CollectionConverters._
    val moved = positive.asMap().asScala.toSeq.iterator.collect {
      // Only YEARLESS rows — a row that already carries a year keeps it (a scraped
      // year wins over a title annotation, exactly as `recordCinemaScrape` orders
      // them). Scan the raw slot spellings: the canonical key strips the annotation.
      case (key, rec) if key.year.isEmpty =>
        EmbeddedYear.ofAll(rec.data.values.flatMap(sd => sd.rawTitle ++ sd.title)).flatMap { year =>
          val newKey = keyOf(key.cleanTitle, Some(year))
          Option.when(newKey != key) {
            // Merge into any existing occupant of the target year-key so a sister
            // row already there isn't overwritten; `settle` then reconciles the
            // wider cluster deterministically (±1-year windows, distinct tmdbIds).
            val occupant = Option(positive.getIfPresent(newKey))
            rekey(key, newKey, cur => {
              val stamped = cur.copy(data = cur.data.view.mapValues(sd =>
                if (sd.releaseYear.isEmpty) sd.copy(releaseYear = Some(year)) else sd).toMap)
              occupant.fold(stamped)(o => MovieRecordMerge.union(stamped, o))
            })
          }
        }.isDefined
    }.count(identity)
    // Always settle: fold the re-keyed rows' ±year / distinct-tmdbId clusters, and
    // (when nothing moved) keep the plain settle semantics callers rely on.
    canonicalizeBySanitize()
    moved
  }

  private def canonicalizeGroups(pairs: Seq[(CacheKey, MovieRecord)]): Unit =
    FilmCanonicalizer.groupByFilm(pairs)
      .foreach(component => FilmCanonicalizer.clusterByFilm(component).foreach(collapseCluster))

  /** Collapse ONE cluster (rows that are the same film) to a single canonical
   *  row, unioning their records. The `(canonical, merged)` DECISION — which
   *  year, which spelling, which merged record — lives in the pure
   *  `FilmCanonicalizer.canonical`; this method owns only the cache MUTATION.
   *
   *  `CacheKey` equality is by NORMALISED title + year, so a case/separator
   *  variant compares EQUAL to the canonical even though its stored string
   *  differs (and the Caffeine-side first-inserted key can disagree with the
   *  repository's last-written title). So `invalidate` every key, then `put` under the
   *  canonical string, rewriting BOTH stores — but only when something differs. */
  private def collapseCluster(cluster: Seq[(CacheKey, MovieRecord)]): Unit = {
    val (canonical, merged) = FilmCanonicalizer.canonical(cluster)
    val keys = cluster.map(_._1)
    // The union base `canonical()` merged onto — the row that would have stood
    // without the merge (mirrors `MovieRecordMerge.unionAll`'s pick). Comparing
    // the merged result against it tells us which enrichment inputs the merge
    // changed (re-key, gained tmdbId/imdbId/searchTitle), to re-kick those.
    val sorted              = cluster.sortBy { case (k, _) => canonicalRank(k) }
    val (baseKey, baseRec)  = sorted.find { case (_, e) => e.tmdbId.isDefined }.getOrElse(sorted.head)
    // Only an actual stored ROW key drives the "anything to fix?" guard: a row is
    // the only thing `collapseCluster` can re-key onto the canonical. A cinema's
    // reported SLOT title is immutable display data — it can never be re-written —
    // so folding slot keys in here left `needsFix` permanently true for any film a
    // cinema SHOUTS: "DZIEŃ OBJAWIENIA" sanitize-equals the canonical "Dzień
    // objawienia" but never string-equals it, so the settled row was
    // delete+upsert-ed on EVERY hydrate/settle tick — pointless Mongo churn that
    // pins the worker at full CPU-credit AND re-kicks the row's enrichment, the
    // "merge→split" flap. (`f2dd5be1` killed this churn for cross-LANGUAGE slots
    // via the sanitize guard below but missed the same-language CASE-drift slot;
    // restricting the guard to row keys covers both.) A slot reporting a genuinely
    // new spelling still drives a re-key: `recordCinemaScrape` merges it onto the
    // row and `FilmCanonicalizer.canonical` moves the canonical, so the ROW key
    // then differs and trips the guard.
    val canonicalSanitized = normalizer.sanitize(canonical.cleanTitle)
    val needsFix = keys.sizeIs > 1 ||
      keys.exists(k => normalizer.sanitize(k.cleanTitle) == canonicalSanitized &&
        (k.cleanTitle != canonical.cleanTitle || k.year != canonical.year))
    if (needsFix) {
      withTitleLock(canonical.cleanTitle) {
        // Split the keys: rows that genuinely go away, versus the canonical row itself.
        //
        // `invalidate` deletes from BOTH stores, and `MovieRepository.delete` cascades to
        // `screenings`/`movie_slots`. Applying it to the CANONICAL key deleted the very
        // row `put` then rewrites — and since `CacheKey` equality is normalised, that is
        // the SAME `_id`. So the side rows went, `upsert`'s re-stitch read the id it had
        // just emptied, and the film lost its showtimes. Measured on prod 2026-07-27:
        // 735 of 941 rows delete+re-inserted under byte-identical ids every 30 minutes,
        // each losing its showtimes until the next scrape restored them — the sawtooth.
        //
        // The canonical row does not need deleting at all: `put` rewrites it in place
        // (`replaceOne` on the same id). Only its CAFFEINE key object needs replacing, so
        // the stored spelling follows the canonical — which is what this invalidate was
        // for. `positive.invalidate` does exactly that and touches no stored row.
        val victims = keys.filterNot(_ == canonical)
        // A victim IS going away, so carry its cinemas onto the winner first — same rule
        // as the fold and the re-key. Only a victim whose rows ACTUALLY reached the winner
        // may then be deleted: `moveFilm` reports false when a read or write it depended on
        // didn't happen, and deleting on that basis destroys the film's only copy. A victim
        // left behind is a duplicate row the next pass folds again (and
        // `scripts.ReapOrphanedFilmRows` clears), which is the recoverable direction.
        val canonicalId = StoredMovieRecord.idFor(canonical.cleanTitle, canonical.year)
        val (moved, stranded) = victims.partition(v =>
          repository.moveFilm(StoredMovieRecord.idFor(v.cleanTitle, v.year), canonicalId))
        if (stranded.nonEmpty)
          logger.warn(s"canonicalize '${canonical.cleanTitle}': keeping ${stranded.size} row(s) whose " +
            "cinemas could not be carried onto the winner — they fold again on the next pass.")
        moved.foreach(invalidate)
        keys.filter(_ == canonical).foreach(positive.invalidate)
        put(canonical, merged)
      }
      // Victims = every other row in the cluster folded away; a lone respelled
      // key (keys.size == 1) is a re-key, not a merge, so it counts 0.
      if (keys.sizeIs > 1) mergeMetrics.recordMerge(MergeReason.Canonicalize, keys.size - 1)
      retriggerChangedEnrichments(baseRec, baseKey, merged, canonical)
    }
  }

  /** Re-kick (as worker tasks) the enrichments whose input fields a merge
   *  changed — `before` is the pre-merge survivor, `after` the merged record now
   *  stored under `afterKey`. Pure decision in [[MergeRetrigger]]; the injected
   *  [[EnrichmentRetrigger]] does the freshness-invalidate + enqueue. */
  private def retriggerChangedEnrichments(
    before: MovieRecord, beforeKey: CacheKey, after: MovieRecord, afterKey: CacheKey
  ): Unit = {
    val kinds = MergeRetrigger.changedEnrichments(before, beforeKey, after, afterKey)
    if (kinds.nonEmpty) retrigger.retrigger(afterKey, after, kinds)
  }

  // An enrichment write (Filmweb slot, IMDb slot, …) at a stable key: before/after
  // share the key, so the merge decision reduces to the input-field deltas.
  private[services] def retriggerAfterEnrichment(key: CacheKey, before: MovieRecord, after: MovieRecord): Unit =
    retriggerChangedEnrichments(before, key, after, key)

  /** The film's currently-stored record for `key`: the live cache entry when
   *  resident, else a direct `movies` read. The TMDB resolve's carry-forward
   *  (`MovieService.runTmdbStageSync`) reads this rather than the Caffeine-only
   *  `get`, so a cold / evicted / re-keyed entry doesn't make the rebuild read
   *  EMPTY and null the scores the `*Ratings` refreshers own (and the cinema
   *  slots) — the "ratings keep disappearing" clobber. On a warm cache it's just
   *  `get`; the `movies` read only happens on a miss. */
  private[services] def storedChecked(key: CacheKey): (Option[MovieRecord], Boolean) =
    get(key) match {
      case cached @ Some(_) => (cached, true)
      case None =>
        val (row, readOk) = repository.findByIdChecked(StoredMovieRecord.idFor(key.cleanTitle, key.year))
        (row.map(_.record), readOk)
    }

  def settleResolved(oldKey: CacheKey, resolved: MovieRecord): CacheKey =
    withTitleLock(oldKey.cleanTitle) {
      import scala.jdk.CollectionConverters._
      // TMDB's resolved year re-keys a YEARLESS row onto it; a row that already
      // carries a year keeps its key — re-keying a yeared row across the async
      // resolve races `canonicalRank` (`canonicalizeBySanitize` owns that
      // migration — run by the staging fold and on every rehydrate).
      val wanted =
        if (oldKey.year.isEmpty && resolved.resolvedYear.isDefined)
          keyOf(oldKey.cleanTitle, resolved.resolvedYear)
        else oldKey
      // Fold any prior occupant of `wanted` into the resolved record up front so
      // re-keying onto an occupied year can't drop its cinema slots.
      // `storedChecked` (cache-or-Mongo), not a Caffeine-only read: a cold/evicted
      // prior occupant must still be folded in, else the union below drops it and
      // the replaced record nulls its ratings + slots.
      //
      // …and not `stored` either, because that collapses "the year is empty" into the
      // same `None` as "the read failed", and the two want opposite actions. `wanted`
      // is a key this caller never touched, so the repository read is the NORMAL path
      // here, not a cold-cache fallback — a Mongo blip therefore re-keys the row onto
      // an occupied year with an EMPTY merge base and writes over whatever lived
      // there, which is how a rated row loses its scores while its showtimes (carried
      // by `moveFilm`) look fine.
      //
      // A failed read defers the RE-KEY rather than throwing: the row keeps its
      // current key and is written there, exactly as it would be had TMDB not
      // resolved a year. Nothing is lost and nothing is overwritten; the periodic
      // `canonicalizeBySanitize` re-keys it once the read works. Throwing is wrong at
      // THIS site specifically — the no-match caller (`MovieService`, the
      // `Success(None)` branch) is outside the `Try` that turns a failure into a
      // retry, so an exception would escape into the task runner and park the task.
      val (priorTarget, targetReadable) =
        if (wanted != oldKey) storedChecked(wanted) else (None, true)
      if (!targetReadable)
        logger.warn(s"settle: could not read '${wanted.cleanTitle}' (${wanted.year.getOrElse("?")}) " +
          s"to fold into the resolved row; leaving '${oldKey.cleanTitle}' on its own key this pass.")
      val target = if (targetReadable) wanted else oldKey
      val base        = priorTarget.fold(resolved)(t => MovieRecordMerge.union(resolved, t))
      val norm        = normalizer.sanitize(oldKey.cleanTitle)
      // Fold ONLY the YEARLESS + IDLESS same-title strays onto the resolved row.
      // These are exactly `clusterByFilm`'s rule (4) rows: with no year and no
      // tmdbId they can belong to no OTHER film in the group, so attaching them
      // here is unambiguous and order-independent — a concurrent scrape that
      // stranded the Multikino "Dzień objawienia" in its own (title, None) row is
      // healed the moment Helios resolves, instead of waiting for the periodic
      // settle. ±1-year and distinct-tmdbId clustering is deliberately NOT done
      // here: that depends on the FULL corpus (which variants have arrived), so
      // doing it on a partial corpus at resolve time is order-dependent — it
      // stays in `canonicalizeBySanitize`, a pure function of the settled corpus
      // (the `ScrapeOrderDeterminismSpec` guard). We also land on the resolved
      // row's OWN key, not a recomputed canonical spelling, for the same reason.
      val strays = positive.asMap().asScala.toSeq.filter { case (k, e) =>
        k != oldKey && k != target && k.year.isEmpty && e.tmdbId.isEmpty &&
        normalizer.sanitize(k.cleanTitle) == norm
      }
      // ONE write carrying every cinema (the resolved row's first
      // `readyToProject` upsert is already complete), so the read model is
      // projected to `web_movies` only after this settle — never the single-
      // cinema (Helios-only) split that made the card flicker.
      val merged = strays.foldLeft(base) { case (acc, (_, e)) => MovieRecordMerge.union(acc, e) }
      (strays.map(_._1) :+ oldKey).distinct.filterNot(_ == target).foreach(invalidate)
      put(target, merged)
      // The resolved row's own re-key (oldKey → target) isn't a merge — only the
      // strays and any prior occupant of the resolved year are folded-away rows.
      val folded = strays.size + priorTarget.size
      if (folded > 0) mergeMetrics.recordMerge(MergeReason.ResolvedSettle, folded)
      target
    }

  /** The canonical key of an already TMDB-concluded row this scrape matches —
   *  same normalised title, and a matching year when the scrape carries one. A
   *  later scrape of a known film lands its slot straight on the
   *  resolved/concluded row, so the enrichment + TMDB trigger is skipped
   *  (`CinemaScrapeRunner.classify` short-circuits on `tmdbConcluded`) and no
   *  held-back variant is spawned beside it. None when no concluded match exists.
   *
   *  Year matching mirrors `clusterByFilm`'s ±1 adjacency, so the duplicate
   *  `canonicalizeBySanitize` would later fold is never spawned: a yearless scrape lands
   *  on any concluded same-title row; a year-bearing scrape prefers the concluded
   *  row at the SAME year, else the nearest within ±1 (a cinema reporting the
   *  production year 2025 lands on the row TMDB resolved to the release year
   *  2026 — the "two copies of Kumotry" bug). Ties break on `canonicalRank`. */
  private def concludedKeyFor(primary: CacheKey): Option[CacheKey] = {
    import scala.jdk.CollectionConverters._
    val norm = normalizer.sanitize(primary.cleanTitle)
    // A scrape lands on a concluded row when its title matches that row's key, OR
    // one of the row's TMDB aliases (its Polish / original title). The alias arm
    // lands a cinema's original-language listing of a film ("Tangled") straight on
    // the existing resolved row ("Zaplątani") instead of spawning a translation
    // duplicate for the next settle to merge.
    //
    // The alias arm is gated on `isBareFilmTitle` — the SAME predicate
    // `FilmCanonicalizer.groupByFilm` uses to decide which same-tmdbId rows may
    // fold. A decorated edition ("Plenerowe Pałacowe: Ścieżki życia") enriched off
    // the base film via the apiQuery programme-prefix strip carries the BASE title
    // ("Ścieżki życia") as a TMDB alias; without the gate a bare "Ścieżki życia"
    // scrape would match that decorated row through the alias and (canonicalRank's
    // 'P' < 'Ś' tiebreak) land on it — splitting the bare film and re-diverting it
    // every tick. Gating keeps the scrape path's redirect consistent with the
    // settle's fold: only a row that is itself a bare presentation of the film is
    // a valid alias target. The decorated→own-row direction is already self-gating
    // (its sanitize matches no alias).
    val concluded = positive.asMap().asScala.iterator
      .collect { case (k, e) if e.tmdbConcluded &&
        (normalizer.sanitize(k.cleanTitle) == norm ||
         (FilmCanonicalizer.isBareFilmTitle((k, e)) &&
          e.tmdbTitleAliases.exists(a => normalizer.sanitize(a) == norm))) => k }
      .toSeq
    // Prefer a row whose OWN key IS this title over one that matches only via a
    // TMDB alias. The alias arm exists to land an original-language listing that
    // has no row yet ("Tangled") onto the resolved row ("Zaplątani"); but once a
    // row keyed by the scraped title exists, it must win — otherwise a same-tmdbId
    // sibling under a different-language key ("Denʹ istyny - UA", its TMDB alias
    // also "Dzień objawienia") steals the scrape whenever its cleanTitle sorts
    // first in the canonicalRank tie-break ("De" < "Dz"), splitting the Polish
    // film across two ever-growing rows the sanitize-keyed canonicalize can't
    // re-merge. So resolve key-matches first, alias-only matches only as fallback.
    val (keyMatches, aliasOnly) = concluded.partition(k => normalizer.sanitize(k.cleanTitle) == norm)
    def nearest(cands: Seq[CacheKey]): Option[CacheKey] = primary.year match {
      case None    => cands.minByOption(canonicalRank)
      case Some(y) =>
        cands.filter(_.year.contains(y)).minByOption(canonicalRank)
          .orElse(cands.filter(_.year.exists(ky => math.abs(ky - y) <= 1)).minByOption(canonicalRank))
    }
    nearest(keyMatches).orElse(nearest(aliasOnly))
  }

  /** Collapse two same-tmdbId rows into one. The surviving key is chosen by
   *  `canonicalRank` (NOT arrival order), and the record is the union of every
   *  per-source slot — so no scraped data is lost whichever key wins, and the
   *  displayed title/year/ratings are derived from that union at read time.
   *  The stored result is therefore identical no matter which row was written
   *  first; enrichment-thread arrival order (which varies across machines, and
   *  used to flip the canonical here, drifting the whole-corpus snapshot
   *  between arm64 dev boxes and amd64 CI) no longer matters. */
  private def foldDeterministically(newKey: CacheKey, newRecord: MovieRecord, siblingKey: CacheKey): Unit = {
    // `stored` (cache-or-Mongo): a cold/evicted sibling read EMPTY would be merged
    // as absent, then full-replaced and its Mongo doc deleted — losing the ratings
    // the `*Ratings` refreshers wrote onto it.
    val siblingRecord = stored(siblingKey).getOrElse(newRecord)
    // Key the surviving row exactly as the settle's `canonicalizeBySanitize`
    // does — `FilmCanonicalizer.canonical` derives the key from the merged
    // record's `displayTitle` (the dominant cinema spelling) and unions onto the
    // tmdbId-bearing base. Picking the alphabetical-min raw key instead would, for
    // a CROSS-language fold ("Tangled" + "Zaplątani"), land on the original-
    // language title no cinema dominantly reports — so the next localised scrape
    // (matched by sanitize via `concludedKeyFor`) wouldn't find it and would
    // re-spawn the duplicate. One rule for both fold paths keeps the stored
    // result a pure function of the row set, not arrival order.
    val (canonical, merged) = FilmCanonicalizer.canonical(Seq(siblingKey -> siblingRecord, newKey -> newRecord))
    val victims = Seq(siblingKey, newKey).distinct.filterNot(_ == canonical)
    // Carry each loser's screenings + slots onto the canonical id BEFORE persisting. A merge
    // is a rename too: the losing row's showtimes are filed under ITS id, while the record we
    // are about to write holds that row STRIPPED (cache residency), so `upsert`'s re-stitch —
    // which looks under the id it is WRITING to — would find nothing and store nothing, and
    // the delete below would then destroy the only copy. Same rule as the re-key in
    // `MovieCache.rekey`; a `movies` row disappearing almost never means the film left.
    // Only a victim whose rows actually reached the canonical id is then deleted below —
    // `moveFilm` reports false when a read or write it depended on didn't happen, and the
    // delete would otherwise destroy the film's only copy. A stranded victim stays a
    // duplicate row that folds again next pass, which is the recoverable direction.
    val canonicalId = StoredMovieRecord.idFor(canonical.cleanTitle, canonical.year)
    val (moved, stranded) = victims.partition(victim =>
      repository.moveFilm(StoredMovieRecord.idFor(victim.cleanTitle, victim.year), canonicalId))
    persist(canonical, merged)
    // The merge may have filled enrichment inputs the canonical lacked (e.g. an
    // imdbId/searchTitle from the victim) — re-kick the affected enrichments.
    retriggerChangedEnrichments(siblingRecord, siblingKey, merged, canonical)
    moved.foreach { victim =>
      positive.invalidate(victim)
      repository.delete(victim.cleanTitle, victim.year)
    }
    if (stranded.nonEmpty)
      logger.warn(s"Fold into '${canonical.cleanTitle}': keeping ${stranded.size} duplicate row(s) whose " +
        "cinemas could not be carried across — they fold again on the next pass.")
    if (moved.nonEmpty) {
      mergeMetrics.recordMerge(MergeReason.TmdbIdentity, moved.size)
      logger.info(s"Folded duplicate(s) ${moved.map(v => s"'${v.cleanTitle}' (${v.year.getOrElse("—")})").mkString(", ")} " +
                  s"into '${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}) — same tmdbId=${newRecord.tmdbId.get}.")
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
   *  guarantee against a concurrent `repository.delete`.
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
    // the pair is atomic. The repository write below uses the pair to compute a
    // per-field diff — out-of-band Mongo edits to fields the updater didn't
    // touch (e.g. `FilmwebUrlAudit` clearing `filmwebUrl` while we're
    // bumping `filmwebRating`) survive the write.
    val before  = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
    val full    = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
    // Cache stores the STRIPPED record; the write below uses the FULL one so the
    // screenings-diff has real showtimes. `current` is the prior (stripped) resident row.
    val updated = positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = {
        before.set(current)
        val f = withoutZeroRatings(updater(current))
        full.set(f)
        forCache(f)
      }
    })
    if (updated == null) false
    else {
    val prior     = before.get()
    val fullAfter = full.get()
    // Write-guard by DIGEST: equal non-showtime fields AND equal per-slot showtime digest
    // ⇒ no real change, skip the write. See ShowtimesDigest.leanEqual.
    if (ShowtimesDigest.leanEqual(fullAfter, prior)) {
      // No real change — the common case: an unchanged cinema re-scrape tick
      // re-asserts the same slot. Skip the write entirely. Otherwise it issues
      // an `updateOne` that bumps only `updatedAt` (see `patchToUpdate`), and
      // each such no-op write is an oplog entry plus a change-stream
      // `updateLookup` full-document read per row, per pass — the dominant load
      // on the shared-CPU Mongo. The row is already in the desired state, so
      // report success without touching the repository (or firing the change stream).
      true
    } else {
      repository.updateIfPresent(key.cleanTitle, key.year, prior, fullAfter)
      touch()
      true
    }
    }
  }

  private[services] def markMissing(key: CacheKey): Unit =
    negative.put(key, java.lang.Boolean.TRUE)

  /** Drop all negative entries — used by the operator-triggered bulk TMDB retry
   *  (`MovieService.retryUnresolvedTmdb`) to give every previously-failed key one
   *  fresh shot. New misses re-populate the cache organically as they happen.
   *  (The scheduled, phase-spread re-try clears negatives one row at a time via
   *  `clearNegative` — see `UnresolvedTmdbReaper`.) */
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
    repository.delete(key.cleanTitle, key.year)
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
    require(normalizer.sanitize(oldKey.cleanTitle) == normalizer.sanitize(newKey.cleanTitle),
      s"rekey requires same normalised cleanTitle: ${oldKey.cleanTitle} vs ${newKey.cleanTitle}")
    withTitleLock(oldKey.cleanTitle) {
      // `stored` (cache-or-Mongo): a cold `oldKey` read EMPTY would be re-`put` at
      // `newKey` rating-less, nulling the scores the `*Ratings` refreshers own.
      //
      // A read that FAILED is worse still, and the cold-read fix above does not cover it:
      // the record would be re-`put` at `newKey` with no ratings AND no cinemas, so
      // `upsert` prunes every one of the film's showtimes. Defer instead — the row stays
      // at `oldKey`, nothing is invalidated, and the settle that asked for this re-key
      // runs again on its next tick. See [[MovieRepository.findByIdChecked]].
      storedChecked(oldKey) match {
        case (_, false) =>
          logger.warn(s"Deferring re-key '${oldKey.cleanTitle}' (${oldKey.year.getOrElse("—")}) → " +
            s"'${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}): the stored row could not be READ, " +
            "and re-keying a row we cannot see would write it back with neither ratings nor cinemas.")
          skippedUnreadable.incrementAndGet()
          ()
        case (row, true) =>
          val updated = update(row.getOrElse(MovieRecord()))
          // Carry the film's screenings + slots to the new id BEFORE the old row (and its
          // side rows with it) is deleted. Without this the rename destroys them: `upsert`
          // re-stitches showtimes from the id it writes to, finds none at the new id, and
          // stores none — see `MovieRepository.moveFilm`.
          //
          // A move that did NOT land defers the whole re-key, for the same reason the
          // unreadable branch above does: writing at `newKey` and invalidating `oldKey`
          // would leave the film keyed where its cinemas aren't and delete the id where
          // they are. Deferring leaves everything exactly as it was, and the settle asks
          // again next tick.
          if (oldKey != newKey &&
              !repository.moveFilm(StoredMovieRecord.idFor(oldKey.cleanTitle, oldKey.year),
                                   StoredMovieRecord.idFor(newKey.cleanTitle, newKey.year))) {
            logger.warn(s"Deferring re-key '${oldKey.cleanTitle}' (${oldKey.year.getOrElse("—")}) → " +
              s"'${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}): its screenings/slots could not " +
              "be carried to the new id, and re-keying without them empties the film.")
            skippedUnreadable.incrementAndGet()
          } else {
            if (oldKey != newKey) invalidate(oldKey)
            put(newKey, updated)
          }
      }
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
   *      (post-redirect). `CinemaScrapeRunner` publishes `MovieDetailsComplete`
   *      against this so two cinemas reporting different `year` values for
   *      the same film land on a single TMDB-stage event, no phantom row.
   *    - `isNew` is true when the `(cinema, raw title, raw year)` tuple is
   *      landing on this row for the first time; false on repeat ticks.
   *      `CinemaScrapeRunner` skips the bus publish for `isNew == false` so
   *      already-enriched rows don't churn event dispatches every scrape.
   *
   *  Doesn't touch enrichment-side fields (imdbId, ratings, URLs, …) — the
   *  TMDB / IMDb / MC / RT / Filmweb stages own those and run independently.
   *  Records whose cinema-side slots become empty are pruned daily by
   *  `UnscreenedCleanup`; a film that returns after the prune ran will
   *  re-pay the full enrichment cost on its next scrape. */
  /** The slot key for one cinema's report of a film under a given shown title.
   *  Every cinema slot is keyed by `(cinema, sanitize(title))` so a venue can hold
   *  several title-variant slots of one film (the original + a dubbed/decorated
   *  edition) without collision — the read-model split renders a card each, and
   *  the same (cinema, title) ALWAYS produces the same key so a re-scrape / fold
   *  merges in place instead of duplicating. */
  private def cinemaSlotKey(cinema: Cinema, title: String): Source =
    CinemaShowing.keyFor(cinema, title)

  /** Every slot this cinema currently holds in the cache. Shared by the two
   *  degraded-scrape guards, which read the same held-slot set along different
   *  axes — how many slots (breadth) and how many showtimes (depth). */
  private def heldSlotsOf(cinema: Cinema): Iterator[(Source, SourceData)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.values.iterator
      .flatMap(_.data.iterator)
      .collect { case (s, sd) if Source.cinemaOf(s).contains(cinema) => (s, sd) }
  }

  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie],
                         listingIsComplete: Boolean = true): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, proxy 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

    // DEPTH guard — the same "trust what we have" bail as above, on the axis
    // neither that check nor the film-count shrink guard further down can see.
    // A CHUNKED cinema is fetched one task per DATE, so a bad fetch window loses
    // whole dates while every film still comes back on the dates that worked:
    // breadth intact, depth collapsed. The shrink guard counts films and reads a
    // full board; every slot is "touched" so the prune never even engages; and the
    // thin showtime list simply REPLACES the full one (a cinema slot is rewritten
    // wholesale). That is how the UK lost ~70% of its upcoming showtimes on
    // 2026-07-27 while its film count ROSE — cinemas measured holding 18% of what
    // a complete re-scrape found, restored in full by the next healthy run.
    //
    // Threshold from production: sampling complete chunked runs against the
    // showtimes already stored for the same cinema, a HEALTHY tick reproduces the
    // stored count to within a few percent (PL+DE: mean 1.04, sd 0.05, range
    // 0.98-1.10), while the degraded UK ticks sat at 0.09-0.40. Reusing the shrink
    // guard's half-floor therefore sits ~10 sd below normal variation — no realistic
    // false positive — and still catches the incident with room to spare.
    // `MinShowtimesForDepthGuard` keeps it off boards small enough for a halving to
    // be real: a two-screen venue genuinely can go from three showings to one.
    //
    // Count via `slotShowtimeCount`, NOT `showtimes.size`: under the read-split every
    // resident slot has been through `stripForCache` and carries `Nil` showtimes, so a
    // direct `.size` reads 0 for every cinema and the floor below never engages. That is
    // production's shape and it made this guard dead code; the specs, which wire no
    // screenings repository, kept their lists resident and passed regardless.
    val knownCinemaShowtimes = heldSlotsOf(cinema).map { case (_, sd) =>
      ShowtimesDigest.slotShowtimeCount(sd) }.sum
    val batchShowtimes       = movies.iterator.map(_.showtimes.size).sum
    if (knownCinemaShowtimes >= MinShowtimesForDepthGuard &&
        batchShowtimes < knownCinemaShowtimes * PruneFloorRatio) {
      val consecutive = depthRejections.updateWith(cinema.displayName)(n => Some(n.getOrElse(0) + 1)).getOrElse(1)
      if (consecutive <= MaxConsecutiveDepthRejections) {
        RemovalAudit.scrapeDepthGuarded(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
        return Seq.empty
      }
      // Sustained across several ticks — stop treating it as a bad fetch and let
      // the smaller board land, rather than serving showtimes that no longer exist.
      RemovalAudit.scrapeDepthAccepted(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
    }
    depthRejections.remove(cinema.displayName)

    // Cold-mirror guard (only matters when diversion is wired). The newcomer test
    // further down reads the in-memory mirror (`knownSanitized` / `knownAliases` /
    // `knownByCinemaSlot`) to tell a genuinely-new film from a known one. A COLD mirror
    // — empty because a post-reboot `bootHydrate` `findAll()` came back empty while Mongo
    // was still coming up, and the change stream only carries post-boot writes (see
    // `bootHydrate`) — makes EVERY known film look new, so a scrape landing in that window
    // re-diverts the whole corpus into staging and the fold then drags it back over
    // ~30 min: the panel-36 `kinowo_worker_corpus_movies` boot flap (observed 2026-06-28,
    // 812→670→814 after a brief worker restart; ColdMirrorReDivertSpec). Sync the mirror
    // from the repository before deciding, restoring the prod invariant that the mirror
    // reflects `movies`. Cheap in steady state (the `estimatedSize` check short-circuits);
    // the `findAll` runs only while the mirror is genuinely cold, and `rehydrate` only
    // when the corpus actually has rows — a genuinely-empty corpus (a fresh deploy, where
    // a brand-new film SHOULD incubate) skips it and diverts as before. ONE-SHOT, on the
    // first scrape: at a real boot the repo already holds the corpus when the first scrape
    // lands, so the sync fires and warms the mirror for the rest; a fresh harness starts
    // with an empty repo, so the first scrape no-ops and the latch disarms — the sync can
    // never fire at a later, arrival-order-dependent scrape (StagingOrderDeterminismSpec).
    if (staging.isDefined && coldMirrorSyncArmed.getAndSet(false)
        && positive.estimatedSize() == 0 && repository.findAll().nonEmpty) rehydrate()

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
    // active rules. A key with no rules is an identity no-op. Display CASING is
    // NOT applied here — the raw spelling is kept as provenance and so the
    // `displayTitle` picker can rank on it (an all-caps variant signals low
    // quality); casing is applied to the chosen title in `MovieRecord.displayTitle`.
    val ruleKey = TitleRuleKey.of(cinema)
    // Central format strip: peel a screen-format/language tag ("(Napisy PL)",
    // "- 2D dubbing", "/napisy", "[2D DUB]") off EVERY cinema's title into the
    // showings' `format`, so a film's dub/subtitle/2D editions fold onto ONE clean
    // slot (each showing keeping its language) for every cinema — existing or new —
    // with no per-client code. `FormatTags` strips only format words, so a programme
    // prefix ("Kino Dostępne:"), a "+ event" suffix, or a Ukrainian screening keep
    // their title and stay their own card (see FormatTags' Ukrainian guard).
    def cleanAndFormat(cm: CinemaMovie): (String, List[String]) =
      FormatTags.extractFormatTags(normalizer.cinemaClean(ruleKey, cm.movie.title))
    val cleaned: CinemaMovie => String = cm => cleanAndFormat(cm)._1
    // Badge each screening with its film's format tokens (unless the client already
    // set one), BEFORE the same-title fold below unions them.
    val formatted: Seq[CinemaMovie] = movies.map { cm =>
      val tokens = cleanAndFormat(cm)._2
      if (tokens.isEmpty) cm
      else cm.copy(showtimes = cm.showtimes.map(st => if (st.format.isEmpty) st.copy(format = tokens) else st))
    }

    // A single cinema can report one film as several rows — one per screening
    // page (Kino Nowe Horyzonty's per-event `op.s?id=…` URLs), or under two
    // spellings that share the slot but differ by year (bare "Ojczyzna" +
    // "Ojczyzna" (2024)) or by a canonical unification the `cleaned` title keeps
    // apart (`&`↔`i`, the "Gwiezdne Wojny:" prefix). They all land on the SAME
    // cinema slot: `CinemaShowing(cinema, sanitize(title))` with NO year (see
    // `cinemaSlotKey`). Recording them one by one let the LAST win, keeping only
    // its filmUrl + showtimes and silently dropping every other screening — and
    // which row won depended on the scraper's emit order; worse, when two variants
    // both survived to write the slot, each identical re-scrape overwrote it in
    // turn — a permanent ping-pong that re-fired a change event + reprojection
    // forever (ReScrapeIdempotencySpec). (The format/language class — napisy /
    // dubbing / 2D — is already folded upstream by the `FormatTags` strip above,
    // which makes those editions `cleaned`-equal; this fold catches the residual
    // year / canonical collisions it can't.) Fold each cinema's same-slot rows
    // into one by grouping at EXACTLY the slot-key `sanitize(title)` granularity —
    // not `(cleaned, year)`, which is finer than the slot and so leaves same-slot
    // variants un-folded. Union every screening's showtimes (deduped, ordered) so
    // none is lost, and keep a deterministic representative for the scalar film
    // fields (incl. the year the row keys on).
    val slotGroupKey: CinemaMovie => String = cm => normalizer.sanitize(cleaned(cm))
    val deduped: Seq[CinemaMovie] =
      formatted.groupBy(slotGroupKey).toSeq
        .sortBy { case (k, _) => k }
        .map { case (_, group) =>
          if (group.lengthCompare(1) == 0) group.head
          else {
            val rep = MovieRecordMerge.slotRepresentative(group)
            // Dedup by *physical* screening identity (dateTime/room/format), not
            // by the whole Showtime: a cinema that lists one film under several
            // event pages (Kino Nowe Horyzonty's `op.s?id=…`) reports the same
            // session with different per-event bookingUrls, and a plain
            // `.distinct` kept each as a phantom duplicate whose order flipped
            // with the scraper's emit order. See MovieRecordMerge.dedupShowtimes.
            rep.copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
          }
        }

    import scala.jdk.CollectionConverters._
    // Partial-scrape guard: count the cinema's slots held BEFORE this tick's writes,
    // then decide whether the fresh batch is implausibly small (a degraded fetch).
    // If so, the prune below is skipped — the films this tick failed to mention keep
    // their slots until a healthy tick, instead of flickering off the site. Generic
    // across cinemas; Multikino (Cloudflare + session wall) is the recurring victim.
    // The BREADTH half of the pair; the depth half bails at the top of this method.
    val knownCinemaSlots = heldSlotsOf(cinema).size
    val scrapeLooksPartial =
      // A caller that KNOWS the listing is short says so, and that beats any inference:
      // a chunked scrape reduced from some of its date-chunks returns most of the board
      // (so the ratio below never engages) while silently omitting every film that only
      // screens on a missing date. Those are the advance-booking titles — Met Opera, RBO
      // season, NT Live, anniversary re-releases — and pruning them is what emptied UK
      // venues on 2026-07-27. The reduce handler computes exactly this fact and used to
      // throw it away.
      !listingIsComplete || (
        knownCinemaSlots >= MinSlotsForShrinkGuard &&
        deduped.size < knownCinemaSlots * PruneFloorRatio)
    // When `staging` is wired, divert a genuinely-NEW film (its `sanitize(title)`
    // group is absent from `movies`) to the staging sink to incubate; a film
    // already known to `movies` keeps the direct path. Snapshot the known
    // sanitized titles once (diversion never adds to `movies`, so the set is
    // stable across this tick) and this cinema's existing staging rows (for the
    // prior-slot carry-forward + the staging prune below).
    val knownSanitized: Set[String] =
      if (staging.isEmpty) Set.empty
      else positive.asMap().asScala.keysIterator.map(k => normalizer.sanitize(k.cleanTitle)).toSet
    // Sanitized TMDB aliases (Polish + original title) of every CONCLUDED, BARE
    // row, so a cinema's original-language listing of a known film ("Tangled") is
    // recognised as already-known and lands on the existing resolved row instead
    // of incubating a parallel newcomer. Gated on `isBareFilmTitle` (the same
    // predicate as `concludedKeyFor`'s alias arm and `groupByFilm`'s fold): a
    // decorated edition that merely carries the base title as an alias must NOT
    // make a genuinely-new bare film look already-known. Empty when staging is
    // unwired (no diversion happens).
    val knownAliases: Set[String] =
      if (staging.isEmpty) Set.empty
      else positive.asMap().asScala.iterator
        .collect { case (k, e) if e.tmdbConcluded && FilmCanonicalizer.isBareFilmTitle((k, e)) =>
          e.tmdbTitleAliases.iterator.map(normalizer.sanitize) }
        .flatten.toSet
    // Widened recognition: index every movies row by each of its CINEMA SLOTS'
    // sanitized titles, scoped to the slot's cinema. So a scrape lands on a film
    // this cinema's data already sits in — even under a decoration NO title rule
    // strips ("Zzz Nonexistent Fest: Toy Story 5") — instead of re-incubating it.
    // Without this, a folded row keyed off the BARE display form never matches the
    // cinema's DECORATED scrape key, so a known film re-diverts into staging every
    // 30-min tick: the served-count flap (Trójmiasto / GCF), reproduced rule-free in
    // `UnknownBannerReDivertSpec`. The per-banner canonical rules fix specific known
    // banners; this catches the general case for unknown ones. Derived fresh each
    // tick from the rows (exactly like `knownSanitized`), so a merge/split that moves
    // a cinema's slot to another row is reflected automatically — no persisted map to
    // keep in sync, nothing to update on fold.
    // Index every movies row by each of its cinema SLOTS' (cinema, sanitized title)
    // → the row's key. Multi-valued slots are covered by `cinemaShowings` (a venue
    // can hold the original AND a dubbed edition). Two uses, both derived fresh each
    // tick from the rows (no persisted map; a merge/split that moves a slot is
    // reflected automatically):
    //   - the divert gate (`knownByCinemaSlot`) recognises a film this cinema
    //     already sits in — even under a decoration no rule strips — so a known
    //     film isn't re-incubated (Trójmiasto/GCF flap, UnknownBannerReDivertSpec);
    //   - the land path routes a scrape onto the row that ALREADY HOLDS this exact
    //     (cinema, title) slot, so a same-cinema dub/decorated edition folded onto
    //     the base film updates its slot IN PLACE rather than re-spawning a
    //     title-keyed row and re-folding every tick (the same-cinema churn the
    //     per-(cinema,title) slot model exists to kill).
    val rowByCinemaSlot: Map[(Cinema, String), CacheKey] =
      positive.asMap().asScala.iterator.flatMap { case (k, rec) =>
        rec.cinemaShowings.iterator.flatMap { case (cin, sd) =>
          sd.title.iterator.map(t => (cin, normalizer.sanitize(t)) -> k)
        }
      }.toSeq.groupBy(_._1).view.mapValues(_.map(_._2).minBy(canonicalRank)).toMap
    val knownByCinemaSlot: Set[(Cinema, String)] =
      if (staging.isEmpty) Set.empty else rowByCinemaSlot.keySet
    val priorStagingRows: Map[String, services.staging.StagingRecord] =
      staging.fold(Map.empty[String, services.staging.StagingRecord]) {
        _.findAll().iterator.collect { case r if r.cinema == cinema => normalizer.sanitize(r.title) -> r }.toMap
      }
    val divertedSanitized = scala.collection.mutable.Set.empty[String]
    // Titles diverted into staging for the FIRST time this cinema (no prior row) —
    // the newcomers whose initial step StagingReaper should kick off an event,
    // rather than the periodic backstop. A re-divert of an already-incubating film
    // (prior row present) is NOT collected, so we don't republish every tick.
    val newlyDiverted = scala.collection.mutable.ArrayBuffer.empty[String]

    val resolved: Seq[((CinemaMovie, CacheKey, Boolean), SourceData)] =
      deduped.sortBy(cm => (cleaned(cm), cm.movie.releaseYear.getOrElse(Int.MinValue))).flatMap { cm =>
      val displayTitle = cleaned(cm)
      val primary      = keyOf(displayTitle, cm.movie.releaseYear)
      val norm         = normalizer.sanitize(displayTitle)
      // A newcomer: `staging` is wired and this film's sanitize group isn't in
      // `movies` yet — AND it isn't a known film listed under another language (an
      // alias of a concluded row). (Same-tick spelling variants already collapsed
      // in `deduped`.)
      val divert       = staging.isDefined && !knownSanitized(norm) && !knownAliases(norm) &&
                         !knownByCinemaSlot((cinema, norm))
      // Lock on the row's NORMALISED cleanTitle — `withTitleLock` keys by
      // `sanitize`, the SAME normalised key the TMDB stage and `rekey` acquire.
      // Serialises every read-modify-write on the row (scrape, rekey, TMDB put)
      // against each other so a concurrent (production-style) scrape can't race
      // into lost cinema slots. Different films (different normalised cleanTitle)
      // still don't contend.
      withTitleLock(primary.cleanTitle) {
        if (divert) {
          // NEWCOMER → staging. Build the slot off this cinema's PRIOR staging
          // slot (preserves two-stage detail fields + the year fallback), write
          // one `cinema|title|year` row, and DON'T touch `movies` — it stays held
          // out of the read model until it resolves and folds in (the promoter +
          // folder own that). Excluded from `resolved`, so no movies-side
          // prune/publish fires for it.
          val priorSlot     = priorStagingRows.get(norm).flatMap(_.record.data.get(cinemaSlotKey(cinema, displayTitle)))
          val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
          val slot          = buildCinemaSlot(cm, displayTitle, priorSlot, effectiveYear)
          staging.get.upsert(cinema, displayTitle, cm.movie.releaseYear, MovieRecord(
            searchTitle = Some(normalizer.apiQuery(normalizer.recase(displayTitle))),
            data        = Map(cinemaSlotKey(cinema, displayTitle) -> slot)
          ))
          divertedSanitized += norm
          if (!priorStagingRows.contains(norm)) newlyDiverted += displayTitle
          None
        } else {
          // Land the slot on the *canonical* key for this film, chosen by
          // `canonicalRank` (NOT arrival order): when this cinema's primary key
          // out-ranks the existing variant's (a year where the row was created
          // yearless, a lower year, or a casing that sorts first), promote the
          // whole row onto it before writing. Keeps the stored `(cleanTitle,
          // year)` a pure function of the reported variants. A scrape of an
          // already-concluded film lands straight on the resolved row; falls back
          // to the unique-match redirect for not-yet-concluded films.
          val key = concludedKeyFor(primary).getOrElse {
            redirectToExistingVariant(primary) match {
              case Some(existingKey) =>
                // A RESOLVED row's key is authoritative — TMDB's title + year,
                // settled when the film concluded — so never re-key it onto a
                // cinema's raw spelling. The promote below is for not-yet-resolved
                // variants only (yearless→yeared, a lower year, a casing that sorts
                // first). Without this gate, a cinema that lists a resolved film
                // ALL-CAPS at a production year ≥2 off TMDB's release year (so
                // `concludedKeyFor`'s ±1 window misses it) would promote the row to
                // that all-caps / lower-year key — the "PÓŁNOC, PÓŁNOCNY ZACHÓD"
                // (1957) re-casing of "Północ, północny zachód" (TMDB 1959). The
                // cinema slot still merges in; only the row's own key is preserved.
                val existingResolved = Option(positive.getIfPresent(existingKey)).exists(_.tmdbId.isDefined)
                if (existingResolved) existingKey
                else {
                  val canonical = Seq(primary, existingKey).minBy(canonicalRank)
                  if (canonical != existingKey) rekey(existingKey, canonical, identity)
                  canonical
                }
              // No title/year match — but if some row ALREADY holds this exact
              // (cinema, title) slot (a same-cinema dub/decorated edition folded
              // onto the base film under a different display title), land on THAT
              // row and update the slot in place, instead of spawning a new
              // title-keyed row that re-resolves and re-folds every tick.
              case None => rowByCinemaSlot.getOrElse((cinema, norm), primary)
            }
          }
          val existingOpt   = Option(positive.getIfPresent(key))
          val existing      = existingOpt.getOrElse(MovieRecord())
          val priorSlot     = existing.data.get(cinemaSlotKey(cinema, displayTitle))
          val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
          val slot          = buildCinemaSlot(cm, displayTitle, priorSlot, effectiveYear)
          // `isNew` controls whether to publish `MovieDetailsComplete`. Dedup
          // against the prior slot for this cinema so the same `(title, year)`
          // reported tick after tick doesn't churn downstream listeners.
          val isNew = !priorSlot.exists(s => s.title.contains(displayTitle) && s.releaseYear == effectiveYear)
          // Existing rows go through `putIfPresent` (a `$set`-diff that preserves
          // out-of-band edits); first-time scrapes `put` (keeps the tmdbId
          // identity gate live).
          val slotKey = cinemaSlotKey(cinema, displayTitle)
          existingOpt match {
            case Some(_) =>
              putIfPresent(key, current => current.copy(data = current.data + (slotKey -> slot)))
            case None =>
              // A cache MISS is not proof of first-time: a restart/eviction/re-key
              // can leave a fully-rated Mongo row unseen by Caffeine. Build the
              // full-record `put` on the STORED record (`stored` = cache-or-Mongo),
              // NOT the bare `existing` (empty on a miss) — else the `replaceOne`
              // nulls the ratings the `*Ratings` refreshers own. We `put` here (not
              // `putIfPresent`) deliberately: `putIfPresent` is a `computeIfPresent`
              // no-op on a cold key, so it would drop the slot write; and `put`
              // keeps the tmdbId identity gate live for a genuine first-time scrape
              // (where `stored` is empty and this collapses to the old behaviour).
              // `searchTitle` stays absent — a row landing straight in `movies`
              // carries none, so movies ingestion stays a pure function of the
              // canonical title, no scrape-order dependence.
              // …and a cache miss whose Mongo read FAILED is not proof of anything at all.
              // Treating it as first-time builds the record from scratch, so it carries
              // only the cinema being scraped right now, and `MovieRepository.upsert`
              // writes that as the whole film — `screenings.replaceFilm` then prunes every
              // OTHER cinema's showtimes. One unreadable row costs that film its board;
              // a cold cache after a restart routes the entire corpus down this branch,
              // which is how 2026-07-27 lost ~60% of the showtimes in every country while
              // the film counts stayed flat. Skip the film instead: the slot is simply not
              // recorded this tick, the stored rows are left exactly as they are, and the
              // cinema's next scrape (or the cache's next hydrate) picks it up.
              storedChecked(key) match {
                case (_, false) =>
                  logger.warn(s"Skipping '${key.cleanTitle}' (${key.year.getOrElse("—")}) from " +
                    s"${cinema.displayName}: its stored row could not be READ, and rebuilding it from " +
                    "this scrape alone would prune every other cinema's showtimes.")
                  skippedUnreadable.incrementAndGet()
                case (row, true) =>
                  val base = row.getOrElse(MovieRecord())
                  put(key, base.copy(data = base.data + (slotKey -> slot)))
              }
          }
          // A brand-new cinema observation grows what the TMDB stage can work
          // with; drop any stale "missing" verdict so the imminent publish
          // re-resolves against the grown row.
          if (isNew) clearNegative(key)
          Some(((cm, key, isNew), slot))
        }
      }
    }

    // Prune (movies): any of THIS cinema's slots that existed before but weren't
    // touched this tick → drop that slot. With per-(cinema,title) slots a venue can
    // hold several (original + dub); a tick scrapes them all, so an untouched slot
    // means that specific title stopped screening (or the dub did) — drop just it,
    // keeping the venue's other slots. The record itself stays.
    //
    // Identify "touched" by the slot's `SourceData` reference rather than by cache
    // key. The prune runs OUTSIDE the per-title lock, so a concurrent `cache.rekey`
    // can move a row to a year-keyed sibling — carrying our just-written slot along.
    // Slot-identity tracking survives that move because `cache.rekey` preserves the
    // SourceData reference verbatim.
    // …BUT skip the prune entirely when this tick looks like a partial/degraded
    // scrape (see `scrapeLooksPartial`): deleting slots a broken fetch merely
    // failed to list is what makes a still-playing film flicker off the site. A
    // healthy tick (full board) prunes normally, dropping whatever genuinely
    // stopped screening.
    val touchedSlots: Set[SourceData] = resolved.iterator.map(_._2).toSet
    if (scrapeLooksPartial)
      // The guard skipped the prune — log the decision (and what it spared) so a
      // degraded-tick episode is on the record even though nothing was removed.
      RemovalAudit.scrapePruneSkipped(cinema.displayName, batchFilms = deduped.size,
        knownSlots = knownCinemaSlots, reason = "partial-scrape-guard")
    else {
      val toPrune = positive.asMap().asScala.iterator
        .flatMap { case (k, e) =>
          val staleKeys = e.data.iterator.collect {
            case (s, sd) if Source.cinemaOf(s).contains(cinema) && !touchedSlots.contains(sd) => s
          }.toSet
          if (staleKeys.nonEmpty) Iterator.single(k -> staleKeys) else Iterator.empty
        }
        .toList
      toPrune.foreach { case (k, staleKeys) =>
        // Drop only the stale slot keys, under the per-title lock, via `putIfPresent`
        // so a concurrent sibling-slot write isn't clobbered. Before dropping, retain
        // each dropped slot's synopsis (longest-seen, keyed by its source) so the
        // displayed synopsis stays sticky once a cinema stops listing the film — see
        // MovieRecord.retainedSynopses / synopsis.
        putIfPresent(k, cur => {
          val captured = staleKeys.iterator.flatMap { s =>
            cur.data.get(s).flatMap(_.synopsis).filter(_.nonEmpty).map(s -> _)
          }.toMap
          cur.copy(
            data             = cur.data -- staleKeys,
            retainedSynopses = MovieRecordMerge.mergeRetainedSynopses(cur.retainedSynopses, captured))
        })
      }
      // The batch signal the served-films sawtooth needed: which cinema dropped how
      // many slots off how many still-known films this tick.
      RemovalAudit.scrapePruned(cinema.displayName, films = toPrune.size,
        slots = toPrune.iterator.map(_._2.size).sum,
        sampleFilmIds = toPrune.map { case (k, _) => s"${k.cleanTitle} (${k.year.getOrElse("—")})" },
        reason = "scrape-prune")
    }

    // Prune (staging): drop this cinema's staging rows it no longer lists this
    // tick — the staging analogue of the movies prune. A row that graduated to
    // `movies` was deleted by the folder (so it's absent from `priorStagingRows`);
    // one that simply stopped screening is removed here. Skipped on a partial
    // scrape for the same reason as the movies prune above.
    if (!scrapeLooksPartial) staging.foreach { s =>
      (priorStagingRows.keySet -- divertedSanitized).foreach { stale =>
        s.deleteRow(priorStagingRows(stale))
      }
    }

    // Publish CinemaMovieAdded for each `movies` row we just first-scraped onto,
    // AFTER the slot put + prune so a handler reading the cache immediately sees
    // the freshly-written slot. Gated on `isNew`. Diverted newcomers don't fire
    // this — their enrichment is driven by the staging promoter.
    resolved.foreach { case ((cm, key, isNew), _) =>
      if (isNew) bus.publish(CinemaMovieAdded(cinema, key.cleanTitle, key.year, cm.filmUrl))
    }

    // Kick each first-time newcomer's staging chain immediately. Published AFTER
    // the staging `upsert`s above so a synchronous StagingReaper handler reading
    // `pending_movies` sees the freshly-written row. Distinct: a same-tick spelling
    // variant can't fire the same film's anchor twice.
    newlyDiverted.distinct.foreach(t => bus.publish(StagingNewcomerDiverted(t)))

    resolved.map(_._1)
  }

  /** Build one cinema's `SourceData` slot for a scraped film — shared by the
   *  `movies` write and the staging divert so both apply the same rules:
   *    - two-stage detail preservation: a deferred cinema (e.g. Kino Muza) ships
   *      `posterUrl`/`synopsis`/`trailerUrl` AND the detail fields
   *      (cast/director/runtime/originalTitle/countries/genres) as None/empty on
   *      the listing tick — keep whatever the detail refresher already wrote
   *      (`priorSlot` carry-forward); else a listing tick WIPES the enrichment;
   *    - year fallback (`effectiveYear`): keep the prior year when a tick drops it
   *      (Helios' REST year flakes), treating a dropped year as loss not a change;
   *    - cast/director Title-Cased when ALL CAPS (Cinema City), runtime-zero
   *      squashed to None, and country names canonicalised. */
  private def buildCinemaSlot(
    cm:            CinemaMovie,
    displayTitle:  String,
    priorSlot:     Option[SourceData],
    effectiveYear: Option[Int]
  ): SourceData =
    SourceData(
      title          = Some(displayTitle),
      // Verbatim upstream title, kept so the merge key is re-derivable when the
      // per-cinema rules change. A rule-driven client carries the pre-strip
      // string in `movie.rawTitle`; others leave it None and `title` is raw.
      rawTitle       = cm.movie.rawTitle.orElse(Some(cm.movie.title)),
      originalTitle  = cm.movie.originalTitle.orElse(priorSlot.flatMap(_.originalTitle)),
      // Collapse a blurb the cinema CMS pasted N× into one description field
      // (Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together)
      // at the ingestion boundary, so we never store the duplicate — not just hide
      // it at read time. See tools.SynopsisMarkdown.collapseRepeats.
      // Intern so a film's N cinema slots carrying the same chain-wide blurb share ONE
      // String instead of N byte-identical copies (see StringPool). Same applies to the
      // cast/director/country/genre fields below — only the FRESH branch needs interning;
      // the prior-slot carry-forward already holds interned instances.
      synopsis       = cm.synopsis.map(tools.SynopsisMarkdown.collapseRepeats).map(StringPool.canonical).orElse(priorSlot.flatMap(_.synopsis)),
      // Detail fields (cast/director/runtime/originalTitle/countries/genres) are
      // filled by the deferred EnrichDetails merge; a listing-only cinema's re-scrape
      // carries none of them. Carry the prior slot's values forward when the fresh
      // listing lacks them — exactly as synopsis/poster/trailer above — so a listing
      // tick doesn't WIPE the enrichment (which EnrichDetails then re-adds, flapping
      // the row + doubling its change-stream writes). A listing that DOES carry the
      // field still wins, matching FilmDetail.mergeInto's "fill only if empty" rule.
      cast           = if (cm.cast.nonEmpty) StringPool.canonicalAll(cm.cast.map(TextNormalization.titleCaseIfAllCaps))
                       else priorSlot.map(_.cast).getOrElse(Seq.empty),
      director       = if (cm.director.nonEmpty) StringPool.canonicalAll(cm.director.map(TextNormalization.titleCaseIfAllCaps))
                       else priorSlot.map(_.director).getOrElse(Seq.empty),
      runtimeMinutes = cm.movie.runtimeMinutes.filter(_ > 0).orElse(priorSlot.flatMap(_.runtimeMinutes)),
      releaseYear    = effectiveYear,
      countries      = { val cs = StringPool.canonicalAll(cm.movie.countries.map(c => CountryNames.canonical(c, enrichmentLanguage)).distinct)
                         if (cs.nonEmpty) cs else priorSlot.map(_.countries).getOrElse(Seq.empty) },
      genres         = if (cm.movie.genres.nonEmpty) StringPool.canonicalAll(cm.movie.genres)
                       else priorSlot.map(_.genres).getOrElse(Seq.empty),
      // Interned like the fields above, and for the same reason: a film's poster,
      // film page and trailer are ONE url repeated across every cinema showing it.
      // Highest-yield strings in the corpus by some margin — the 2026-07-27 UK heap
      // dump held 136,064 poster-url instances for 1,896 distinct values (71.8x) and
      // 138,199 film-page instances for 2,004 (69.0x). `Showtime.bookingUrl` is
      // deliberately NOT interned: it is per-screening, only 1.6x repeated
      // (182,719 -> 116,571 distinct), so pooling it would evict this whole
      // low-cardinality vocabulary for almost no saving.
      posterUrl      = cm.posterUrl.map(StringPool.canonical).orElse(priorSlot.flatMap(_.posterUrl)),
      filmUrl        = cm.filmUrl.map(StringPool.canonical),
      trailerUrl     = cm.trailerUrl.map(StringPool.canonical).orElse(priorSlot.flatMap(_.trailerUrl)),
      // Canonical order so a reorder-only re-scrape stores a byte-identical slot and
      // the write-through guard skips it. Past showings the fresh scrape drops are NOT
      // retained: under the index-only cache the resident `priorSlot` is stripped (Nil
      // showtimes + a digest), so there's nothing to retain FROM, and re-stitching a
      // film's screenings from Mongo per scrape would cost far more read I/O than the
      // one deferred write it would save. Dropping a just-passed showtime is
      // display-neutral (the web filters past showtimes at render). See
      // MovieRecordMerge.sortShowtimes.
      showtimes      = MovieRecordMerge.sortShowtimes(cm.showtimes),
      // Carry the certificate forward on a listing-only re-scrape, like the detail
      // fields above, so a tick that lacks it doesn't wipe a value the detail merge added.
      ageRating      = cm.ageRating.map(StringPool.canonical).orElse(priorSlot.flatMap(_.ageRating))
    )

  /** If `primary` doesn't currently exist in the cache, look for an existing
   *  row that already knows `primary.cleanTitle` (via its `cinemaTitles`
   *  set). Returns that row's key when there's exactly one such match —
   *  ambiguous (cross-film collision) and zero-match cases fall through to
   *  None and the caller creates a fresh row.
   *
   *  Match uses `normalizer.sanitize` so an Arabic/Roman /
   *  punctuation / case variant ("Mortal Kombat 2") redirects onto a row
   *  that only knows the canonical form ("Mortal Kombat II") — without
   *  this, the FIRST cinema's raw spelling would pin the row's
   *  `cinemaTitles` and every other cinema's variant of the same film
   *  would create a separate row at its own key. */
  private def redirectToExistingVariant(primary: CacheKey): Option[CacheKey] = {
    if (positive.getIfPresent(primary) != null) return None
    import scala.jdk.CollectionConverters._
    val normalizedRaw = normalizer.sanitize(primary.cleanTitle)
    // Match by cleanTitle normalisation. Cross-script titles produce
    // different normalised forms (sanitize keeps Unicode letters), so a
    // Cyrillic row can't be matched by a Latin scrape and vice versa.
    val candidates = positive.asMap().asScala.iterator
      .filter { case (k, _) => normalizer.sanitize(k.cleanTitle) == normalizedRaw }
      .map(_._1)
      .toSet  // unique by CacheKey (which dedups by normalized form)
    if (candidates.size == 1) Some(candidates.head) else None
  }

  def hasResolvedSiblingByTitle(rawTitle: String): Boolean = {
    import scala.jdk.CollectionConverters._
    val normalizedRaw = normalizer.sanitize(rawTitle)
    positive.asMap().asScala.iterator.exists { case (k, e) =>
      e.tmdbId.isDefined && normalizer.sanitize(k.cleanTitle) == normalizedRaw
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
    val rows          = repository.findAll()
    val tFindAllMs    = (System.nanoTime() - tFindAllStart) / 1000000
    // `repository.findAll()` swallows every Mongo failure into `Seq.empty` — a
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
                  "Mongo connection disabled, query timed out, or repository genuinely empty. " +
                  "Pages will render with no films until the next successful tick.")
    }
    val tPostFetch = System.nanoTime()
    // Group by key BEFORE putting: a merge-key rule added after these documents were
    // written (a new GlobalStructural strip) makes two stored titles collide on
    // `CacheKey`, and a bare `put`-per-row is last-write-wins — it would silently
    // drop one document's showtimes until the next scrape. Union the colliding rows
    // instead, so the cache is lossless the moment the rule lands (the orphaned
    // Mongo `_id` is reconciled by a later scrape / the reaper).
    val byKey: Map[CacheKey, MovieRecord] =
      rows.groupBy(r => CacheKey(r.title, r.year))
        .map { case (k, rs) => k -> MovieRecordMerge.unionAll(rs.map(_.record)) }
    // Count what this backstop reload catches that the incremental change stream missed:
    // a put whose cached value DIFFERED (a missed upsert) and a key no longer in Mongo (a
    // missed delete). After resume tokens + delete-apply these should be ~0 in steady state
    // — the signal (kinowo_worker_cache_rehydrate_changes) that the rehydrate is redundant.
    var changed = 0
    byKey.foreach { case (k, record) =>
      if (!Option(positive.getIfPresent(k)).exists(ShowtimesDigest.leanEqual(_, record))) changed += 1
      positive.put(k, forCache(record))
    }
    val removed = positive.asMap().keySet().asScala.toSeq.filterNot(byKey.keySet.contains)
    removed.foreach(positive.invalidate)
    cacheMetrics.recordRehydrate(changed, removed.size)
    if (changed > 0 || removed.nonEmpty)
      logger.info(s"MovieCache rehydrate: caught $changed changed row(s) + ${removed.size} orphan-delete(s) " +
        "the change stream missed.")
    // Reap mis-keyed Mongo orphans. A stored row whose persisted `_id` no longer
    // equals the canonical id for its RE-DERIVED identity — `idFor(displayTitle,
    // year)` — was first stored under one title and later drifted to another (a
    // cinema's English "Tangled" row whose TMDB Polish title is "Zaplątani", so it
    // now displays, and keys in this cache, as "Zaplątani" alongside a separate
    // "zaplatani|2010" doc). `byKey` already merged its slots into the single cache
    // entry, but the stale doc lingers in `movies` and shows as a /debug duplicate
    // (the cross-title settle never sees two rows — they collapsed to one CacheKey
    // on load). Rewrite the canonical doc with the merged record, then delete the
    // orphan id(s). Gated on an orphan existing, so a canonical corpus writes nothing.
    val orphans = rows.collect {
      case r if r.persistedId.exists(_ != StoredMovieRecord.idFor(r.title, r.year)) =>
        CacheKey(r.title, r.year) -> r.persistedId.get
    }
    if (orphans.nonEmpty) {
      orphans.groupBy(_._1).foreach { case (k, items) =>
        byKey.get(k).foreach(repository.upsert(k.cleanTitle, k.year, _))
        items.map(_._2).distinct.foreach(repository.deleteById)
      }
      logger.info(s"MovieCache rehydrate: reaped ${orphans.size} mis-keyed `movies` orphan(s) whose `_id` drifted from the display title.")
    }
    // Hydrate is a PURE LOAD: it rebuilds the cache from Mongo (`fromStorage`, which
    // keys each row by `displayTitle`) and stops. It deliberately does NOT
    // re-canonicalise. The raw `positive.put` above can leave same-film rows split
    // across years/spellings (`Kumotry|2025` + `Kumotry|2026`, both one tmdbId);
    // collapsing them is the periodic `SettleReaper`'s job (`MovieService.settle`),
    // NOT the load's — re-merging here, right after `fromStorage` re-derives every
    // key, was the per-deploy re-key flap. The newcomer path stays settled via the
    // staging fold; cross-title/cross-year splits are reconciled by the reaper.
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
  //  1. INCREMENTAL (primary): a change stream (`repository.watchChanges`) applies
  //     each inserted/updated/replaced row (`applyUpsert`) AND each DELETE
  //     (`applyDelete`) to the cache the moment it lands in Mongo — O(changes),
  //     near-instant, and costs nothing when nothing changes. This replaced a
  //     periodic full `findAll()` that re-read the ENTIRE collection on a timer: at
  //     ~200 rows that was ~150 ms (so the old 30-s cadence felt free), but at 500+
  //     rows it climbed to 6–9 s and, run twice a minute, burned 20–30% of the single
  //     shared vCPU continuously and starved page renders. The change stream makes the
  //     cost proportional to real edits instead of corpus size.
  //
  //  2. BACKSTOP (safety net): a RARE full `rehydrate()` — now that upserts (`applyUpsert`),
  //     deletes (`applyDelete`) and CacheKey collisions (the settle-reaper's
  //     `canonicalizeBySanitize` merges them in the source, and the stream propagates the
  //     result) all resolve incrementally, and the stream resumes from a persisted token
  //     across restarts, its three former jobs (deletes / gaps / collisions) are covered
  //     elsewhere. What's left is belt-and-suspenders + the rare orphan-reap (drifted-`_id`
  //     Mongo hygiene, below), so the expensive findAll dropped from every 30 s → 30 min →
  //     now every `BackstopIntervalSeconds` (default 6 h). A full retire would need a
  //     source-id↔key reverse-index across all 9 cache write paths — not worth it for a
  //     case the reaper already self-heals. Tunable via KINOWO_CACHE_REHYDRATE_SECONDS.
  //
  // The scheduler + watch only start when `start()` is called (Wiring does,
  // tests don't), so unit tests still get a single one-shot hydrate at
  // construction unless they opt into the live sync.
  private val refreshScheduler        = DaemonExecutors.scheduler("movie-cache-refresh")
  private val BackstopIntervalSeconds = Env.positiveLong("KINOWO_CACHE_REHYDRATE_SECONDS", 21600L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Apply one out-of-band upsert from the change stream to the in-memory cache.
   *  Mirrors a single row of `rehydrate` — a direct `positive.put`, bypassing
   *  the identity-gate `put` (Mongo is already the source of truth here, no
   *  re-folding needed). */
  private def applyUpsert(r: StoredMovieRecord): Unit = {
    positive.put(CacheKey(r.title, r.year), forCache(r.record))
    touch()
  }

  /** Apply an out-of-band DELETE from the change stream: drop the mirrored row whose
   *  source `_id` was removed (a fold/merge loser, an `UnscreenedCleanup` removal, a
   *  re-key's old id). The stream carries only the `_id`, so map it back to the CacheKey
   *  the row was stored under via `idFor`. Applying deletes incrementally is what lets the
   *  backstop rehydrate stop being the ONLY thing that catches them; a rare drifted /
   *  duplicate key that doesn't map is still swept by the backstop. */
  private[services] def applyDelete(id: String): Unit = {
    import scala.jdk.CollectionConverters._
    positive.asMap().keySet().asScala
      .find(k => StoredMovieRecord.idFor(k.cleanTitle, k.year) == id)
      .foreach { k =>
        positive.invalidate(k)
        // An out-of-band Mongo delete arriving via the change stream — the mirror
        // of a fold/UnscreenedCleanup/re-key removal another writer made. The only
        // signal that a cache row vanished for a reason NOT originating on this node.
        RemovalAudit.filmRemoved("cache.applyDelete", id, reason = "change-stream-delete")
        touch()
      }
  }

  def start(): Unit = {
    watchHandle = repository.watchChanges(applyUpsert, applyDelete)
    logger.info(
      s"MovieCache incremental change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
      s"backstop rehydrate every ${BackstopIntervalSeconds}s.")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(rehydrate()).recover {
        case exception => logger.warn(s"MovieCache rehydrate tick failed: ${exception.getMessage}")
      },
      BackstopIntervalSeconds, BackstopIntervalSeconds, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    refreshScheduler.shutdown()
  }
}
