package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
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
   *  `(CinemaMovie, CacheKey, isNew)` triple per input movie. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)]

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
  // Clock for the "now" used to tell past from future when a scrape merges its
  // showtimes (see `buildCinemaSlot` → `MovieRecordMerge.retainPastShowtimes`).
  // Injectable so tests can fix "now" deterministically.
  clock: java.time.Clock = java.time.Clock.systemDefaultZone()
) extends MovieCache with Stoppable with Logging {

  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().build()
  private val negative: Cache[CacheKey, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(24, TimeUnit.HOURS).build()

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

  private def persist(key: CacheKey, e: MovieRecord): Unit = {
    val clean = withoutZeroRatings(e)
    positive.put(key, clean)
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
    val target = TitleNormalizer.sanitize(key.cleanTitle)
    val sameTitle = positive.asMap().asScala.keysIterator
      .filter(k => TitleNormalizer.sanitize(k.cleanTitle) == target).toSeq
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
    val canonicalSanitized = TitleNormalizer.sanitize(canonical.cleanTitle)
    val needsFix = keys.sizeIs > 1 ||
      keys.exists(k => TitleNormalizer.sanitize(k.cleanTitle) == canonicalSanitized &&
        (k.cleanTitle != canonical.cleanTitle || k.year != canonical.year))
    if (needsFix) {
      withTitleLock(canonical.cleanTitle) {
        keys.foreach(invalidate)
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

  def settleResolved(oldKey: CacheKey, resolved: MovieRecord): CacheKey =
    withTitleLock(oldKey.cleanTitle) {
      import scala.jdk.CollectionConverters._
      // TMDB's resolved year re-keys a YEARLESS row onto it; a row that already
      // carries a year keeps its key — re-keying a yeared row across the async
      // resolve races `canonicalRank` (`canonicalizeBySanitize` owns that
      // migration — run by the staging fold and on every rehydrate).
      val target =
        if (oldKey.year.isEmpty && resolved.resolvedYear.isDefined)
          keyOf(oldKey.cleanTitle, resolved.resolvedYear)
        else oldKey
      // Fold any prior occupant of `target` into the resolved record up front so
      // re-keying onto an occupied year can't drop its cinema slots.
      val priorTarget = if (target != oldKey) Option(positive.getIfPresent(target)) else None
      val base        = priorTarget.fold(resolved)(t => MovieRecordMerge.union(resolved, t))
      val norm        = TitleNormalizer.sanitize(oldKey.cleanTitle)
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
        TitleNormalizer.sanitize(k.cleanTitle) == norm
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
    val norm = TitleNormalizer.sanitize(primary.cleanTitle)
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
        (TitleNormalizer.sanitize(k.cleanTitle) == norm ||
         (FilmCanonicalizer.isBareFilmTitle((k, e)) &&
          e.tmdbTitleAliases.exists(a => TitleNormalizer.sanitize(a) == norm))) => k }
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
    val (keyMatches, aliasOnly) = concluded.partition(k => TitleNormalizer.sanitize(k.cleanTitle) == norm)
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
    val siblingRecord = Option(positive.getIfPresent(siblingKey)).getOrElse(newRecord)
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
    persist(canonical, merged)
    // The merge may have filled enrichment inputs the canonical lacked (e.g. an
    // imdbId/searchTitle from the victim) — re-kick the affected enrichments.
    retriggerChangedEnrichments(siblingRecord, siblingKey, merged, canonical)
    val victims = Seq(siblingKey, newKey).distinct.filterNot(_ == canonical)
    victims.foreach { victim =>
      positive.invalidate(victim)
      repository.delete(victim.cleanTitle, victim.year)
    }
    if (victims.nonEmpty) {
      mergeMetrics.recordMerge(MergeReason.TmdbIdentity, victims.size)
      logger.info(s"Folded duplicate(s) ${victims.map(v => s"'${v.cleanTitle}' (${v.year.getOrElse("—")})").mkString(", ")} " +
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
      // report success without touching the repository (or firing the change stream).
      true
    } else {
      repository.updateIfPresent(key.cleanTitle, key.year, before.get(), updated)
      touch()
      true
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

  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie]): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, proxy 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

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
      FormatTags.extractFormatTags(TitleNormalizer.cinemaClean(ruleKey, cm.movie.title))
    val cleaned: CinemaMovie => String = cm => cleanAndFormat(cm)._1
    // Badge each screening with its film's format tokens (unless the client already
    // set one), BEFORE the same-title fold below unions them.
    val formatted: Seq[CinemaMovie] = movies.map { cm =>
      val tokens = cleanAndFormat(cm)._2
      if (tokens.isEmpty) cm
      else cm.copy(showtimes = cm.showtimes.map(st => if (st.format.isEmpty) st.copy(format = tokens) else st))
    }

    // A single cinema can report one film as several rows — one per screening
    // page (Kino Nowe Horyzonty's per-event `op.s?id=…` URLs), which all
    // normalise to the same `(cleanTitle, year)` slot. Recording them one by
    // one let the LAST win, keeping only its filmUrl + showtimes and silently
    // dropping every other screening — and which row won depended on the order
    // the scraper emitted them. Fold each cinema's same-key rows into one:
    // union every screening's showtimes (deduped, ordered) so none is lost, and
    // keep a deterministic representative for the scalar film fields.
    val deduped: Seq[CinemaMovie] =
      formatted.groupBy(cm => (cleaned(cm), cm.movie.releaseYear)).toSeq
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

    import scala.jdk.CollectionConverters._
    // When `staging` is wired, divert a genuinely-NEW film (its `sanitize(title)`
    // group is absent from `movies`) to the staging sink to incubate; a film
    // already known to `movies` keeps the direct path. Snapshot the known
    // sanitized titles once (diversion never adds to `movies`, so the set is
    // stable across this tick) and this cinema's existing staging rows (for the
    // prior-slot carry-forward + the staging prune below).
    val knownSanitized: Set[String] =
      if (staging.isEmpty) Set.empty
      else positive.asMap().asScala.keysIterator.map(k => TitleNormalizer.sanitize(k.cleanTitle)).toSet
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
          e.tmdbTitleAliases.iterator.map(TitleNormalizer.sanitize) }
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
          sd.title.iterator.map(t => (cin, TitleNormalizer.sanitize(t)) -> k)
        }
      }.toSeq.groupBy(_._1).view.mapValues(_.map(_._2).minBy(canonicalRank)).toMap
    val knownByCinemaSlot: Set[(Cinema, String)] =
      if (staging.isEmpty) Set.empty else rowByCinemaSlot.keySet
    val priorStagingRows: Map[String, services.staging.StagingRecord] =
      staging.fold(Map.empty[String, services.staging.StagingRecord]) {
        _.findAll().iterator.collect { case r if r.cinema == cinema => TitleNormalizer.sanitize(r.title) -> r }.toMap
      }
    val divertedSanitized = scala.collection.mutable.Set.empty[String]

    val resolved: Seq[((CinemaMovie, CacheKey, Boolean), SourceData)] =
      deduped.sortBy(cm => (cleaned(cm), cm.movie.releaseYear.getOrElse(Int.MinValue))).flatMap { cm =>
      val displayTitle = cleaned(cm)
      val primary      = keyOf(displayTitle, cm.movie.releaseYear)
      val norm         = TitleNormalizer.sanitize(displayTitle)
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
            searchTitle = Some(TitleNormalizer.apiQuery(TitleNormalizer.recase(displayTitle))),
            data        = Map(cinemaSlotKey(cinema, displayTitle) -> slot)
          ))
          divertedSanitized += norm
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
              // `searchTitle` is a STAGING-only field (set on the newcomer that
              // diverts to `pending_movies`); a row landing straight in `movies`
              // carries none, so movies ingestion stays a pure function of the
              // canonical title — no scrape-order dependence.
              put(key, existing.copy(data = existing.data + (slotKey -> slot)))
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
    val touchedSlots: Set[SourceData] = resolved.iterator.map(_._2).toSet
    positive.asMap().asScala.iterator
      .flatMap { case (k, e) =>
        val staleKeys = e.data.iterator.collect {
          case (s, sd) if Source.cinemaOf(s).contains(cinema) && !touchedSlots.contains(sd) => s
        }.toSet
        if (staleKeys.nonEmpty) Iterator.single(k -> staleKeys) else Iterator.empty
      }
      .toList
      .foreach { case (k, staleKeys) =>
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

    // Prune (staging): drop this cinema's staging rows it no longer lists this
    // tick — the staging analogue of the movies prune. A row that graduated to
    // `movies` was deleted by the folder (so it's absent from `priorStagingRows`);
    // one that simply stopped screening is removed here.
    staging.foreach { s =>
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

    resolved.map(_._1)
  }

  /** Build one cinema's `SourceData` slot for a scraped film — shared by the
   *  `movies` write and the staging divert so both apply the same rules:
   *    - two-stage detail preservation: a deferred cinema (e.g. Kino Muza) ships
   *      `posterUrl`/`synopsis`/`trailerUrl` as None on the listing tick — keep
   *      whatever the detail refresher already wrote (`priorSlot` carry-forward);
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
      originalTitle  = cm.movie.originalTitle,
      // Collapse a blurb the cinema CMS pasted N× into one description field
      // (Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together)
      // at the ingestion boundary, so we never store the duplicate — not just hide
      // it at read time. See tools.SynopsisMarkdown.collapseRepeats.
      synopsis       = cm.synopsis.map(tools.SynopsisMarkdown.collapseRepeats).orElse(priorSlot.flatMap(_.synopsis)),
      cast           = cm.cast.map(TextNormalization.titleCaseIfAllCaps),
      director       = cm.director.map(TextNormalization.titleCaseIfAllCaps),
      runtimeMinutes = cm.movie.runtimeMinutes.filter(_ > 0),
      releaseYear    = effectiveYear,
      countries      = cm.movie.countries.map(CountryNames.canonical).distinct,
      genres         = cm.movie.genres,
      posterUrl      = cm.posterUrl.orElse(priorSlot.flatMap(_.posterUrl)),
      filmUrl        = cm.filmUrl,
      trailerUrl     = cm.trailerUrl.orElse(priorSlot.flatMap(_.trailerUrl)),
      // Canonical order (a reorder-only re-scrape stores an identical slot so the
      // write-through guard skips it) AND retain already-past showings the scrape
      // dropped — deleting a past showtime is pure churn (it no longer displays,
      // nothing reaps it), so keeping it lets an aging-only re-scrape hit the guard.
      // See MovieRecordMerge.sortShowtimes / retainPastShowtimes.
      showtimes      = MovieRecordMerge.retainPastShowtimes(
                         priorSlot.map(_.showtimes).getOrElse(Seq.empty), cm.showtimes, java.time.LocalDateTime.now(clock))
    )

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
    byKey.foreach { case (k, record) => positive.put(k, record) }
    positive.asMap().keySet().asScala.toSeq
      .filterNot(byKey.keySet.contains)
      .foreach(positive.invalidate)
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
  //  1. INCREMENTAL (primary): a change stream (`repository.watchUpserts`) applies
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
    watchHandle = repository.watchUpserts(applyUpsert)
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
