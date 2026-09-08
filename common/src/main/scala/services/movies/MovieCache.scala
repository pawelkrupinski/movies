package services.movies

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, MovieRecord}
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{EventBus, InProcessEventBus}
import tools.{DaemonExecutors, Env}

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
  def apply(cleanTitle: String, year: Option[Int], normalizer: TitleNormalizer): CacheKey =
    new CacheKey(cleanTitle, year, normalizer.sanitize(cleanTitle))

  /** The key a stored row answers to: its `key` field AS STORED (`normalized|year`), with
   *  the display title as its label. The stored key is the lookup identity; re-deriving
   *  it from the display title — which is a vote over the row's slots and moves as
   *  they do — used to make the cache and the store disagree about which key a row
   *  held (the drifted-id orphans of old). */
  def stored(cleanTitle: String, storedKey: String): CacheKey = {
    val sep = storedKey.lastIndexOf('|')
    val (normalized, year) =
      if (sep >= 0) (storedKey.substring(0, sep), storedKey.substring(sep + 1).toIntOption) else (storedKey, None)
    new CacheKey(cleanTitle, year, normalized)
  }

  /** The character separating a disambiguated key's base title from its suffix —
   *  never emitted by `TitleNormalizer.sanitize` (which strips every non-alphanumeric
   *  Unicode character), so it is an unambiguous marker inside `normalized`. */
  private val DisambiguatorMarker = '~'

  /** A retitle that keeps `base`'s identity apart from whichever OTHER film already
   *  holds `base`'s bare key — see `StagingFold.resolveKeyCollisions`. Only the
   *  literal `normalized` string (and therefore the stored `key` field / Mongo's
   *  unique index on it) changes; `lookupBase` strips the suffix straight back off,
   *  so `CorpusIndex` still buckets this row together with the film that kept the
   *  plain key, and `ScrapeLanding.concludedKeyFor` / `chooseConcluded` still see
   *  both candidates for every future listing of either film. `suffix` must be
   *  content-derived and stable (a tmdbId/imdbId), never an arrival-order artifact
   *  (a `FilmId`) — see the caller. */
  def disambiguated(base: CacheKey, suffix: String): CacheKey =
    new CacheKey(base.cleanTitle, base.year, s"${base.normalized}$DisambiguatorMarker$suffix")

  /** The bare lookup form of a (possibly disambiguated) `normalized` string — what
   *  `CorpusIndex` buckets rows by, and what a row's OWN spelling must sanitize back
   *  to for `StoredMovieRecord.fromStorage`'s mangled-title fallback. A no-op for
   *  every ordinary key: `sanitize` never produces [[DisambiguatorMarker]], so this
   *  only ever strips something on a key `disambiguated` built. */
  def lookupBase(normalized: String): String = {
    val i = normalized.indexOf(DisambiguatorMarker)
    if (i >= 0) normalized.substring(0, i) else normalized
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
  /** The country whose title rules key this cache's rows. Exposed on the READ
   *  surface because the enrichers and the read-model projection hold a cache and
   *  need to fold titles the same way it did — a resolution hint key or a
   *  projected `_id` built under different rules addresses a row that isn't
   *  there. ABSTRACT: an implementation that could silently fall back to a
   *  process default is how a German title came to be keyed `minionsimonster`. */
  def normalizer: TitleNormalizer

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
   *  `(CinemaMovie, CacheKey, isNew)` triple per input movie WHOSE SLOT WAS WRITTEN —
   *  a listing whose write was skipped (its stored row could not be read) yields none,
   *  so a caller cannot classify or announce a row the cache does not hold.
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
   *  rows loaded. Used at construction and by the admin rehydrate endpoint. */
  def rehydrate(): Int

  // ── Internal write surface (services.* only) ─────────────────────────────
  private[services] def put(key: CacheKey, e: MovieRecord): Unit
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
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
   *  re-keyed. Driven by `MovieService.settle` (the periodic `SettleReaper`). */
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

  /** The film as STORED — its slots carrying the showtime LISTS, re-stitched from
   *  `screenings`. `(row, readOk)` like [[storedChecked]].
   *
   *  Not the same read: [[storedChecked]] answers from the cache when the row is
   *  resident, and a resident row has been through `ShowtimesDigest.stripForCache`,
   *  so every slot on it holds `showtimes = Nil` (the lists live in `screenings`,
   *  keyed by film id). That is the right shape for the cache and the wrong shape
   *  for anyone MOVING a slot somewhere the screenings don't follow it — which is
   *  what `MixedFilmSplitter` does when it sends a stray cinema back to staging.
   *  Skipping the cache is therefore the point, not an oversight.
   *
   *  A caller that hands the result somewhere else must honour `readOk`: staging a
   *  slot on the strength of a FAILED read writes an empty board over a real one. */
  private[services] def restitchedChecked(key: CacheKey): (Option[MovieRecord], Boolean)

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
  private[services] def rekey(oldKey: CacheKey, newKey: CacheKey, update: MovieRecord => MovieRecord, reason: RekeyReason): Unit

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
 *
 * What this class OWNS is the resident corpus and the way into it: the Caffeine
 * map and its [[CorpusIndex]], the write funnels (`put`'s tmdbId / imdbId identity
 * gate, `putIfPresent`, `rekey`), the per-title and per-tmdbId locks, the
 * cache-or-store reads, hydration and the change-stream mirror, and the settle
 * paths that self-heal the corpus (`canonicalizeBySanitize`, `settleResolved`,
 * `backfillEmbeddedYears`). The scrape-time LANDING — where one venue's fresh
 * listing goes, and what its arrival displaces — is [[ScrapeLanding]]'s, reached
 * through the [[LandingStore]] seam this class implements; `recordCinemaScrape`
 * here is a delegation. See `docs/stable-film-id.md`.
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
  // cache directly) disables diversion — every scrape lands in `movies`. Read by
  // `ScrapeLanding` alone, as are `bus`, `enrichmentLanguage` and `screeningTokens`
  // below: they stay here so every caller's construction is unchanged, and are
  // handed straight to the landing.
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
  // The country's badge vocabulary. Sibling of `enrichmentLanguage` above and
  // wired from the same `country`: one token in it — the voice-over version — is
  // the country's own to spell (`LEK` in Poland, `LEC` in the English-speaking
  // deployments). Defaults to the default country like its siblings, so every
  // existing single-country construction is unchanged. Unlike `normalizer` a
  // wrong value here mis-SPELLS a badge rather than mis-keying a row, which is
  // why this one may default at all.
  screeningTokens: ScreeningTokens = ScreeningTokens.Default,
  // The country's title rules. Sibling of `enrichmentLanguage` above and wired
  // from the same `country` by the worker; both default to Poland so existing
  // single-country constructions are unchanged. Every `CacheKey` this cache
  // builds — including the ones built on the change-stream driver thread and in
  // the rehydrate scheduler — keys through THIS instance, which is what makes
  // the cache's identity country-correct rather than process-global. REQUIRED,
  // not defaulted: this is the production cache, and every CacheKey it builds
  // is a row identity.
  override val normalizer: TitleNormalizer,
  // How many consecutive thin ticks `ScrapeLanding`'s depth guard holds a venue
  // before accepting a degraded listing, forwarded verbatim to it. Defaults from
  // THIS deployment's own scrape cadence (`KINOWO_SCRAPE_FRESHNESS_MINUTES`, read
  // once here rather than by `ScrapeLanding` itself so the guard's pure functions
  // stay pure) — see `ScrapeHealth.maxRejectionsFor` for why a flat "3" isn't safe
  // for the slower-cadence countries.
  maxConsecutiveDepthRejections: Int = ScrapeHealth.maxRejectionsFor(services.freshness.Freshness.defaultScrapeTtl)
) extends MovieCache with LandingStore with Stoppable with Logging {

  // Supplies `CacheKey.apply` throughout this class, so a key can never be built
  // here under another country's rules.

  // Films skipped this process's lifetime because their stored row could not be read.
  // Exposed for tests + diagnostics: a non-zero value means scrapes are landing against
  // an unreadable corpus, which is the state that used to silently prune boards.
  private[services] val skippedUnreadable = new java.util.concurrent.atomic.AtomicLong(0)
  // Writes refused because a different film already holds the key (two films, one
  // title and year). Exposed for tests + diagnostics.
  private[services] val keyCollisions = new java.util.concurrent.atomic.AtomicLong(0)

  // `recordStats` so the resident corpus can report its hit ratio — a read served
  // here is a Mongo read not made. Unbounded, so its eviction count stays 0 by
  // construction rather than by luck.
  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().recordStats().build()

  /** The derived views of `positive` that `recordCinemaScrape` needs, kept current as
   *  rows are written rather than rebuilt per venue — see [[CorpusIndex]] for the
   *  quadratic that cost the United States leg every run it ever had.
   *
   *  It shadows `positive`, so it is only as correct as the funnels below: EVERY write
   *  to `positive` goes through `store` / `evict` / the `putIfPresent` compute, and
   *  nothing else may call `positive.put` or `positive.invalidate` directly. The cache
   *  is unbounded, so there is no eviction path to miss. */
  /** What makes a row a valid ALIAS target: resolved, and a bare presentation of its
   *  film rather than a decorated edition of it.
   *
   *  Named once because the index is built twice — live, and rebuilt from the rows by
   *  [[rebuiltIndexSnapshot]] for the consistency check. Two copies of a predicate that
   *  MUST agree is how the check starts comparing an index against a differently-defined
   *  one and calls the disagreement drift. */
  private val isConcludedBareRow: (CacheKey, MovieRecord) => Boolean =
    (k, r) => r.tmdbConcluded && FilmCanonicalizer.isBareFilmTitle((k, r), normalizer)

  private[movies] val corpusIndex: CorpusIndex =
    new CorpusIndex(normalizer, isConcludedBareRow)

  /** Write a row and keep the index with it. The ONLY way into `positive`. */
  private def store(key: CacheKey, record: MovieRecord, id: FilmId): Unit = {
    positive.put(key, record)
    corpusIndex.put(key, record, id)
  }

  /** The permanent id behind `key`: the one the index holds, else the stored row's, else
   *  a fresh one for a row this cache is about to create. Ids are never re-derived from
   *  a key — see [[FilmId]]. */
  private def idFor(key: CacheKey): Option[FilmId] =
    corpusIndex.idOf(key).orElse(repository.findByKeyChecked(key) match {
      case (Some(row), _) => Some(row.id)
      case (None, true)   => Some(FilmId.fresh(key, corpusIndex.holdsId))
      // The store could not say whether a document holds this key. Minting an id
      // here would write a SECOND document for the key once the store recovers
      // (a failed read is not "absent") — the caller defers instead.
      case (None, false)  => None
    })

  /** The id of a RESIDENT row. Every write into `positive` goes through `store`, which
   *  indexes the id, so a resident row without one is a broken funnel — say so, rather
   *  than mint an id for a row that has a document. */
  private def residentIdOf(key: CacheKey): FilmId =
    corpusIndex.idOf(key).getOrElse(throw new IllegalStateException(s"resident row '${key.cleanTitle}' (${key.year.getOrElse("—")}) has no film id"))

  private def deferUnreadable(what: String, key: CacheKey): Unit = {
    logger.warn(s"Deferring $what of '${key.cleanTitle}' (${key.year.getOrElse("—")}): the store could not say " +
      "whether a document already holds the key, and writing would risk a second one.")
    skippedUnreadable.incrementAndGet(); ()
  }

  private[services] def idOf(key: CacheKey): Option[FilmId] = corpusIndex.idOf(key)

  /** Drop a row and keep the index with it. The ONLY way out of `positive`. */
  private def evict(key: CacheKey): Unit = {
    positive.invalidate(key)
    corpusIndex.remove(key)
  }

  /** What the index currently believes, and what the rows actually say.
   *
   *  The pair exists for [[CorpusIndexConsistencySpec]], which replays a realistic
   *  scrape/fold/prune/rekey sequence and asserts they stay equal. A funnel that
   *  stopped updating the index would otherwise fail SILENTLY and far away — as a film
   *  re-diverting into staging every tick, which is the exact flap the widened divert
   *  gate was built to stop. */
  private[movies] def indexSnapshot: CorpusIndex.Snapshot = corpusIndex.snapshot

  private[movies] def rowsRebuiltIndexSnapshot: CorpusIndex.Snapshot = {
    import scala.jdk.CollectionConverters._
    val rebuilt = new CorpusIndex(normalizer, isConcludedBareRow)
    positive.asMap().asScala.foreach { case (k, r) => rebuilt.put(k, r, corpusIndex.idOf(k).getOrElse(FilmId.legacy(k))) }
    rebuilt.snapshot
  }
  /** The resident corpus, for `kinowo_worker_cache_*`. UNBOUNDED by design — it is
   *  the hydrated corpus, not a working set — so it reports entries and no maximum:
   *  a maximum of zero would render as "full" on a ratio panel. What it is worth
   *  watching for is the SHAPE, a count that tracks the corpus rather than climbing
   *  past it. */
  def occupancy: services.metrics.CacheOccupancy =
    services.metrics.CacheOccupancy.of(positive, weighted = false)

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
    CacheKey(title, year, normalizer)

  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

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
  private[services] def put(key: CacheKey, e: MovieRecord): Unit =
    idFor(key).fold(deferUnreadable("write", key))(putAs(key, e, _))

  /** [[put]] for a caller that holds the row's id — a retitle, where the index no
   *  longer maps the new key and a lookup would mint a fresh id for a film that has one. */
  private def putAs(key: CacheKey, e: MovieRecord, id: FilmId): Unit = e.tmdbId match {
    case Some(tid) =>
      tmdbLockFor(tid).synchronized {
        siblingKeyByTmdb(tid, excluding = key) match {
          case Some(siblingKey) => foldDeterministically(key, e, id, siblingKey, MergeReason.TmdbIdentity)
          case None =>
            // The settle's imdbId edge, at write time: the same IMDb id under another
            // tmdbId is one film TMDB holds twice — unless the cinemas describe two
            // films, the edge's own veto. The sibling's tmdbId is locked too, in id
            // order, so a concurrent write under it cannot race this fold.
            siblingKeyByImdb(e, excluding = key) match {
              case Some(siblingKey) =>
                val siblingTmdb = get(siblingKey).flatMap(_.tmdbId).getOrElse(tid)
                withTmdbLocks(Seq(tid, siblingTmdb).filter(_ != tid)) {
                  foldDeterministically(key, e, id, siblingKey, MergeReason.ImdbIdentity)
                }
              case None => persist(key, e, id)
            }
        }
      }
    case None =>
      persist(key, e, id)
  }

  /** Nested per-tmdbId locks in ascending id order (the caller already holds the
   *  incoming row's), so two writers folding the same pair cannot deadlock. */
  private def withTmdbLocks[A](ids: Seq[Int])(body: => A): A = ids.sorted match {
    case Seq()        => body
    case head +: rest => tmdbLockFor(head).synchronized(withTmdbLocks(rest)(body))
  }

  // Strip only when the read-split is active (showtimes live in `screenings`); without it
  // the cache must keep showtimes — there's nowhere else to hold them.
  private def forCache(r: MovieRecord): MovieRecord =
    if (repository.hasScreenings) ShowtimesDigest.stripForCache(r) else r

  private def persist(key: CacheKey, e: MovieRecord): Unit =
    idFor(key).fold(deferUnreadable("write", key))(persist(key, e, _))

  private def persist(key: CacheKey, e: MovieRecord, id: FilmId): Unit = corpusIndex.idOf(key).filter(_ != id) match {
    case Some(holder) =>
      // A DIFFERENT film already answers to this key (the same-film cases were folded
      // by `putAs` before this point): two films sharing a title and a year. Writing
      // would put a second document under one key and drop the holder out of the
      // cache — the property spec's first find. The row stays where it is instead.
      logger.warn(s"Refusing to write '${key.cleanTitle}' (${key.year.getOrElse("—")}) as $id: another film " +
        s"($holder) holds that key. The row keeps its current key.")
      keyCollisions.incrementAndGet(); ()
    case None =>
      val clean = withoutZeroRatings(e)
      store(key, forCache(clean), id)
      // `clean` may carry stripped slots (folds/canonicalize read from the stripped cache);
      // `upsert` re-stitches those from the film's screenings so a full write never deletes them.
      repository.upsert(id, key, clean)
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
  private def siblingKeyByTmdb(tid: Int, excluding: CacheKey): Option[CacheKey] =
    (corpusIndex.keysWithTmdbId(tid) - excluding).minByOption(canonicalRank)

  /** A resolved row under a DIFFERENT tmdbId that carries this record's imdbId, and
   *  whose cinemas do not describe a different film — the row the settle's imdbId edge
   *  would union this one with. */
  private def siblingKeyByImdb(e: MovieRecord, excluding: CacheKey): Option[CacheKey] =
    e.imdbId.flatMap { imdb =>
      (corpusIndex.keysWithImdbId(imdb) - excluding).iterator
        .filter { k => get(k).exists(v => v.tmdbId.isDefined && v.tmdbId != e.tmdbId &&
                                            !MixedFilmDetector.describeDifferentFilms(v, e, normalizer)) }
        .minByOption(canonicalRank)
    }

  /** Total order picking the canonical (surviving) key among same-tmdbId,
   *  same-normalised-title rows — see `FilmCanonicalizer.canonicalRank` for the
   *  rule. Delegates so there is ONE definition shared with the pure
   *  canonicaliser. */
  private def canonicalRank(k: CacheKey): (Boolean, Int, String) =
    FilmCanonicalizer.canonicalRank(k)

  private[services] def canonicalKeyFor(key: CacheKey): Option[CacheKey] = {
    // The index's per-title map, not a walk of the whole cache: this runs up to four
    // times per listing resolved, against a corpus of thousands.
    val sameTitle = corpusIndex.entriesFor(key.normalized).map(_._1)
    // Prefer a row at this EXACT year. A same-title row at a DIFFERENT year is a
    // distinct film — a remake or re-release carrying the original's name
    // ("Zaproszenie" 2022 "The Invitation" vs 2026 "The Invite", "Diuna" 1984 vs
    // 2021) — not a stale-key alias of this one, so a resolve/read must never
    // redirect onto it (year-blind `minByOption` clobbered the lowest-year row).
    // The year-blind fallback still fires when NO exact-year row exists, which is
    // the only shape the genuine redirect needs: `recordCinemaScrape`'s rekeys
    // change spelling at the SAME year (case/separator) and a yearless key whose
    // row gained a resolved year both reach their row through it.
    //
    // `canonicalRank` alone ties when two DIFFERENT films share this exact title
    // AND year (`StagingFold.resolveKeyCollisions`'s disambiguated pair) — both
    // have the same `cleanTitle` and `year`, so the only thing left to break the
    // tie deterministically is `normalized`, which the plain (unsuffixed) row
    // always sorts before a disambiguated one. This has no runtime/venue evidence
    // to go on (unlike `ScrapeLanding.chooseConcluded`), so it does not always pick
    // the "right" film for an ambiguous caller — it only guarantees the pick is the
    // SAME one every time, which is what this method's callers (redirect-after-
    // rekey, not scrape-time landing) need.
    def rank(k: CacheKey) = (canonicalRank(k), k.normalized)
    sameTitle.filter(_.year == key.year).minByOption(rank)
      .orElse(sameTitle.minByOption(rank))
  }

  /** Collapse every set of rows that are the SAME FILM into ONE row under the
   *  canonical key, unioning their records. Film identity is `groupByFilm`: rows
   *  sharing a normalised cleanTitle, OR (both bare film titles) a tmdbId — so a
   *  film keyed under two languages ("Tangled" + "Zaplątani", same tmdbId) folds
   *  to one row, while a decorated edition that merely carries the base tmdbId
   *  stays separate. A concurrent scrape/enrichment can transiently split a film
   *  across two spellings — a stale-keyed TMDB write seeds a phantom ("Nowa fala"
   *  beside the canonical "Nowa Fala"), and once two same-sanitize rows exist
   *  `ScrapeLanding.redirectToExistingVariant` stops merging (it only redirects on a UNIQUE
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
            }, RekeyReason.EmbeddedYear)
          }
        }.isDefined
    }.count(identity)
    // Always settle: fold the re-keyed rows' ±year / distinct-tmdbId clusters, and
    // (when nothing moved) keep the plain settle semantics callers rely on.
    canonicalizeBySanitize()
    moved
  }

  private def canonicalizeGroups(pairs: Seq[(CacheKey, MovieRecord)]): Unit =
    FilmCanonicalizer.groupByFilm(pairs, normalizer)
      .foreach(component => FilmCanonicalizer.clusterByFilm(component, normalizer).foreach(collapseCluster))

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
    val (canonical, merged) = FilmCanonicalizer.canonical(cluster, normalizer)
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
    val canonicalSanitized = canonical.normalized
    val needsFix = keys.sizeIs > 1 ||
      keys.exists(k => k.normalized == canonicalSanitized &&
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
        // The SURVIVOR is an existing row — the one already at the canonical key, else the
        // best-ranked member — and the film keeps its id: a canonical spelling or year no
        // row holds yet is a RETITLE of that member, not a new document (see `FilmId`).
        // Every other member is a victim: going away, so carry its cinemas onto the
        // survivor first — same rule as the fold and the re-key. Only a victim whose rows
        // ACTUALLY reached the survivor may then be deleted: `moveFilm` reports false when a
        // read or write it depended on didn't happen, and deleting on that basis destroys
        // the film's only copy. A victim left behind is a duplicate row the next pass folds
        // again (and `scripts.ReapOrphanedFilmRows` clears), which is the recoverable
        // direction.
        val members     = keys.map(k => k -> residentIdOf(k))
        val survivorId  = FilmCanonicalizer.survivor(members, canonical).get   // members is non-empty
        val survivorKey = members.collectFirst { case (k, id) if id == survivorId => k }.get
        // The canonical key may already belong to a film OUTSIDE this cluster — two films
        // sharing a title and a year, clustered apart by tmdbId. Re-keying onto it would put
        // two documents under one key and drop this cluster's rows out of the cache
        // (found by `FilmIdentityInvariantsSpec`). The cluster keeps its keys instead.
        // The canonical key may belong to a film OUTSIDE this cluster — two films sharing a
        // title and a year, clustered apart by tmdbId. The cluster still collapses (its rows
        // are one film), but onto the survivor's CURRENT key, never by taking another
        // film's: that put two documents under one key and dropped the cluster's rows out
        // of the cache (found by `FilmIdentityInvariantsSpec`).
        val target = corpusIndex.idOf(canonical).filterNot(id => members.exists(_._2 == id)) match {
          case Some(holder) =>
            logger.info(s"canonicalize '${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}): another film " +
              s"($holder) holds the canonical key; the cluster collapses onto '${survivorKey.cleanTitle}' " +
              s"(${survivorKey.year.getOrElse("—")}) instead.")
            keyCollisions.incrementAndGet()
            survivorKey
          case None => canonical
        }
        locally {
            val victims     = keys.filterNot(_ == survivorKey)
            val (moved, stranded) = victims.partition(v =>
              repository.moveFilm(residentIdOf(v), survivorId))
            if (stranded.nonEmpty)
              logger.warn(s"canonicalize '${canonical.cleanTitle}': keeping ${stranded.size} row(s) whose " +
                "cinemas could not be carried onto the winner — they fold again on the next pass.")
            moved.foreach(invalidate)
            evict(survivorKey)
            putAs(target, merged, survivorId)
        }
      }
      // Victims = every other row in the cluster folded away; a lone respelled
      // key (keys.size == 1) is a re-key, not a merge, so it counts 0.
      if (keys.sizeIs > 1) mergeMetrics.recordMerge(MergeReason.Canonicalize, keys.size - 1)
      else mergeMetrics.recordRekey(RekeyReason.Canonicalize)
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
    // A queued re-resolve is not short-circuited by the miss it was queued to
    // overcome: the row's `tmdbAttempt` fingerprints the inputs the miss was reached
    // on, and an input that earns a ResolveTmdb here — a Filmweb-discovered
    // `originalTitle`, a director — is exactly what changes that fingerprint.
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
        val (row, readOk) = findStoredChecked(key)
        (row.map(_.record), readOk)
    }

  // Straight to the repository, never `get`: the resident copy is the stripped one,
  // and stripped is exactly what this read exists to avoid (see the trait's note).
  private[services] def restitchedChecked(key: CacheKey): (Option[MovieRecord], Boolean) = {
    val (row, readOk) = findStoredChecked(key)
    (row.map(_.record), readOk)
  }

  /** The stored row behind `key` — by id when the index knows it, by key otherwise. */
  private def findStoredChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) =
    corpusIndex.idOf(key).fold(repository.findByKeyChecked(key))(repository.findByIdChecked)

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
      val norm        = oldKey.normalized
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
        k.normalized == norm
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
      if (target != oldKey) mergeMetrics.recordRekey(RekeyReason.ResolvedYear)
      target
    }

  /** Collapse two same-tmdbId rows into one. The surviving key is chosen by
   *  `canonicalRank` (NOT arrival order), and the record is the union of every
   *  per-source slot — so no scraped data is lost whichever key wins, and the
   *  displayed title/year/ratings are derived from that union at read time.
   *  The stored result is therefore identical no matter which row was written
   *  first; enrichment-thread arrival order (which varies across machines, and
   *  used to flip the canonical here, drifting the whole-corpus snapshot
   *  between arm64 dev boxes and amd64 CI) no longer matters. */
  private def foldDeterministically(newKey: CacheKey, newRecord: MovieRecord, newId: FilmId, siblingKey: CacheKey, reason: MergeReason): Unit = {
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
    // (matched by sanitize via `ScrapeLanding.concludedKeyFor`) wouldn't find it and would
    // re-spawn the duplicate. One rule for both fold paths keeps the stored
    // result a pure function of the row set, not arrival order.
    val (canonical, merged) = FilmCanonicalizer.canonical(Seq(siblingKey -> siblingRecord, newKey -> newRecord), normalizer)
    // The sibling is the film's EXISTING row, so the film keeps the sibling's id whichever
    // key wins: a canonical key the sibling does not hold is a retitle of the sibling. A
    // stored row of its own under the incoming key is the victim — carry its screenings +
    // slots onto the survivor BEFORE anything is written or deleted. A merge is a rename
    // too: the losing row's showtimes are filed under ITS id, while the record we are about
    // to write holds that row STRIPPED (cache residency), so `upsert`'s re-stitch — which
    // looks under the id it is WRITING to — would find nothing and store nothing, and the
    // delete would then destroy the only copy. Same rule as the merge arm of
    // `MovieCache.rekey`; a `movies` row disappearing almost never means the film left.
    // Only a victim whose rows actually reached the survivor is deleted — `moveFilm`
    // reports false when a read or write it depended on didn't happen, and the delete
    // would otherwise destroy the film's only copy. A stranded victim stays a duplicate
    // row that folds again next pass, which is the recoverable direction.
    //
    // The victim is deleted BY ID, never through `invalidate(newKey)`: when the incoming
    // key is the canonical one, the survivor is about to be stored under it, and a
    // key-addressed delete after the write would take the survivor with it.
    // The incoming row's own id is a victim too whenever it is not the survivor: it may
    // have a document (a retitle arriving here, a cold key `idFor` found in the store)
    // whose side rows must reach the survivor — a brand-new id simply has nothing to move.
    val survivorId = residentIdOf(siblingKey)
    // The canonical key may belong to a THIRD film (two films sharing a title and a
    // year, clustered apart by tmdbId). The merge still happens — these two rows are one
    // film — but under the sibling's current key, never by taking another film's.
    val target =
      if (corpusIndex.idOf(canonical).exists(id => id != survivorId && id != newId)) { keyCollisions.incrementAndGet(); siblingKey }
      else canonical
    val victimIds  = (Seq(newId) ++ corpusIndex.idOf(newKey)).distinct.filter(_ != survivorId)
    val (moved, stranded) = victimIds.partition(repository.moveFilm(_, survivorId))
    if (stranded.nonEmpty) {
      // A move that did not land defers the WHOLE fold, as `rekey` does: writing the
      // survivor under the incoming key would drop the stranded document out of the
      // index while it still holds the key — two documents, one key, and nothing left
      // to fold it on the next pass.
      logger.warn(s"Deferring fold of '${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}) into " +
        s"'${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}): its screenings/slots could not be carried " +
        "onto the survivor; the rows stay as they are and the settle asks again.")
      skippedUnreadable.incrementAndGet()
      return
    }
    moved.foreach(repository.delete)
    if (corpusIndex.idOf(newKey).exists(moved.contains)) evict(newKey)
    if (target != siblingKey) evict(siblingKey)
    persist(target, merged, survivorId)
    // The merge may have filled enrichment inputs the canonical lacked (e.g. an
    // imdbId/searchTitle from the victim) — re-kick the affected enrichments.
    retriggerChangedEnrichments(siblingRecord, siblingKey, merged, target)
    // Counted whether the incoming key had a stored row of its own or was a fresh write
    // that never became one: either way a would-be duplicate was folded at write time.
    if (newKey != siblingKey) {
      mergeMetrics.recordMerge(reason, 1)
      val shared = if (reason == MergeReason.ImdbIdentity) s"same imdbId=${newRecord.imdbId.getOrElse("?")}, tmdbId ${merged.tmdbId.getOrElse("?")} kept"
                   else s"same tmdbId=${newRecord.tmdbId.get}"
      logger.info(s"Folded duplicate '${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}) " +
                  s"into '${canonical.cleanTitle}' (${canonical.year.getOrElse("—")}) — $shared" +
                  (if (moved.nonEmpty) ", its stored row retired." else "."))
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
    // `computeIfPresent` writes inside Caffeine's own lock, so it cannot go through
    // `store`; index the value it produced instead. Same contract, one line later.
    val id = residentIdOf(key)
    corpusIndex.put(key, updated, id)
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
      repository.updateIfPresent(id, key, prior, fullAfter)
      touch()
      true
    }
    }
  }

  /** Drop a row from positive cache + Mongo — used by the TMDB stage to clear
   *  a stale row before re-keying it under a corrected (title, year). */
  private[services] def invalidate(key: CacheKey): Unit = {
    val id = corpusIndex.idOf(key)
    evict(key)
    id.orElse(repository.findByKeyChecked(key)._1.map(_.id)).foreach(repository.delete)
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
  private[services] def rekey(oldKey: CacheKey, newKey: CacheKey, update: MovieRecord => MovieRecord, reason: RekeyReason): Unit = {
    require(oldKey.normalized == newKey.normalized,
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
      val (storedRow, readOk) = get(oldKey) match {
        case Some(resident) => (Some(StoredMovieRecord(oldKey.cleanTitle, oldKey.year, resident, residentIdOf(oldKey))), true)
        case None           => findStoredChecked(oldKey)
      }
      (storedRow.map(_.record), readOk) match {
        case (_, false) =>
          logger.warn(s"Deferring re-key '${oldKey.cleanTitle}' (${oldKey.year.getOrElse("—")}) → " +
            s"'${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}): the stored row could not be READ, " +
            "and re-keying a row we cannot see would write it back with neither ratings nor cinemas.")
          skippedUnreadable.incrementAndGet()
          ()
        case (row, true) =>
          val updated = update(row.getOrElse(MovieRecord()))
          // A re-key is a RETITLE: the film keeps its id, so its screenings, slots and
          // read-model rows stay where they are and only the document's `key` moves
          // (see `FilmId` for what the id-as-key era cost here). The one case that is
          // still a merge is a DIFFERENT film already holding `newKey` — then this row
          // folds into it the way every same-film duplicate does, through `put`'s
          // identity gate, and the loser's side rows are carried across first.
          val id = corpusIndex.idOf(oldKey).orElse(storedRow.map(_.id)).getOrElse(FilmId.fresh(oldKey, corpusIndex.holdsId))
          corpusIndex.idOf(newKey).filter(_ != id) match {
            case Some(holder) if oldKey != newKey =>
              if (repository.moveFilm(id, holder)) {
                // A MERGE, not a retitle: the holder keeps its id, and the two records are
                // merged the way every fold merges — `canonical` picks the union base by
                // runtime corroboration when the tmdbIds differ — never one written over
                // the other. (The holder's record was replaced, silently, before 2026-09-07.)
                val merged = get(newKey).fold(updated)(holderRecord =>
                  FilmCanonicalizer.canonical(Seq(newKey -> holderRecord, oldKey -> updated), normalizer)._2)
                invalidate(oldKey)
                mergeMetrics.recordMerge(MergeReason.Canonicalize, 1)
                putAs(newKey, merged, holder)
              } else {
                logger.warn(s"Deferring re-key '${oldKey.cleanTitle}' (${oldKey.year.getOrElse("—")}) → " +
                  s"'${newKey.cleanTitle}' (${newKey.year.getOrElse("—")}): another row holds the new key and " +
                  "this one's screenings/slots could not be carried onto it.")
                skippedUnreadable.incrementAndGet()
              }
            case _ =>
              if (oldKey != newKey) {
                evict(oldKey)
                mergeMetrics.recordRekey(reason)
                logger.info(s"retitle ${StoredMovieRecord.keyFor(oldKey)} -> ${StoredMovieRecord.keyFor(newKey)} ($id, $reason)")
              }
              putAs(newKey, updated, id)
          }
      }
    }
  }

  /** Scrape-time landing — [[ScrapeLanding]] owns the whole path; this cache is its
   *  [[LandingStore]]. Constructed here, on `this`, because the store IS this cache;
   *  the landing reads nothing from it until the first scrape, so the not-yet-built
   *  `this` it receives is never observed. */
  private val landing = new ScrapeLanding(this, repository, staging, bus, screeningTokens, enrichmentLanguage,
    maxConsecutiveDepthRejections)
  /** [[LandingStore]]: how many rows are resident — zero is the cold mirror the
   *  landing's first scrape guards against. */
  private[services] def residentCount: Long = positive.estimatedSize()

  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie],
                         listingIsComplete: Boolean = true): Seq[(CinemaMovie, CacheKey, Boolean)] =
    landing.recordCinemaScrape(cinema, movies, listingIsComplete)

  def hasResolvedSiblingByTitle(rawTitle: String): Boolean =
    corpusIndex.entriesFor(normalizer.sanitize(rawTitle)).exists { case (_, e) => e.tmdbId.isDefined }

  def snapshot(): Seq[StoredMovieRecord] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => StoredMovieRecord(k.cleanTitle, k.year, e, corpusIndex.idOf(k).getOrElse(FilmId.legacy(k)), Some(StoredMovieRecord.keyFor(k))) }
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
    val byKey: Map[CacheKey, Seq[StoredMovieRecord]] =
      rows.groupBy(_.cacheKey(normalizer))
    // Count what this backstop reload catches that the incremental change stream missed:
    // a put whose cached value DIFFERED (a missed upsert) and a key no longer in Mongo (a
    // missed delete). After resume tokens + delete-apply these should be ~0 in steady state
    // — the signal (kinowo_worker_cache_rehydrate_changes) that the rehydrate is redundant.
    var changed = 0
    byKey.foreach { case (k, rs) =>
      val record = MovieRecordMerge.unionAll(rs.map(_.record))
      if (!Option(positive.getIfPresent(k)).exists(ShowtimesDigest.leanEqual(_, record))) changed += 1
      // Several documents under one key are the same film twice; the lowest id is the
      // survivor, deterministically, and the others are reconciled below.
      store(k, forCache(record), rs.map(_.id).minBy(_.value))
    }
    val removed = positive.asMap().keySet().asScala.toSeq.filterNot(byKey.keySet.contains)
    removed.foreach(evict)
    cacheMetrics.recordRehydrate(changed, removed.size)
    if (changed > 0 || removed.nonEmpty)
      logger.info(s"MovieCache rehydrate: caught $changed changed row(s) + ${removed.size} orphan-delete(s) " +
        "the change stream missed.")
    // Reconcile duplicate documents. Two stored rows that collapse onto ONE CacheKey
    // here (a merge-key rule added after they were written; a row whose display title
    // drifted onto another row's key) are the same film twice in `movies`, and the
    // cross-title settle never sees them — they are one entry in this cache. `byKey`
    // already unioned them; carry the losers' cinemas onto the survivor, rewrite it
    // whole, and delete the loser documents. Gated on a duplicate existing, so a clean
    // corpus writes nothing.
    val duplicates = byKey.collect { case (k, rs) if rs.sizeIs > 1 => k -> rs.map(_.id).sortBy(_.value) }
    if (duplicates.nonEmpty) {
      duplicates.foreach { case (k, ids) =>
        val survivor = ids.head
        val (moved, stranded) = ids.tail.partition(repository.moveFilm(_, survivor))
        Option(positive.getIfPresent(k)).foreach(repository.upsert(survivor, k, _))
        moved.foreach(repository.delete)
        if (stranded.nonEmpty)
          logger.warn(s"MovieCache rehydrate: kept ${stranded.size} duplicate document(s) of '${k.cleanTitle}' whose " +
            "cinemas could not be carried onto the survivor — reconciled again next pass.")
      }
      logger.info(s"MovieCache rehydrate: reconciled ${duplicates.size} key(s) stored as several `movies` documents.")
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
    val key = r.cacheKey(normalizer)
    // A retitle arriving from another writer: the id's previous key entry is stale.
    corpusIndex.keyOf(r.id).filter(_ != key).foreach(evict)
    store(key, forCache(r.record), r.id)
    touch()
  }

  /** Apply an out-of-band DELETE from the change stream: drop the mirrored row whose
   *  source `_id` was removed (a fold/merge loser, an `UnscreenedCleanup` removal, a
   *  re-key's old id). The stream carries only the `_id`, so map it back to the CacheKey
   *  the row was stored under via `idFor`. Applying deletes incrementally is what lets the
   *  backstop rehydrate stop being the ONLY thing that catches them; a rare drifted /
   *  duplicate key that doesn't map is still swept by the backstop. */
  private[services] def applyDelete(id: FilmId): Unit = {
    corpusIndex.keyOf(id)
      .foreach { k =>
        evict(k)
        // An out-of-band Mongo delete arriving via the change stream — the mirror
        // of a fold/UnscreenedCleanup/re-key removal another writer made. The only
        // signal that a cache row vanished for a reason NOT originating on this node.
        RemovalAudit.filmRemoved("cache.applyDelete", id.value, reason = "change-stream-delete")
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
