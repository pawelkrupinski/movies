package services.movies

import java.util.Locale

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{Cinema, CinemaMovie, MovieRecord, Source, SourceData}
import play.api.Logging
import services.Stoppable
import settings.{BootHydrateMaxAttempts, BootHydrateRetryInterval, CacheRehydrateInterval}
import tools.DaemonExecutors

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.DurationInt
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

  /** The same title key at another year. Re-derive nothing from `cleanTitle`: on a
   *  key read from the store it is the row's DISPLAY title, which need not sanitize to
   *  the stored key's title ("La luz que imaginamos" labelling `la luz|2026`). */
  def atYear(other: Option[Int]): CacheKey = new CacheKey(cleanTitle, other, normalized)
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

/**
 * Caffeine-backed `MovieCache` with write-through to `MovieRepository`.
 *
 * Two caches:
 *   - **Positive**: successful enrichments, never expire in-process (they
 *     change slowly; restarts re-warm via `rehydrate`).
 *   - **Negative**: known misses (events, festivals, retrospectives that
 *     don't match a real film), 24h TTL — failed TMDB lookups get retried
 *     about once a day (a phase-spread reaper clearing
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
 * gate, `putIfPresent`), the per-title locks, the cache-or-store reads, and
 * hydration and the change-stream mirror. See `docs/stable-film-id.md`.
 */
class CaffeineMovieCache(
  repository: MovieRepository,
  // Boot-hydrate retry — OFF by default (0 attempts) so tests and a genuine
  // cold start pay nothing. Prod turns it on via the Fly env
  // `KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS` so a not-ready Mongo at boot can't leave
  // the cache empty (see `bootHydrate`).
  bootHydrateMaxAttempts: BootHydrateMaxAttempts   = BootHydrateMaxAttempts(0),
  bootHydrateRetry:       BootHydrateRetryInterval = BootHydrateRetryInterval(1.second),
  // Called after a merge whose inputs changed an enrichment's resolution, to
  // re-kick that enrichment as a worker task (per case). Default no-op for unit
  // tests + non-worker builds; the worker wires `QueueEnrichmentRetrigger`.
  retrigger: EnrichmentRetrigger = EnrichmentRetrigger.noop,
  // Measures what the periodic backstop rehydrate catches that the incremental change
  // stream missed — the redundancy signal for retiring the rehydrate. No-op for web/tests.
  cacheMetrics: CacheSyncMetrics = CacheSyncMetrics.noop,
  // The country's title rules, wired from the worker's `country`. Every `CacheKey` this cache
  // builds — including the ones built on the change-stream driver thread and in
  // the rehydrate scheduler — keys through THIS instance, which is what makes
  // the cache's identity country-correct rather than process-global. REQUIRED,
  // not defaulted: this is the production cache, and every CacheKey it builds
  // is a row identity.
  override val normalizer: TitleNormalizer,
  // The silent-write-skip counter (`putIfPresent`) — see `ListingIntakeMetrics`. No-op for
  // web/tests; the worker wires `WorkerTaskMetrics`.
  listingIntakeMetrics: ListingIntakeMetrics = ListingIntakeMetrics.noop,
  // Stamps `lastModified`. System time in production; specs pin or step it.
  val clock: java.time.Clock,
  // Where the cache interns the strings a slot repeats across cinemas. The worker hands every
  // country's cache the process's one pool (owned by `WorkerMetrics`, whose gauges read it); a
  // lone cache — tests included — gets its own.
  val stringPool: StringPool = new StringPool,
  // The backstop rehydrate (`KINOWO_CACHE_REHYDRATE_SECONDS`, resolved by the worker's root);
  // the compiled-in 6 hours for specs.
  rehydrateInterval: CacheRehydrateInterval = CacheRehydrateInterval(6.hours),
  // The boot's other whole-corpus readers, handed the boot hydrate's read so the boot reads
  // the corpus once (see [[BootCorpusReader]]). None for specs and a lone cache.
  bootReaders: Seq[BootCorpusReader] = Nil
) extends MovieCache with Stoppable with Logging {

  // `recordStats` so the resident corpus can report its hit ratio — a read served
  // here is a Mongo read not made. Unbounded, so its eviction count stays 0 by
  // construction rather than by luck.
  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().recordStats().build()

  // Writes deferred this process's lifetime because the store could not say whether a document
  // holds their key. Read by the specs: a non-zero value means scrapes are landing against an
  // unreadable corpus, which is the state that used to silently prune boards.
  private[services] val skippedUnreadable = new java.util.concurrent.atomic.AtomicLong(0)

  /** The film id behind each resident key, and back ([[CorpusIndex]]): what every write asks
   *  before it lands and every change-stream apply asks to find a film's key, without a store
   *  round-trip.
   *
   *  It shadows `positive`, so it is only as correct as the funnels below: EVERY write
   *  to `positive` goes through `store` / `evict` / the `computeResident` compute, and
   *  nothing else may call `positive.put` or `positive.invalidate` directly. The same
   *  funnels `announce` each change to the resident listeners (`onResident`); a rollback
   *  that puts a row back by hand announces it itself. The cache
   *  is unbounded, so there is no eviction path to miss. */
  private[movies] val corpusIndex: CorpusIndex =
    new CorpusIndex

  /** Write a row and keep the index with it. The ONLY way into `positive`. */
  private def store(key: CacheKey, record: MovieRecord, id: FilmId): Unit = {
    positive.put(key, record)
    corpusIndex.put(key, id)
    announce(key)
  }

  private val residentListeners = new java.util.concurrent.CopyOnWriteArrayList[(CacheKey, Option[StoredMovieRecord]) => Unit]()

  def onResident(listener: (CacheKey, Option[StoredMovieRecord]) => Unit): Unit = {
    residentListeners.add(listener)
    positive.asMap().keySet().forEach(key => announce(key, java.util.List.of(listener)))
  }

  /** Tell the resident listeners what `key` holds now. Read under the key's own lock, so two writes to one key cannot
   *  reach a listener in the opposite order; `positive` itself is left as it is. Every change to `positive` calls it
   *  once the change is made — `store` and `evict`, and the writes that cannot go through them. */
  private def announce(key: CacheKey, to: java.util.List[(CacheKey, Option[StoredMovieRecord]) => Unit] = residentListeners): Unit =
    if (!to.isEmpty) { positive.asMap().compute(key, (_, held) => {
      val film = Option(held).map(storedAt(key, _))
      // A listener that throws must not fail the write it is told of: the row is already held.
      to.forEach(listener => try listener(key, film) catch {
        case scala.util.control.NonFatal(e) => logger.warn(s"MovieCache: a resident listener failed on '${key.cleanTitle}': ${e.getMessage}")
      })
      held
    }); () }

  /** The film `key` holds as `record`, under its indexed id. */
  private def storedAt(key: CacheKey, record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(key.cleanTitle, key.year, record, corpusIndex.idOf(key).getOrElse(FilmId.legacy(key)), Some(StoredMovieRecord.keyFor(key)))

  /** The permanent id behind `key`: the one the index holds, else the stored row's, else
   *  a fresh one for a row this cache is about to create. Ids are never re-derived from
   *  a key — see [[FilmId]]. */
  private def idFor(key: CacheKey): Option[FilmId] =
    corpusIndex.idOf(key).orElse(repository.findByKeyChecked(key) match {
      case tools.ReadOutcome.Answered(row)      => Some(row.id)
      case tools.ReadOutcome.Absent(_) => Some(FilmId.fresh(key, corpusIndex.holdsId))
      // The store could not say whether a document holds this key. Minting an id
      // here would write a SECOND document for the key once the store recovers
      // (a failed read is not "absent") — the caller defers instead.
      case tools.ReadOutcome.Failed(_) => None
    })

  /** The id of a RESIDENT row. Every write into `positive` goes through `store`, which
   *  indexes the id, so a resident row without one is a broken funnel — say so, rather
   *  than mint an id for a row that has a document. */
  private def residentIdOf(key: CacheKey): FilmId =
    corpusIndex.idOf(key).getOrElse(throw new IllegalStateException(s"resident row '${key.cleanTitle}' (${key.year.getOrElse("—")}) has no film id"))

  private def deferUnreadableWrite(key: CacheKey): WriteOutcome = {
    logger.warn(s"Deferring write of '${key.cleanTitle}' (${key.year.getOrElse("—")}): the store could not say " +
      "whether a document already holds the key, and writing would risk a second one.")
    skippedUnreadable.incrementAndGet()
    WriteOutcome.Declined("key-unreadable")
  }

  private[services] def idOf(key: CacheKey): Option[FilmId] = corpusIndex.idOf(key)

  /** What the index currently believes, for `VenueSlotsEquivalenceSpec`: a venue apply must leave
   *  it exactly as a whole-film apply does. */
  private[services] def indexSnapshot: Map[CacheKey, FilmId] = corpusIndex.snapshot

  /** Drop a row and keep the index with it. The ONLY way out of `positive`. */
  private def evict(key: CacheKey): Unit = {
    positive.invalidate(key)
    corpusIndex.remove(key)
    announce(key)
  }

  /** The resident corpus, for `kinowo_worker_cache_*`. UNBOUNDED by design — it is
   *  the hydrated corpus, not a working set — so it reports entries and no maximum:
   *  a maximum of zero would render as "full" on a ratio panel. What it is worth
   *  watching for is the SHAPE, a count that tracks the corpus rather than climbing
   *  past it. */
  def occupancy: services.metrics.CacheOccupancy =
    services.metrics.CacheOccupancy.of(positive, weighted = false)

  // Read from the injected clock, like every other stamp this cache takes — strictly monotonic, so a
  // put in the same instant as the last one still moves it.
  private val _lastModified = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](clock.instant())
  def lastModified: java.time.Instant = _lastModified.get()
  private def touch(): Unit = { _lastModified.updateAndGet(tools.MonotonicStamp.after(_, clock)); () }

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

  // Hydrate from Mongo on construction. Synchronous: Wiring builds the cache
  // during `start()`, so the first HTTP request only lands after the initial
  // findAll has completed. Pages render against a fully-populated cache; no
  // first-request flicker, no scrape-vs-hydrate race.
  //
  // RETRY an empty result (prod only): the worker boots alongside its Mongo, so
  // an empty or failed read at boot is almost always "Mongo not ready yet" (`rehydrate`
  // loads nothing either way). Without retry the cache starts empty and the
  // change stream only ever delivers rows written AFTER boot — leaving every
  // quiescent row (one not re-scraped since) Mongo-only and invisible to the
  // in-memory fold / settle, so its duplicate sits stranded forever. Bounded, so
  // a genuinely empty corpus still proceeds after the attempts. Default 0
  // attempts = one plain hydrate (tests, cold start); prod sets the env.
  // Set once a rehydrate's corpus read has been answered whole — an empty corpus included — and
  // never cleared: from then on a failed read only leaves the cache a little stale. Declared
  // BEFORE the boot hydrate below, whose write a later initialiser would reset.
  @volatile private var wholeCorpusRead = false
  def hydrated: Boolean = wholeCorpusRead

  bootHydrate()

  private def bootHydrate(): Unit = {
    // The first complete, non-empty read goes to the boot readers once the cache holds it. An empty
    // answer is not offered, so it cannot pass for the corpus: at boot it is as likely a Mongo that
    // is not ready yet as an empty store, and the readers read again for themselves.
    var offered = false
    def offer(rows: Seq[StoredMovieRecord]): Unit = if (!offered && rows.nonEmpty) {
      offered = true
      bootReaders.foreach(_.bootCorpus(Some(rows)))
    }
    var attempt = 0
    while (rehydrateFrom(offer) == 0 && attempt < bootHydrateMaxAttempts.value) {
      attempt += 1
      Thread.sleep(bootHydrateRetry.value.toMillis) // an interrupt (shutdown mid-boot) ends the construction
    }
    if (!offered) bootReaders.foreach(_.bootCorpus(None))
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

  /** Persist a row at `key`, under the film id the key already has (the index's, else the stored
   *  document's, else a fresh one). Deferred when the store cannot say whether a document holds
   *  the key, and refused when another film does (`persist`). Which listings are one film is the
   *  identity projection's to decide ([[writeProjected]]); this write folds nothing. */
  private[services] def put(key: CacheKey, e: MovieRecord): WriteOutcome =
    idFor(key).fold(deferUnreadableWrite(key)) { id =>
      val outcome = persist(key, e, id)
      if (outcome == WriteOutcome.Written) changed(id)
      outcome
    }

  private val changeListeners = new java.util.concurrent.CopyOnWriteArrayList[FilmId => Unit]()
  def onChanged(listener: FilmId => Unit): Unit = { changeListeners.add(listener); () }
  private def changed(id: FilmId): Unit = changeListeners.forEach(_(id))

  // Strip only when the read-split is active (showtimes live in `screenings`); without it
  // the cache must keep showtimes — there's nowhere else to hold them.
  private def forCache(r: MovieRecord): MovieRecord = r.copy(data = r.data.view.mapValues(forCacheSlot).toMap)
  /** [[forCache]] of one slot — the slot itself when it is one already: stripped, every string the pool's. The identity
   *  projection writes the lean slots it keeps, so a film written again at the same venues hands the cache back the very
   *  objects it holds, and every comparison of the two stops at `eq` ([[forCacheOver]]). */
  private def forCacheSlot(sd: SourceData): SourceData = {
    val lean   = if (repository.hasScreenings) ShowtimesDigest.stripSlot(sd) else sd
    val pooled = stringPool.slot(lean)
    if (CaffeineMovieCache.pooledAlike(pooled, lean)) lean else pooled
  }

  /** [[forCache]] of `after` written over `before`, which the cache holds: a slot `after` holds as `before` does is
   *  kept as it is, not stripped and pooled again — on a film at thousands of venues, every write did that to all. */
  private def forCacheOver(before: MovieRecord, after: MovieRecord): MovieRecord =
    after.copy(data = after.data.map { case (source, sd) =>
      source -> (if (before.data.get(source).exists(_ eq sd)) sd else forCacheSlot(sd))
    })

  private def persist(key: CacheKey, e: MovieRecord, id: FilmId): WriteOutcome = corpusIndex.idOf(key).filter(_ != id) match {
    case Some(holder) =>
      // A DIFFERENT film already answers to this key: two films sharing a title and a
      // year. Writing would put a second document under one key and drop the holder out
      // of the cache — the property spec's first find. The row stays where it is instead.
      logger.warn(s"Refusing to write '${key.cleanTitle}' (${key.year.getOrElse("—")}) as $id: another film " +
        s"($holder) holds that key. The row keeps its current key.")
      WriteOutcome.Declined("key-held-by-another-film")
    case None => repository.writeFence.writing(id) {
      val clean  = withoutZeroRatings(e)
      val cached = forCache(clean)
      val prior  = Option(positive.getIfPresent(key))
      store(key, cached, id)
      // `clean` may carry stripped slots (folds/canonicalize read from the stripped cache);
      // `upsert` re-stitches those from the film's screenings so a full write never deletes them.
      val outcome = repository.upsert(id, key, clean)
      if (outcome.failed || outcome == WriteOutcome.IdentityHeld) {
        // …and a write that FAILED — or that `movies` DECLINED because another document holds
        // the key or tmdbId, which writes nothing just the same — leaves the cache as it was
        // before it. Kept, the unwritten row made every later identical scrape diff as a no-op
        // (`putIfPresent`'s write guard compares against it), so the write was never retried:
        // on 2026-09-24 a codec bug failed 34 upserts over ~6h and two new films never reached
        // the site until a restart. Rolled back, the next scrape finds the row as Mongo has it
        // — absent for a new film, so it `put`s again — and retries (a held identity is usually
        // a merge's losers, gone by then). Only if the entry is still the one written here: a
        // concurrent writer or an eviction may have replaced it since. A failure itself is
        // logged and counted by the repository.
        prior match {
          case Some(previous) => if (positive.asMap().replace(key, cached, previous)) corpusIndex.put(key, id)
          case None           => if (positive.asMap().remove(key, cached)) corpusIndex.remove(key)
        }
        announce(key)
      }
      touch()
      outcome
    }
  }

  // Rating sources occasionally hand us a literal zero — MC/RT search pages
  // that surface 0% for an unrated title, Filmweb's API for a film with no
  // votes yet, IMDb GraphQL for a brand-new entry. Zero isn't a real rating;
  // persisting it would render a misleading "0/10" badge. Squash to None at
  // the single write boundary so neither Caffeine nor Mongo holds the
  // phantom score. Applied to every write (`persist` and `putIfPresent`, which
  // `putSlotIfPresent` defers to for a row carrying one), so any future caller
  // automatically inherits the rule.
  private def withoutZeroRatings(e: MovieRecord): MovieRecord = e.copy(
    imdbRating     = e.imdbRating.filter(_ > 0.0),
    metascore      = e.metascore.filter(_ > 0),
    filmwebRating  = e.filmwebRating.filter(_ > 0.0),
    rottenTomatoes = e.rottenTomatoes.filter(_ > 0)
  )

  /** Would [[withoutZeroRatings]] change `e`? */
  private def carriesZeroRating(e: MovieRecord): Boolean =
    e.imdbRating.exists(_ <= 0.0) || e.metascore.exists(_ <= 0) ||
    e.filmwebRating.exists(_ <= 0.0) || e.rottenTomatoes.exists(_ <= 0)

  private[services] def writeProjected(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome =
    corpusIndex.idOf(key).filter(_ != id) match {
      case Some(_) => WriteOutcome.IdentityHeld
      case None => repository.writeFence.writing(id) {
        val clean   = withoutZeroRatings(e)
        val outcome = repository.upsert(id, key, clean)
        if (outcome == WriteOutcome.Written) {
          corpusIndex.keyOf(id).filter(_ != key).foreach(evict)
          store(key, forCache(clean), id)
          touch()
        }
        outcome
      }
    }

  private[services] def holdsSlotsLean: Boolean = repository.hasScreenings && repository.hasSlots

  private[services] def patchProjected(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord,
                                       only: Option[Set[Source]]): WriteOutcome = {
    // Only over the production storage split: a store holding showtimes inline in the record compares slots
    // showtime-blind, so a patch would not see a slot whose showtimes alone moved. And never a film changing TMDB id:
    // that write's order against the other films' is the unique index's, which the whole write is ordered by.
    val patched = repository.hasScreenings && repository.hasSlots && before.tmdbId == after.tmdbId && withTitleLock(key.cleanTitle) {
      corpusIndex.idOf(key).contains(id) && repository.writeFence.writing(id) {
        // Resident as the projection read it: what the store holds, so the patch from it is the whole change.
        (positive.getIfPresent(key) eq before) && {
          val clean = withoutZeroRatings(after)
          // Diffed only `only` when given: the sources that may differ, every other the same slot on both sides.
          repository.updateIfPresent(id, key, only.fold(before)(LeanRecords.only(before, _)), only.fold(clean)(LeanRecords.only(clean, _))) &&
            { store(key, forCacheOver(before, clean), id); touch(); true }
        }
      }
    }
    if (patched) WriteOutcome.Written else writeProjected(id, key, after)
  }

  private[services] def retireProjected(id: FilmId): WriteOutcome = {
    val outcome = repository.delete(id)
    if (outcome == WriteOutcome.Written) {
      corpusIndex.keyOf(id).foreach(evict)
      RemovalAudit.filmRemoved("identity.projection", id.value, reason = "retired-by-overlap")
      touch()
    }
    outcome
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
  private[services] def storedChecked(key: CacheKey): tools.ReadOutcome[MovieRecord] =
    get(key).fold(findStoredChecked(key).map(_.record))(tools.ReadOutcome.Answered(_))

  /** The stored row behind `key` — by id when the index knows it, by key otherwise. */
  private def findStoredChecked(key: CacheKey): tools.ReadOutcome[StoredMovieRecord] =
    corpusIndex.idOf(key).fold(repository.findByKeyChecked(key))(repository.findByIdChecked)

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
    withTitleLock(key.cleanTitle) { fencingResident(key) {
    // Capture both `before` and `after` inside the Caffeine compute lock so
    // the pair is atomic. The repository write below uses the pair to compute a
    // per-field diff — out-of-band Mongo edits to fields the updater didn't
    // touch (e.g. `FilmwebUrlAudit` clearing `filmwebUrl` while we're
    // bumping `filmwebRating`) survive the write.
    val before  = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
    val full    = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
    // Cache stores the STRIPPED record; the write below uses the FULL one so the
    // screenings-diff has real showtimes. `current` is the prior (stripped) resident row.
    val updated = computeResident(key) { current =>
      before.set(current)
      val u = updater(current)
      // An updater that hands the row back untouched changed nothing: keep the resident
      // value as it is, rather than stripping, re-indexing and diffing every slot of it
      // to find that out. The landing's duplicate-slot drop does this once per listing,
      // on a row that can carry thousands of venues. A zero rating still takes the long
      // way, because `withoutZeroRatings` would change it.
      if ((u eq current) && !carriesZeroRating(current)) current
      else {
        val f = withoutZeroRatings(u)
        full.set(f)
        forCache(f)
      }
    }
    updated.fold(false) { updated =>
      val prior = before.get()
      if (updated eq prior) true
      else {
        // `computeIfPresent` writes inside Caffeine's own lock, so it cannot go through
        // `store` — and need not: the key keeps the id the index holds for it.
        val id = residentIdOf(key)
        val fullAfter = full.get()
        // Write-guard by DIGEST: equal non-showtime fields AND equal per-slot showtime digest
        // ⇒ no real change, skip the write. See ShowtimesDigest.leanEqual.
        // Equal is the common case — an unchanged re-scrape re-asserting its slot — and it
        // skips the repository entirely: each no-op write would be an `updatedAt`-only
        // `updateOne`, an oplog entry and a change-stream `updateLookup` per row, per pass,
        // the dominant load on the shared-CPU Mongo.
        ShowtimesDigest.leanEqual(fullAfter, prior) || { val written = writeThrough(key, id, prior, updated, prior, fullAfter)
          if (written) changed(id)
          written }
      }
    }
    } }

  /**
   * [[putIfPresent]] of `_.copy(data = _.data + (source -> slot))` — the scrape landing's
   * write — in work independent of how many OTHER slots the row carries.
   *
   * THE BUG THIS REPLACES. The landing wrote through `putIfPresent`, which re-indexes the
   * whole row, strips every slot and compares every slot: O(slots) per landing. A film
   * shown at N venues lands N times a tick, so that was O(N²) per film per tick — a re-scrape
   * of the US corpus took ~12x its first scrape, and prod paid it on every landing of a
   * widely shown film. Here the no-op guard compares the ONE slot the write touches, the
   * cache strips that slot alone, and the repository is handed the two records narrowed to it — every diff the repository
   * makes is per source, so the narrowed pair yields exactly the patch the whole pair did.
   *
   * Identical results to `putIfPresent` by construction: the other slots and every
   * top-level field are the resident row's own on both sides of the diff. A row carrying a
   * zero rating, or a source that is not a cinema slot, takes `putIfPresent` itself —
   * those are the writes that change more than the slot.
   */
  private[services] def putSlotIfPresent(key: CacheKey, source: Source, slot: SourceData): Boolean =
    if (Source.cinemaOf(source).isEmpty || get(key).exists(carriesZeroRating))
      putIfPresent(key, current => current.copy(data = current.data + (source -> slot)))
    else withTitleLock(key.cleanTitle) { fencingResident(key) {
      val before  = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
      val cached  = forCacheSlot(slot)
      val updated = computeResident(key) { current =>
        before.set(current)
        // The write guard, asked of the one slot: `leanEqual` of the whole pair is exactly
        // this, since nothing else differs between them.
        if (current.data.get(source).exists(ShowtimesDigest.slotLeanEqual(_, slot))) current
        else current.copy(data = current.data.updated(source, cached))
      }
      updated.fold(false) { updated =>
        val prior = before.get()
        if (updated eq prior) true
        else {
          val id        = residentIdOf(key)
          val priorSlot = prior.data.get(source)
          def only(sd: Option[SourceData]) = prior.copy(data = sd.map(source -> _).toMap)
          writeThrough(key, id, prior, updated, only(priorSlot), only(Some(slot)))
        }
      }
    } }

  /** Run a local write of the row RESIDENT at `key` inside the repository's write fence, so a
   *  change-stream read taken before it cannot roll it back (see [[FilmWriteFence]]). A key
   *  with no resident row has nothing a read could roll back — and nothing to write. */
  private def fencingResident[A](key: CacheKey)(write: => A): A =
    corpusIndex.idOf(key).fold(write)(repository.writeFence.writing(_)(write))

  /** Caffeine's `computeIfPresent` on `key`: the value `f` produced, or `None` — metered —
   *  when the key had left the cache by the time it computed. */
  private def computeResident(key: CacheKey)(f: MovieRecord => MovieRecord): Option[MovieRecord] = {
    // Whether `f` handed back another row than the one held: a write that changes nothing is not announced, since the
    // landing hands every listing's unchanged row back, once per venue of a film shown at thousands.
    var moved = false
    val computed = Option(positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = { val next = f(current); moved = next ne current; next }
    }))
    if (moved) announce(key)
    computed.orElse {
      // The Caffeine-level race the since-deleted `ScrapeLanding`'s comment on `landed` named: a
      // concurrent `rekey` of some OTHER title invalidated this key between the
      // read and this compute. Recorded here, not at each caller, because this is
      // the one place that KNOWS it happened — every `putIfPresent` caller
      // (scrape, rating refresh, rekey) shares the same race.
      listingIntakeMetrics.recordWriteSkipped(ListingIntakeMetrics.SkipReason.CacheMissRace)
      None
    }
  }

  /** The repository half of [[putIfPresent]] / [[putSlotIfPresent]]: write the `before` →
   *  `after` diff, and on failure put the resident `prior` back in place of `updated`. */
  private def writeThrough(key: CacheKey, id: FilmId, prior: MovieRecord, updated: MovieRecord,
                           before: MovieRecord, after: MovieRecord): Boolean = {
    // ITS RESULT, not `true` — fixed 2026-09-13. This discarded
    // `repository.updateIfPresent`'s answer and reported success unconditionally,
    // so a genuine persistence failure (the Mongo document didn't match, or the
    // write threw and was caught into `false`) was invisible at every caller:
    // the index update already made the CACHE look correct, the since-deleted `ScrapeLanding`'s
    // `landed` gate — which existed precisely "to read the write", per its own
    // comment — saw `true` regardless, and the title was never spared from that
    // tick's prune nor counted anywhere. A row this happens to can look perfectly
    // healthy in-memory while Mongo silently never catches up.
    val wrote = repository.updateIfPresent(id, key, before, after)
    if (!wrote) {
      logger.warn(s"MovieCache.putIfPresent(${key.cleanTitle}, ${key.year.getOrElse("—")}): " +
        "the repository write reported failure for a row the cache still holds resident " +
        "— the Mongo document didn't match, or the write itself failed.")
      listingIntakeMetrics.recordWriteSkipped(ListingIntakeMetrics.SkipReason.RepositoryWriteFailed)
      // …and put the resident row BACK. The write guard diffs against it, so a
      // cache left holding the state Mongo never took would make the next identical
      // update (the next tick re-asserting the same slot) look like a no-op: skipped,
      // reported as success, the failure healed in memory and never in Mongo. `replace`
      // only if it is still ours — under the title lock nothing else wrote this key,
      // but an eviction may have.
      if (positive.asMap().replace(key, updated, prior)) { corpusIndex.put(key, id); announce(key) }
    }
    touch()
    wrote
  }

  /** Drop a row from positive cache + Mongo — used by the TMDB stage to clear
   *  a stale row before re-keying it under a corrected (title, year). */
  private[services] def invalidate(key: CacheKey): Unit = {
    val id = corpusIndex.idOf(key)
    evict(key)
    id.orElse(repository.findByKeyChecked(key).answered.map(_.id)).foreach(repository.delete)
    touch()
  }

  def snapshot(): Seq[StoredMovieRecord] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => storedAt(k, e) }
      .toSeq
      .sortBy(_.title.toLowerCase(Locale.ROOT))
  }

  /** Snapshot of (key, enrichment) pairs for the IMDb refresh loop. Copy so a
   *  concurrent `put` mid-iteration doesn't surprise the caller. */
  private[services] def entries: Seq[(CacheKey, MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }

  /** Put every stored row and evict the keys gone from the store; how many rows it put. An incomplete
   *  read (a page the scan could not read is skipped whole) changes nothing — no row put, none
   *  evicted: its missing films are not gone. The next backstop tick reads again. */
  def rehydrate(): Int = rehydrateFrom(_ => ())

  /** [[rehydrate]], handing a complete read to `onRead` once the cache holds it. */
  private def rehydrateFrom(onRead: Seq[StoredMovieRecord] => Unit): Int = {
    // Additive sync — never blank the cache mid-rehydrate. The backstop
    // tick (see `start()` below) runs while readers walk `snapshot()`;
    // an `invalidateAll()` window would briefly show them an empty corpus. Instead: put every Mongo row (cache's
    // copy gets overwritten if it changed), then evict only the keys
    // that disappeared from Mongo since the last sync.
    import scala.jdk.CollectionConverters._
    val findingAll    = tools.Stopwatch.start()
    // Marked BEFORE the read: a row this cache writes while `findAll` runs is newer than the
    // snapshot, and storing (or evicting) it from the snapshot would undo the write — see
    // [[FilmWriteFence]]. Such a row is left as the write made it; its own change-stream event,
    // or the next backstop, reconciles it.
    val marks         = repository.writeFence.markAll()
    val read          = repository.findAllChecked()
    val tFindAllMs    = findingAll.millis
    if (read.answered.isEmpty) {
      logger.warn(s"MovieCache rehydrate: the corpus read ${read.explain} (in ${tFindAllMs}ms) — " +
        "nothing put or evicted; the next tick reads again.")
      return 0
    }
    val rows          = read.answered.get
    wholeCorpusRead = true
    // A failed read was turned away above. An ANSWERED empty corpus while the cache
    // holds rows would evict every one of them — a real Mongo wipe is a degenerate
    // manual operation, acceptable to handle only on a restart, so the cache is left
    // intact rather than trusted to that one answer.
    val cachedSize = positive.estimatedSize()
    if (rows.isEmpty && cachedSize > 0) {
      logger.warn(s"MovieCache rehydrate: the corpus read answered empty while the cache holds $cachedSize row(s) — " +
                  "cache left intact.")
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
    val populating = tools.Stopwatch.start()
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
      // Several documents under one key are the same film twice; the lowest id is the
      // survivor, deterministically, and the others are reconciled below.
      val survivor = rs.map(_.id).minBy(_.value)
      repository.writeFence.ifUndisturbed(survivor.value, marks.of(survivor.value)) {
        if (!Option(positive.getIfPresent(k)).exists(ShowtimesDigest.leanEqual(_, record))) changed += 1
        store(k, forCache(record), survivor)
      }
    }
    // Only a key whose film nobody wrote since the snapshot: one this cache created or
    // retitled meanwhile is absent from the snapshot because it is NEWER, not gone.
    val removed = positive.asMap().keySet().asScala.toSeq.filterNot(byKey.keySet.contains).filter { k =>
      corpusIndex.idOf(k).fold { evict(k); true }(id => repository.writeFence.ifUndisturbed(id.value, marks.of(id.value))(evict(k)))
    }
    cacheMetrics.recordRehydrate(changed, removed.size)
    if (changed > 0 || removed.nonEmpty)
      logger.info(s"MovieCache rehydrate: caught $changed changed row(s) + ${removed.size} orphan-delete(s) " +
        "the change stream missed.")
    // Hydrate is a PURE LOAD: it rebuilds the cache from Mongo and stops. Which listings are one
    // film is the identity projection's to decide, never the load's.
    val tPopulateMs = populating.millis
    if (rows.nonEmpty)
      logger.info(s"Hydrated ${rows.size} enrichment(s) from Mongo — findAll=${tFindAllMs}ms populate=${tPopulateMs}ms.")
    touch()
    onRead(rows)
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
  private val BackstopIntervalSeconds = rehydrateInterval.value.toSeconds
  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Apply one out-of-band upsert from the change stream to the in-memory cache.
   *  Mirrors a single row of `rehydrate` — a direct `positive.put`, bypassing
   *  the identity-gate `put` (Mongo is already the source of truth here, no
   *  re-folding needed).
   *
   *  …unless this cache has written the film since the stream re-read it (`mark`, see
   *  [[FilmWriteFence]]): that read is older than the resident row, and storing it rolled
   *  the write back until the write's own event re-read the film. That event still comes,
   *  so skipping the older read loses nothing. */
  private[services] def applyUpsert(r: StoredMovieRecord, mark: Long): Unit = {
    val key = r.cacheKey(normalizer)
    val applied = repository.writeFence.ifUndisturbed(r.id.value, mark) {
      // A retitle arriving from another writer: the id's previous key entry is stale.
      corpusIndex.keyOf(r.id).filter(_ != key).foreach(evict)
      val held   = get(key)
      val cached = forCache(r.record)
      store(key, cached, r.id)
      // The echo of a write this cache made holds what it holds already: not another writer's change.
      if (!held.exists(LeanRecords.equal(_, cached))) changed(r.id)
    }
    if (applied) touch()
    else logger.debug(s"MovieCache: skipped a change-stream read of '${key.cleanTitle}' taken before this cache's own write of it.")
  }

  /** Apply a change confined to some venues' showtimes from those venues alone: their slots
   *  stripped again, the rest of the resident row untouched — exactly what [[applyUpsert]] of the
   *  whole film would store. Declined, having stored nothing, when the film is not resident or a
   *  venue holds other slots than the resident ones (a slot came or went), and the caller hands
   *  over the whole film instead. Fenced like [[applyUpsert]]. */
  private[services] def applyVenueSlots(venues: VenueSlots, mark: Long): VenueVerdict = {
    import ChangeStreamMetrics.VenueDecline as Why
    corpusIndex.keyOf(venues.filmId).flatMap(key => get(key).map(key -> _)) match {
      case None => VenueVerdict.Declined(Why.CacheNotResident)
      case Some((key, resident)) =>
        // The venues' slots replace the resident ones whole, read as a whole-film read reads them, so
        // the row stored is that read's exactly when the venues hold the same slots it does.
        val fits = venues.atCinemas.forall { case (cinema, slots) =>
          resident.data.keysIterator.collect { case s @ models.CinemaShowing(`cinema`, _) => s }.toSet == slots.map(_._1).toSet
        }
        if (!fits) VenueVerdict.Declined(Why.CacheSlotsDiffer)
        else {
          // Only the venues' slots are stripped again, not the whole row as `forCache` would: a
          // one-venue change to a wide film stripped thousands of slots — 4% of the US worker's CPU
          // (JFR 2026-10-01). The slot set is unchanged (checked above).
          val applied = repository.writeFence.ifUndisturbed(venues.filmId.value, mark) {
            val slots   = venues.atCinemas.valuesIterator.flatten.map { case (source, slot) => source -> forCacheSlot(slot) }.toSeq
            store(key, resident.copy(data = resident.data ++ slots), venues.filmId)
            if (!slots.forall { case (source, slot) => resident.data.get(source).exists(LeanRecords.slotsEqual(_, slot)) })
              changed(venues.filmId)
          }
          if (applied) touch()
          VenueVerdict.Applied
        }
    }
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
        changed(id)
      }
  }

  /** Re-read while no corpus read has ever completed. A boot read that failed past its attempts
   *  (Mongo slow, or briefly gone) left the cache empty, and only the change stream — rows written
   *  after boot — and the backstop hours later filled it: every quiescent row was invisible to the
   *  fold and the settle meanwhile. Once hydrated this is one field read. */
  private[movies] def coldRetryTick(): Unit =
    if (!wholeCorpusRead) {
      logger.warn("MovieCache cold-retry: no corpus read has completed yet — reading again.")
      rehydrate(); ()
    }

  def start(): Unit = {
    refreshScheduler.scheduleAtFixedRate(
      () => Try(coldRetryTick()).recover { case exception => logger.warn(s"MovieCache cold-retry tick failed: ${exception.getMessage}") },
      CaffeineMovieCache.ColdRetryInterval.toSeconds, CaffeineMovieCache.ColdRetryInterval.toSeconds, TimeUnit.SECONDS)
    watchHandle = repository.watchChangesFencedWithVenues(applyUpsert, applyDelete, applyVenueSlots)
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

object CaffeineMovieCache {
  /** Whether pooling `slot` ([[StringPool.slot]] → `pooled`) changed none of its fields: every one the pool's already. */
  private[movies] def pooledAlike(pooled: SourceData, slot: SourceData): Boolean =
    (pooled.title eq slot.title) && (pooled.rawTitle eq slot.rawTitle) && (pooled.originalTitle eq slot.originalTitle) &&
      (pooled.englishTitle eq slot.englishTitle) && (pooled.synopsis eq slot.synopsis) && (pooled.cast eq slot.cast) &&
      (pooled.director eq slot.director) && (pooled.countries eq slot.countries) && (pooled.genres eq slot.genres) &&
      (pooled.posterUrl eq slot.posterUrl) && (pooled.filmUrl eq slot.filmUrl) && (pooled.trailerUrl eq slot.trailerUrl) &&
      (pooled.language eq slot.language) && (pooled.ageRating eq slot.ageRating) &&
      (pooled.runtimeMinutes eq slot.runtimeMinutes) && (pooled.releaseYear eq slot.releaseYear)

  /** How often a cache with no completed corpus read reads again — tight, because the state it
   *  recovers from is a cache missing every quiescent row, not drift. */
  val ColdRetryInterval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.FiniteDuration(30, TimeUnit.SECONDS)
}
