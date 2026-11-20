package services.movies

import models.{CacheKey, FilmId, StoredMovieRecord}

import services.titles.TitleNormalizer


import models.MovieRecord
import settings.{BootHydrateMaxAttempts, BootHydrateRetryInterval, CacheRehydrateInterval}
import tools.{ThreadPools, ScheduledTick, TickRecorder, TickRun}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

/**
 * The resident corpus of a [[CaffeineMovieCache]] as its [[CorpusMirror]] reaches it: the store
 * it mirrors, and the cache's own write funnels and index — never the Caffeine map itself, so
 * `store` / `evict` stay the only ways into it (see `CaffeineMovieCache.corpusIndex`).
 */
private[movies] trait MirroredCorpus {
  def repository: MovieRepositoryReader & MovieChangeFeed
  def normalizer: TitleNormalizer
  /** How a row is shaped to be held resident ([[ResidentRecords]]). */
  def shape: ResidentRecords
  def index: CorpusIndex
  /** The key a stored row is held under: its title as the corpus's title rules read it. */
  def keyOf(row: StoredMovieRecord): CacheKey = row.cacheKey(normalizer)
  /** The film held under `key`, from the corpus's index. */
  def idOf(key: CacheKey): Option[FilmId] = index.idOf(key)
  /** `record` as the corpus holds it ([[ResidentRecords.of]]). */
  def asHeld(record: MovieRecord): MovieRecord = shape.of(record)
  /** The store's fence over the films its writers are writing ([[FilmWriteFence]]). */
  def writeFence: FilmWriteFence = repository.writeFence
  def held(key: CacheKey): Option[MovieRecord]
  def heldKeys: Seq[CacheKey]
  def heldCount: Long
  def store(key: CacheKey, record: MovieRecord, id: FilmId): Unit
  def evict(key: CacheKey): Unit
  /** Stamp the cache's `lastModified`. */
  def touch(): Unit
  /** Tell the cache's change listeners (`onChanged`) that another writer changed film `id`. */
  def changed(id: FilmId): Unit
}

/**
 * How a [[CaffeineMovieCache]] is kept in step with its store: the boot hydrate, the cold-retry
 * and backstop rehydrates, and the change-stream mirror. Built at the composition root, which
 * decides its retry budget, cadence, metrics and boot readers; the cache attaches it to its
 * resident corpus once, at construction, and its `hydrated` / `rehydrate` / `start` / `stop`
 * are this mirror's.
 *
 * @param bootHydrateMaxAttempts boot-hydrate retry — OFF by default (0 attempts) so tests and a
 *        genuine cold start pay nothing. Prod turns it on (`KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS`) so
 *        a not-ready Mongo at boot can't leave the cache empty (see `bootHydrate`).
 * @param cacheMetrics measures what the periodic backstop rehydrate catches that the incremental
 *        change stream missed — the redundancy signal for retiring the rehydrate. No-op for tests.
 * @param rehydrateInterval the backstop rehydrate (`KINOWO_CACHE_REHYDRATE_SECONDS`, resolved by
 *        the worker's root); the compiled-in 6 hours for specs.
 * @param bootReaders the boot's other whole-corpus readers, handed the boot hydrate's read so the
 *        boot reads the corpus once (see [[BootCorpusReader]]). None for specs and a lone cache.
 * @param ticks where each completed cold-retry and backstop tick is reported (`ScheduledTickStale`).
 * @param threadPools where each attached mirror's refresh scheduler comes from: the root's managed pools.
 * @param sleep the wait between boot-hydrate attempts ([[tools.Sleep.Blocking]] from the root).
 */
final class CorpusMirror(
  bootHydrateMaxAttempts: BootHydrateMaxAttempts   = BootHydrateMaxAttempts(0),
  bootHydrateRetry:       BootHydrateRetryInterval = BootHydrateRetryInterval(1.second),
  cacheMetrics:           CacheSyncMetrics         = CacheSyncMetrics.noop,
  rehydrateInterval:      CacheRehydrateInterval   = CacheRehydrateInterval(6.hours),
  bootReaders:            Seq[BootCorpusReader]    = Nil,
  ticks:                  TickRecorder             = TickRecorder.Unrecorded,
  threadPools:            ThreadPools,
  sleep:                  FiniteDuration => Unit
) {
  /** This mirror over one cache's corpus — once per cache: the result holds the hydrate's state,
   *  scheduler and change-stream watch, and logs to `logger`, the cache's own, as it did before the
   *  mirror was its own class. */
  private[movies] def attach(corpus: MirroredCorpus, logger: play.api.Logger): Attached = new Attached(corpus, logger)

  private[services] final class Attached(corpus: MirroredCorpus, logger: play.api.Logger) {
    private val hydrate = new CorpusHydrate(corpus, bootHydrateMaxAttempts, bootHydrateRetry, cacheMetrics, bootReaders, sleep, logger)
    private val changes = new CorpusStreamApplies(corpus, logger)

    /** Set once a rehydrate's corpus read has been answered whole, and never cleared ([[CorpusHydrate]]). */
    def hydrated: Boolean = hydrate.hydrated
    /** The boot hydrate ([[CorpusHydrate]]): the cache calls it from its constructor. */
    def bootHydrate(): Unit = hydrate.bootHydrate()
    /** Put every stored row and evict the keys gone from the store; how many rows it put ([[CorpusHydrate]]). */
    def rehydrate(): Int = hydrate.rehydrate()
    /** The change stream's applies ([[CorpusStreamApplies]]). */
    private[services] def applyUpsert(r: StoredMovieRecord, mark: Long): Unit = changes.applyUpsert(r, mark)
    private[services] def applyVenueSlots(venues: VenueSlots, mark: Long): VenueVerdict = changes.applyVenueSlots(venues, mark)
    private[services] def applyDelete(id: FilmId): Unit = changes.applyDelete(id)

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
    private val refreshScheduler        = threadPools.scheduler("movie-cache-refresh")
    private val BackstopIntervalSeconds = rehydrateInterval.value.toSeconds
    @volatile private var watchHandle: Option[AutoCloseable] = None

    /** Re-read while no corpus read has ever completed. A boot read that failed past its attempts
     *  (Mongo slow, or briefly gone) left the cache empty, and only the change stream — rows written
     *  after boot — and the backstop hours later filled it: every quiescent row was invisible to the
     *  fold and the settle meanwhile. Once hydrated this is one field read. */
    private[movies] def coldRetryTick(): TickRun =
      if (hydrate.hydrated) TickRun.Completed
      else {
        logger.warn("MovieCache cold-retry: no corpus read has completed yet — reading again.")
        hydrate.rehydrateTick()
      }

    def start(): Unit = {
      ticks.started("movie-cache-cold-retry", CorpusMirror.ColdRetryInterval, CorpusMirror.ColdRetryInterval)
      refreshScheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("movie-cache-cold-retry", CorpusMirror.ColdRetryInterval, logger, ticks)(coldRetryTick()),
        CorpusMirror.ColdRetryInterval.toSeconds, CorpusMirror.ColdRetryInterval.toSeconds, TimeUnit.SECONDS)
      watchHandle = corpus.repository.watchChangesFencedWithVenues(changes.applyUpsert, changes.applyDelete, changes.applyVenueSlots)
      logger.info(
        s"MovieCache incremental change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
        s"backstop rehydrate every ${BackstopIntervalSeconds}s.")
      ticks.started("movie-cache-rehydrate", rehydrateInterval.value, rehydrateInterval.value)
      refreshScheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("movie-cache-rehydrate", rehydrateInterval.value, logger, ticks)(hydrate.rehydrateTick()),
        BackstopIntervalSeconds, BackstopIntervalSeconds, TimeUnit.SECONDS
      )
    }

    def stop(): Unit = {
      watchHandle.foreach(h => Try(h.close()))
      refreshScheduler.shutdown()
    }
  }
}

object CorpusMirror {
  /** The mirror of a lone cache — specs, tools, and any cache built without its root's: no boot-hydrate retry, the
   *  compiled-in cadence, ticks unrecorded, unmanaged threads, `sleep` between boot-hydrate attempts. */
  def standalone(sleep: FiniteDuration => Unit): CorpusMirror = new CorpusMirror(threadPools = ThreadPools.Unmanaged, sleep = sleep)

  /** How often a cache with no completed corpus read reads again — tight, because the state it
   *  recovers from is a cache missing every quiescent row, not drift. */
  val ColdRetryInterval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.FiniteDuration(30, TimeUnit.SECONDS)
}
