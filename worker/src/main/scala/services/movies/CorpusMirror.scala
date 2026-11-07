package services.movies

import models.MovieRecord
import settings.{BootHydrateMaxAttempts, BootHydrateRetryInterval, CacheRehydrateInterval}
import tools.{DaemonExecutors, ScheduledTick, TickRecorder, TickRun}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.DurationInt
import scala.util.Try

/**
 * The resident corpus of a [[CaffeineMovieCache]] as its [[CorpusMirror]] reaches it: the store
 * it mirrors, and the cache's own write funnels and index — never the Caffeine map itself, so
 * `store` / `evict` stay the only ways into it (see `CaffeineMovieCache.corpusIndex`).
 */
private[movies] trait MirroredCorpus {
  def repository: MovieRepository
  def normalizer: TitleNormalizer
  /** How a row is shaped to be held resident ([[ResidentRecords]]). */
  def shape: ResidentRecords
  def index: CorpusIndex
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
 */
final class CorpusMirror(
  bootHydrateMaxAttempts: BootHydrateMaxAttempts   = BootHydrateMaxAttempts(0),
  bootHydrateRetry:       BootHydrateRetryInterval = BootHydrateRetryInterval(1.second),
  cacheMetrics:           CacheSyncMetrics         = CacheSyncMetrics.noop,
  rehydrateInterval:      CacheRehydrateInterval   = CacheRehydrateInterval(6.hours),
  bootReaders:            Seq[BootCorpusReader]    = Nil,
  ticks:                  TickRecorder             = TickRecorder.Unrecorded
) {
  /** This mirror over one cache's corpus — once per cache: the result holds the hydrate's state,
   *  scheduler and change-stream watch. */
  private[movies] def attach(corpus: MirroredCorpus): Attached = new Attached(corpus)

  private[movies] final class Attached(corpus: MirroredCorpus) {
    // Logged as the cache it mirrors, as before the mirror was its own class.
    private val logger = play.api.Logger(classOf[CaffeineMovieCache])
    private val repository    = corpus.repository
    private val normalizer    = corpus.normalizer
    private val residentShape = corpus.shape
    private val corpusIndex   = corpus.index
    import corpus.{changed, evict, store, touch}
    private def get(key: CacheKey): Option[MovieRecord] = corpus.held(key)

    // Set once a rehydrate's corpus read has been answered whole — an empty corpus included — and
    // never cleared: from then on a failed read only leaves the cache a little stale. Initialised
    // with the mirror, before the cache's boot hydrate writes it.
    @volatile private var wholeCorpusRead = false
    def hydrated: Boolean = wholeCorpusRead

    // Hydrate from Mongo on construction (the cache calls this from its constructor).
    // Synchronous: Wiring builds the cache during `start()`, so the first HTTP request only lands after the initial
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
    def bootHydrate(): Unit = {
      // Each read's pages go to the boot readers as they are read; the first complete, non-empty read is
      // the corpus. An empty answer is not the corpus: at boot it is as likely a Mongo that is not ready
      // yet as an empty store, so its pages are forgotten and the hydrate reads again.
      val offer: Seq[StoredMovieRecord] => Unit = page => bootReaders.foreach(_.bootPage(page))
      var attempt = 0
      var read    = rehydrateFrom(offer).getOrElse(0)
      while (read == 0 && attempt < bootHydrateMaxAttempts.value) {
        bootReaders.foreach(_.bootReadEnded(BootReadEnd.Retrying))
        attempt += 1
        Thread.sleep(bootHydrateRetry.value.toMillis) // an interrupt (shutdown mid-boot) ends the construction
        read = rehydrateFrom(offer).getOrElse(0)
      }
      bootReaders.foreach(_.bootReadEnded(if (read > 0) BootReadEnd.Whole else BootReadEnd.GaveUp))
    }

    /** Put every stored row and evict the keys gone from the store; how many rows it put. An incomplete
     *  read (a page the scan could not read is skipped whole) changes nothing — no row put, none
     *  evicted: its missing films are not gone. The next backstop tick reads again. */
    def rehydrate(): Int = rehydrateFrom(_ => ()).getOrElse(0)

    /** [[rehydrate]], as a tick's run: [[TickRun.Incomplete]] when the read could not complete or was refused (logged). */
    private def rehydrateTick(): TickRun = TickRun.completedWhen(rehydrateFrom(_ => ()).isDefined)

    /** [[rehydrate]], handing each page of its read to `onPage` as it is read.
     *
     *  Streamed, not collected: each page is stripped to what the cache keeps ([[forCache]]) as it
     *  arrives and then dropped, so the read never holds the whole stitched corpus — every showtime
     *  of every film — at once. Collected, it did, for the whole read and then for the boot readers'
     *  derivation after it: long enough at boot that the corpus's showtimes were promoted into the old
     *  generation only to die there (see [[BootCorpusReader]]). Nothing is put or evicted until the
     *  read is complete. */
    private def rehydrateFrom(onPage: Seq[StoredMovieRecord] => Unit): Option[Int] = {
      // Additive sync — never blank the cache mid-rehydrate. The backstop
      // tick (see `start()` below) runs while readers walk `snapshot()`;
      // an `invalidateAll()` window would briefly show them an empty corpus. Instead: put every Mongo row (cache's
      // copy gets overwritten if it changed), then evict only the keys
      // that disappeared from Mongo since the last sync.
      val findingAll = tools.Stopwatch.start()
      // Marked BEFORE the read: a row this cache writes while `findAll` runs is newer than the
      // snapshot, and storing (or evicting) it from the snapshot would undo the write — see
      // [[FilmWriteFence]]. Such a row is left as the write made it; its own change-stream event,
      // or the next backstop, reconciles it.
      val marks = repository.writeFence.markAll()
      // None: a read that could not complete, or an empty answer refused — nothing put or evicted.
      readByKey(onPage, findingAll).flatMap { case (byKey, rows) =>
        val tFindAllMs = findingAll.millis
        if (refusesEmpty(rows, tFindAllMs)) None
        else {
          val populating = tools.Stopwatch.start()
          val changed    = populate(byKey, marks)
          val removed    = evictGone(byKey, marks)
          cacheMetrics.recordRehydrate(changed, removed)
          if (changed > 0 || removed > 0)
            logger.info(s"MovieCache rehydrate: caught $changed changed row(s) + $removed orphan-delete(s) " +
              "the change stream missed.")
          // Hydrate is a PURE LOAD: it rebuilds the cache from Mongo and stops. Which listings are one
          // film is the identity projection's to decide, never the load's.
          if (rows > 0)
            logger.info(s"Hydrated $rows enrichment(s) from Mongo — findAll=${tFindAllMs}ms populate=${populating.millis}ms.")
          touch()
          Some(rows)
        }
      }
    }

    /** Whether a complete read of `rows` documents is one the rehydrate must not act on.
     *
     *  An ANSWERED empty corpus while the cache holds rows would evict every one of them — a real Mongo
     *  wipe is a degenerate manual operation, acceptable to handle only on a restart, so the cache is
     *  left intact rather than trusted to that one answer. On a cold cache an empty answer is acted on
     *  (a no-op), but said out loud: don't silently start serving an empty repertoire — a Mongo timeout
     *  / disabled connection / projection bug is then obvious in the boot log instead of hiding behind
     *  a 200 response with zero films on it. */
    private def refusesEmpty(rows: Int, tFindAllMs: Long): Boolean = {
      val cachedSize = corpus.heldCount
      if (rows == 0 && cachedSize > 0) {
        logger.warn(s"MovieCache rehydrate: the corpus read answered empty while the cache holds $cachedSize row(s) — " +
                    "cache left intact.")
        true
      } else {
        if (rows == 0)
          logger.warn(s"MovieCache rehydrate: findAll() returned empty on a cold cache (findAll=${tFindAllMs}ms) — " +
                      "Mongo connection disabled, query timed out, or repository genuinely empty. " +
                      "Pages will render with no films until the next successful tick.")
        false
      }
    }

    /** Store every key's record; how many differed from the resident one. Count what this backstop
     *  reload catches that the incremental change stream missed: a put whose cached value DIFFERED (a
     *  missed upsert) — with `evictGone`'s missed deletes, after resume tokens + delete-apply these
     *  should be ~0 in steady state — the signal (kinowo_worker_cache_rehydrate_changes) that the
     *  rehydrate is redundant. */
    private def populate(byKey: java.util.LinkedHashMap[CacheKey, CorpusMirror.HydratedRows], marks: FilmWriteFence.Marks): Int = {
      var changed = 0
      byKey.forEach { (k, held) =>
        // Several documents under one key are the same film twice; the lowest id is the
        // survivor, deterministically, and the others are reconciled later.
        val survivor = held.ids.minBy(_.value)
        repository.writeFence.ifUndisturbed(survivor.value, marks.of(survivor.value)) {
          if (!get(k).exists(ShowtimesDigest.leanEqual(_, held.record))) changed += 1
          store(k, held.record, survivor)
        }
      }
      changed
    }

    /** Evict every resident key the read no longer holds; how many. Only a key whose film nobody wrote
     *  since the snapshot: one this cache created or retitled meanwhile is absent from the snapshot
     *  because it is NEWER, not gone. */
    private def evictGone(byKey: java.util.LinkedHashMap[CacheKey, CorpusMirror.HydratedRows], marks: FilmWriteFence.Marks): Int = {
      corpus.heldKeys.filterNot(byKey.containsKey).count { k =>
        corpusIndex.idOf(k).fold { evict(k); true }(id => repository.writeFence.ifUndisturbed(id.value, marks.of(id.value))(evict(k)))
      }
    }

    /** Read the whole corpus, by key, as the cache keeps it — or None, having logged why, when the read
     *  was incomplete or a key two documents share could not be read again whole.
     *
     *  Grouped by key BEFORE putting: a merge-key rule added after these documents were written (a new
     *  GlobalStructural strip) makes two stored titles collide on `CacheKey`, and a bare `put`-per-row
     *  is last-write-wins — it would silently drop one document's showtimes until the next scrape.
     *  The colliding rows are unioned instead, so the cache is lossless the moment the rule lands (the
     *  orphaned Mongo `_id` is reconciled by a later scrape / the reaper). Each row is kept as the cache
     *  keeps it; a key two rows share is read again whole, since their union needs the showtimes. */
    private def readByKey(onPage: Seq[StoredMovieRecord] => Unit, reading: tools.Stopwatch.Started)
        : Option[(java.util.LinkedHashMap[CacheKey, CorpusMirror.HydratedRows], Int)] = {
      import scala.jdk.CollectionConverters._
      val byKey = new java.util.LinkedHashMap[CacheKey, CorpusMirror.HydratedRows]()
      var rows  = 0
      val scan  = repository.foreachPage { page =>
        page.foreach { row =>
          val held = CorpusMirror.HydratedRows(row.id, residentShape.of(row.record))
          byKey.merge(row.cacheKey(normalizer), held, (first, next) => first.and(next.ids))
          ()
        }
        rows += page.size
        onPage(page)
      }
      val collided = byKey.asScala.collect { case (key, held) if held.ids.sizeIs > 1 => key -> held.ids }
      val read     = if (!scan.isComplete) None else unionCollided(collided.toSeq)
      if (read.isEmpty)
        logger.warn(s"MovieCache rehydrate: the corpus read ${if (scan.isComplete) "could not re-read the rows two documents share a key with" else scan.explain} " +
          s"(in ${reading.millis}ms) — nothing put or evicted; the next tick reads again.")
      read.map { unions =>
        unions.foreach { case (key, record) => byKey.computeIfPresent(key, (_, held) => held.copy(record = record)); () }
        wholeCorpusRead = true
        (byKey, rows)
      }
    }
    /** The union of each collided key's documents, read again whole — as the cache keeps it — or None
     *  when one of them could not be read: a union of what could be read would drop the rest's
     *  showtimes, the loss the union exists to prevent. A document deleted since the scan is no
     *  longer part of the film. */
    private def unionCollided(collided: Seq[(CacheKey, Seq[FilmId])]): Option[Seq[(CacheKey, MovieRecord)]] =
      collided.foldLeft(Option(Vector.empty[(CacheKey, MovieRecord)])) { case (acc, (key, ids)) =>
        acc.flatMap { done =>
          val reads = ids.map(repository.findByIdChecked)
          Option.when(reads.forall(read => read.answered.isDefined || read.isInstanceOf[tools.ReadOutcome.Absent])) {
            reads.flatMap(_.answered).map(_.record) match {
              case Seq()  => done
              case whole  => done :+ (key -> residentShape.of(MovieRecordMerge.unionAll(whole)))
            }
          }
        }
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
        val cached = held.fold(residentShape.of(r.record))(residentShape.keeping(_, r.record))
        // The echo of a write this cache made reads back what it holds already — kept as the very object, not replaced by
        // a copy that every reader then compares slot by slot and that lives, until the film's next write, long enough to
        // be promoted (worker-us).
        if (!(held.exists(_ eq cached) && corpusIndex.idOf(key).contains(r.id))) store(key, cached, r.id)
        // ...and it is not another writer's change.
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
              val slots   = venues.atCinemas.valuesIterator.flatten.map { case (source, slot) =>
                source -> residentShape.kept(resident.data.get(source), residentShape.slot(slot)) }.toSeq
              // Venues read back as the row holds them (the echo of this cache's own write) leave the row as it is.
              if (!slots.forall { case (source, slot) => resident.data.get(source).exists(_ eq slot) }) {
                store(key, resident.copy(data = resident.data ++ slots), venues.filmId)
                if (!slots.forall { case (source, slot) => resident.data.get(source).exists(LeanRecords.slotsEqual(_, slot)) })
                  changed(venues.filmId)
              }
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
    private[movies] def coldRetryTick(): TickRun =
      if (wholeCorpusRead) TickRun.Completed
      else {
        logger.warn("MovieCache cold-retry: no corpus read has completed yet — reading again.")
        rehydrateTick()
      }

    def start(): Unit = {
      ticks.started("movie-cache-cold-retry", CorpusMirror.ColdRetryInterval, CorpusMirror.ColdRetryInterval)
      refreshScheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("movie-cache-cold-retry", CorpusMirror.ColdRetryInterval, logger, ticks)(coldRetryTick()),
        CorpusMirror.ColdRetryInterval.toSeconds, CorpusMirror.ColdRetryInterval.toSeconds, TimeUnit.SECONDS)
      watchHandle = repository.watchChangesFencedWithVenues(applyUpsert, applyDelete, applyVenueSlots)
      logger.info(
        s"MovieCache incremental change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
        s"backstop rehydrate every ${BackstopIntervalSeconds}s.")
      ticks.started("movie-cache-rehydrate", rehydrateInterval.value, rehydrateInterval.value)
      refreshScheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("movie-cache-rehydrate", rehydrateInterval.value, logger, ticks)(rehydrateTick()),
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
  /** The documents a rehydrate read under one key, and the record the cache keeps for them: the
   *  first document's, until the documents' union replaces it (see `rehydrateFrom`). */
  private final case class HydratedRows(ids: Seq[FilmId], record: MovieRecord) {
    def and(more: Seq[FilmId]): HydratedRows = copy(ids = ids ++ more)
  }
  private object HydratedRows {
    def apply(id: FilmId, record: MovieRecord): HydratedRows = HydratedRows(Seq(id), record)
  }

  /** How often a cache with no completed corpus read reads again — tight, because the state it
   *  recovers from is a cache missing every quiescent row, not drift. */
  val ColdRetryInterval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.FiniteDuration(30, TimeUnit.SECONDS)
}
