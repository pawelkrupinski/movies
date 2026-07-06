package services.readmodel

import models.CityScreening
import play.api.Logging
import services.Stoppable
import services.movies.{MovieRepository, StoredMovieRecord}
import tools.{DaemonExecutors, Env}

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Maintains the denormalised read model (`web_movies` + `web_screenings`) from
 * the source `movies` collection.
 *
 * Two mechanisms, mirroring `MovieCache`'s sync design:
 *
 *  1. INCREMENTAL — subscribes to the `movies` change stream
 *     (`MovieRepository.watchUpserts`); each changed row is re-projected and the
 *     resulting documents are diffed against the last projection so only the documents
 *     that actually changed are written.
 *  2. RECONCILE (backstop) — a periodic full re-projection that also prunes
 *     derived documents whose source film has vanished (the change stream delivers
 *     no deletes; a fold-victim / `UnscreenedCleanup` removal is reconciled
 *     here).
 *
 * Minimal writes: the last-projected document per film is kept in memory (hydrated
 * from the read model at boot, so a restart rewrites only what changed since
 * the projector last ran). A showtime-only edit re-projects to the same
 * `ResolvedMovie` (skipped) and the same screenings except one (written) — so
 * the web's change-stream delta is that one screening document. The movie document is
 * always written before its screenings, so a consumer joining screening→movie
 * sees the metadata first; the web join also tolerates the reverse order, so
 * neither side depends on it.
 *
 * The change-stream callback and the reconcile tick are serialised by one lock,
 * so the in-memory last-projection state needs no further synchronisation.
 */
class ReadModelProjector(
  movieRepository: MovieRepository,
  writer:    ReadModelWriter,
  reader:    ReadModelReader,
  metrics:   ReadModelProjectionMetrics = ReadModelProjectionMetrics.noop
) extends Stoppable with Logging {
  import ReadModelProjectionMetrics.{Op, ReconcileKind, Target}

  // Diff state for minimal writes: the CONTENT HASH of the last-projected document per
  // film (and per screening), NOT the full document. The projection is deterministic, so
  // an unchanged row hashes to the same value and its write is skipped; keeping only the
  // hash resident holds the whole read model for diffing at a few bytes per doc instead of
  // the full ResolvedMovie / CityScreening object graph (which also lives in Mongo — this
  // is a pure duplicate we don't need to keep inflated). A 32-bit hash collision
  // (astronomically rare) would skip one genuine write, leaving a stale doc until the
  // row's next real change re-projects it — self-healing, never permanently wrong.
  private val lastMovie      = scala.collection.mutable.Map.empty[String, Int]
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, Int]]
  private val lock           = new AnyRef

  private val scheduler = DaemonExecutors.scheduler("read-model-projector")
  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick); the expensive full re-projection runs RARELY
  // (its only unique job is catching upserts missed during a stream outage — a ~1-core
  // whole-corpus burst, formerly every 30 min, was the reprojection credit-drain).
  private val PruneSeconds     = Env.positiveLong("KINOWO_READMODEL_PRUNE_SECONDS", 1800L)      // 30 min
  private val ReconcileSeconds = Env.positiveLong("KINOWO_READMODEL_RECONCILE_SECONDS", 21600L) // 6 h (was 30 min)
  // Both are deferred off the boot path — running a full scan synchronously at `start()`
  // stacked a second scan onto the cache hydrate + first scrape on a cold JVM (the boot
  // CPU-credit drain). The boot catch-up reproject runs ONLY when the change stream is
  // UNAVAILABLE (see `reconcileInitialDelaySeconds`); the prune staggers a little later.
  private val ReconcileBootDelaySeconds = Env.positiveLong("KINOWO_READMODEL_RECONCILE_BOOT_DELAY_SECONDS", 60L)
  private val PruneBootDelaySeconds     = Env.positiveLong("KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS", 300L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit = lock.synchronized { project(stored); () }

  // Caller holds `lock`. Project the row and write only what changed, movie
  // document before screenings. A row whose enrichment hasn't concluded
  // (`readyToProject` false) is held back — publishing the pre-enrichment,
  // yearless row is exactly what creates the duplicate `foo|` + `foo|2025`
  // cards, so it must never reach the read model until it has settled.
  /** Returns the number of derived documents actually (re)written for this row —
   *  0 when the projection was byte-identical to what's already stored. The
   *  reconcile sweep sums this to know whether a full re-projection caught
   *  anything the change stream missed. */
  private def project(stored: StoredMovieRecord): Int = {
    if (!stored.record.readyToProject) return 0
    // A row fans out into one card per display-title variant (Cyrillic / English
    // / banner-prefixed listings of one film); the common single-title row yields
    // exactly one. Each variant card is diffed and written independently. Time the
    // pure projection (CPU-bound, no I/O) — its rate() in centi-cores attributes how
    // much worker CPU the reproject/enrich churn spends here (the credit-floor driver).
    val projectStart = System.nanoTime()
    val variants     = ReadModelProjection.projectAll(stored)
    metrics.recordProject((System.nanoTime() - projectStart) / 1e9)
    var written = 0
    variants.foreach { case (movie, screenings) =>
      if (!lastMovie.get(movie._id).contains(movie.##)) {
        writer.upsertMovie(movie)
        metrics.recordWrite(Target.Movie, Op.Upsert, 1)
        lastMovie.update(movie._id, movie.##)
        written += 1
      }
      written += diffScreenings(movie._id, screenings)
    }
    written
  }

  /** Returns the number of screening documents written (upserts + deletes). */
  private def diffScreenings(filmId: String, next: Seq[CityScreening]): Int = {
    val nextById = next.map(s => s._id -> s).toMap
    val previous = lastScreenings.getOrElse(filmId, Map.empty)
    var upserted = 0
    nextById.foreach { case (id, s) => if (!previous.get(id).contains(s.##)) { writer.upsertScreening(s); upserted += 1 } }
    val deletes = previous.keysIterator.filterNot(nextById.contains).toSeq
    deletes.foreach(writer.deleteScreening)
    if (upserted > 0)        metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    if (deletes.nonEmpty)    metrics.recordWrite(Target.Screening, Op.Delete, deletes.size)
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById.view.mapValues(_.##).toMap)
    upserted + deletes.size
  }

  // Only `reconcile` calls this — a film whose source row vanished or was re-keyed
  // (its filmId changed) is dropped wholesale. `recordFilmPruned` is the link-break
  // signal; the document deletes are also counted as reprojection writes.
  private def deleteFilm(filmId: String): Unit = {
    writer.deleteMovie(filmId)
    val screeningIds = lastScreenings.getOrElse(filmId, Map.empty).keys.toSeq
    screeningIds.foreach(writer.deleteScreening)
    metrics.recordWrite(Target.Movie, Op.Delete, 1)
    if (screeningIds.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, screeningIds.size)
    metrics.recordFilmPruned(1)
    lastMovie.remove(filmId)
    lastScreenings.remove(filmId)
  }

  /** One reconciliation pass over the whole corpus.
   *
   *  `reproject = true` — the FULL sweep: re-project every ready row (the diff keeps
   *  writes minimal — only genuinely changed documents are written) AND prune. This
   *  is the expensive path (projecting ~1400 rows is a ~1-core burst that, on the old
   *  30-min cadence, filled the 320m heap → GC thrash → CPU-credit starvation). Its
   *  only unique job over the change-stream path is catching upserts MISSED while the
   *  stream was down or terminally errored, so it now runs rarely (boot + `ReconcileSeconds`).
   *
   *  `reproject = false` — the CHEAP sweep (`pruneOrphans`): build the live-id set from
   *  `ReadModelProjection.filmIds` (no projection) and prune only. This is the frequent
   *  backstop for DELETES / re-keys, which the change stream drops (`watchUpserts` nulls
   *  onDelete) — the film-id is `sanitize(title)|resolvedYear`, many source rows to one
   *  card, so a single source delete can't be mapped to a card without a reverse index;
   *  the set-difference prune is the correct, cheap way to reconcile them.
   *
   *  Self-healing: the prune diffs the ACTUAL read-model ids
   *  (`reader.findAllMovieIds`/`findAllScreeningRefs`) against the live source, NOT this
   *  process's in-memory `lastMovie` — a film a PRIOR worker process wrote and re-keyed
   *  is invisible to a memory-based prune, which is how a re-key across a restart leaked a
   *  duplicate card permanently. Streaming `foreachRecord` (not a whole-corpus `findAll`)
   *  and id-only read-model reads keep the whole corpus off the heap.
   *
   *  A row that fails to project must not abort the prune (the prune is what removes the
   *  duplicates), so each projection is guarded individually. */
  private def sweep(reproject: Boolean): Unit = lock.synchronized {
    val kind = if (reproject) ReconcileKind.Reproject else ReconcileKind.Prune
    val liveIds = scala.collection.mutable.Set.empty[String]
    var reprojected = 0
    val scanComplete = movieRepository.foreachRecord { row =>
      if (row.record.readyToProject) {
        liveIds ++= ReadModelProjection.filmIds(row)
        if (reproject)
          try reprojected += project(row)
          catch { case exception: Throwable =>
            logger.warn(s"read-model $kind: a row failed to project, continuing: ${exception.getMessage}") }
      }
    }
    var prunedFilms      = 0
    var prunedScreenings = 0
    if (!scanComplete) {
      // The source scan failed mid-way (a Mongo batch read threw after its retries —
      // typically a server-selection / socket timeout while the worker is CPU-throttled,
      // the 2026-06-29 01:34–02:19 served-films flap). `liveIds` is therefore a TRUNCATED
      // view, so pruning on it would delete every read-model card whose source row this
      // scan never reached. Keep the (idempotent) projections we did make and SKIP the
      // destructive prune; the next tick reconciles cleanly once reads recover.
      logger.warn(s"read-model $kind: source scan was incomplete (Mongo read failed mid-scan) — " +
        "skipping the prune this tick to avoid deleting live read-model rows; will retry next tick.")
    } else {
      // Prune off id-only projections — the prune reads ids/filmIds, never payloads.
      reader.findAllMovieIds().iterator.filterNot(liveIds).foreach { id => deleteFilm(id); prunedFilms += 1 }
      reader.findAllScreeningRefs().iterator.filterNot(ref => liveIds(ref.filmId)).foreach { ref =>
        writer.deleteScreening(ref._id)
        metrics.recordWrite(Target.Screening, Op.Delete, 1)
        lastScreenings.updateWith(ref.filmId)(_.map(_ - ref._id).filter(_.nonEmpty))
        prunedScreenings += 1
      }
    }
    // Measurement: does this sweep still do anything? A `reproject` sweep that is
    // consistently didWork=false proves the change stream is reliable and the full
    // sweep is redundant; a `prune` sweep with didWork=true is the deletes/re-keys the
    // stream can't deliver. Surfaced as kinowo_worker_readmodel_reconcile_sweeps.
    val didWork = reprojected > 0 || prunedFilms > 0 || prunedScreenings > 0
    metrics.recordReconcileSweep(kind, didWork)
    logger.info(s"read-model $kind sweep: reprojected $reprojected doc(s), pruned $prunedFilms film(s) + " +
      s"$prunedScreenings orphan screening(s)${if (scanComplete) "" else " [scan INCOMPLETE — prune skipped]"}.")
  }

  /** Full re-projection + prune — the boot/periodic gap backstop (expensive). */
  def reconcile(): Unit = sweep(reproject = true)

  /** Cheap id-only orphan prune — the frequent backstop for deleted/re-keyed rows. */
  def pruneOrphans(): Unit = sweep(reproject = false)

  /** Initial delay for the periodic full reproject. When the change stream is ACTIVE it
   *  replays events missed while the worker was down from the persisted resume token, so a
   *  boot catch-up reproject would only re-project what the stream is already delivering — a
   *  ~13-doc write burst counted as a false-positive `did_work` on every restart. So gate it:
   *  with an active stream the FIRST reproject is the periodic one (`ReconcileSeconds`), i.e.
   *  no boot reproject; only when the stream is UNAVAILABLE (no replica set / failed to open,
   *  nothing to replay) does the boot catch-up run at `ReconcileBootDelaySeconds`. The +300s
   *  prune and the eventual periodic reproject remain the backstops either way. */
  private[readmodel] def reconcileInitialDelaySeconds(streamActive: Boolean): Long =
    if (streamActive) ReconcileSeconds else ReconcileBootDelaySeconds

  def start(): Unit = if (enabled) {
    // Seed the last-projection state from the derived collections, so a restart
    // doesn't rewrite documents that are already correct.
    lock.synchronized {
      reader.findAllMovies().foreach(m => lastMovie.update(m._id, m.##))
      reader.findAllScreenings().groupBy(_.filmId).foreach { case (fid, ss) =>
        lastScreenings.update(fid, ss.map(s => s._id -> s.##).toMap)
      }
    }
    // The change-stream watch covers live changes from now on; the seeded state above
    // means incremental writes are no-ops for already-correct documents. Both sweeps are
    // deferred off the boot path so they don't compete with boot hydrate + the first scrape.
    watchHandle = movieRepository.watchUpserts(onMovieUpsert)
    // Full re-projection: periodic, plus a boot catch-up ONLY when the stream can't replay.
    val reconcileInitialDelay = reconcileInitialDelaySeconds(watchHandle.isDefined)
    scheduler.scheduleAtFixedRate(
      () => Try(reconcile()).recover { case exception => logger.warn(s"read-model reconcile tick failed: ${exception.getMessage}") },
      reconcileInitialDelay, ReconcileSeconds, TimeUnit.SECONDS)
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU).
    scheduler.scheduleAtFixedRate(
      () => Try(pruneOrphans()).recover { case exception => logger.warn(s"read-model prune tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    logger.info(s"ReadModelProjector started; orphan-prune every ${PruneSeconds}s (first in ${PruneBootDelaySeconds}s), " +
      s"full reproject every ${ReconcileSeconds}s (first in ${reconcileInitialDelay}s — " +
      s"${if (watchHandle.isDefined) "no boot reproject, stream replays the gap" else "boot catch-up, stream unavailable"}); " +
      s"change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — reconcile only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
