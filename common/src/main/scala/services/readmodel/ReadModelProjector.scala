package services.readmodel

import models.{CityScreening, ResolvedMovie}
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
  import ReadModelProjectionMetrics.{Op, Target}

  private val lastMovie      = scala.collection.mutable.Map.empty[String, ResolvedMovie]
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, CityScreening]]
  private val lock           = new AnyRef

  private val scheduler        = DaemonExecutors.scheduler("read-model-projector")
  private val ReconcileSeconds = Env.positiveLong("KINOWO_READMODEL_RECONCILE_SECONDS", 1800L)
  // The boot reconcile is a full `movieRepository.foreachRecord` project-every-row scan.
  // Running it synchronously at `start()` stacked a second full scan onto the
  // cache hydrate and the first scrape on a cold JVM (the boot CPU-credit drain).
  // Defer it to the first scheduled tick this many seconds in — short, NOT the
  // full ReconcileSeconds, so stale-prune latency stays seconds not 30 min.
  private val ReconcileBootDelaySeconds =
    Env.positiveLong("KINOWO_READMODEL_RECONCILE_BOOT_DELAY_SECONDS", 60L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit = lock.synchronized(project(stored))

  // Caller holds `lock`. Project the row and write only what changed, movie
  // document before screenings. A row whose enrichment hasn't concluded
  // (`readyToProject` false) is held back — publishing the pre-enrichment,
  // yearless row is exactly what creates the duplicate `foo|` + `foo|2025`
  // cards, so it must never reach the read model until it has settled.
  private def project(stored: StoredMovieRecord): Unit = {
    if (!stored.record.readyToProject) return
    val (movie, screenings) = ReadModelProjection.project(stored)
    if (!lastMovie.get(movie._id).contains(movie)) {
      writer.upsertMovie(movie)
      metrics.recordWrite(Target.Movie, Op.Upsert, 1)
      lastMovie.update(movie._id, movie)
    }
    diffScreenings(movie._id, screenings)
  }

  private def diffScreenings(filmId: String, next: Seq[CityScreening]): Unit = {
    val nextById = next.map(s => s._id -> s).toMap
    val previous     = lastScreenings.getOrElse(filmId, Map.empty)
    var upserted = 0
    nextById.foreach { case (id, s) => if (!previous.get(id).contains(s)) { writer.upsertScreening(s); upserted += 1 } }
    val deletes = previous.keysIterator.filterNot(nextById.contains).toSeq
    deletes.foreach(writer.deleteScreening)
    if (upserted > 0)        metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    if (deletes.nonEmpty)    metrics.recordWrite(Target.Screening, Op.Delete, deletes.size)
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById)
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

  /** Re-project every source row (the diff keeps it cheap — only genuinely
   *  changed documents are written) and prune derived documents whose source film is gone.
   *
   *  Self-healing: the prune diffs the ACTUAL read model ids
   *  (`reader.findAllMovieIds`/`findAllScreeningRefs`) against the live source,
   *  NOT this process's in-memory `lastMovie`. The
   *  change stream delivers no deletes, so a film the worker re-keyed — a scrape
   *  pins a raw cinema year, enrichment resolves a different TMDB year, `settle`
   *  folds same-tmdbId variants — leaves its old `web_movies`/`web_screenings`
   *  documents behind. Those orphans may have been written by a PRIOR worker process
   *  and so were never in this process's `lastMovie`; a memory-based prune can't
   *  see them, which is how a re-key across a restart leaked a duplicate card
   *  permanently. Reading the read model's own ids closes that gap.
   *
   *  A row that fails to project must not abort the prune (the prune is what
   *  removes the duplicates), so each projection is guarded individually. */
  def reconcile(): Unit = lock.synchronized {
    // Stream the source rows one at a time (`foreachRecord`) rather than
    // materialising the whole ~13 MB corpus — the projector already holds the read
    // model resident in `lastMovie`/`lastScreenings`, and a second full copy on top
    // (plus the prune's read-model copies below) is the transient that exhausted the
    // worker's 320m heap on the 30-min reconcile tick. Only READY rows are part of
    // the read model — held-back rows are absent from `liveIds`, so they neither
    // project nor leave a stale document behind, and a row that becomes ready
    // between ticks gets projected on the next one.
    val liveIds = scala.collection.mutable.Set.empty[String]
    movieRepository.foreachRecord { row =>
      if (row.record.readyToProject) {
        liveIds += ReadModelProjection.filmId(row)
        try project(row)
        catch { case exception: Throwable =>
          logger.warn(s"read-model reconcile: a row failed to project, continuing: ${exception.getMessage}") }
      }
    }
    // Prune off id-only projections — the prune reads ids/filmIds, never payloads,
    // so projecting them server-side keeps the whole `web_movies`/`web_screenings`
    // corpus off the heap (the read model is already resident in memory anyway).
    reader.findAllMovieIds().iterator.filterNot(liveIds).foreach(deleteFilm)
    reader.findAllScreeningRefs().iterator.filterNot(ref => liveIds(ref.filmId)).foreach { ref =>
      writer.deleteScreening(ref._id)
      metrics.recordWrite(Target.Screening, Op.Delete, 1)
      lastScreenings.updateWith(ref.filmId)(_.map(_ - ref._id).filter(_.nonEmpty))
    }
  }

  def start(): Unit = if (enabled) {
    // Seed the last-projection state from the derived collections, so a restart
    // doesn't rewrite documents that are already correct.
    lock.synchronized {
      reader.findAllMovies().foreach(m => lastMovie.update(m._id, m))
      reader.findAllScreenings().groupBy(_.filmId).foreach { case (fid, ss) =>
        lastScreenings.update(fid, ss.map(s => s._id -> s).toMap)
      }
    }
    // The change-stream watch covers live changes from now on; the seeded state
    // above means incremental writes are no-ops for already-correct documents. The
    // full reconcile (which additionally prunes derived documents whose source row
    // vanished while the worker was down) is deferred to the first scheduled tick
    // so it doesn't compete with boot hydrate + the first scrape.
    watchHandle = movieRepository.watchUpserts(onMovieUpsert)
    scheduler.scheduleAtFixedRate(
      () => Try(reconcile()).recover { case exception => logger.warn(s"read-model reconcile tick failed: ${exception.getMessage}") },
      ReconcileBootDelaySeconds, ReconcileSeconds, TimeUnit.SECONDS)
    logger.info(s"ReadModelProjector started; first reconcile in ${ReconcileBootDelaySeconds}s, " +
      s"then every ${ReconcileSeconds}s; " +
      s"change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — reconcile only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
