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
  reader:    ReadModelReader
) extends Stoppable with Logging {

  private val lastMovie      = scala.collection.mutable.Map.empty[String, ResolvedMovie]
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, CityScreening]]
  private val lock           = new AnyRef

  private val scheduler        = DaemonExecutors.scheduler("read-model-projector")
  private val ReconcileSeconds = Env.positiveLong("KINOWO_READMODEL_RECONCILE_SECONDS", 1800L)
  // The boot reconcile is a full `movieRepository.findAll()` + project-every-row scan.
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
      lastMovie.update(movie._id, movie)
    }
    diffScreenings(movie._id, screenings)
  }

  private def diffScreenings(filmId: String, next: Seq[CityScreening]): Unit = {
    val nextById = next.map(s => s._id -> s).toMap
    val previous     = lastScreenings.getOrElse(filmId, Map.empty)
    nextById.foreach { case (id, s) => if (!previous.get(id).contains(s)) writer.upsertScreening(s) }
    previous.keysIterator.filterNot(nextById.contains).foreach(writer.deleteScreening)
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById)
  }

  private def deleteFilm(filmId: String): Unit = {
    writer.deleteMovie(filmId)
    lastScreenings.getOrElse(filmId, Map.empty).keysIterator.foreach(writer.deleteScreening)
    lastMovie.remove(filmId)
    lastScreenings.remove(filmId)
  }

  /** Re-project every source row (the diff keeps it cheap — only genuinely
   *  changed documents are written) and prune derived documents whose source film is gone.
   *
   *  Self-healing: the prune diffs the ACTUAL read model (`reader.findAll*`)
   *  against the live source, NOT this process's in-memory `lastMovie`. The
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
    // Only READY rows are part of the read model — held-back rows are absent
    // from `liveIds`, so they neither project nor leave a stale document behind, and
    // a row that becomes ready between ticks gets projected on the next one.
    val ready   = movieRepository.findAll().filter(_.record.readyToProject)
    val liveIds = ready.iterator.map(ReadModelProjection.filmId).toSet
    ready.foreach { row =>
      try project(row)
      catch { case exception: Throwable =>
        logger.warn(s"read-model reconcile: a row failed to project, continuing: ${exception.getMessage}") }
    }
    reader.findAllMovies().iterator.map(_._id).filterNot(liveIds).foreach(deleteFilm)
    reader.findAllScreenings().iterator.filterNot(s => liveIds(s.filmId)).foreach { s =>
      writer.deleteScreening(s._id)
      lastScreenings.updateWith(s.filmId)(_.map(_ - s._id).filter(_.nonEmpty))
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
