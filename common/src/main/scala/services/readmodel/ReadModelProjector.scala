package services.readmodel

import models.{CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import services.movies.{MovieRepo, StoredMovieRecord}
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
 *     (`MovieRepo.watchUpserts`); each changed row is re-projected and the
 *     resulting docs are diffed against the last projection so only the docs
 *     that actually changed are written.
 *  2. RECONCILE (backstop) — a periodic full re-projection that also prunes
 *     derived docs whose source film has vanished (the change stream delivers
 *     no deletes; a fold-victim / `UnscreenedCleanup` removal is reconciled
 *     here).
 *
 * Minimal writes: the last-projected doc per film is kept in memory (hydrated
 * from the read model at boot, so a restart rewrites only what changed since
 * the projector last ran). A showtime-only edit re-projects to the same
 * `ResolvedMovie` (skipped) and the same screenings except one (written) — so
 * the web's change-stream delta is that one screening doc. The movie doc is
 * always written before its screenings, so a consumer joining screening→movie
 * sees the metadata first; the web join also tolerates the reverse order, so
 * neither side depends on it.
 *
 * The change-stream callback and the reconcile tick are serialised by one lock,
 * so the in-memory last-projection state needs no further synchronisation.
 */
class ReadModelProjector(
  movieRepo: MovieRepo,
  writer:    ReadModelWriter,
  reader:    ReadModelReader
) extends Stoppable with Logging {

  private val lastMovie      = scala.collection.mutable.Map.empty[String, ResolvedMovie]
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, CityScreening]]
  private val lock           = new AnyRef

  private val scheduler        = DaemonExecutors.scheduler("read-model-projector")
  private val ReconcileSeconds = Env.positiveLong("KINOWO_READMODEL_RECONCILE_SECONDS", 1800L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  def enabled: Boolean = writer.enabled && movieRepo.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit = lock.synchronized(project(stored))

  // Caller holds `lock`. Project the row and write only what changed, movie
  // doc before screenings.
  private def project(stored: StoredMovieRecord): Unit = {
    val (movie, screenings) = ReadModelProjection.project(stored)
    if (!lastMovie.get(movie._id).contains(movie)) {
      writer.upsertMovie(movie)
      lastMovie.update(movie._id, movie)
    }
    diffScreenings(movie._id, screenings)
  }

  private def diffScreenings(filmId: String, next: Seq[CityScreening]): Unit = {
    val nextById = next.map(s => s._id -> s).toMap
    val prev     = lastScreenings.getOrElse(filmId, Map.empty)
    nextById.foreach { case (id, s) => if (!prev.get(id).contains(s)) writer.upsertScreening(s) }
    prev.keysIterator.filterNot(nextById.contains).foreach(writer.deleteScreening)
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById)
  }

  private def deleteFilm(filmId: String): Unit = {
    writer.deleteMovie(filmId)
    lastScreenings.getOrElse(filmId, Map.empty).keysIterator.foreach(writer.deleteScreening)
    lastMovie.remove(filmId)
    lastScreenings.remove(filmId)
  }

  /** Re-project every source row (the diff keeps it cheap — only genuinely
   *  changed docs are written) and prune derived films whose source row is
   *  gone. */
  def reconcile(): Unit = lock.synchronized {
    val rows    = movieRepo.findAll()
    val liveIds = rows.iterator.map(ReadModelProjection.filmId).toSet
    rows.foreach(project)
    (lastMovie.keySet.toSet -- liveIds).foreach(deleteFilm)
  }

  def start(): Unit = if (enabled) {
    // Seed the last-projection state from the derived collections, so a restart
    // doesn't rewrite docs that are already correct.
    lock.synchronized {
      reader.findAllMovies().foreach(m => lastMovie.update(m._id, m))
      reader.findAllScreenings().groupBy(_.filmId).foreach { case (fid, ss) =>
        lastScreenings.update(fid, ss.map(s => s._id -> s).toMap)
      }
    }
    reconcile()
    watchHandle = movieRepo.watchUpserts(onMovieUpsert)
    scheduler.scheduleAtFixedRate(
      () => Try(reconcile()).recover { case ex => logger.warn(s"read-model reconcile tick failed: ${ex.getMessage}") },
      ReconcileSeconds, ReconcileSeconds, TimeUnit.SECONDS)
    logger.info(s"ReadModelProjector started; reconcile every ${ReconcileSeconds}s; " +
      s"change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — reconcile only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repo not enabled).")

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
