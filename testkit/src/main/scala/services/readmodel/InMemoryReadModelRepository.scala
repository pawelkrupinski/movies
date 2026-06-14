package services.readmodel

import models.{CityScreening, ResolvedMovie}

import scala.collection.mutable

/**
 * In-memory [[ReadModelReader]] + [[ReadModelWriter]] for tests — full
 * read/write/watch semantics without a real Mongo cluster. Mirrors
 * `InMemoryMovieRepository`: every write is recorded (for write-through assertions)
 * and notified to any registered watcher, standing in for Mongo's change
 * stream (including deletes, which the read-model stream does deliver).
 */
class InMemoryReadModelRepository extends ReadModelReader with ReadModelWriter {

  private val moviesStore     = mutable.LinkedHashMap.empty[String, ResolvedMovie]
  private val screeningsStore = mutable.LinkedHashMap.empty[String, CityScreening]
  private val lock            = new AnyRef

  val movieUpserts     = mutable.ListBuffer.empty[ResolvedMovie]
  val movieDeletes     = mutable.ListBuffer.empty[String]
  val screeningUpserts = mutable.ListBuffer.empty[CityScreening]
  val screeningDeletes = mutable.ListBuffer.empty[String]
  // Combined, in-order log of every write — lets a test assert ordering across
  // the two collections (e.g. movie-doc-before-its-screenings). Entries:
  // "movie:<id>", "screening:<id>", "del-movie:<id>", "del-screening:<id>".
  val writeOrder       = mutable.ListBuffer.empty[String]

  @volatile private var movieWatcher:     Option[(ResolvedMovie => Unit, String => Unit)]  = None
  @volatile private var screeningWatcher: Option[(CityScreening => Unit, String => Unit)]   = None

  def enabled: Boolean = true

  def findAllMovies():     Seq[ResolvedMovie]  = lock.synchronized(moviesStore.values.toSeq)
  def findAllScreenings(): Seq[CityScreening]  = lock.synchronized(screeningsStore.values.toSeq)

  def upsertMovie(m: ResolvedMovie): Unit = {
    lock.synchronized { moviesStore.put(m._id, m); movieUpserts += m; writeOrder += s"movie:${m._id}" }
    movieWatcher.foreach { case (onUpsert, _) => onUpsert(m) }
  }

  def deleteMovie(id: String): Unit = {
    lock.synchronized { moviesStore.remove(id); movieDeletes += id; writeOrder += s"del-movie:$id" }
    movieWatcher.foreach { case (_, onDelete) => onDelete(id) }
  }

  def upsertScreening(s: CityScreening): Unit = {
    lock.synchronized { screeningsStore.put(s._id, s); screeningUpserts += s; writeOrder += s"screening:${s._id}" }
    screeningWatcher.foreach { case (onUpsert, _) => onUpsert(s) }
  }

  def deleteScreening(id: String): Unit = {
    lock.synchronized { screeningsStore.remove(id); screeningDeletes += id; writeOrder += s"del-screening:$id" }
    screeningWatcher.foreach { case (_, onDelete) => onDelete(id) }
  }

  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit): Option[AutoCloseable] = {
    movieWatcher = Some((onUpsert, onDelete))
    Some(new AutoCloseable { override def close(): Unit = movieWatcher = None })
  }

  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit): Option[AutoCloseable] = {
    screeningWatcher = Some((onUpsert, onDelete))
    Some(new AutoCloseable { override def close(): Unit = screeningWatcher = None })
  }

  def close(): Unit = ()
}
