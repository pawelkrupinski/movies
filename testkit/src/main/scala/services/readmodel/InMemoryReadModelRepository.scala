package services.readmodel

import models.{CityScreening, ResolvedMovie}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable

/**
 * In-memory [[ReadModelReader]] + [[ReadModelWriter]] for tests — full
 * read/write/watch semantics without a real Mongo cluster. Mirrors
 * `InMemoryMovieRepository`: every write is recorded (for write-through assertions)
 * and notified to any registered watcher, standing in for Mongo's change
 * stream (including deletes, which the read-model stream does deliver).
 *
 * Stream liveness is modelled too: `watch*` hands back a [[StreamSubscription]]
 * whose `live` flips false on close or via [[failMovieStream]] /
 * [[failScreeningStream]], so a test can drive the consumer's stream-died
 * fallback. `findAll*Calls` count the full reloads for asserting the backstop
 * skips them while the model is consistent.
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
  // the two collections (e.g. movie-document-before-its-screenings). Entries:
  // "movie:<id>", "screening:<id>", "del-movie:<id>", "del-screening:<id>".
  val writeOrder       = mutable.ListBuffer.empty[String]

  val findAllMoviesCalls     = new AtomicInteger(0)
  val findAllScreeningsCalls = new AtomicInteger(0)

  @volatile private var movieWatcher:     Option[(ResolvedMovie => Unit, String => Unit)]  = None
  @volatile private var screeningWatcher: Option[(CityScreening => Unit, String => Unit)]   = None
  private val movieStreamLive     = new AtomicBoolean(false)
  private val screeningStreamLive = new AtomicBoolean(false)

  /** Simulate the change stream terminally ending — the subscription stays
   *  registered (so already-applied state is intact) but reports `live == false`. */
  def failMovieStream():     Unit = movieStreamLive.set(false)
  def failScreeningStream(): Unit = screeningStreamLive.set(false)

  def enabled: Boolean = true

  def findAllMovies():     Seq[ResolvedMovie]  = { findAllMoviesCalls.incrementAndGet();     lock.synchronized(moviesStore.values.toSeq) }
  def findAllScreenings(): Seq[CityScreening]  = { findAllScreeningsCalls.incrementAndGet(); lock.synchronized(screeningsStore.values.toSeq) }

  def countMovies():     Long = lock.synchronized(moviesStore.size.toLong)
  def countScreenings(): Long = lock.synchronized(screeningsStore.size.toLong)

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

  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit): Option[StreamSubscription] = {
    movieWatcher = Some((onUpsert, onDelete))
    movieStreamLive.set(true)
    Some(subscription(movieStreamLive, { movieWatcher = None }))
  }

  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit): Option[StreamSubscription] = {
    screeningWatcher = Some((onUpsert, onDelete))
    screeningStreamLive.set(true)
    Some(subscription(screeningStreamLive, { screeningWatcher = None }))
  }

  private def subscription(liveFlag: AtomicBoolean, onClose: => Unit): StreamSubscription =
    new StreamSubscription {
      override def live: Boolean = liveFlag.get()
      override def close(): Unit = { liveFlag.set(false); onClose }
    }

  def close(): Unit = ()
}
