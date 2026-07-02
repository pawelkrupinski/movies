package services.movies

import models.Showtime

import java.util.concurrent.CopyOnWriteArrayList
import scala.jdk.CollectionConverters._

/**
 * Per-cinema showtimes, split out of the embedded `movies.sourceData` map into
 * their own `screenings` collection so a showtime change no longer rewrites the
 * (1–2 MB, 150–250-slot) film document that the change stream re-decodes on every
 * write. One row per cinema slot, keyed by `(filmId, slotKey)` where `slotKey` is
 * the slot's wire form — `Source.displayName` (`"<cinema>␟<titleKey>"`) — i.e. the
 * same key `movies.sourceData` uses.
 *
 * The domain keeps `SourceData.showtimes` in memory; this repository is purely the
 * storage split. `MongoMovieRepository` writes/reads/streams through it and stitches
 * showtimes back into each slot on read, so every downstream consumer (projection,
 * ingest merge, serving, metrics) is unchanged.
 *
 * Two implementations share this contract: [[MongoScreeningsRepository]] (durable,
 * multi-instance-safe — the authority for showtimes) and [[InMemoryScreeningsRepository]]
 * (tests / Mongo-less dev). There is no business logic here — both just store — so
 * neither needs to re-implement any rule.
 */
trait ScreeningsRepository {

  /** Every slot's showtimes for one film: `slotKey -> showtimes`. Empty when the
   *  film has no recorded screenings. */
  def findForFilm(filmId: String): Map[String, Seq[Showtime]]

  /** Every film's screenings: `filmId -> (slotKey -> showtimes)`. For the boot
   *  hydrate / `findAll` read-stitch. */
  def findAll(): Map[String, Map[String, Seq[Showtime]]]

  /** Set a film's screenings to EXACTLY `slots` — upsert those present, delete any
   *  no longer present. The whole-record write path (`MovieRepository.upsert`). */
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit

  /** Upsert one slot's showtimes — the per-slot patch write path
   *  (`MovieRepository.updateIfPresent`). */
  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit

  /** Drop one slot's screenings (the slot left the film's listings). */
  def deleteSlot(filmId: String, slotKey: String): Unit

  /** Drop all of a film's screenings (the film was deleted / re-keyed). */
  def deleteFilm(filmId: String): Unit

  /** Push: ring `onChange(filmId)` whenever a film's screenings actually change, so
   *  the change-stream fanout can re-stitch + re-dispatch that film. A no-op write
   *  (unchanged showtimes) does NOT ring — mirroring the `movies` no-op guard.
   *  Returns a handle to stop watching, or None when this impl can't push. */
  def watch(onChange: String => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/**
 * In-memory `ScreeningsRepository` for tests and Mongo-less dev. Mirrors
 * [[MongoScreeningsRepository]]'s semantics: idempotent per-slot writes, per-film
 * grouping, and a change ring that fires only on a real change. One monitor guards
 * all mutations — fine at test/dev scale.
 */
class InMemoryScreeningsRepository extends ScreeningsRepository {

  // filmId -> (slotKey -> showtimes)
  private val byFilm    = scala.collection.mutable.Map.empty[String, Map[String, Seq[Showtime]]]
  private val lock      = new Object
  private val listeners = new CopyOnWriteArrayList[String => Unit]()

  def findForFilm(filmId: String): Map[String, Seq[Showtime]] =
    lock.synchronized(byFilm.getOrElse(filmId, Map.empty))

  def findAll(): Map[String, Map[String, Seq[Showtime]]] =
    lock.synchronized(byFilm.toMap)

  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit = {
    val changed = lock.synchronized {
      if (byFilm.getOrElse(filmId, Map.empty) == slots) false
      else { if (slots.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, slots); true }
    }
    if (changed) ring(filmId)
  }

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = {
    val changed = lock.synchronized {
      val cur = byFilm.getOrElse(filmId, Map.empty)
      if (cur.get(slotKey).contains(showtimes)) false
      else { byFilm.update(filmId, cur + (slotKey -> showtimes)); true }
    }
    if (changed) ring(filmId)
  }

  def deleteSlot(filmId: String, slotKey: String): Unit = {
    val changed = lock.synchronized {
      val cur = byFilm.getOrElse(filmId, Map.empty)
      if (!cur.contains(slotKey)) false
      else { val next = cur - slotKey; if (next.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, next); true }
    }
    if (changed) ring(filmId)
  }

  def deleteFilm(filmId: String): Unit = {
    val changed = lock.synchronized(byFilm.remove(filmId).isDefined)
    if (changed) ring(filmId)
  }

  override def watch(onChange: String => Unit): Option[AutoCloseable] = {
    listeners.add(onChange)
    Some(new AutoCloseable { override def close(): Unit = { listeners.remove(onChange); () } })
  }

  // Ring outside the lock, only for genuine changes (see the per-method guards).
  private def ring(filmId: String): Unit = listeners.asScala.foreach(_(filmId))
}
