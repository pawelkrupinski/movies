package services.movies

import java.util.concurrent.CopyOnWriteArrayList
import scala.jdk.CollectionConverters._

/**
 * The in-memory store both side-collection fakes ARE: `filmId -> (slotKey -> row)` under
 * one monitor, with a change ring that fires only for a genuine change.
 *
 * [[InMemoryScreeningsRepository]] and [[InMemorySlotsRepository]] differ only in the row
 * type, and the rule they both owe their trait — "a no-op write does NOT ring", mirroring
 * the Mongo stores' write guards ([[SlotKeyed.changedRows]], the point read in
 * `upsertSlot`) — is the kind of thing that drifts when it is written twice. The slots
 * fake had, in fact, no ring at all before `movie_slots` grew a cursor. Each mutation
 * answers whether it changed anything and rings only then, OUTSIDE the lock.
 *
 * One monitor guards all mutations — fine at test/dev scale.
 */
final class InMemorySlotRows[A] {

  private val byFilm    = scala.collection.mutable.Map.empty[String, Map[String, A]]
  private val lock      = new Object
  private val listeners = new CopyOnWriteArrayList[String => Unit]()

  def forFilm(filmId: String): Map[String, A] = lock.synchronized(byFilm.getOrElse(filmId, Map.empty))

  def all(): Map[String, Map[String, A]] = lock.synchronized(byFilm.toMap)

  /** Set a film's rows to EXACTLY `rows` — an empty map removes the film. */
  def replaceFilm(filmId: String, rows: Map[String, A]): Unit =
    ringIf(filmId, lock.synchronized {
      if (byFilm.getOrElse(filmId, Map.empty) == rows) false
      else { if (rows.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, rows); true }
    })

  def upsert(filmId: String, slotKey: String, row: A): Unit =
    ringIf(filmId, lock.synchronized {
      val current = byFilm.getOrElse(filmId, Map.empty)
      if (current.get(slotKey).contains(row)) false
      else { byFilm.update(filmId, current + (slotKey -> row)); true }
    })

  def delete(filmId: String, slotKey: String): Unit =
    ringIf(filmId, lock.synchronized {
      val current = byFilm.getOrElse(filmId, Map.empty)
      if (!current.contains(slotKey)) false
      else {
        val next = current - slotKey
        if (next.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, next)
        true
      }
    })

  def deleteFilm(filmId: String): Unit =
    ringIf(filmId, lock.synchronized(byFilm.remove(filmId).isDefined))

  /** Ring `onChange(filmId)` on every genuine change until the handle is closed. Listeners
   *  are rung synchronously, so there is no backlog and nothing for a demand window to
   *  bound — which is why the repositories' `watch` accept one only to honour their trait. */
  def watch(onChange: String => Unit): AutoCloseable = {
    listeners.add(onChange)
    new AutoCloseable { override def close(): Unit = { listeners.remove(onChange); () } }
  }

  private def ringIf(filmId: String, changed: Boolean): Unit =
    if (changed) listeners.asScala.foreach(_(filmId))
}
