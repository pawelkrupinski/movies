package integration

import models.{Showtime, SourceData}
import services.movies.{ChangeStreamDemand, ScreeningsRepository, SlotsRepository}

import java.util.concurrent.atomic.AtomicInteger

/**
 * Pass-through decorators over the two side-collection stores that COUNT which read and
 * write shapes their caller reached for.
 *
 * Three copies of this had appeared across the integration specs — one counting whole-
 * collection versus batched reads for the paging guard, one counting whole-film rewrites,
 * one counting the batch read to prove a scan skipped it — each re-declaring every other
 * method just to delegate it. They all broke together the first time either trait grew a
 * parameter, which is the usual argument for having one.
 *
 * Every method delegates; the counters are the only behaviour. Each spec reads the counters
 * it cares about and ignores the rest, so a new question needs a counter here rather than a
 * fourth decorator. `findForFilmsChecked` and `watch` are delegated EXPLICITLY rather than
 * inherited: the trait defaults are the un-batched / un-pushed fallbacks, and silently
 * swapping the real store's batch read for a per-id loop would make the decorator a
 * different repository from the one under test.
 */
final class CountingScreeningsRepository(underlying: ScreeningsRepository) extends ScreeningsRepository {
  /** Whole-film rewrites — `MovieRepository.upsert`'s write path. */
  val replaceFilmCalls = new AtomicInteger(0)
  /** Batched per-page reads — what a corpus scan issues, one per page. */
  val batchReadCalls   = new AtomicInteger(0)
  /** Whole-collection reads — what a scan must NEVER issue. */
  val findAllCalls     = new AtomicInteger(0)

  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]],
                  stored: Option[Map[String, Seq[Showtime]]] = None): Unit = {
    replaceFilmCalls.incrementAndGet()
    underlying.replaceFilm(filmId, slots, stored)
  }

  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean) =
    underlying.findForFilmChecked(filmId)
  override def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, Seq[Showtime]]], Boolean) = {
    batchReadCalls.incrementAndGet()
    underlying.findForFilmsChecked(filmIds)
  }
  def findAll(): Map[String, Map[String, Seq[Showtime]]] = {
    findAllCalls.incrementAndGet()
    underlying.findAll()
  }
  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit =
    underlying.upsertSlot(filmId, slotKey, showtimes)
  def deleteSlot(filmId: String, slotKey: String): Unit = underlying.deleteSlot(filmId, slotKey)
  def deleteFilm(filmId: String): Unit                  = underlying.deleteFilm(filmId)
  override def watch(onChange: String => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    underlying.watch(onChange, demand)
  override def close(): Unit = underlying.close()

  def reset(): Unit = { replaceFilmCalls.set(0); batchReadCalls.set(0); findAllCalls.set(0) }
}

/** The slots twin of [[CountingScreeningsRepository]]. */
final class CountingSlotsRepository(underlying: SlotsRepository) extends SlotsRepository {
  val replaceFilmCalls = new AtomicInteger(0)
  val batchReadCalls   = new AtomicInteger(0)
  val findAllCalls     = new AtomicInteger(0)

  def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean) =
    underlying.findForFilmChecked(filmId)
  override def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, SourceData]], Boolean) = {
    batchReadCalls.incrementAndGet()
    underlying.findForFilmsChecked(filmIds)
  }
  def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean) = {
    findAllCalls.incrementAndGet()
    underlying.findAllChecked()
  }
  def replaceFilm(filmId: String, slots: Map[String, SourceData],
                  stored: Option[Map[String, SourceData]] = None): Boolean = {
    replaceFilmCalls.incrementAndGet()
    underlying.replaceFilm(filmId, slots, stored)
  }
  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit =
    underlying.upsertSlot(filmId, slotKey, slot)
  def deleteSlot(filmId: String, slotKey: String): Unit = underlying.deleteSlot(filmId, slotKey)
  def deleteFilm(filmId: String): Unit                  = underlying.deleteFilm(filmId)
  override def close(): Unit                            = underlying.close()

  def reset(): Unit = { replaceFilmCalls.set(0); batchReadCalls.set(0); findAllCalls.set(0) }
}
