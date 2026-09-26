package services.movies

import models.{Showtime, SourceData}

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
 * Shared from testkit because the fixpoint ledger (`tools.ChurnLedger`) needs the same
 * count the paging and rewrite guards do: `writes` is every write that reached the store,
 * which is what a pass over unchanged input must not produce — the in-memory stores ring
 * their change listeners only on a REAL change, so an identical rewrite (the
 * `43b595136` shape: Mongo still pays for it) is visible only here.
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
  /** Every write of any shape that reached the store. */
  val writes           = new AtomicInteger(0)
  private def write[A](body: => A): A = { writes.incrementAndGet(); body }

  def replaceFilm(filmId: String, slots: Map[String, ListedShowtimes],
                  stored: Option[Map[String, ListedShowtimes]] = None): WriteOutcome = {
    replaceFilmCalls.incrementAndGet()
    write(underlying.replaceFilm(filmId, slots, stored))
  }

  def findListedForFilmChecked(filmId: String): (Map[String, ListedShowtimes], Boolean) =
    underlying.findListedForFilmChecked(filmId)
  override def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, Seq[Showtime]]], Boolean) = {
    batchReadCalls.incrementAndGet()
    underlying.findForFilmsChecked(filmIds)
  }
  def findAll(): Map[String, Map[String, Seq[Showtime]]] = {
    findAllCalls.incrementAndGet()
    underlying.findAll()
  }
  def upsertSlot(filmId: String, slotKey: String, row: ListedShowtimes): WriteOutcome =
    write(underlying.upsertSlot(filmId, slotKey, row))
  def deleteSlot(filmId: String, slotKey: String): WriteOutcome = write(underlying.deleteSlot(filmId, slotKey))
  def deleteFilm(filmId: String): WriteOutcome                  = write(underlying.deleteFilm(filmId))
  def filmIdsChecked(): (Set[String], Boolean)          = underlying.filmIdsChecked()
  def deleteFilms(filmIds: Set[String]): Long           = write(underlying.deleteFilms(filmIds))
  def rowIdsChecked(): (Set[String], Boolean)           = underlying.rowIdsChecked()
  def rowWrittenAtChecked(): (Map[String, java.time.Instant], Boolean) = underlying.rowWrittenAtChecked()
  def deleteRows(ids: Set[String]): Long                = write(underlying.deleteRows(ids))
  def rowListingKeysChecked(): (Map[String, Option[String]], Boolean) = underlying.rowListingKeysChecked()
  def rowIdsForListingKeyChecked(listingKey: String): (Set[String], Boolean) = underlying.rowIdsForListingKeyChecked(listingKey)
  override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    underlying.watchApplied(onChange, demand)
  override def close(): Unit = underlying.close()

  def reset(): Unit = { replaceFilmCalls.set(0); batchReadCalls.set(0); findAllCalls.set(0); writes.set(0) }
}

/** The slots twin of [[CountingScreeningsRepository]]. */
final class CountingSlotsRepository(underlying: SlotsRepository) extends SlotsRepository {
  val replaceFilmCalls = new AtomicInteger(0)
  val batchReadCalls   = new AtomicInteger(0)
  val findAllCalls     = new AtomicInteger(0)
  val writes           = new AtomicInteger(0)
  private def write[A](body: => A): A = { writes.incrementAndGet(); body }

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
                  stored: Option[Map[String, SourceData]] = None): WriteOutcome = {
    replaceFilmCalls.incrementAndGet()
    write(underlying.replaceFilm(filmId, slots, stored))
  }
  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): WriteOutcome =
    write(underlying.upsertSlot(filmId, slotKey, slot))
  def deleteSlot(filmId: String, slotKey: String): WriteOutcome = write(underlying.deleteSlot(filmId, slotKey))
  def deleteFilm(filmId: String): WriteOutcome                  = write(underlying.deleteFilm(filmId))
  def filmIdsChecked(): (Set[String], Boolean)          = underlying.filmIdsChecked()
  def deleteFilms(filmIds: Set[String]): Long           = write(underlying.deleteFilms(filmIds))
  def rowIdsChecked(): (Set[String], Boolean)           = underlying.rowIdsChecked()
  def rowWrittenAtChecked(): (Map[String, java.time.Instant], Boolean) = underlying.rowWrittenAtChecked()
  def deleteRows(ids: Set[String]): Long                = write(underlying.deleteRows(ids))
  def rowListingKeysChecked(): (Map[String, Option[String]], Boolean) = underlying.rowListingKeysChecked()
  def rowIdsForListingKeyChecked(listingKey: String): (Set[String], Boolean) = underlying.rowIdsForListingKeyChecked(listingKey)
  override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    underlying.watchApplied(onChange, demand)
  override def close(): Unit                            = underlying.close()

  def reset(): Unit = { replaceFilmCalls.set(0); batchReadCalls.set(0); findAllCalls.set(0); writes.set(0) }
}
