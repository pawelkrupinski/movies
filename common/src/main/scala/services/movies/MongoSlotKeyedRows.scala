package services.movies

import com.mongodb.WriteConcern
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

/**
 * A Mongo side collection keyed by [[SlotKeyed]] — `screenings` and `movie_slots` share the addressing, so what follows
 * was written twice, once per store, alike but for the store's name: opening the collection, every write's exception
 * handling ([[RepositoryWrite]]), the slot and film deletes and what they tell the [[RemovalAudit]], the
 * whole-collection reads and bulk deletes [[StrandedSideRows]] sweeps through, and the two row reads both stores make
 * (the keyset scan of every row and one row by its composite `_id`).
 *
 * @param sharedDb       the database; `None` holds no rows, and every write is declined.
 * @param storeName      the store as its log lines name it (`ScreeningsRepository`).
 * @param collectionName the collection, as its write failures are counted and its removals audited.
 * @param auditedRows    what the removal audit calls a film's rows here (`screenings`, `slots`).
 */
trait MongoSlotKeyedRows[Dto: ClassTag](
  sharedDb:       Option[MongoDatabase],
  storeName:      String,
  collectionName: String,
  auditedRows:    String,
  paging:         SlotKeyed.Paging,
  writeMetrics:   RepositoryWriteMetrics
) extends SlotKeyedRows with play.api.Logging {
  /** The collection, with the movie codecs, unjournaled W1 writes and the slot-keyed indexes; `None` (no database)
   *  holds no rows. */
  protected final lazy val rowsCollection: Option[MongoCollection[Dto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[Dto](collectionName)
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    SlotKeyed.ensureIndexes(c)
    c
  }

  /** Every write's exception handling — see [[RepositoryWrite]]. */
  protected def write(op: String, what: => String)(body: => WriteOutcome): WriteOutcome =
    RepositoryWrite.attempt(collectionName, op, what, writeMetrics, logger)(body)

  def deleteSlot(filmId: String, slotKey: String): WriteOutcome = onRows[WriteOutcome](WriteOutcome.Declined("no-store")) { c =>
    write("deleteSlot", s"$storeName.deleteSlot($filmId,$slotKey)") {
      Await.result(c.deleteOne(Filters.eq("_id", SlotKeyed.idOf(filmId, slotKey))).toFuture(), 10.seconds)
      RemovalAudit.slotRemoved(s"$collectionName.deleteSlot", filmId, slotKey, "slot-deleted")
      WriteOutcome.Written
    }
  }

  def deleteFilm(filmId: String): WriteOutcome = onRows[WriteOutcome](WriteOutcome.Declined("no-store")) { c =>
    write("deleteFilm", s"$storeName.deleteFilm($filmId)") {
      val deleted = Await.result(c.deleteMany(SlotKeyed.filmFilter(filmId)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0)
        RemovalAudit.screeningsCleared(s"$collectionName.deleteFilm", filmId, deleted.toInt, whole = true,
          reason = "film-deleted", what = auditedRows)
      WriteOutcome.Written
    }
  }

  /** Tell the removal audit the `pruned` stale rows a `replaceFilm` of `filmId` dropped — all of them when `whole`. */
  protected def auditStalePrune(filmId: String, pruned: Long, whole: Boolean): Unit =
    if (pruned > 0)
      RemovalAudit.screeningsCleared(s"$collectionName.replaceFilm", filmId, pruned.toInt, whole = whole,
        reason = "stale-slot-prune", what = auditedRows)

  private def onRows[A](none: => A)(read: MongoCollection[Dto] => A): A = rowsCollection.fold(none)(read)

  def filmIdsChecked(): tools.ReadOutcome[Set[String]] =
    onRows(tools.ReadOutcome.Answered(Set.empty[String]))(SlotKeyed.distinctFilmIdsChecked(_, storeName, logger.warn(_)))

  def rowIdsChecked(): tools.ReadOutcome[Set[String]] =
    onRows(tools.ReadOutcome.Answered(Set.empty[String]))(SlotKeyed.rowIdsChecked(_, storeName, logger.warn(_), paging))

  def rowWrittenAtChecked(): tools.ReadOutcome[Map[String, java.time.Instant]] =
    onRows(tools.ReadOutcome.Answered(Map.empty[String, java.time.Instant]))(SlotKeyed.rowWrittenAtChecked(_, storeName, logger.warn(_), paging))

  override def existingRowIdsChecked(ids: Set[String]): tools.ReadOutcome[Set[String]] =
    onRows(tools.ReadOutcome.Answered(Set.empty[String]))(SlotKeyed.existingRowIdsChecked(_, ids, storeName, logger.warn(_)))

  def deleteRows(ids: Set[String]): Long = onRows(0L)(SlotKeyed.deleteRows(_, ids, collectionName, writeMetrics, logger))

  def deleteFilms(filmIds: Set[String]): Long = onRows(0L)(SlotKeyed.deleteFilms(_, filmIds, collectionName, writeMetrics, logger))

  /** Every row of `c`, keyset-paged by `_id` ([[tools.KeysetScan]]) rather than pulled through ONE unbounded
   *  `find().toFuture()`, which recursed the async Mongo driver into a `StackOverflowError` on a driver I/O thread
   *  once the collection grew (Sentry KINOWO-19). `replies` rows a reply. An INCOMPLETE scan (a page still failing
   *  after retries) is a failed read, never "no film has a row". */
  protected def allRowsChecked(c: MongoCollection[Dto], replies: Int)(keyOf: Dto => String): tools.ReadOutcome[Vector[Dto]] =
    tools.KeysetScan.collect[Dto, Dto](
      label          = s"$storeName keyset batch",
      batchSize      = paging.batchSize,
      maxAttempts    = paging.attempts,
      initialBackoff = paging.backoff,
      keyOf          = keyOf,
      fetchPage      = (afterId, limit) => Await.result(
        c.find(afterId.fold(Filters.empty())(Filters.gt("_id", _))).sort(Sorts.ascending("_id")).limit(limit).batchSize(replies).toFuture(), 60.seconds),
      onIncomplete   = exception =>
        logger.warn(s"$storeName.findAll keyset scan failed after retries: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
    )(Seq(_))

  /** One stored row, or None when it is absent OR unreadable — the two cases a point write treats alike, because both
   *  mean "we cannot say this write is redundant". */
  protected def storedRow(c: MongoCollection[Dto], filmId: String, slotKey: String): Option[Dto] =
    Try(Await.result(c.find(Filters.eq("_id", SlotKeyed.idOf(filmId, slotKey))).first().toFuture(), 10.seconds)).toOption.flatMap(Option(_))
}
