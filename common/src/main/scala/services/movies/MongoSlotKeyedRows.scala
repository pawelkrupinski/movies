package services.movies

import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

/**
 * [[SlotKeyedRows]]' whole-collection reads and bulk deletes over a Mongo side collection — what
 * [[StrandedSideRows]] sweeps through — and the two row reads both stores make: the keyset scan of every row and
 * one row by its composite `_id`. `screenings` and `movie_slots` share the addressing ([[SlotKeyed]]), so each of
 * these was written twice, once per store, alike but for the store's name.
 *
 * @param storeName      the store as its log lines name it (`ScreeningsRepository`).
 * @param collectionName the collection, as its write failures are counted.
 */
trait MongoSlotKeyedRows[Dto: ClassTag](
  storeName:      String,
  collectionName: String,
  paging:         SlotKeyed.Paging,
  writeMetrics:   RepositoryWriteMetrics
) extends SlotKeyedRows with play.api.Logging {
  /** The collection; `None` (no database) holds no rows. */
  protected def rowsCollection: Option[MongoCollection[Dto]]

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
