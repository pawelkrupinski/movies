package services.movies

import scala.concurrent.Await
import scala.concurrent.duration.*

import org.bson.conversions.Bson
import org.mongodb.scala.model.{Filters, Projections, Sorts}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture}
import play.api.Logging
import tools.KeysetScan

/** THE KEYSET PAGING of the `movies` collection by `_id`, which every corpus scan of [[MongoMovieRepository]] reads
 *  through — the documents, and the bare `_id`s. Reads one `batchSize`-row page at a time — each a fresh, bounded
 *  `find(_id > lastSeen).sort(_id).limit(n)` — and hands every decoded batch on. Two guarantees every caller relies on:
 *
 *   - Exactly-once: `_id` is unique and immutable and the `gt`/sort run server-side,
 *     so a concurrent write (the worker re-keys years, …) can
 *     neither resurface a visited row nor hide one — no duplicate at a page boundary,
 *     no skip. (The prior single `_id`-sorted cursor gave the same guarantee.)
 *   - Bounded: no single cursor buffers the entire corpus, so the async driver's
 *     synchronous read-completion chain can't recurse into a `StackOverflowError`
 *     (see [[MongoMovieRepository.findAll]]).
 *
 *  Each BATCH read is retried independently (keyset pagination makes every batch a
 *  fresh, idempotent `find`), `batchAttempts` times from `batchBackoff`, before the scan is declared incomplete —
 *  complete only when the scan reached the last page. Rows delivered before a batch that still failed after
 *  its retries were handed on, so a PRUNING caller must treat an incomplete scan as
 *  "not the complete corpus" and skip its destructive step. */
private[movies] final class MoviesKeysetPages(batchSize: Int, batchAttempts: Int, batchBackoff: FiniteDuration,
                                              decodeFailures: DecodeFailureMetrics) extends Logging {

  /** Every document of `collection` matching `filter`, a page at a time, handed to `onBatch`: incomplete when
   *  there is no collection. */
  def documents(collection: Option[MongoCollection[StoredMovieDto]], filter: Bson)(onBatch: Seq[StoredMovieDto] => Unit): tools.ScanOutcome =
    collection match {
      case Some(c) =>
        scan[StoredMovieDto]("keyset", _._id, countIfUndecodable) { (afterId, limit) =>
          val page = afterId.fold(filter)(id => Filters.and(filter, Filters.gt("_id", id)))
          Await.result(
            c.find(page).sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Films).toFuture(), 60.seconds)
        }(onBatch)
      case None => tools.ScanOutcome.Incomplete(new tools.IncompleteScanException("no movies collection"))
    }

  /** Every `_id` of `collection`, projected to `_id` alone — a few thousand short strings, not the documents.
   *  `None` when a page still failed after its retries. */
  def ids(collection: MongoCollection[StoredMovieDto]): Option[Set[String]] = {
    val ids = Set.newBuilder[String]
    val complete = scan[String]("id keyset", identity) { (afterId, limit) =>
      val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
      Await.result(collection.find[Document](filter).projection(Projections.include("_id"))
        .sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), 60.seconds)
        .map(_("_id").asString.getValue)
    }(ids ++= _)
    Option.when(complete.isComplete)(ids.result())
  }

  /** A read that failed on a document the codec refused: counted, since it fails every read that
   *  meets it (one such document leaves the whole corpus scan incomplete) and is otherwise a WARN. */
  def countIfUndecodable(failure: Throwable): Unit =
    if (DecodeFailureMetrics.isDecodeFailure(failure))
      decodeFailures.recordDecodeFailure(DecodeFailureMetrics.SourceMoviesCollection)

  /** The keyset scan both reads share, with this store's page size and per-batch retry budget. A scan still
   *  failing after its retries is logged, then handed to `onFailure`. */
  private def scan[A](what: String, keyOf: A => String, onFailure: Throwable => Unit = _ => ())(
      fetchPage: (Option[String], Int) => Seq[A])(onBatch: Seq[A] => Unit): tools.ScanOutcome =
    KeysetScan.scan[A](
      label          = s"MovieRepository $what batch",
      batchSize      = batchSize,
      maxAttempts    = batchAttempts,
      initialBackoff = batchBackoff,
      keyOf          = keyOf,
      fetchPage      = fetchPage,
      onIncomplete   = exception => {
        logger.warn(s"MovieRepository $what scan failed after retries: " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
        onFailure(exception)
      }
    )(onBatch)
}
