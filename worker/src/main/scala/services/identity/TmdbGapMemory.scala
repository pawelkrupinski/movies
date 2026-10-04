package services.identity

import org.bson.{BsonDocument, BsonInt64}

import java.time.Clock
import scala.concurrent.duration._

/**
 * The fill's memory of what it asked TMDB and got no answer to — a request that failed, or one the
 * normalized store takes no answer from (a search TMDB answered 404) — so a gap TMDB cannot answer
 * is asked again a day later, not every round. The model still reads it as a gap (`Unknown`, never
 * "no film"): only the fill's asking backs off. What the raw capture's stored failure did, before
 * TMDB's answers stopped being kept raw.
 *
 * A marker beside the answers (`tmdb_queries`, `unanswered|…`), never on one: a real answer that
 * arrives later is read as it is, and the gap is gone. A marker past the store sweep's grace is
 * deleted by it: long past [[RetryAfter]], it holds nothing back, and a question the corpus stopped
 * asking would otherwise keep its marker for good.
 *
 * Only a DURABLE non-answer ([[unanswered]]: TMDB's 404/410, or an answer the store takes nothing
 * from) waits the day. A question whose read FAILED ([[failed]]: a 5xx, a timeout, a 429 — what
 * `ReadOutcome.classify` calls a failure) learned nothing, and is asked again after
 * [[FirstFailureRetry]], the wait doubling while it keeps failing, up to [[MaxFailureRetry]]: one
 * day for every failure held a whole round's questions back for 24 hours after a ten-minute outage.
 */
final class TmdbGapMemory(docs: TmdbDocuments, language: String, clock: Clock, retryAfter: FiniteDuration = TmdbGapMemory.RetryAfter) {
  import TmdbGapMemory._

  /** `gaps` without those still inside their wait: a day after a durable non-answer, the failure
   *  backoff after a failed read. */
  def due(gaps: AnswersChanged): AnswersChanged = {
    val ids   = gaps.queries.map(q => q -> queryMarker(q)).toMap
    val films = gaps.films.map(id => id -> filmMarker(id)).toMap
    val held  = docs.get(TmdbKind.Query, (ids.values ++ films.values).toSeq)
    val now   = clock.millis()
    def recent(marker: String) = held.get(marker).exists(doc =>
      TmdbStore.fetchedAt(doc).exists(_ > now - waitOf(doc)))
    gaps.copy(queries = gaps.queries.filterNot(q => recent(ids(q))), films = gaps.films.filterNot(id => recent(films(id))))
  }

  /** The questions and records asked just now that TMDB durably left unanswered. */
  def unanswered(queries: Iterable[CandidateQuery], films: Iterable[Int]): Unit =
    mark(queries, films)(_ => new BsonDocument(TmdbStore.FetchedAt, BsonInt64(clock.millis())))

  /** The questions and records whose read failed just now — asked again on the failure backoff. */
  def failed(queries: Iterable[CandidateQuery], films: Iterable[Int]): Unit = {
    val markers = queries.map(queryMarker).toSeq ++ films.map(filmMarker).toSeq
    if (markers.nonEmpty) {
      val held = docs.get(TmdbKind.Query, markers)
      val now  = clock.millis()
      docs.put(TmdbKind.Query, markers.map { marker =>
        // A marker without a failure wait is a durable one (or none): the backoff starts over.
        val previous = held.get(marker).flatMap(failureWaitOf)
        val wait     = previous.fold(FirstFailureRetry.toMillis)(p => (p * 2).min(MaxFailureRetry.toMillis))
        marker -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(FailureWait, BsonInt64(wait))
      })
    }
  }

  // Stamped as a fetch, so the store sweep ages markers out like answers (`TmdbStoreSweep`).
  private def mark(queries: Iterable[CandidateQuery], films: Iterable[Int])(doc: String => BsonDocument): Unit = {
    val markers = queries.map(queryMarker).toSeq ++ films.map(filmMarker).toSeq
    if (markers.nonEmpty) docs.put(TmdbKind.Query, markers.map(marker => marker -> doc(marker)))
  }

  private def failureWaitOf(doc: BsonDocument): Option[Long] =
    Option(doc.get(FailureWait)).filter(_.isInt64).map(_.asInt64.getValue)
  private def waitOf(doc: BsonDocument): Long = failureWaitOf(doc).getOrElse(retryAfter.toMillis)

  private def queryMarker(q: CandidateQuery) = s"$Prefix${TmdbStore.questionId(language, q)}"
  private def filmMarker(id: Int)            = s"${Prefix}film|$id"
}

object TmdbGapMemory {
  val Prefix = "unanswered|"
  /** How long a question TMDB durably left unanswered waits before the fill asks it again. */
  val RetryAfter: FiniteDuration = 1.day
  /** The first wait after a failed read, doubled per further failure up to [[MaxFailureRetry]]. */
  val FirstFailureRetry: FiniteDuration = 5.minutes
  val MaxFailureRetry: FiniteDuration   = 6.hours
  /** The marker field holding a failed question's current wait, in millis. */
  private val FailureWait = "failureWaitMs"
}
