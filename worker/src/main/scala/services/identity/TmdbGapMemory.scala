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
 */
final class TmdbGapMemory(docs: TmdbDocuments, language: String, clock: Clock, retryAfter: FiniteDuration = TmdbGapMemory.RetryAfter) {
  import TmdbGapMemory._

  /** `gaps` without those asked and left unanswered within the last [[RetryAfter]]. */
  def due(gaps: AnswersChanged): AnswersChanged = {
    val ids   = gaps.queries.map(q => q -> queryMarker(q)).toMap
    val films = gaps.films.map(id => id -> filmMarker(id)).toMap
    val held  = docs.get(TmdbKind.Query, (ids.values ++ films.values).toSeq)
    val since = clock.millis() - retryAfter.toMillis
    def recent(marker: String) = held.get(marker).flatMap(TmdbStore.fetchedAt).exists(_ > since)
    gaps.copy(queries = gaps.queries.filterNot(q => recent(ids(q))), films = gaps.films.filterNot(id => recent(films(id))))
  }

  /** The questions and records asked just now that are still unanswered. */
  def unanswered(queries: Iterable[CandidateQuery], films: Iterable[Int]): Unit = {
    // Stamped as a fetch, so the store sweep ages markers out like answers (`TmdbStoreSweep`).
    val now = new BsonDocument(TmdbStore.FetchedAt, BsonInt64(clock.millis()))
    val markers = queries.map(queryMarker).toSeq ++ films.map(filmMarker).toSeq
    if (markers.nonEmpty) docs.put(TmdbKind.Query, markers.map(_ -> now))
  }

  private def queryMarker(q: CandidateQuery) = s"$Prefix${TmdbStore.questionId(language, q)}"
  private def filmMarker(id: Int)            = s"${Prefix}film|$id"
}

object TmdbGapMemory {
  val Prefix = "unanswered|"
  /** How long a question TMDB left unanswered waits before the fill asks it again. */
  val RetryAfter: FiniteDuration = 1.day
}
