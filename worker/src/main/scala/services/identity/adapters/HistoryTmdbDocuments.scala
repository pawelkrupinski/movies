package services.identity
package adapters

import org.bson.BsonDocument

/**
 * `inner`, every answer whose VALUE a write or a delete moved appended to `history` ([[HistoryEntry.answer]]): the
 * value before and after, each as an answer reads it ([[TmdbDocuments.project]], the stamps aside), so a replay reads
 * at any moment what the model and the agreement read then ([[IdentityHistoryReplay]]). A re-stamp of an unchanged
 * answer — TMDB's daily re-fetch, a family answer filed again alike — is no entry: only changes are kept. A marker of
 * the question memory is kept whole, stamps and all: each is an attempt, its failure and when it is due again.
 *
 * Over the whole store, under the coalescing: the stores' filings ([[TmdbStore]], [[FamilyAnswerStore]],
 * [[QuestionMemory]]) and the retention sweep's deletes ([[TmdbStoreSweep]]) all pass here.
 */
final class HistoryTmdbDocuments(inner: TmdbDocuments & TmdbDocumentRetention, history: IdentityHistory, clock: java.time.Clock,
                                 build: String) extends TmdbDocuments with TmdbDocumentRetention {
  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.get(kind, ids)
  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answers(kind, ids)
  override def filmHits(ids: Seq[String]): Map[String, Hit] = inner.filmHits(ids)
  override def answersReadThrough(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answersReadThrough(kind, ids)
  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = inner.fetchedBefore(kind, cutoff)

  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = if (history eq IdentityHistory.Discard) inner.put(kind, docs) else {
    val before = valuesOf(kind, docs.map(_._1))
    inner.put(kind, docs)
    val at = clock.millis()
    history.append(docs.flatMap { case (id, d) =>
      val after = HistoryTmdbDocuments.valueOf(kind, d)
      Option.when(!before.get(id).contains(after))(HistoryEntry(HistoryEntry.answer(kind), id, at, before.get(id), Some(after), build))
    })
  }

  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int = if (history eq IdentityHistory.Discard) inner.deleteIfStill(kind, stamped) else {
    val ids    = stamped.map(_._1)
    val before = valuesOf(kind, ids)
    val n      = inner.deleteIfStill(kind, stamped)
    if (n > 0) {
      val at   = clock.millis()
      val left = inner.answers(kind, ids).keySet
      history.append(before.toSeq.collect { case (id, was) if !left(id) => HistoryEntry(HistoryEntry.answer(kind), id, at, Some(was), None, build) })
    }
    n
  }

  private def valuesOf(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
    inner.answers(kind, ids).view.mapValues(HistoryTmdbDocuments.valueOf(kind, _)).toMap
}

object HistoryTmdbDocuments {
  /** What the history keeps of a document of `kind`: its answer ([[TmdbDocuments.project]]), stamps aside — but a
   *  marker's whole, whose stamps are what it says. */
  def valueOf(kind: TmdbKind, d: BsonDocument): BsonDocument =
    if (kind == TmdbKind.Marker) d.clone() else TmdbStore.unstamped(TmdbDocuments.project(kind, d))
}
