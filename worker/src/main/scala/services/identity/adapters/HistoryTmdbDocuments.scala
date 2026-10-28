package services.identity
package adapters

import org.bson.BsonDocument

/**
 * `inner`, what the stores other than [[TmdbStore]] file appended to `history` ([[HistoryEntry.answer]]) — the
 * families' answers ([[FamilyAnswerStore]]), each as it reads, a re-file alike no entry; the question memory's markers
 * ([[QuestionMemory]]), whole, stamps and all: each is an attempt, its failure and when it is due again — and the
 * retention sweep's deletes ([[TmdbStoreSweep]]). The TMDB store keeps its own moves, before and after, off the read its
 * write makes anyway ([[TmdbStore]]'s `history`). A write here reads nothing: the history never costs a write a read,
 * and its writer is a queue ([[AsyncIdentityHistory]]).
 */
final class HistoryTmdbDocuments(inner: TmdbDocuments & TmdbDocumentRetention, history: IdentityHistory, clock: java.time.Clock,
                                 build: String) extends TmdbDocuments with TmdbDocumentRetention {
  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.get(kind, ids)
  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answers(kind, ids)
  override def filmHits(ids: Seq[String]): Map[String, Hit] = inner.filmHits(ids)
  override def answersReadThrough(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answersReadThrough(kind, ids)
  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = inner.fetchedBefore(kind, cutoff)

  /** Each family answer's value as last kept here, by its hash: a re-file alike is no entry. Bounded: one forgotten is
   *  kept once more at its next filing, no more. */
  private val lastFamily: com.github.benmanes.caffeine.cache.Cache[String, java.lang.Integer] =
    tools.BoundedCache.ofSize(HistoryTmdbDocuments.FamiliesHeld).build[String, java.lang.Integer]()

  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = {
    inner.put(kind, docs)
    if ((history ne IdentityHistory.Discard) && (kind == TmdbKind.Family || kind == TmdbKind.Marker)) {
      val at = clock.millis()
      history.append(docs.flatMap { case (id, d) =>
        val after = HistoryTmdbDocuments.valueOf(kind, d)
        val alike = kind == TmdbKind.Family && {
          val hash = after.hashCode
          val was  = lastFamily.asMap.put(id, hash)
          was != null && was.intValue == hash
        }
        Option.unless(alike)(HistoryEntry(HistoryEntry.answer(kind), id, at, None, Some(after), build, beforeUnknown = true))
      })
    }
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
  /** How many family answers' last values are remembered to tell a re-file alike. */
  val FamiliesHeld = 200000
  /** What the history keeps of a document of `kind`: its answer ([[TmdbDocuments.project]]), stamps aside — but a
   *  marker's whole, whose stamps are what it says. */
  def valueOf(kind: TmdbKind, d: BsonDocument): BsonDocument =
    if (kind == TmdbKind.Marker) d.clone() else TmdbStore.unstamped(TmdbDocuments.project(kind, d))
}
