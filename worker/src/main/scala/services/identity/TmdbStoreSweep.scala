package services.identity

import org.bson.{BsonDocument, BsonString}
import play.api.Logging

import java.time.{Clock, LocalDate, ZoneOffset}
import scala.concurrent.duration._

/**
 * Keeps the normalized TMDB store to what the live model reads. A document no current question
 * reaches — the model's read index files every store key a question read ([[ObservationReads]]) —
 * and not fetched for [[TmdbStoreSweep.Grace]] is deleted; a listing that comes back within the
 * grace finds its answers still there. Before this nothing ever left: every film once answered
 * stayed, and TMDB's daily change list ([[TmdbChangesSweep]]) fetched again each one it edited,
 * reached or not.
 *
 * `reachable` is the model's index, or None while the model is not taken up — then nothing is
 * deleted, and an index naming no store document at all (the cut-over's, which reads observations)
 * counts as none. A sweep that would delete more than `maxShare` of the old documents deletes
 * nothing and says why: an index that lost most of its keys is a fault, not garbage.
 */
final class TmdbStoreSweep(docs: TmdbDocuments, reachable: () => Option[Set[String]], clock: Clock,
                           grace: FiniteDuration = TmdbStoreSweep.Grace, maxShare: Double = TmdbStoreSweep.MaxShare)
    extends Logging {
  import TmdbStoreSweep._

  def behind: Boolean = watermark.forall(_.isBefore(today))

  def sweep(): Option[SweepResult] = synchronized {
    reachable().filter(_.exists(key => TmdbKind.values.exists(kind => key.startsWith(kind.collection + ":")))) match {
      case None =>
        logger.info("identity store: sweep skipped — the model is not taken up, or its index reads no store document")
        None
      case Some(keys) =>
        val oldest  = clock.millis() - grace.toMillis
        val byKind  = TmdbKind.values.toSeq.map { kind =>
          val old = Seq.newBuilder[String]; val garbage = Seq.newBuilder[String]
          val complete = docs.scan(kind)(_.foreach { case (id, fetched) =>
            if (!marker(kind, id) && fetched.exists(_ < oldest)) {
              old += id
              if (!keys(TmdbStore.keyOf(kind, id))) garbage += id
            }
          })
          (kind, old.result().size, if (complete) garbage.result() else Nil)
        }
        val old     = byKind.map(_._2).sum
        val garbage = byKind.map(_._3.size).sum
        val refused = old > 0 && garbage > maxShare * old
        if (refused)
          logger.warn(f"identity store: sweep refused — $garbage of $old documents older than $grace are unreached, " +
            f"over the ${maxShare * 100}%.0f%% a sweep may take; the model's index looks incomplete")
        else {
          byKind.foreach { case (kind, _, ids) => if (ids.nonEmpty) docs.delete(kind, ids) }
          logger.info(s"identity store: swept ${byKind.map { case (k, _, ids) => s"${k.collection} ${ids.size}" }.mkString(", ")} " +
            s"— unreached and older than $grace; ${old - garbage} old documents still read")
        }
        docs.put(TmdbKind.Query, Seq(Watermark -> new BsonDocument("day", BsonString(today.toString))))
        Some(SweepResult(if (refused) 0 else garbage, old - garbage, refused))
    }
  }

  private def today: LocalDate = LocalDate.now(clock.withZone(ZoneOffset.UTC))
  private def watermark: Option[LocalDate] =
    docs.get(TmdbKind.Query, Seq(Watermark)).get(Watermark).flatMap(d => Option(d.get("day"))).map(v => LocalDate.parse(v.asString.getValue))
}

object TmdbStoreSweep {
  /** How long an unreached answer is kept: a listing gone for a week and back re-fetches. */
  val Grace: FiniteDuration = 7.days
  /** The most of the old documents one sweep may delete. */
  val MaxShare: Double = 0.25
  val Watermark = "meta|store-swept"

  /** The store's own bookkeeping in `tmdb_queries`, never a question's answer. */
  def marker(kind: TmdbKind, id: String): Boolean =
    kind == TmdbKind.Query && (id.startsWith("meta|") || id.startsWith(TmdbGapMemory.Prefix))

  final case class SweepResult(deleted: Int, kept: Int, refused: Boolean)
}
