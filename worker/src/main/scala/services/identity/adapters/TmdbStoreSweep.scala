package services.identity
package adapters

import play.api.Logging

import java.time.Clock
import scala.concurrent.duration._

/**
 * The normalized TMDB store's retention: what it keeps is only what the model still reads.
 *
 * Every answer the pipeline or the fill ever fetched was filed (`tmdb_films`, `tmdb_people`,
 * `tmdb_queries`) and nothing ever deleted one: a search for a title no venue lists any more, a film
 * only that search named, a person a since-dropped listing credited — and each `unanswered|` gap
 * marker ([[QuestionMemory]]), one per question its source could not answer, long after the question was
 * last asked. The store grew with every title the country ever screened.
 *
 * A sweep deletes:
 *  - a non-answer marker stamped longer ago than `markerGrace` — long past every wait the memory holds, it
 *    holds nothing back;
 *  - an answer last fetched longer ago than `keepUnread` that NONE of the model's questions reads now
 *    (`liveKeys`, the keys [[ObservationReads]] tracks). An answer the model reads is kept however old
 *    it is: deleting it would turn the model's answer into a gap. With no model taken up there is no
 *    such set, and no answer is deleted.
 *
 * Another film database family's answer ([[TmdbKind.Family]]) is no question of the model's: it is kept until it was
 * last fetched longer ago than `keepFamily`, read by the agreement or not.
 *
 * Every kind is scanned before anything is deleted, so a scan that fails deletes nothing; and each
 * delete is conditional on the `fetchedAt` the scan read, so a document re-fetched meanwhile is kept.
 */
final class TmdbStoreSweep(
  documents:   TmdbDocumentRetention,
  liveKeys:    () => Option[Set[String]],
  clock:       Clock,
  keepUnread:  FiniteDuration = QuestionPolicies.KeepUnread,
  markerGrace: FiniteDuration = QuestionPolicies.MarkerGrace,
  keepFamily:  FiniteDuration = QuestionPolicies.KeepFamily
) extends Logging {
  import TmdbStoreSweep._

  def sweep(): Swept = {
    val now          = clock.millis()
    val answerCutoff = now - keepUnread.toMillis
    val markerCutoff = now - markerGrace.toMillis
    val familyCutoff = now - keepFamily.toMillis
    // Every scan first: one that throws leaves the store untouched.
    val scanned = TmdbKind.values.toSeq.map(kind =>
      kind -> documents.fetchedBefore(kind, if (kind == TmdbKind.Family) familyCutoff else math.max(answerCutoff, markerCutoff)))
    val live    = liveKeys()
    val doomed  = scanned.map { case (kind, stamped) =>
      kind -> stamped.filter { case (id, at) =>
        if (kind == TmdbKind.Family) at < familyCutoff
        else if (kind == TmdbKind.Marker || id.startsWith(QuestionMemory.Prefix)) at < markerCutoff
        else at < answerCutoff && live.exists(keys => !keys.contains(TmdbStore.keyOf(kind, id)))
      }
    }
    val swept = Swept(
      markers = doomed.flatMap { case (kind, ids) => ids.filter(i => kind == TmdbKind.Marker || i._1.startsWith(QuestionMemory.Prefix)) }.size,
      deleted = doomed.map { case (kind, stamped) => documents.deleteIfStill(kind, stamped) }.sum,
      modelUp = live.isDefined)
    logger.info(s"TMDB store sweep: ${swept.deleted} deleted (${swept.markers} of them gap markers past ${markerGrace.toDays}d)" +
      (if (swept.modelUp) s"; answers unread by the model and unfetched for ${keepUnread.toDays}d included"
       else "; no model taken up, so no answer was deleted"))
    swept
  }
}

object TmdbStoreSweep {
  /** How often the sweep runs. */
  val Interval: FiniteDuration = 24.hours

  /** `deleted` documents in all, `markers` of them chosen as gap markers; `modelUp` whether answers were swept. */
  final case class Swept(markers: Int, deleted: Int, modelUp: Boolean)
}
