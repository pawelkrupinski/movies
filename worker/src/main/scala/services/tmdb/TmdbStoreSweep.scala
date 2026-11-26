package services.tmdb

import java.time.Clock

import scala.concurrent.duration.*

import play.api.Logging

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
    val cutoffs = Cutoffs(clock.millis(), keepUnread, markerGrace, keepFamily)
    // Every scan first: one that throws leaves the store untouched.
    val scanned = TmdbKind.values.toSeq.map(kind => kind -> documents.fetchedBefore(kind, cutoffs.scannedBefore(kind)))
    val live    = liveKeys()
    val doomed  = scanned.map { case (kind, stamped) => kind -> stamped.filter { case (id, at) => cutoffs.doomed(kind, id, at, live) } }
    val swept = Swept(
      markers = doomed.map { case (kind, stamped) => stamped.count { case (id, _) => isMarker(kind, id) } }.sum,
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

  /** Is the document `id` of `kind` a gap marker: in the markers' own collection, or a question memory's row? */
  private def isMarker(kind: TmdbKind, id: String): Boolean = kind == TmdbKind.Marker || id.startsWith(QuestionMemory.Prefix)

  /** A sweep's keeps as epoch millis before `now`: an answer's (`keepUnread`), a gap marker's (`markerGrace`) and
   *  another family's answer's (`keepFamily`). */
  private final case class Cutoffs(answer: Long, marker: Long, family: Long) {
    /** How far back `kind`'s scan reads: a family's answers by their own keep, every other kind by the later of the two. */
    def scannedBefore(kind: TmdbKind): Long = if (kind == TmdbKind.Family) family else math.max(answer, marker)

    /** Is the document `id` of `kind`, fetched `at`, past its keep: a family's answer before `family`, a gap marker
     *  ([[isMarker]]) before `marker`, any other answer before `answer` and unread by the model taken up (`live`: none
     *  taken up, none deleted)? */
    def doomed(kind: TmdbKind, id: String, at: Long, live: Option[Set[String]]): Boolean =
      if (kind == TmdbKind.Family) at < family
      else if (isMarker(kind, id)) at < marker
      else at < answer && live.exists(keys => !keys.contains(TmdbStore.keyOf(kind, id)))
  }
  private object Cutoffs {
    def apply(now: Long, keepUnread: FiniteDuration, markerGrace: FiniteDuration, keepFamily: FiniteDuration): Cutoffs =
      Cutoffs(now - keepUnread.toMillis, now - markerGrace.toMillis, now - keepFamily.toMillis)
  }
}
