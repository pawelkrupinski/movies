package services.identity.vocabulary

import models.StoredMovieRecord
import models.{CinemaShowing, ListingKey, Source, SourceData}

/** The slots of a stored film as a projection's index reads them ([[StoredSlots.of]], [[LiveProjectionIndex]]):
 *  where each is, and which listing it is the slot of. */
object StoredSlots {
  /** One slot of a stored film: where a venue's same-title fold puts it (`at`, a venue slot's venue and slot key), its
   *  data, and the listing key it is the slot of, if any. */
  final case class Slot(at: Option[(String, String)], data: SourceData, key: Option[ListingKey])

  /** Every slot of a stored film ([[slotOf]]). */
  def of(r: StoredMovieRecord): Seq[Slot] = r.record.data.toSeq.map { case (source, sd) => slotOf(source, sd) }

  /** The slot `sd` of a stored film is, at `source`. */
  def slotOf(source: Source, sd: SourceData): Slot =
    Slot(source match { case cs: CinemaShowing => Some(cs.cinema.displayName -> cs.titleKey); case _ => None }, sd, ListingKey.ofSource(source, sd))
}
