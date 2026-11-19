package services.enrichment

import models.{Imdb, MovieRecord, Source, SourceData}

import java.util.Locale

/** What an IMDb read writes on a row, its country names in the deployment's `language` (Polish "Wielka Brytania" on
 *  `kinowo`, English "United Kingdom" on the UK site): the rating, when its badge moved, and the details slot, when its
 *  content changed. */
final class ImdbRowUpdates(language: Locale) {

  /** What reading `freshRating` and `freshDetails` off IMDb changes on `row`. */
  def of(row: MovieRecord, freshRating: Option[Double], freshDetails: Option[ImdbClient.Details]): ImdbRowUpdates.Update = {
    // Store at the precision the badge shows (`%.1f`), so a sub-decimal vote drift the user can't see isn't a
    // "change" — see RatingDisplay.
    val fresh = freshRating.map(RatingDisplay.oneDecimal)
    ImdbRowUpdates.Update(
      fresh,
      rating = fresh.filter(r => !row.imdbRating.contains(r)),
      slot   = freshDetails.flatMap(d => ImdbSlot.of(d, language).filter(s => !row.data.get(Imdb).contains(s))))
  }
}

object ImdbRowUpdates {
  /** One IMDb read's effect: the rating it read (`fresh`, at badge precision), the rating to write (when it moved) and
   *  the slot to write (when its content changed). */
  final case class Update(fresh: Option[Double], rating: Option[Double], slot: Option[SourceData]) {
    def moves: Boolean = rating.isDefined || slotMoves

    /** Whether the read's slot changes the row's. */
    def slotMoves: Boolean = slot.isDefined

    /** The live row with this read written: a rating it lacks or a slot it lacks leaves the row's own. */
    def applyTo(current: MovieRecord): MovieRecord = current.copy(
      imdbRating = rating.orElse(current.imdbRating),
      data       = slot.map(s => current.data + ((Imdb: Source) -> s)).getOrElse(current.data))

    /** The cadence signal: the DISPLAYED rating moving (its badge text); a details-slot-only refresh isn't one. */
    def badge: Option[String] = rating.map(RatingDisplay.label)
  }
}
