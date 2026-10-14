package services.identity

import services.movies.LatestTitleYear

import models.{CinemaShowing, SourceData}
import services.movies.{EmbeddedYear, ListingConstraints, ListingKey, ScrapeListing, StoredMovieRecord, TitleNormalizer}
import services.resolution.YearWindow

/** A stored film a listing is on before a projection — the film it inherits its id from
 *  ([[IdentityProjectionPlan.index]]): its film id and TMDB id. */
final case class StoredFilmRef(id: String, tmdbId: Option[Int])

object StoredFilmRef {
  /** The order a listing's films are picked in (`StoredFilmSlots.pick`: the smallest). */
  given Ordering[StoredFilmRef] = Ordering.by(r => (r.id, r.tmdbId.getOrElse(0)))
}

/**
 * Which of the pipeline's films each listing is on: found by its SLOT — the venue and the slot key
 * its title folds to — and, where one venue's two films share that key (Belle 2013/2021), the slot
 * whose own year and directors the listing's agree with, the discriminator the slot fold itself
 * splits on (`ScrapeListing.filmsOf`). A listing still in staging is on no film.
 */
object StoredFilmSlots {

  /** `films` are anything carrying a film's venue slots `(venue, slot key, slot)`. */
  def assign[F](listings: Seq[Listing], films: Seq[(F, Seq[(String, String, SourceData)])],
                normalizer: TitleNormalizer)(using Ordering[F], LatestTitleYear): Map[ListingKey, F] = {
    val bySlot = films.flatMap { case (f, slots) => slots.map { case (v, k, sd) => (v, k) -> (f, sd) } }.groupMap(_._1)(_._2)
    listings.flatMap(l => bySlot.get(slotOf(l, normalizer)).flatMap(pick(l, _, normalizer)).map(l.key -> _)).toMap
  }

  /** The venue slot `listing` is on: its venue, and the slot key its title folds to. */
  def slotOf(listing: Listing, normalizer: TitleNormalizer): (String, String) = (listing.venue, normalizer.sanitize(listing.cleanTitle))

  /** The film `listing` is on among those holding a slot at [[slotOf]] it (`held`, never empty): the one film, or of
   *  several the one whose slot's year and directors the listing's agree with, the smallest first. */
  def pick[F](listing: Listing, held: Seq[(F, SourceData)], normalizer: TitleNormalizer)(using Ordering[F], LatestTitleYear): Option[F] = held match {
    case Seq((f, _)) => Some(f)
    case several =>
      val year = listing.year.orElse(EmbeddedYear.ofAll(Seq(listing.rawTitle, listing.cleanTitle), LatestTitleYear.current))
      val fits = several.filter { case (_, sd) =>
        val slotYear = ScrapeListing.yearOf(sd)
        (year.isEmpty || slotYear.isEmpty || math.abs(year.get - slotYear.get) <= YearWindow.ProductionToRelease) &&
          ListingConstraints.venueCreditsApart(listing.directors, sd.director, normalizer).isEmpty
      }
      (if (fits.nonEmpty) fits else several).map(_._1).minOption
  }

  /** The venue slots of a stored film row. */
  def slotsOf(row: StoredMovieRecord): Seq[(String, String, SourceData)] =
    row.record.data.toSeq.collect { case (cs: CinemaShowing, sd) => (cs.cinema.displayName, cs.titleKey, sd) }

  /** Each listing's film among the pipeline's stored rows. */
  def of(listings: Seq[Listing], rows: Seq[StoredMovieRecord], normalizer: TitleNormalizer)(using LatestTitleYear): Map[ListingKey, StoredFilmRef] =
    assign(listings, rows.map(r => StoredFilmRef(r.id.value, r.record.tmdbId) -> slotsOf(r)), normalizer)
}
