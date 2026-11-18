package services.identityadapters

import services.identity.{Answer, DetailFacts, Listing}
import models.Cinema
import services.cinemas.common.DetailEnricher

/** A listing's venue detail through its cinema's enricher: the page's facts as the model reads them. */
final class VenueDetails(enrichers: Seq[DetailEnricher], gaps: TmdbIdentityLookups.Gaps) {
  private val enricherOf: Map[Cinema, DetailEnricher] = enrichers.map(e => e.cinema -> e).toMap

  def hasDetail(listing: Listing): Boolean = listing.page.isDefined && enricherOf.contains(listing.cinema)

  def detail(listing: Listing): Answer[Option[DetailFacts]] =
    (listing.page, enricherOf.get(listing.cinema)) match {
      case (Some(page), Some(e)) => gaps.answered(e.fetchFilmDetail(page).map(d =>
        DetailFacts(d.releaseYear, d.director.map(_.trim).filter(_.nonEmpty), d.runtimeMinutes, d.originalTitle, d.countries, d.synopsis, d.cast)))
      case _                     => Answer.Known(None)
    }
}

