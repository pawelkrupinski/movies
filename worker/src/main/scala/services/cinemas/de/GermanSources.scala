package services.cinemas.de

import services.cinemas.common.{CineamoClient, KinoheldVenue}

/** One source a German venue's programme can be read from, with the venue's handle in it. */
sealed trait GermanFeed {
  /** The source's name, as /uptime and the fallback alerts spell it. */
  def label: String
  /** The venue's handle in the source, for the fallback status page's link. */
  def ref: String
}

object GermanFeed {
  final case class Filmstarts(theaterId: String) extends GermanFeed {
    def label = "Filmstarts"; def ref = theaterId
  }
  final case class Cineamo(cinemaId: CineamoClient.CinemaId) extends GermanFeed {
    def label = "Cineamo"; def ref = cinemaId.value
  }
  final case class Kinoheld(venue: KinoheldVenue) extends GermanFeed {
    def label = "kinoheld"; def ref = venue.path
  }
  final case class Kinoprogramm(path: String) extends GermanFeed {
    def label = "Kinoprogramm"; def ref = path
  }
}

/**
 * Which sources serve each rostered German venue, primary first — the choice
 * `data/germany/scripts/harvest_sources.py` measures (every source's shows over the
 * next fortnight and its whole horizon) and records in `data/germany/venue-sources.json`,
 * generated into [[GermanSourcesData]]. The primary is the venue's scraper; the rest,
 * in order, its fallback chain ([[services.cinemas.common.FirstAnsweringScraper]]).
 *
 * A venue the measurement has no row for keeps the arrangement it had before it:
 * Filmstarts, with its kinoprogramm.com page as the fallback where it has one.
 */
object GermanSources {
  /** The venue's sources, primary first, by its Filmstarts `theaterId`. */
  def of(theaterId: String, kinoprogrammPath: Option[String]): Seq[GermanFeed] =
    GermanSourcesData.byTheaterId.getOrElse(theaterId,
      GermanFeed.Filmstarts(theaterId) +: kinoprogrammPath.map(GermanFeed.Kinoprogramm(_)).toSeq)
}
