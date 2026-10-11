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
 * Which sources serve each German venue, primary first — the choice
 * `data/germany/scripts/harvest_sources.py` measures (every source's shows over the
 * next fortnight and its whole horizon) and records in `data/germany/venue-sources.json`,
 * generated into [[GermanSourcesData]]. The primary is the venue's scraper; the rest,
 * in order, its fallback chain ([[services.cinemas.common.FirstAnsweringScraper]]).
 *
 * Keyed by the venue's KEY (`GermanRoster.venueKeyByCinema`): its Filmstarts `theaterId`,
 * or the feed that found a venue Filmstarts does not list ("cineamo:997"). A Filmstarts
 * venue the measurement has no row for keeps the arrangement it had before it:
 * Filmstarts, with its kinoprogramm.com page as the fallback where it has one.
 */
object GermanSources {
  /** The venue's sources, primary first, by its venue key. */
  def of(venueKey: String, kinoprogrammPath: Option[String]): Seq[GermanFeed] =
    GermanSourcesData.byVenueKey.getOrElse(venueKey, {
      require(!venueKey.contains(':'), s"$venueKey is no Filmstarts venue, and venue-sources.json names no source for it")
      GermanFeed.Filmstarts(venueKey) +: kinoprogrammPath.map(GermanFeed.Kinoprogramm(_)).toSeq
    })
}
