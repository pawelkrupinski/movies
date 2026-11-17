package services.cinemas.common

/** What feeds the listing a [[CinemaScraper]] reads ([[CinemaScraper.listingFeed]]). */
enum ListingFeed {
  /** The venue's own site or ticketing system: the only feed the Filmweb fallback may stand in for. */
  case OwnSource
  /** A chain's central network (Cinema City, Helios, Multikino, Cineworld, AMC …): large, reliably fed, its own API
   *  the source of truth, which Filmweb's per-cinema feed doesn't model venue-for-venue. */
  case Chain
  /** Filmweb's listing of the venue: a Filmweb-fed venue gets no Filmweb fallback. */
  case Filmweb
}

object ListingFeed {
  /** The feed of a venue read off several listings: a chain's if any listing is a chain's, Filmweb's only when every
   *  listing is Filmweb's, its own source otherwise. */
  def of(listings: Seq[ListingFeed]): ListingFeed =
    if (listings.contains(Chain)) Chain
    else if (listings.nonEmpty && listings.forall(_ == Filmweb)) Filmweb
    else OwnSource
}
