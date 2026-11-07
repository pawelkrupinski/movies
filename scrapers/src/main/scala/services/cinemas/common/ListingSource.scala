package services.cinemas.common

/** Who publishes the listing a [[CinemaScraper]] reads: the venue itself (its own site, its
 *  ticketing system, its chain's API) or a third-party aggregator covering many venues. The
 *  wiring reads it instead of testing for a concrete client: a Filmweb-fed venue gets no
 *  Filmweb fallback (`FallbackEligibility`), and one fed only by Filmweb is a migration
 *  candidate (`ScrapeWiring.filmwebOnlyCinemas`). */
enum ListingSource {
  case OwnSite
  case Aggregator(name: String)
}

object ListingSource {
  val Filmweb: ListingSource = Aggregator("Filmweb")
  val Flicks: ListingSource  = Aggregator("Flicks")
  val Webedia: ListingSource = Aggregator("Webedia")
}
