package services.cinemas.us

/**
 * The US independents whose OWN site is the scrape primary, keyed by the roster
 * display name (`Cinema.displayName`) — the same key `UsChainVenues` uses, for the
 * same reason: a US venue is built at runtime from `data/us/venues.json`, so there is
 * no case object to name.
 *
 * Unlike the chain venues these keep NO flicks.us fallback. A venue lands here because
 * Flicks stopped listing it: ACME Screening Room's Flicks page has answered 404 since
 * 2026-08-31 while the cinema kept selling (October 2026: its Halloween Film Festival),
 * and the closure sweep, reading that 404, retired it. A fallback that can only 404
 * would corroborate the next false closure rather than prevent it — with none, the
 * sweep judges the venue on its own site alone, and over the longer uncorroborated
 * spell.
 */
object UsIndependentVenues {

  /** A venue whose programme is a Squarespace events collection — see
   *  [[SquarespaceEventsClient]]. `zoneId` reads the collection's epoch instants as the
   *  venue's wall clock. */
  final case class SquarespaceVenue(siteUrl: String, eventsPath: String, zoneId: String)

  val squarespace: Map[String, SquarespaceVenue] = Map(
    "ACME Screening Room Lambertville" -> SquarespaceVenue("https://www.acmescreeningroom.org", "/now-playing", "America/New_York"),
  )

  /** Every display name served by an independent's own site — off the flicks.us sweep. */
  val all: Set[String] = squarespace.keySet
}
