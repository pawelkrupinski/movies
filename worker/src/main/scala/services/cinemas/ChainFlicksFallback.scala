package services.cinemas

import models._
import services.cinemas.common.FlicksMarket
import services.cinemas.uk.{UkFeed, UkSourcesData}

/**
 * UK chain venues whose own-site scraper (Cineworld / Vue / Showcase / Everyman /
 * Odeon) is the catalogue primary, mapped to the flicks.co.uk slug they keep as
 * their aggregator FALLBACK — the mirror of the Polish own-site→Filmweb setup.
 *
 * `CinemaScraperCatalog.flicksFallbackSlugs` exposes this to `WorkerWiring`, which
 * builds a fallback `FlicksClient` on demand when a chain venue's own scrape has
 * been failing for the grace window. Both halves are read off the venues' sources,
 * generated from `data/uk/roster.json` into [[UkSourcesData]]: a venue whose primary is
 * its own site and that keeps a `flicks` slug there. Every cinema here is catalogued
 * under its own-site client, never under Flicks, so it is primary + fallback without
 * being double-wired.
 */
object ChainFlicksFallback {

  /** Which Flicks MARKET a venue's fallback slug belongs to, alongside the slug.
   *  The two ccTLDs are separate sites on separate Cloudflare zones, so a
   *  fallback has to name which one it means — a US venue looked up on
   *  `flicks.co.uk` would simply 404. */
  case class FlicksFallback(market: FlicksMarket, slug: String)

  /** The UK chain venues (Cineworld / Vue / Showcase / Everyman / Odeon) and
   *  their flicks.co.uk slugs. */
  val ukChainSlugs: Map[Cinema, String] = ukFallbacks(_.chain)

  /** UK independents read from their own box office or site — Savoy Systems
   *  ([[services.cinemas.uk.SavoyClient]]), Spektrix
   *  ([[services.cinemas.uk.SpektrixClient]]) or Ticketsolve
   *  ([[services.cinemas.uk.TicketsolveClient]]), or a one-venue client such as
   *  [[services.cinemas.uk.NovaCinemaClient]] or [[services.cinemas.uk.BfiSouthbankClient]] —
   *  because Flicks lists them with
   *  few or no times. Flicks stays their fallback, the same arrangement as the chains:
   *  should the box office fail or come back empty, the venue reads Flicks again. */
  val ukOwnSiteSlugs: Map[Cinema, String] = ukFallbacks(!_.chain)

  /** The UK venues whose primary `keeps` and that Flicks also lists, with their slugs. */
  private def ukFallbacks(keeps: UkFeed => Boolean): Map[Cinema, String] =
    UkSourcesData.venues.filter(venue => keeps(venue.primary))
      .flatMap(venue => venue.flicksFallback.map(venue.cinema -> _)).toMap

  /** Every UK venue whose own-site client is primary, with its flicks.co.uk slug:
   *  the chains and the independents alike. */
  val ukSlugs: Map[Cinema, String] = ukChainSlugs ++ ukOwnSiteSlugs

  /** Every chain venue that keeps a Flicks aggregator fallback, with the market
   *  its slug lives on. */
  val slugs: Map[Cinema, FlicksFallback] =
    ukSlugs.map { case (cinema, slug) => cinema -> FlicksFallback(FlicksMarket.UnitedKingdom, slug) }
}
