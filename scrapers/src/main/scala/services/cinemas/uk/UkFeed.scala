package services.cinemas.uk

import models.Cinema

/** The source a UK venue's programme is scraped from, with the venue's handle in it — a row's
 *  `primary` in `data/uk/roster.json`, generated into [[UkSourcesData]]. `UkScraperCatalog`
 *  builds each venue's scraper from it. */
sealed trait UkFeed {
  /** A chain's own site (Cineworld / Vue / Showcase / Everyman / Odeon), rather than an
   *  independent's box office or flicks.co.uk. */
  def chain: Boolean = false
}

object UkFeed {
  /** flicks.co.uk's page for the venue. */
  final case class Flicks(slug: String) extends UkFeed
  final case class Cineworld(slug: String) extends UkFeed { override def chain = true }
  final case class Vue(id: String) extends UkFeed { override def chain = true }
  final case class Showcase(id: String) extends UkFeed { override def chain = true }
  final case class Everyman(id: String) extends UkFeed { override def chain = true }
  final case class Odeon(id: String) extends UkFeed { override def chain = true }
  /** A Savoy Systems box office on the venue's own site. */
  final case class Savoy(dllBase: String) extends UkFeed
  /** A booking-only Savoy host: the venue's site is elsewhere, Savoy only sells its tickets. */
  final case class SavoyBoxOffice(dllBase: String) extends UkFeed
  /** A Spektrix client; `building` names the venue's, for a client that sells several. */
  final case class Spektrix(client: String, building: Option[String]) extends UkFeed
  final case class Ticketsolve(client: String) extends UkFeed
  /** Derby QUAD's Spektrix with its own detail pages ([[QuadDerbyClient]], built for that one venue). */
  case object QuadDerby extends UkFeed
  case object BfiSouthbank extends UkFeed
  case object NovaCinema extends UkFeed
  /** The Old Court, Windsor: on no Flicks page, scraped off its own site. */
  case object TheOldCourt extends UkFeed
}

/** One UK venue's sources: its primary, and — for an own-site primary Flicks also lists — the
 *  flicks.co.uk slug it falls back to (`ChainFlicksFallback`). */
final case class UkVenueSources(cinema: Cinema, primary: UkFeed, flicksFallback: Option[String])
