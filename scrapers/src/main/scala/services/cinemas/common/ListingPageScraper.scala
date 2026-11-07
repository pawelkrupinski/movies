package services.cinemas.common

import models.CinemaMovie
import tools.{HttpFetch, HttpRead}

/** A venue whose whole programme is ONE page: the scrape reads `pageUrl` and parses it.
 *  The page's host is the scrape's only host and `publicUrl` the venue's link on /uptime,
 *  so a client of this shape writes only its parse.
 *
 *  `pageUrl` is read on every scrape, never cached here: a failed read fails the scrape
 *  (`HttpRead.page`), so a broken page never passes for an empty programme. */
abstract class ListingPageScraper(http: HttpFetch, pageUrl: String, publicUrl: String) extends CinemaScraper {

  /** The venue's programme, read off the page's body. */
  protected def parsePage(html: String): Seq[CinemaMovie]

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(pageUrl)

  override def sourceUrl: Option[String] = Some(publicUrl)

  def fetch(): Seq[CinemaMovie] = parsePage(HttpRead.page(http, pageUrl))
}
