package services.cinemas.es

import models.{Cinema, CinemaMovie}
import services.cinemas.common.CinemaScraper
import tools.{HttpFetch, HttpRead}

/**
 * Artesiete (Spain) — each venue off its own page on the chain's site,
 * `https://<venue>.artesiete.es/Cine/<id>/<slug>`: ONE server-rendered page
 * whose Vue props embed every session the venue sells, about two months out
 * (2026-12-20 when captured on 2026-10-12), with room and format on each —
 * see [[ArtesieteParser]]. One request a scrape; plain HTTP, no auth, robots.txt
 * allows everything.
 *
 * Read from the venue's own subdomain — the address `www.artesiete.es/Cine/<id>`
 * redirects to — so the one fetch is the page itself. The map of venues and
 * pages is `data/spain/artesiete.json`.
 */
class ArtesieteClient(http: HttpFetch, pageUrl: String, override val cinema: Cinema) extends CinemaScraper {

  private val baseUrl = {
    val uri = java.net.URI.create(pageUrl)
    s"${uri.getScheme}://${uri.getAuthority}"
  }

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(pageUrl)

  /** A chain's own box office, not a single independent venue. */
  override def chain: Boolean = true

  override def sourceUrl: Option[String] = Some(pageUrl)

  def fetch(): Seq[CinemaMovie] = ArtesieteParser.films(HttpRead.page(http, pageUrl), baseUrl, cinema)
}
