package controllers

import models.Country

/** Builds the `robots.txt` body.
 *
 *  Serving one at all is not optional: a 404 here made Facebook's link-preview
 *  scraper (`facebookexternalhit`) report "403 due to robots.txt block" against
 *  the film page. So the file allowlists every crawler explicitly, and spends
 *  its `Disallow` lines on the operational paths (`/debug`, `/admin`, `/tasks`,
 *  `/uptime`, `/auth/`, the JSON APIs) plus the `/{city}/movies` browse facets —
 *  those last for CRAWL BUDGET rather than secrecy: a city listing links ~480 of
 *  them off its genre pills, each a thin filtered slice of a corpus the film
 *  deep-links already cover. `SitemapBuilder` omits them for the same reason.
 *
 *  TWO shapes, because a crawler only ever reads `robots.txt` at a HOST's root
 *  and one host can now carry several countries:
 *
 *   - [[forCountry]] on a country that owns its domain (`kinowo.net/robots.txt`),
 *   - [[frontDoor]] at the brand apex (`showtimes.cc/robots.txt`), which has to
 *     speak for every country mounted beneath it — `showtimes.cc/uk/robots.txt`
 *     is a file no crawler will ever fetch.
 */
object RobotsTxt {

  // The operational paths, relative to a mount point. A `*` in a robots.txt
  // path matches any run of characters, slashes included, so one wildcard rule
  // covers every city under that prefix.
  private val disallowed =
    Seq("/debug", "/admin", "/tasks", "/uptime", "/auth/", "/*/api/", "/*/debug/", "/*/movies", "/*/filmy")

  private def body(prefixes: Seq[String], sitemaps: Seq[String]): String = {
    val rules = for { prefix <- prefixes; path <- disallowed } yield s"Disallow: $prefix$path"
    (Seq("User-agent: *", "Allow: /") ++ rules ++ Seq("") ++ sitemaps.map(s => s"Sitemap: $s"))
      .mkString("", "\n", "\n")
  }

  /** One country's own site. `base` is its public base URL with no trailing
   *  slash (`https://kinowo.net`), which already carries the mount point. */
  def forCountry(base: String, country: Country): String =
    body(prefixes = Seq(country.pathPrefix), sitemaps = Seq(s"$base/sitemap.xml"))

  /** The brand front door. Fences off the operational paths of every country
   *  mounted under the apex and points at each one's sitemap, since none of them
   *  has a host root of its own to be discovered from. */
  def frontDoor(countries: Seq[Country]): String =
    body(prefixes = countries.map(_.pathPrefix), sitemaps = countries.flatMap(_.webUrl).map(_ + "/sitemap.xml"))
}
