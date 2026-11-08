package controllers

import play.api.mvc._

/** What a crawler asks for before any page: `robots.txt` and the sitemaps, cut by
 *  [[CrawlerFiles]] from the live schedules and each city's own stamp. */
class CrawlerController(cc: ControllerComponents,
                        cityRequests: CityRequests,
                        crawlerFiles: CrawlerFiles,
                       ) extends AbstractController(cc) {

  // robots.txt — see `CrawlerFiles.robots` for which shape a request wants. The share-card
  // images (`/share-cards/…`) are deliberately NOT disallowed — Facebook honours robots.txt when
  // fetching `og:image`, so blocking them would break every share preview.
  def robotsTxt: Action[AnyContent] = Action { request =>
    Ok(crawlerFiles.robots(PageMeta.host(request), PageMeta.origin(request))).as("text/plain; charset=utf-8")
  }

  /** `sitemap.xml` — the full crawl map, or an index of per-city ones for a large
   *  corpus; see [[CrawlerFiles.sitemap]]. Cached for an hour at the edge/browser;
   *  the corpus changes on the order of scrape cadence, not per request. */
  def sitemap: Action[AnyContent] = Action { request =>
    okSitemap(crawlerFiles.sitemap(PageMeta.host(request), PageMeta.origin(request)))
  }

  /** `/sitemap-root.xml` — the landing-only sitemap a partitioned country's index points at. */
  def sitemapRoot: Action[AnyContent] = Action { request =>
    okSitemap(crawlerFiles.sitemapRoot(PageMeta.origin(request)))
  }

  /** `/{city}/sitemap.xml` — one city's slice of the full crawl map, served
   *  only once a country's flat `sitemap.xml` has been partitioned. */
  def citySitemap(city: String): Action[AnyContent] = Action { request =>
    cityRequests.withCity(city, request)(c => okSitemap(crawlerFiles.citySitemap(PageMeta.origin(request), c)))
  }

  private def okSitemap(body: String): Result =
    Ok(body).as("application/xml; charset=utf-8").withHeaders("Cache-Control" -> "public, max-age=3600")
}
