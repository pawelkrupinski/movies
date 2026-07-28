package services.cinemas.common

import models.Cinema

/**
 * Base for every scrape DECORATOR — the wrappers that add retry, an adaptive timeout,
 * uptime recording and the secondary-source fallback around a real client. A decorator
 * overrides `fetch()`; everything else about the venue is the delegate's answer, and
 * this forwards all of it in ONE place.
 *
 * WHY it is one place. Each decorator used to hand-copy `cinema` + `scrapeHosts` and
 * silently inherit `CinemaScraper`'s DEFAULT for every other member. So a field added
 * to the trait was live on the client and dead through the chain, with nothing failing:
 * `listingIsComplete` shipped exactly that way. `ScrapeChunkReduceHandler` publishes a
 * partial reduce as `PreScrapedCinemaScraper(listingComplete = false)`, but
 * `WorkerWiring.publishScrape` wraps it before `CinemaScrapeRunner` reads the flag — and
 * all three wrappers answered the default `true`, so the cache never learned the listing
 * was short and pruned every film that only screened on a missing date. `chain`,
 * `maxFetchAttempts` and `sourceUrl` were in the same state and merely happened to be
 * read off the undecorated scraper at every call site.
 *
 * ADD A MEMBER TO `CinemaScraper` ⇒ FORWARD IT HERE. `DecoratorTransparencySpec` fails
 * when a decorator answers something the delegate did not.
 */
abstract class DelegatingCinemaScraper(delegate: CinemaScraper) extends CinemaScraper {
  val cinema: Cinema                      = delegate.cinema
  def scrapeHosts: Set[String]            = delegate.scrapeHosts
  override def maxFetchAttempts: Int      = delegate.maxFetchAttempts
  override def chain: Boolean             = delegate.chain
  override def listingIsComplete: Boolean = delegate.listingIsComplete
  override def sourceUrl: Option[String]  = delegate.sourceUrl
}
