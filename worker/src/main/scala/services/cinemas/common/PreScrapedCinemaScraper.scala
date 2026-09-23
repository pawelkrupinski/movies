package services.cinemas.common

import models.{Cinema, CinemaMovie}

/**
 * A trivial `CinemaScraper` whose `fetch()` returns (or throws) a precomputed
 * result. It lets the chunked reduce step push its aggregated listing — or a
 * plan/reduce failure — through the SAME recording/fallback decorator chain and
 * `CinemaScrapeRunner` a normal scrape uses, so uptime classification, the
 * Filmweb fallback, and event publishing are all reused unchanged.
 */
class PreScrapedCinemaScraper(
  val cinema:   Cinema,
  hosts:        Set[String],
  isChain:      Boolean,
  result:       () => Seq[CinemaMovie],
  // False when the chunked run this was reduced from was missing chunks — see
  // `CinemaScraper.listingIsComplete`.
  listingComplete: Boolean = true,
  // The original scraper's `sourceKey`, so a chunked venue's rewire is recognised
  // exactly as a plain one's is — see `MovieCache.recordCinemaScrape`.
  key:          Option[String] = None
) extends CinemaScraper {
  def scrapeHosts: Set[String]  = hosts
  def fetch(): Seq[CinemaMovie] = result()
  // Carry the original scraper's chain flag so the recording wrapper picks the
  // same Filmweb-fallback eligibility (`FallbackEligibility.eligible`) it would
  // for the live scrape — a chunked chain must not become fallback-eligible.
  override def chain: Boolean = isChain
  override def listingIsComplete: Boolean = listingComplete
  override def sourceKey: Option[String]  = key
}

object PreScrapedCinemaScraper {
  /** `result`, standing in for the live scrape of `scraper` — every identity it
   *  carries (venue, hosts, chain flag, source key) copied from the scraper itself. */
  def of(scraper: CinemaScraper, result: () => Seq[CinemaMovie], listingComplete: Boolean = true): PreScrapedCinemaScraper =
    new PreScrapedCinemaScraper(scraper.cinema, scraper.scrapeHosts, scraper.chain, result, listingComplete, scraper.sourceKey)
}
