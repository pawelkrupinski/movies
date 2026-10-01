package services.cinemas.common

import models.{Cinema, CinemaMovie}

/**
 * A `CinemaScraper` whose `fetch()` returns (or throws) a precomputed result,
 * standing in for the live scraper it was computed from. It lets the chunked
 * reduce step push its aggregated listing — or a plan/reduce failure — through
 * the SAME recording/fallback decorator chain and `CinemaScrapeRunner` a normal
 * scrape uses, so uptime classification, the Filmweb fallback, and event
 * publishing are all reused unchanged.
 *
 * Every identity (venue, hosts, chain flag, attempts, source URL and key, chain
 * venue id) is the stood-for scraper's, forwarded by [[DelegatingCinemaScraper]]
 * so a member added to the trait cannot be dropped here either; only the listing,
 * whether it is whole, and whether its emptiness is the source saying so are its own.
 */
class PreScrapedCinemaScraper(
  standsFor: CinemaScraper,
  result:    () => Seq[CinemaMovie],
  // False when the chunked run this was reduced from was missing chunks — see
  // `CinemaScraper.listingIsComplete`.
  listingComplete: Boolean = true,
  // True when the plan this stands for was the source listing no schedule — see
  // `CinemaScraper.noScheduleListed`.
  noSchedule: Boolean = false
) extends DelegatingCinemaScraper(standsFor) {
  def fetch(): Seq[CinemaMovie] = result()
  override def listingIsComplete: Boolean = listingComplete
  override def noScheduleListed: Boolean  = noSchedule
}

object PreScrapedCinemaScraper {
  /** `result`, standing in for the live scrape of `scraper`. */
  def of(scraper: CinemaScraper, result: () => Seq[CinemaMovie], listingComplete: Boolean = true,
         noScheduleListed: Boolean = false): PreScrapedCinemaScraper =
    new PreScrapedCinemaScraper(scraper, result, listingComplete, noScheduleListed)

  /** `movies`, a whole listing of `cinema` read from no live source — an archived
   *  corpus replayed into the pipeline. It has no hosts, source or chain identity. */
  def replaying(cinema: Cinema, movies: Seq[CinemaMovie]): PreScrapedCinemaScraper =
    of(Anonymous(cinema), () => movies)

  private final case class Anonymous(cinema: Cinema) extends CinemaScraper {
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = Seq.empty
  }
}
