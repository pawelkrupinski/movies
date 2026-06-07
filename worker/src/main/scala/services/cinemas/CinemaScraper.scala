package services.cinemas

import models.{Cinema, CinemaMovie}

import java.net.URI

/**
 * Single contract every cinema-source obeys: a name (`cinema`), a thunk
 * (`fetch`) that produces the films currently scheduled at that cinema, and
 * the set of HTTP hosts that scrape touches (`scrapeHosts`).
 *
 * Adding a new cinema is a new `CinemaScraper` instance wired in `AppLoader`
 * — `ShowtimeCache` doesn't change (CLAUDE.md OCP guidance). Per-cinema
 * clients implement this directly when they map 1:1 to a cinema; the
 * Cinema City client maps 1:N (Plaza + Kinepolis), so a thin
 * `CinemaCityScraper` wrapper captures the per-cinema parameters.
 *
 * `scrapeHosts` is the single source of truth for "which hosts belong to a
 * cinema scrape". `MonitoringHttpFetch` reads the union of every scraper's
 * `scrapeHosts` (via `CinemaScraperCatalog.scrapeHosts`) and suppresses
 * per-host uptime rows for them — the cinema's health is already tracked
 * under its `displayName` by `RetryingCinemaScraper`, so a second per-host row
 * would be a duplicate that lands in the uptime page's "Other" bucket. Because
 * the method is abstract, a newly-added cinema client can't compile without
 * declaring its host(s), so a new cinema can never silently leak into "Other".
 * Derive it from the same base-URL constant the client fetches with (via
 * `CinemaScraper.hostsOf`) rather than re-spelling the host, so the two can't
 * drift.
 */
trait CinemaScraper {
  def cinema: Cinema
  def fetch(): Seq[CinemaMovie]
  def scrapeHosts: Set[String]
}

object CinemaScraper {
  /** Lower-cased hosts of the given URLs, skipping any that don't parse to a
   *  host. The canonical way a scraper derives `scrapeHosts` from the base
   *  URL(s) it already fetches with — no second copy of the host string. */
  def hostsOf(urls: String*): Set[String] =
    urls.flatMap(u => Option(URI.create(u).getHost)).map(_.toLowerCase).toSet
}

/**
 * Adapter for `CinemaCityClient`, whose `fetch(cinemaId, cinema)` serves
 * multiple cinema variants. Each `CinemaCityScraper` instance captures one
 * (cinemaId, cinema) pair.
 */
class CinemaCityScraper(
  client:   CinemaCityClient,
  cinemaId: String,
  val cinema: Cinema
) extends CinemaScraper {
  def fetch(): Seq[CinemaMovie] = client.fetch(cinemaId, cinema)
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(CinemaCityClient.BaseApiUrl)
}
