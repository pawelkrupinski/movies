package services.cinemas

import models.{Cinema, CinemaCityChain, CinemaMovie, Source}

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

  /** How many times the production wrapper (`RetryingCinemaScraper`) attempts
   *  this cinema before giving up on a tick. The default 3 suits a normal
   *  upstream — one transient blip per refresh recovers on a retry. A cinema
   *  whose origin *flaps* (frequent but CHEAP, fast-failing errors) overrides
   *  this higher: extra independent attempts cost almost nothing when each
   *  failure returns in well under a second, and together they span enough
   *  wall-clock to ride out the upstream's bad windows so the cinema doesn't
   *  drop out of the cache. Don't raise it for upstreams that fail *slowly*
   *  (sockets that hang to the 30s request timeout) — there each extra attempt
   *  is a 30s tax on the tick. `WorkerWiring` clamps this to its attempt
   *  ceiling, so the fixture-replay test wiring can still force a single
   *  no-retry attempt for every cinema. */
  def maxFetchAttempts: Int = 3
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
 *
 * It is the per-venue `DetailEnricher` for the *listing* side (this venue's
 * showtimes + `filmUrl`), but the per-film detail is shared chain-wide: every
 * venue uses one `"cinema-city"` `detailGroup` (so a film is fetched + parsed
 * once per network per freshness window — the queue's unique index + freshness
 * drop the sibling venues' duplicate schedules) and writes the result into a
 * single `CinemaCityChain` network source rather than each venue's own slot.
 * `MovieRecord`'s merged accessors are film-level, so one shared slot surfaces
 * the synopsis/cast/genres at every venue. Enrichment health reports once as
 * "Globalne: Cinema City" instead of one row per venue.
 */
class CinemaCityScraper(
  client:   CinemaCityClient,
  cinemaId: String,
  val cinema: Cinema,
  deferDetail: Boolean = false
) extends CinemaScraper with DetailEnricher {
  def fetch(): Seq[CinemaMovie] = client.fetch(cinemaId, cinema, deferDetail)
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(CinemaCityClient.BaseApiUrl)
  override val detailGroup: String = "cinema-city"
  override def detailTarget: Source = CinemaCityChain
  override def enrichmentServiceOverride: Option[String] = Some("Globalne: Cinema City")
  override def fetchFilmDetail(ref: String): Option[FilmDetail] = client.fetchFilmDetail(ref)
}
