package services.cinemas.common

import models.{Cinema, CinemaMovie}
import play.api.Logging
import tools.HostScrapeStats

import java.util.concurrent.{Callable, ExecutionException, ExecutorService, TimeUnit, TimeoutException}

/**
 * Decorator that bounds a single scrape's wall-clock to an ADAPTIVE budget
 * derived from how long this host's scrapes usually take ([[HostScrapeStats]]):
 * `2× the host's rolling median`, clamped. A scrape that overruns is cancelled
 * — its thread interrupted, so it stops pinning a worker slot and burning the
 * shared-cpu credit — and a `TimeoutException` is thrown. The outer
 * `UptimeRecordingScraper` / `FilmwebFallbackScraper` then records that as a
 * normal failure (a red bar, or a Filmweb fallback for eligible venues), so a
 * cut scrape surfaces on /uptime exactly like any other timeout.
 *
 * Wrapped OUTSIDE `RetryingCinemaScraper` so the budget bounds the ENTIRE
 * scrape — all retries plus their backoff — not one attempt. The throttle
 * incident this guards against was the *aggregate* wall-clock a slow host
 * pinned (Kinoteka's day pages summing to ~30–70s), not one slow request; the
 * per-request timeout in `RealHttpFetch` already bounds individual requests.
 *
 * Only a successful scrape's duration feeds the stats — a cut or thrown scrape
 * never updates the baseline, so a stalling host can't normalise its own
 * stalls. The host key is the delegate's whole `scrapeHosts` set, so a chain's
 * venues share one baseline (see [[HostScrapeStats]]).
 */
class AdaptiveTimeoutScraper(
  delegate: CinemaScraper,
  stats:    HostScrapeStats,
  executor: ExecutorService
) extends CinemaScraper with Logging {

  val cinema: Cinema = delegate.cinema
  def scrapeHosts: Set[String] = delegate.scrapeHosts

  /** The stats key: the delegate's whole host set (so all of a chain's venues
   *  pool together), or the cinema name as a fallback for the degenerate case
   *  of a scraper that declares no parseable host. */
  private[cinemas] val hostKey: String =
    if (delegate.scrapeHosts.nonEmpty) delegate.scrapeHosts.toList.sorted.mkString(",")
    else cinema.displayName

  def fetch(): Seq[CinemaMovie] = {
    val budget = stats.deadlineFor(hostKey)
    val t0     = System.currentTimeMillis()
    val task   = executor.submit(new Callable[Seq[CinemaMovie]] {
      override def call(): Seq[CinemaMovie] = delegate.fetch()
    })
    try {
      val result = task.get(budget.toMillis, TimeUnit.MILLISECONDS)
      stats.record(hostKey, System.currentTimeMillis() - t0)
      result
    } catch {
      case _: TimeoutException =>
        // Interrupt the in-flight scrape so it stops holding a worker slot and
        // CPU instead of running on to its own (much longer) request timeouts.
        task.cancel(true)
        logger.warn(s"${cinema.displayName} scrape cut after ${budget.toMillis}ms — host $hostKey exceeded its adaptive budget")
        throw new TimeoutException(
          s"${cinema.displayName} scrape exceeded ${budget.toMillis}ms adaptive budget for host $hostKey")
      case e: ExecutionException =>
        // Unwrap so retry/uptime see the real scrape failure, not the wrapper.
        throw Option(e.getCause).getOrElse(e)
    }
  }
}
