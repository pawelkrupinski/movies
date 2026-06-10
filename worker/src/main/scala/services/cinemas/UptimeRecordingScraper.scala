package services.cinemas

import models.{Cinema, CinemaMovie}
import services.UptimeMonitor

import scala.util.control.NonFatal

/**
 * Decorator that records a scrape's *outcome* against the `UptimeMonitor`
 * under `cinema.displayName`, classifying the final result of the tick:
 *
 *  - threw  → a failure (a red bar). The exception propagates unchanged, so
 *             the caller's existing handling is untouched. Only `NonFatal`
 *             throws are recorded; OOM / VM errors pass straight through.
 *  - parsed cleanly but yielded zero screenings → an empty (a white "no
 *             screenings" bar). A cinema that's up but silently returning
 *             nothing — an upstream layout change, an empty repertoire page —
 *             is a real failure mode a green bar would hide.
 *  - any screenings → a success (a green bar).
 *
 * Wrapped OUTSIDE `RetryingCinemaScraper`, so the failed attempts a retry
 * recovers from never reach here — only the tick's final outcome is recorded.
 * Splitting record from retry is what lets `FilmwebFallbackScraper` slot
 * between the two and record the outcome it actually served (incl. the
 * "served via Filmweb" flag) instead of the primary's failure.
 */
class UptimeRecordingScraper(
  delegate: CinemaScraper,
  monitor:  UptimeMonitor
) extends CinemaScraper {

  val cinema: Cinema = delegate.cinema

  def scrapeHosts: Set[String] = delegate.scrapeHosts

  def fetch(): Seq[CinemaMovie] = {
    val t0 = System.currentTimeMillis()
    val result =
      try delegate.fetch()
      catch {
        case NonFatal(t) =>
          monitor.recordFailure(cinema.displayName, UptimeRecordingScraper.errorLabel(t))
          throw t
      }
    val ms = System.currentTimeMillis() - t0
    if (result.iterator.map(_.showtimes.size).sum == 0)
      monitor.recordEmpty(cinema.displayName, ms)
    else
      monitor.recordSuccess(cinema.displayName, ms)
    result
  }
}

object UptimeRecordingScraper {
  /** Compact `ClassName: message` label (capped at 200 chars) for the uptime
   *  errors list. */
  def errorLabel(t: Throwable): String =
    s"${t.getClass.getSimpleName}: ${Option(t.getMessage).getOrElse("")}".take(200)
}
