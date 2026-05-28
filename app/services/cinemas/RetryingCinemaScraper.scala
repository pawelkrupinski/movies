package services.cinemas

import models.{Cinema, CinemaMovie}
import services.UptimeMonitor
import tools.RetryWithBackoff
import tools.RetryWithBackoff.AttemptOutcome

import scala.concurrent.duration._

/**
 * Decorator that wraps any `CinemaScraper.fetch()` with attempt-bounded
 * retry-with-backoff. One short upstream blip per refresh tick (a 5xx, a
 * dropped connection, a parser throwing on a momentarily-broken HTML
 * block) becomes a transient hiccup instead of a missing-cinema gap in
 * the cache.
 *
 * Composition layering: most cinemas have *no* retry logic of their own
 * — this is the first line of defense. Multikino does have an internal
 * session-handling retry plus Zyte's own per-call retry, so
 * the outer retry only fires if every inner layer is exhausted. That's
 * fine: extra coverage in the worst case, no extra calls in the happy
 * path.
 *
 * Constructor knobs default to the same values as `RetryWithBackoff` —
 * 3 attempts × 1s initial backoff — which is the right shape for cinema
 * fetches that normally take 1–10s and rarely need more than one retry
 * to recover.
 *
 * Each attempt is recorded against the `UptimeMonitor` under
 * `cinema.displayName`. A scrape that succeeds on attempt 2 records
 * 1 failure + 1 success in the bucket — the yellow bar reflects the
 * real reliability of the upstream, not just the final post-retry
 * outcome.
 */
class RetryingCinemaScraper(
  delegate:       CinemaScraper,
  monitor:        UptimeMonitor,
  maxAttempts:    Int            = 3,
  initialBackoff: FiniteDuration = 1.second
) extends CinemaScraper {

  val cinema: Cinema = delegate.cinema

  def fetch(): Seq[CinemaMovie] =
    RetryWithBackoff(
      label          = s"${cinema.displayName} fetch",
      maxAttempts    = maxAttempts,
      initialBackoff = initialBackoff,
      onAttempt      = recordAttempt
    ) {
      delegate.fetch()
    }

  private def recordAttempt(outcome: AttemptOutcome): Unit = outcome match {
    case AttemptOutcome.Success(_) =>
      monitor.recordSuccess(cinema.displayName)
    case AttemptOutcome.Failure(_, t, _) =>
      val msg = s"${t.getClass.getSimpleName}: ${Option(t.getMessage).getOrElse("")}".take(200)
      monitor.recordFailure(cinema.displayName, msg)
  }
}
