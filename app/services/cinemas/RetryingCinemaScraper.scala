package services.cinemas

import models.{Cinema, CinemaMovie}
import tools.RetryWithBackoff

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
 * session-handling retry plus Zyte/ScrapingAnt's own per-call retry, so
 * the outer retry only fires if every inner layer is exhausted. That's
 * fine: extra coverage in the worst case, no extra calls in the happy
 * path.
 *
 * Constructor knobs default to the same values as `RetryWithBackoff` —
 * 3 attempts × 1s initial backoff — which is the right shape for cinema
 * fetches that normally take 1–10s and rarely need more than one retry
 * to recover.
 */
class RetryingCinemaScraper(
  delegate:       CinemaScraper,
  maxAttempts:    Int            = 3,
  initialBackoff: FiniteDuration = 1.second
) extends CinemaScraper {

  val cinema: Cinema = delegate.cinema

  def fetch(): Seq[CinemaMovie] =
    RetryWithBackoff(s"${cinema.displayName} fetch", maxAttempts, initialBackoff) {
      delegate.fetch()
    }
}
