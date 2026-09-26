package services.cinemas.common

import java.time.{Duration, Instant}
import scala.concurrent.duration.FiniteDuration

/** When a failing primary has failed long enough to hand its venue to the fallback
 *  ([[SourceFallbackScraper]]). Both rules ride out a blip on last-good data; they
 *  differ in what "long enough" is measured in, which matters with the cadence: an
 *  hourly venue fails six times inside six hours, a 10-hourly one fails once. */
sealed trait FallbackAfter {
  /** Has the current failing spell — begun at `failingSince`, now `failedRuns` separate
   *  failed runs long, this one included — earned the fallback? */
  def reached(failingSince: Instant, failedRuns: Int, now: Instant): Boolean
}

object FallbackAfter {
  /** After the primary has been failing, without a success, for `duration`. */
  final case class FailingFor(duration: FiniteDuration) extends FallbackAfter {
    def reached(failingSince: Instant, failedRuns: Int, now: Instant): Boolean =
      Duration.between(failingSince, now).toMillis >= duration.toMillis
  }

  /** After `count` separate scrape runs in a row have failed, however far apart they
   *  fall. A run is one call of the wrapper, so the retries inside it are not counted. */
  final case class FailedRuns(count: Int) extends FallbackAfter {
    def reached(failingSince: Instant, failedRuns: Int, now: Instant): Boolean = failedRuns >= count
  }
}
