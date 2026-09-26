package services.scrapes

import java.time.{Duration, Instant}

/** When a failed scrape attempt is a new RUN rather than a retry of the last one.
 *
 *  The reaper re-runs a failed venue on its next ticks, a minute apart, before
 *  parking it (`ScrapeFreshnessPolicy.immediateRetries`), and a chunked reduce
 *  reschedules on its own failure. Counted as runs, one bad minute would read as
 *  three separate failures. Anything that counts failures across runs — the
 *  gone-venue page, a fallback's `FallbackAfter.FailedRuns` — counts an attempt only
 *  when it lands at least [[MinGap]] after the one before it: well past the
 *  retries, and short of the shortest scrape cadence (Poland's hour). */
object SeparateRuns {
  val MinGap: Duration = Duration.ofMinutes(30)

  def isNewRun(previousAttempt: Instant, attempt: Instant): Boolean =
    !attempt.isBefore(previousAttempt.plus(MinGap))
}
