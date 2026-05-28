package tools

import play.api.Logging

import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * Generic attempts-bounded retry with exponential backoff for production
 * code. Use for any operation whose failure mode is dominated by transient
 * upstream blips (cinema HTML 5xx, proxy 409, Mongo connection
 * hiccup) where one more try after a short wait usually works.
 *
 * Defaults: 3 attempts, 1s initial backoff (1s, 2s sleeps before attempts
 * 2 and 3 — total worst-case wall clock is `n attempts × per-attempt time
 * + 1s + 2s + 4s + …` of sleeping). Tune via the parameters when the
 * caller knows its time budget.
 *
 * Logs every failed attempt at WARN with the label and the exception class
 * + message; the final failure throws the last exception (not a wrapper),
 * so the caller's existing try/catch handling continues to see the same
 * exception type. `NonFatal` is the catch filter — OOM / VM errors /
 * interrupts pass through immediately rather than being papered over with
 * another attempt.
 *
 * `onAttempt` fires once per attempt with the outcome — used by callers
 * that want per-attempt observability (uptime monitor records each
 * attempt, not just the final result, so a green-after-2-retries bucket
 * still shows yellow for the failed first attempts).
 *
 * Note: there's also `integration.RetryWithBackoff` in `it/scala`, which
 * is *budget*-based (give up after N seconds total). Different semantics,
 * different scope — the IT one wraps live-network probes that may take
 * 30+ s per attempt and need a hard wall-clock cap; this one wraps
 * production operations where we want bounded attempts, not bounded
 * time.
 */
object RetryWithBackoff extends Logging {

  sealed trait AttemptOutcome { def attempt: Int }
  object AttemptOutcome {
    case class Success(attempt: Int) extends AttemptOutcome
    case class Failure(attempt: Int, error: Throwable, isFinal: Boolean) extends AttemptOutcome
  }

  def apply[T](
    label:          String,
    maxAttempts:    Int            = 3,
    initialBackoff: FiniteDuration = 1.second,
    sleep:          Long => Unit   = Thread.sleep,
    onAttempt:      AttemptOutcome => Unit = _ => ()
  )(block: => T): T = {
    require(maxAttempts >= 1, s"maxAttempts must be ≥ 1 (got $maxAttempts)")
    var attempt                = 1
    var lastFailure: Throwable = null
    while (attempt <= maxAttempts) {
      try {
        val result = block
        onAttempt(AttemptOutcome.Success(attempt))
        return result
      } catch {
        case NonFatal(t) =>
          lastFailure = t
          val isFinal = attempt >= maxAttempts
          onAttempt(AttemptOutcome.Failure(attempt, t, isFinal))
          if (!isFinal) {
            val wait = initialBackoff * (1L << (attempt - 1))   // 1×, 2×, 4×, …
            logger.warn(
              s"$label attempt $attempt/$maxAttempts failed: " +
              s"${t.getClass.getSimpleName}: ${t.getMessage}; " +
              s"retrying in ${wait.toMillis}ms"
            )
            sleep(wait.toMillis)
          }
          attempt += 1
      }
    }
    throw lastFailure
  }
}
