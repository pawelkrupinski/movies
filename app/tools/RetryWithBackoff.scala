package tools

import play.api.Logging

import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * Generic attempts-bounded retry with exponential backoff for production
 * code. Use for any operation whose failure mode is dominated by transient
 * upstream blips (cinema HTML 5xx, ScrapingAnt 409, Mongo connection
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
 * Note: there's also `integration.RetryWithBackoff` in `it/scala`, which
 * is *budget*-based (give up after N seconds total). Different semantics,
 * different scope — the IT one wraps live-network probes that may take
 * 30+ s per attempt and need a hard wall-clock cap; this one wraps
 * production operations where we want bounded attempts, not bounded
 * time.
 */
object RetryWithBackoff extends Logging {

  def apply[T](
    label:          String,
    maxAttempts:    Int            = 3,
    initialBackoff: FiniteDuration = 1.second,
    sleep:          Long => Unit   = Thread.sleep
  )(block: => T): T = {
    require(maxAttempts >= 1, s"maxAttempts must be ≥ 1 (got $maxAttempts)")
    var attempt                = 1
    var lastFailure: Throwable = null
    while (attempt <= maxAttempts) {
      try return block
      catch {
        case NonFatal(t) =>
          lastFailure = t
          if (attempt < maxAttempts) {
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
