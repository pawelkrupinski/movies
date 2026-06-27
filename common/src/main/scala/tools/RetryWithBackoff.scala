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
 * `onAttempt` fires once per attempt with the outcome (and an `isFinal`
 * flag on failures) — for callers that want per-attempt observability.
 * The cinema uptime monitor uses it to record only the final outcome:
 * a recovered-within-the-tick scrape is a success, only an exhausted one
 * is a failure.
 *
 * Note: there's also `integration.RetryWithBackoff` in `it/scala`, which
 * is *budget*-based (give up after N seconds total). Different semantics,
 * different scope — the IT one wraps live-network probes that may take
 * 30+ s per attempt and need a hard wall-clock cap; this one wraps
 * production operations where we want bounded attempts, not bounded
 * time.
 */
object RetryWithBackoff extends Logging {

  sealed trait AttemptOutcome { def attempt: Int; def durationMs: Long }
  object AttemptOutcome {
    case class Success(attempt: Int, durationMs: Long) extends AttemptOutcome
    case class Failure(attempt: Int, error: Throwable, isFinal: Boolean, durationMs: Long) extends AttemptOutcome
  }

  def apply[T](
    label:          String,
    maxAttempts:    Int            = 3,
    initialBackoff: FiniteDuration = 1.second,
    sleep:          Long => Unit   = Thread.sleep,
    onAttempt:      AttemptOutcome => Unit = _ => (),
    // Which failures are worth another attempt. Default retries every NonFatal
    // (the original behaviour). A caller that knows some failures are permanent
    // — a 404 / 4xx that won't change on a retry — passes a predicate so those
    // fail fast instead of burning the remaining attempts (and their backoff).
    retryOn:        Throwable => Boolean = _ => true
  )(block: => T): T = {
    require(maxAttempts >= 1, s"maxAttempts must be ≥ 1 (got $maxAttempts)")
    var attempt                = 1
    var lastFailure: Throwable = null
    while (attempt <= maxAttempts) {
      val t0 = System.nanoTime()
      try {
        val result = block
        onAttempt(AttemptOutcome.Success(attempt, (System.nanoTime() - t0) / 1000000L))
        return result
      } catch {
        case NonFatal(t) =>
          val ms = (System.nanoTime() - t0) / 1000000L
          lastFailure = t
          val isFinal = attempt >= maxAttempts || !retryOn(t)
          onAttempt(AttemptOutcome.Failure(attempt, t, isFinal, ms))
          if (isFinal) throw t   // attempts exhausted, or a non-retryable failure
          val wait = initialBackoff * (1L << (attempt - 1))   // 1×, 2×, 4×, …
          logger.warn(
            s"$label attempt $attempt/$maxAttempts failed: " +
            s"${t.getClass.getSimpleName}: ${t.getMessage}; " +
            s"retrying in ${wait.toMillis}ms"
          )
          sleep(wait.toMillis)
          attempt += 1
      }
    }
    throw lastFailure
  }
}
