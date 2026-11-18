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
 * Defaults: 3 attempts, 1s initial backoff. The wait before attempt n+1 is
 * FULL JITTER over the exponential cap — uniform in [0, initial × 2^(n-1)] —
 * so callers that failed together (a pool of threads meeting one upstream
 * blip) don't come back in lockstep and fail together again. Worst case
 * sleeping is still `1s + 2s + 4s + …`. A 429 or 503 carrying `Retry-After`
 * waits what the server asked instead, capped at `maxRetryAfter` so a hint of
 * an hour can't park the calling thread for one. Tune via the parameters
 * when the caller knows its time budget.
 *
 * Logs the call's OUTCOME, not each attempt: a call that ran out of attempts,
 * or failed after at least one retry, warns ONCE with the label, how many
 * attempts it made and the last failure (`describe`); every attempt, and a
 * call that recovered on a retry, is logged at DEBUG. Warned per attempt, a
 * request failing for good wrote a line a try and one that recovered still
 * wrote one (~600 a quiet day, 2,500 on a bad one, 2026-11-05..12). A failure
 * its first attempt may not retry is the caller's to log, as before, and one
 * its caller abandoned ([[CallerCancelled]]) is no failure to warn about.
 * The final failure throws the last exception (not a wrapper),
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
    retryOn:        Throwable => Boolean = _ => true,
    // The jitter draw, uniform in [0, 1). Injected so a spec pins the waits.
    random:         () => Double = () => java.util.concurrent.ThreadLocalRandom.current().nextDouble(),
    // The longest a server's Retry-After is honoured for.
    maxRetryAfter:  FiniteDuration = RetryWithBackoff.DefaultMaxRetryAfter,
    // How a failure reads in the log: a caller whose failures repeat a URL that may carry a key masks it.
    describe:       Throwable => String = RetryWithBackoff.describe
  )(block: => T): T = {
    require(maxAttempts >= 1, s"maxAttempts must be ≥ 1 (got $maxAttempts)")
    val call = Call(label, maxAttempts, onAttempt, retryOn, describe)
    @scala.annotation.tailrec
    def from(attempt: Int): T = call.attempt(attempt)(block) match {
      case Right(result) => result
      case Left(failure) =>
        val wait = waitBefore(attempt, failure, initialBackoff, maxRetryAfter, random)
        logger.debug(s"$label attempt $attempt/$maxAttempts failed: ${describe(failure)}; retrying in ${wait.toMillis}ms")
        sleep(wait.toMillis)
        from(attempt + 1)
    }
    from(1)
  }

  /** One retried call's fixed parts, and what one attempt of it does: run the block, tell `onAttempt`, and either
   *  answer (`Right`), throw the failure that ends the call (attempts exhausted, or one `retryOn` refuses), or hand
   *  back a failure worth another attempt (`Left`). */
  private final case class Call(label: String, maxAttempts: Int, onAttempt: AttemptOutcome => Unit,
                                retryOn: Throwable => Boolean, describe: Throwable => String) {
    def attempt[T](attempt: Int)(block: => T): Either[Throwable, T] = {
      val started = Stopwatch.start()
      try {
        val result = block
        onAttempt(AttemptOutcome.Success(attempt, started.millis))
        if (attempt > 1) logger.debug(s"$label succeeded on attempt $attempt/$maxAttempts")
        Right(result)
      } catch {
        case NonFatal(t) =>
          val ms      = started.millis
          val isFinal = attempt >= maxAttempts || !retryOn(t)
          onAttempt(AttemptOutcome.Failure(attempt, t, isFinal, ms))
          if (isFinal) giveUp(attempt, t)
          Left(t)
      }
    }

    /** Log the call's outcome once -- WARN when it was retried, or could have been, and its caller still wanted it --
     *  and throw its last failure. */
    private def giveUp(attempt: Int, failure: Throwable): Nothing = {
      val attempts = if (attempt == 1) "1 attempt" else s"$attempt attempts"
      val line     = s"$label failed after $attempts: ${describe(failure)}"
      if ((attempt > 1 || retryOn(failure)) && !CallerCancelled(failure)) logger.warn(line) else logger.debug(line)
      throw failure
    }
  }

  val DefaultMaxRetryAfter: FiniteDuration = 30.seconds

  /** A failure as one log line's tail: its type and message. */
  def describe(failure: Throwable): String = s"${failure.getClass.getSimpleName}: ${failure.getMessage}"

  /** The wait after failed attempt `attempt`: the server's Retry-After on a 429/503 (capped), else
   *  full jitter over `initial × 2^(attempt-1)`. */
  private def waitBefore(attempt: Int, failure: Throwable, initialBackoff: FiniteDuration,
                         maxRetryAfter: FiniteDuration, random: () => Double): FiniteDuration =
    failure match {
      case e: HttpStatusException if (e.code == 429 || e.code == 503) && e.retryAfter.isDefined =>
        e.retryAfter.get.min(maxRetryAfter)
      case _ =>
        val cap = initialBackoff * (1L << (attempt - 1))   // 1×, 2×, 4×, …
        math.round(cap.toMillis * random()).millis
    }
}
