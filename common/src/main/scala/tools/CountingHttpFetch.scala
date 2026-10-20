package tools

import java.util.concurrent.CompletableFuture

/**
 * Counts the outcome of every outbound HTTP attempt, classified by
 * [[HttpOutcome]], into an [[HttpOutcomeRecorder]]. Transparent: it records and
 * then returns/re-throws exactly what the delegate did — it never retries,
 * swallows, or alters an outcome.
 *
 * Placement is load-bearing. Wire it INNERMOST — directly around the leaf
 * [[RealHttpFetch]], inside [[PacedHttpFetch]] and the 429 gate — because
 * "every retry is one entry" only holds where each retry is a distinct call:
 * the 429 retry in [[ThrottledHttpFetch]] and the scraper-level
 * `RetryWithBackoff` both re-invoke the chain, so each attempt passes through
 * here once. A circuit-breaker fast-fail never reaches the leaf, so it is
 * correctly NOT counted as a wire attempt.
 *
 * Every synchronous member is counted through the one [[ForwardingHttpFetch]] hook, into the
 * recorder [[HttpOutcomeRecorder.forRequest]] picks for the attempt's URL.
 */
class CountingHttpFetch(delegate: HttpFetch, recorder: HttpOutcomeRecorder) extends ForwardingHttpFetch {

  protected def around[T](url: String)(call: HttpFetch => T): T =
    try {
      val result = call(delegate)
      recorder.forRequest(url).record(HttpOutcome.Success)
      result
    } catch {
      case t: Throwable =>
        recorder.forRequest(url).record(HttpOutcome.classify(t))
        throw t
    }

  /** The async fan-out records on completion. A `CompletableFuture` reports its
   *  failure wrapped in a `CompletionException`, so unwrap the cause before
   *  classifying; `whenComplete` is pass-through (it returns the same result /
   *  re-raises the same error). */
  override def getAsync(url: String): CompletableFuture[String] =
    delegate.getAsync(url).whenComplete { (_, err) =>
      val forUrl = recorder.forRequest(url)
      if (err == null) forUrl.record(HttpOutcome.Success)
      else forUrl.record(HttpOutcome.classify(unwrap(err)))
    }

  private def unwrap(t: Throwable): Throwable = t match {
    case _: java.util.concurrent.CompletionException | _: java.util.concurrent.ExecutionException
        if t.getCause != null => t.getCause
    case _ => t
  }
}
