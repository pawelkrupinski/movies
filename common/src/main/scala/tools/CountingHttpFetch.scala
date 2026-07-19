package tools

import java.util.concurrent.CompletableFuture

/**
 * Counts the outcome of every outbound HTTP attempt, classified by
 * [[HttpOutcome]], into an [[HttpOutcomeRecorder]]. Transparent: it records and
 * then returns/re-throws exactly what the delegate did — it never retries,
 * swallows, or alters an outcome.
 *
 * Placement is load-bearing. Wire it INNERMOST — directly around the leaf
 * [[RealHttpFetch]], inside [[RateLimitedHttpFetch]] and the 429 gate — because
 * "every retry is one entry" only holds where each retry is a distinct call:
 * the 429 retry in [[ThrottledHttpFetch]] and the scraper-level
 * `RetryWithBackoff` both re-invoke the chain, so each attempt passes through
 * here once. A circuit-breaker fast-fail never reaches the leaf, so it is
 * correctly NOT counted as a wire attempt.
 *
 * `getBytes` forwards to the delegate's `getBytes` (not the String `get`), per
 * the HttpFetch contract note — inheriting the default would round-trip a legacy
 * single-byte page through a UTF-8 decode and mojibake it.
 */
class CountingHttpFetch(delegate: HttpFetch, recorder: HttpOutcomeRecorder) extends HttpFetch {

  private def counted[T](block: => T): T =
    try {
      val result = block
      recorder.record(HttpOutcome.Success)
      result
    } catch {
      case t: Throwable =>
        recorder.record(HttpOutcome.classify(t))
        throw t
    }

  override def get(url: String): String                          = counted(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = counted(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte]                = counted(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    counted(delegate.post(url, body, contentType))

  /** The async fan-out records on completion. A `CompletableFuture` reports its
   *  failure wrapped in a `CompletionException`, so unwrap the cause before
   *  classifying; `whenComplete` is pass-through (it returns the same result /
   *  re-raises the same error). */
  override def getAsync(url: String): CompletableFuture[String] =
    delegate.getAsync(url).whenComplete { (_, err) =>
      if (err == null) recorder.record(HttpOutcome.Success)
      else recorder.record(HttpOutcome.classify(unwrap(err)))
    }

  private def unwrap(t: Throwable): Throwable = t match {
    case _: java.util.concurrent.CompletionException | _: java.util.concurrent.ExecutionException
        if t.getCause != null => t.getCause
    case _ => t
  }
}
