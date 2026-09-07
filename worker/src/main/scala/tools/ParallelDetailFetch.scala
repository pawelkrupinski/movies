package tools

import play.api.Logging

import java.util.concurrent.{Callable, ExecutionException, TimeUnit, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._

/** Parallel detail-page fetch, each fetch bounded by `timeout`.
 *
 *  The cinema clients share the same shape: spin up a bounded virtual-thread
 *  EC, map each URL to a `Future(url -> fetchOne(url))`, `Future.sequence`,
 *  `Await.result`, shutdown. Before this extraction none of them logged
 *  progress, so a timeout surfaced in Sentry as a bare `TimeoutException`
 *  with no indication of how many URLs were involved or which ones hung.
 *
 *  Adds:
 *    - DEBUG log at the start with batch size
 *    - A PER-FETCH timeout: a fetch that overruns loses only its own key
 *    - WARN log naming how many of the batch timed out, and which URLs
 *    - DEBUG log on success with elapsed time */
object ParallelDetailFetch extends Logging {

  /** `maxConcurrent` caps how many detail pages fetch at once within this one
   *  call (default 2). Each call still gets its OWN pool — it must not share
   *  the scrape/enrichment budget, since the caller `Await`s on these futures
   *  while itself holding a scrape permit (sharing would self-deadlock). The
   *  cap stops a single cinema with 30+ films from spinning up 30+ parsing
   *  threads at once and spiking the (single) vCPU on a cold-start scrape.
   *
   *  `timeout` bounds EACH fetch from the moment it starts (not the batch: a
   *  fetch queued behind the cap is not charged for the wait). A fetch that
   *  overruns is interrupted and its key is simply absent from the result —
   *  the rest of the batch returns. This used to be a batch deadline, then no
   *  deadline at all on the reasoning that the HTTP layer bounds every request;
   *  a fetch that hung past that bound then held the whole batch, and with it
   *  the scrape permit the caller was sitting on.
   *
   *  The `fetch` function MUST swallow its own failures and return a default
   *  (e.g. `Try(http.get(url)).toOption.map(parse).getOrElse(empty)`): a
   *  `fetch` that throws fails the whole `Future.sequence`, so one dead detail
   *  page would lose the entire batch instead of just that page. */
  def apply[T](
    label:         String,
    urls:          Seq[String],
    timeout:       FiniteDuration,
    maxConcurrent: Int = 2
  )(fetch: String => T): Map[String, T] =
    keyed(label, urls, timeout, maxConcurrent)(identity)(fetch)

  /** Like [[apply]] but keyed by a caller domain key `K` (film id, slug, date)
   *  rather than the URL. Each key maps to exactly one URL via `urlOf`; the
   *  result is keyed back by `K`, so call sites that group showings by id/slug
   *  don't have to round-trip through the URL. Keys are de-duplicated before
   *  fetching, so a key that appears twice is fetched once. */
  def keyed[K, T](
    label:         String,
    keys:          Seq[K],
    timeout:       FiniteDuration,
    maxConcurrent: Int = 2
  )(urlOf: K => String)(fetch: String => T): Map[K, T] = {
    val distinct = keys.distinct
    if (distinct.isEmpty) return Map.empty
    logger.debug(s"$label: fetching ${distinct.size} detail pages (≤$maxConcurrent at once)")
    val executionContext = DaemonExecutors.boundedEC(label, maxConcurrent)
    // The fetch itself runs on a second, uncapped virtual thread so the capped
    // slot can time it out and move on; an overrunning fetch is interrupted and
    // its (daemon) thread abandoned.
    val fetchThreads = DaemonExecutors.virtualThreadExecutor(s"$label-fetch")
    val t0 = System.currentTimeMillis()
    val timedOut = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    try {
      val futures = distinct.map { key =>
        val url = urlOf(key)
        Future {
          val attempt = fetchThreads.submit((() => fetch(url)): Callable[T])
          try Some(key -> attempt.get(timeout.toMillis, TimeUnit.MILLISECONDS))
          catch {
            case _: TimeoutException     => attempt.cancel(true); timedOut.add(url); None
            case e: ExecutionException   => throw e.getCause
          }
        }(using executionContext)
      }
      // Every fetch is bounded by `timeout` from its own start, so the batch
      // settles within ceil(n / maxConcurrent) × timeout — no batch deadline needed.
      val result = Await.result(Future.sequence(futures)(using implicitly, executionContext), Duration.Inf).flatten.toMap
      val elapsed = System.currentTimeMillis() - t0
      if (timedOut.isEmpty) logger.debug(s"$label: fetched ${result.size} detail pages in ${elapsed}ms")
      else logger.warn(s"$label: ${timedOut.size}/${distinct.size} detail pages timed out after $timeout (${elapsed}ms total): ${timedOut.asScala.mkString(", ")}")
      result
    } finally { executionContext.shutdown(); fetchThreads.shutdown() }
  }
}
