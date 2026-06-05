package tools

import play.api.Logging

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/** Parallel detail-page fetch with completion tracking.
 *
 *  Four cinema clients (Apollo, CharlieMonroe, Bulgarska, Pałacowe) share
 *  the same shape: spin up a virtual-thread EC, map each URL to a
 *  `Future(url -> fetchOne(url))`, `Future.sequence`, `Await.result`,
 *  shutdown. Before this extraction none of them logged progress, so a
 *  timeout surfaced in Sentry as a bare `TimeoutException` with no
 *  indication of how many URLs were involved or which ones were still
 *  in flight.
 *
 *  Adds:
 *    - DEBUG log at the start with batch size
 *    - Per-URL completion tracking via a concurrent set
 *    - On timeout: WARN log naming completed/total count and the pending URLs
 *    - DEBUG log on success with elapsed time */
object ParallelDetailFetch extends Logging {

  /** `maxConcurrent` caps how many detail pages fetch at once within this one
   *  call (default 6). Each call still gets its OWN pool — it must not share
   *  the scrape/enrichment budget, since the caller `Await`s on these futures
   *  while itself holding a scrape permit (sharing would self-deadlock). The
   *  cap stops a single cinema with 30+ films from spinning up 30+ parsing
   *  threads at once and spiking the (single) vCPU on a cold-start scrape.
   *
   *  The `fetch` function MUST swallow its own failures and return a default
   *  (e.g. `Try(http.get(url)).toOption.map(parse).getOrElse(empty)`): a
   *  `fetch` that throws fails the whole `Future.sequence`, so one dead detail
   *  page would lose the entire batch instead of just that page. */
  def apply[T](
    label:         String,
    urls:          Seq[String],
    timeout:       FiniteDuration,
    maxConcurrent: Int = 6
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
    maxConcurrent: Int = 6
  )(urlOf: K => String)(fetch: String => T): Map[K, T] = {
    val distinct = keys.distinct
    if (distinct.isEmpty) return Map.empty
    logger.debug(s"$label: fetching ${distinct.size} detail pages (timeout ${timeout.toSeconds}s, ≤$maxConcurrent at once)")
    val ec = DaemonExecutors.boundedEC(label, maxConcurrent)
    val t0 = System.currentTimeMillis()
    val completed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    try {
      val futures = distinct.map { key =>
        val url = urlOf(key)
        Future {
          val result = key -> fetch(url)
          completed.add(url)
          result
        }(using ec)
      }
      val result = Await.result(Future.sequence(futures)(using implicitly, ec), timeout).toMap
      val elapsed = System.currentTimeMillis() - t0
      logger.debug(s"$label: fetched ${result.size} detail pages in ${elapsed}ms")
      result
    } catch {
      case e: TimeoutException =>
        val elapsed = System.currentTimeMillis() - t0
        val done    = completed.size()
        val pending = distinct.map(urlOf).filterNot(completed.contains)
        logger.warn(
          s"$label: timed out after ${elapsed}ms — completed $done/${distinct.size}" +
          (if (pending.nonEmpty) s", pending: ${pending.mkString(", ")}" else "")
        )
        throw e
    } finally ec.shutdown()
  }
}
