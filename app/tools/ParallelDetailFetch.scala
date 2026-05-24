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

  def apply[T](
    label:   String,
    urls:    Seq[String],
    timeout: FiniteDuration
  )(fetch: String => T): Map[String, T] = {
    if (urls.isEmpty) return Map.empty
    logger.debug(s"$label: fetching ${urls.size} detail pages (timeout ${timeout.toSeconds}s)")
    val ec = DaemonExecutors.virtualThreadEC(label)
    val t0 = System.currentTimeMillis()
    val completed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    try {
      val futures = urls.map { url =>
        Future {
          val result = url -> fetch(url)
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
        val pending = urls.filterNot(u => completed.contains(u))
        logger.warn(
          s"$label: timed out after ${elapsed}ms — completed $done/${urls.size}" +
          (if (pending.nonEmpty) s", pending: ${pending.mkString(", ")}" else "")
        )
        throw e
    } finally ec.shutdown()
  }
}
