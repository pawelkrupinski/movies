package services.facebook

import play.api.Logging
import tools.{DaemonExecutors, LogThrottle}

import java.time.{Clock, Instant}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * Records each film page Facebook's crawler fetches into [[FacebookFetches]] — the pages
 * Facebook holds a preview of, and so the only ones the worker's re-scrapes ask for.
 *
 * A fetch only joins a batch on the request thread; the batch is written every [[FacebookFetchRecorder.FlushEvery]]
 * on a thread of its own, one upsert per page however often Facebook fetched it (it fetched each
 * film page ~7.5 times in a fortnight), and holds at most [[FacebookFetchRecorder.MaxPending]] pages so a crawl of
 * every page cannot grow the heap between writes. A batch the store refuses is dropped and logged:
 * losing a fetch only means that page's next card change is not re-scraped.
 */
final class FacebookFetchRecorder(store: FacebookFetches, clock: Clock, stopwatch: tools.Stopwatch) extends services.Stoppable with Logging {
  import FacebookFetchRecorder.*

  private val pending  = new ConcurrentHashMap[String, Instant]()
  private val failures = new LogThrottle(10.minutes.toNanos, stopwatch)

  private val scheduler = DaemonExecutors.scheduler("facebook-fetches")
  scheduler.scheduleWithFixedDelay(() => flush(), FlushEvery.toMillis, FlushEvery.toMillis, TimeUnit.MILLISECONDS)

  /** A film page served at `url` to a client naming itself `userAgent`: joins the batch when that
   *  is Facebook's crawler. */
  def served(userAgent: Option[String], url: String): Unit =
    if (userAgent.exists(FacebookFetches.isFacebookCrawler) && (pending.size < MaxPending || pending.containsKey(url)))
      pending.put(url, clock.instant())

  /** Write the batch held so far. */
  def flush(): Unit = {
    val batch = pending.asScala.toMap
    batch.foreach { case (url, at) => pending.remove(url, at) }
    Try(store.record(batch)).failed.foreach { e =>
      failures.admit().foreach(suppressed => logger.warn(
        s"facebook fetches: ${batch.size} not recorded (${e.getClass.getSimpleName}: ${e.getMessage}); $suppressed more failures since the last report"))
    }
  }

  def stop(): Unit = {
    scheduler.shutdownNow()
    flush()
  }
}

object FacebookFetchRecorder {
  /** How often the batch is written. */
  val FlushEvery: FiniteDuration = 30.seconds
  /** The most pages a batch holds between writes. */
  val MaxPending: Int = 2000
}
