package services.metrics

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._

/**
 * Renders the Prometheus exposition OFF the scrape request path.
 *
 * The worker's `/metrics` handler used to call [[WorkerTaskMetrics.scrape]]
 * synchronously, which samples the queue depth + staging counts from Mongo (a
 * `find`, three `countDocuments`, and a full staging scan — each guarded by a
 * 10s `Await`). VictoriaMetrics scrapes every 15s with a 10s `scrape_timeout`,
 * so whenever that Mongo work was momentarily slow the scrape exceeded the
 * timeout and recorded `up=0` — blanking EVERY `kinowo_worker_*` panel for that
 * window. Measured at ~13% of scrapes even on an otherwise-healthy worker.
 *
 * This cache breaks the coupling: the handler returns the bytes the last render
 * stored — an `AtomicReference.get`, which never touches Mongo and never blocks —
 * and asks for the next render on a daemon thread ([[serve]]), so each scrape is
 * served what the previous one asked for: one render per scrape, a sample at most
 * one scrape interval old. A render failure (a transient Mongo blip) keeps the last
 * good bytes instead of emptying the response, so a blip becomes a slightly-stale
 * sample rather than a gap.
 *
 * `render` is the (expensive) exposition supplier — injected, not constructed —
 * so a test drives it without Mongo or a real HttpServer.
 */
class MetricsSnapshotCache(
  render:     () => String,
  // The fewest milliseconds between two renders, however often the endpoint is read.
  minRefresh: FiniteDuration           = 10.seconds,
  scheduler:  ScheduledExecutorService = DaemonExecutors.scheduler("worker-metrics-refresh"),
  clock:      java.time.Clock
) extends Logging {

  private val latest     = new AtomicReference[Array[Byte]](Array.emptyByteArray)
  private val refreshing = new java.util.concurrent.atomic.AtomicBoolean(false)
  @volatile private var renderedAt = Long.MinValue

  /** Re-render and cache the exposition; keep the last good bytes on failure so a
   *  transient Mongo blip is a stale sample, not an empty scrape. */
  private[metrics] def refresh(): Unit = {
    renderedAt = clock.millis()
    try latest.set(render().getBytes("UTF-8"))
    catch {
      case e: Throwable =>
        logger.warn(s"/metrics snapshot refresh failed (serving last good): ${e.getMessage}")
    }
  }

  /** The most recently rendered exposition — a non-blocking read off the request
   *  path. Empty only before the first [[refresh]] (i.e. before [[start]]). */
  def current(): Array[Byte] = latest.get()

  /** What a scrape is served: [[current]], and a render queued for the NEXT scrape — so the
   *  exposition is rendered once per scrape, not on a timer. It used to re-render every 10 s
   *  under a 30 s scrape interval, and two renders in three were never served: each reads a
   *  thousand active tasks from Mongo, and rendering was 3% of the US worker's CPU (JFR,
   *  2026-10-01). A sample is therefore up to one scrape interval old. Never two renders at
   *  once, nor two within [[minRefresh]], however often the endpoint is read. */
  def serve(): Array[Byte] = {
    val served = current()
    if (clock.millis() - renderedAt >= minRefresh.toMillis && refreshing.compareAndSet(false, true))
      scala.util.Try(scheduler.execute(() => try refresh() finally refreshing.set(false)))
        .failed.foreach(_ => refreshing.set(false))
    served
  }

  /** Prime once synchronously, so the first scrape after `start()` has data; every later
   *  render is asked for by a scrape ([[serve]]). */
  def start(): Unit = refresh()

  def stop(): Unit = { scheduler.shutdownNow(); () }
}
