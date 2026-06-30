package services.metrics

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._
import scala.util.Try

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
 * This cache breaks the coupling: a daemon thread re-renders the exposition
 * every `refreshInterval` and stores the bytes; the handler just returns the
 * latest bytes, which is an `AtomicReference.get` — it never touches Mongo and
 * never blocks. At worst the served sample is one refresh interval stale (well
 * under the 15s scrape interval). A render failure (a transient Mongo blip)
 * keeps the last good bytes instead of emptying the response, so a blip becomes
 * a slightly-stale sample rather than a gap.
 *
 * `render` is the (expensive) exposition supplier — injected, not constructed —
 * so a test drives it without Mongo or a real HttpServer.
 */
class MetricsSnapshotCache(
  render:          () => String,
  refreshInterval: FiniteDuration           = 10.seconds,
  scheduler:       ScheduledExecutorService = DaemonExecutors.scheduler("worker-metrics-refresh")
) extends Logging {

  private val latest = new AtomicReference[Array[Byte]](Array.emptyByteArray)

  /** Re-render and cache the exposition; keep the last good bytes on failure so a
   *  transient Mongo blip is a stale sample, not an empty scrape. */
  private[metrics] def refresh(): Unit =
    try latest.set(render().getBytes("UTF-8"))
    catch {
      case e: Throwable =>
        logger.warn(s"/metrics snapshot refresh failed (serving last good): ${e.getMessage}")
    }

  /** The most recently rendered exposition — a non-blocking read off the request
   *  path. Empty only before the first [[refresh]] (i.e. before [[start]]). */
  def current(): Array[Byte] = latest.get()

  /** Prime once synchronously so the first scrape after `start()` has data, then
   *  refresh on a fixed cadence off the request path. */
  def start(): Unit = {
    refresh()
    scheduler.scheduleWithFixedDelay(
      () => Try(refresh()), refreshInterval.toMillis, refreshInterval.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  def stop(): Unit = { scheduler.shutdownNow(); () }
}
