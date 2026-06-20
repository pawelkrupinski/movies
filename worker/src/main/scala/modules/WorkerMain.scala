package modules

import com.sun.net.httpserver.HttpServer
import play.api.Logging

import java.net.InetSocketAddress
import java.time.Instant
import java.util.concurrent.{CountDownLatch, Executors}

/**
 * Entry point for the scrape/enrich worker. A plain `def main` (not `extends
 * App`) so the body runs on an initialised JVM rather than in static-init — a
 * Mongo timeout or a Sentry-via-logback page during boot then surfaces as an
 * ordinary stack trace + non-zero exit (which Fly restarts), not an
 * `ExceptionInInitializerError`.
 *
 * The only inbound HTTP is a Fly health check, served by the JDK's built-in
 * HttpServer — no Play server, router, or Twirl. The main thread parks on a
 * latch so the JVM stays up; a SIGTERM (Fly machine stop) runs the drain hook.
 */
object WorkerMain extends Logging {
  def main(args: Array[String]): Unit = {
    val commit = Option(System.getenv("COMMIT_SHA")).getOrElse("unknown")
    logger.info(s"Worker starting — commit $commit")

    // Bring up /health BEFORE the scrape+enrich boot. The first full scrape +
    // cache hydrate can take ~a minute; liveness ("the process is up") must not
    // wait on readiness ("warmed up"), or Fly's health check times out within
    // its grace period and the deploy fails on a machine that's actually fine.
    // If wiring init throws (e.g. Mongo unreachable) we stop /health and exit
    // non-zero so the failure still surfaces as a crash-loop rather than a
    // healthy-but-idle worker.
    val port   = Option(System.getenv("PORT")).map(_.toInt).getOrElse(9000)
    val health = startHealthServer(port)
    logger.info(s"Worker health up on :$port/health — booting scrape/enrich…")

    val wiring =
      try {
        val w = new WorkerWiring()
        w.start()
        w
      } catch {
        case e: Throwable =>
          logger.error(s"Worker failed to start — shutting down: ${e.getMessage}", e)
          health.stop(0)
          sys.exit(1)
      }
    logger.info("Worker up — scraping/enriching")

    // Register /metrics now that the queue + metrics are live (it reads both).
    addMetricsEndpoint(health, wiring)
    logger.info(s"Worker metrics up on :$port/metrics")

    // /throttle — an external pusher (a Grafana alert on fly_instance_cpu_balance)
    // flips the worker's credit backoff on/off here; the credit threshold lives
    // outside the worker, this just receives the decision.
    addThrottleEndpoint(health, wiring)
    logger.info(s"Worker throttle control up on :$port/throttle")

    val done = new CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      logger.info("Worker received shutdown signal — draining the enrichment cascade…")
      try wiring.stop()
      catch { case e: Throwable => logger.warn(s"Drain error on shutdown: ${e.getMessage}") }
      finally {
        health.stop(0)
        done.countDown()
      }
    }))
    done.await()
  }

  private def startHealthServer(port: Int): HttpServer = {
    val server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 0)
    server.createContext("/health", exchange => {
      val body = "ok".getBytes("UTF-8")
      exchange.sendResponseHeaders(200, body.length.toLong)
      val os = exchange.getResponseBody
      try os.write(body) finally os.close()
    })
    // A tiny daemon pool (not the default single caller-runs executor) so a
    // /metrics scrape — which reads the queue depth from Mongo and can block up
    // to its Await timeout if Mongo is slow — can't delay the /health check.
    server.setExecutor(Executors.newFixedThreadPool(2, (r: Runnable) => {
      val t = new Thread(r, "worker-http"); t.setDaemon(true); t
    }))
    server.start()
    server
  }

  /** Worker task-pipeline metrics for Fly's Prometheus scrape (the `[[metrics]]`
   *  block in fly.worker.toml). Registered on the SAME HttpServer as /health,
   *  AFTER WorkerWiring is up since it reads the live queue + metrics. Renders
   *  the current snapshot; on a Mongo hiccup it answers 500 (empty) rather than
   *  throwing, so a transient blip is a gap in the series, not a crash. */
  /** External throttle control: the credit-balance logic lives outside the worker
   *  (a Grafana alert on `fly_instance_cpu_balance`), and toggles the worker's
   *  reaper backoff through this endpoint. Accepts a simple `?state=on|off`
   *  (curl/manual), a `?throttled=true|false`, OR a Grafana webhook body whose
   *  `"status"` is `"firing"` (→ on) / `"resolved"` (→ off). Reads no metric and
   *  holds no threshold — it just sets `ExternalThrottleGate`. */
  private def addThrottleEndpoint(server: HttpServer, wiring: WorkerWiring): Unit = {
    server.createContext("/throttle", exchange => {
      val state = throttleStateFrom(exchange)
      state.foreach(wiring.externalThrottleGate.setThrottled)
      val body = state.fold("no state — pass ?state=on|off or a Grafana firing/resolved webhook")(
        on => s"throttled=$on").getBytes("UTF-8")
      exchange.sendResponseHeaders(if (state.isDefined) 200 else 400, body.length.toLong)
      val os = exchange.getResponseBody
      try os.write(body) finally os.close()
    })
    ()
  }

  private def throttleStateFrom(exchange: com.sun.net.httpserver.HttpExchange): Option[Boolean] =
    services.tasks.ExternalThrottleGate.parse(
      Option(exchange.getRequestURI.getQuery),
      new String(exchange.getRequestBody.readAllBytes(), "UTF-8"))

  private val MetricsActiveLimit = 1000
  private def addMetricsEndpoint(server: HttpServer, wiring: WorkerWiring): Unit = {
    server.createContext("/metrics", exchange => {
      val body =
        try wiring.taskMetrics.scrape(wiring.taskQueue.monitor(MetricsActiveLimit), wiring.stagingReaper.stepCounts(), Instant.now()).getBytes("UTF-8")
        catch {
          case e: Throwable =>
            logger.warn(s"/metrics scrape failed: ${e.getMessage}")
            Array.emptyByteArray
        }
      if (body.isEmpty) {
        exchange.sendResponseHeaders(500, -1)
        exchange.close()
      } else {
        exchange.getResponseHeaders.set("Content-Type", "text/plain; version=0.0.4; charset=utf-8")
        exchange.sendResponseHeaders(200, body.length.toLong)
        val os = exchange.getResponseBody
        try os.write(body) finally os.close()
      }
    })
    ()
  }
}
