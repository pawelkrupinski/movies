package modules

import com.sun.net.httpserver.HttpServer
import models.Country
import org.mongodb.scala.MongoClient
import play.api.Logging
import services.MongoConnection
import tools.{Env, ExecutionBudget, SharedExecutionBudget}

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

  // The liveness signal `/health` reports. Starts permissive — `/health` comes up
  // BEFORE wiring (a slow Mongo boot must not fail the check), and a still-booting
  // process IS alive — then is swapped to the LivenessWatchdog once wiring is up.
  @volatile private var livenessProbe: () => Boolean = () => true

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

    // The countries this worker runs (KINOWO_COUNTRIES, default just the default
    // country). One shared background concurrency budget + one shared MongoClient
    // are built ONCE here and injected into every country's wiring, so all
    // countries draw run permits from one cap and reuse one connection pool /
    // monitor-thread set — each country still keeps its OWN event bus and its OWN
    // per-country database view on that shared client.
    val countries    = resolveCountries()
    val sharedBudget: ExecutionBudget =
      new SharedExecutionBudget(Env.positiveInt("KINOWO_BG_CONCURRENCY", 4))
    val sharedClient: Option[MongoClient] = MongoConnection.sharedClientFromEnv()
    // ONE metrics bundle for the whole JVM: a single Prometheus registry + one set
    // of metric objects (each tagged with a `country` label), shared by every
    // country's wiring. This is what fixes the earlier "primary country's registry
    // only, others headless" gap — every country writes its own `country="…"` slice
    // and ALL of them surface on the single /metrics endpoint below.
    val workerMetrics = new services.metrics.WorkerMetrics(
      countries.map(_.code), Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4))
    logger.info(s"Worker running countries: ${countries.map(_.code).mkString(", ")}")

    val wirings =
      try {
        val ws = countries.map(c => new WorkerWiring(c, sharedBudget, sharedClient, Some(workerMetrics)))
        ws.foreach(_.start())
        workerMetrics.start() // process-level JVM/native samplers, once
        ws
      } catch {
        case e: Throwable =>
          logger.error(s"Worker failed to start — shutting down: ${e.getMessage}", e)
          sharedClient.foreach(_.close())
          health.stop(0)
          sys.exit(1)
      }
    // Liveness/throttle below target the FIRST country's wiring (the default-country
    // one on a single-country deploy). Metrics now cover ALL countries (shared
    // registry, country label). TODO(multi-country): per-country liveness/watchdog +
    // per-country /throttle routing; for now the primary's watchdog is exposed.
    val wiring = wirings.head
    logger.info("Worker up — scraping/enriching")

    // Register /metrics now that every country's queue + metrics are live: one
    // scrape renders the shared registry with all countries' series.
    addMetricsEndpoint(health, workerMetrics, wirings)
    logger.info(s"Worker metrics up on :$port/metrics")

    // /throttle — an external pusher (a Grafana alert on fly_instance_cpu_balance)
    // flips the worker's credit backoff on/off here; the credit threshold lives
    // outside the worker, this just receives the decision.
    addThrottleEndpoint(health, wiring)
    logger.info(s"Worker throttle control up on :$port/throttle")

    // Now that the heartbeat + watchdog are running, let /health report real
    // liveness: it goes 503 (and the watchdog restarts the process) only once the
    // heartbeat pulse has been stale for minutes — a wedged JVM, not a slow boot.
    livenessProbe = () => wiring.livenessWatchdog.isAlive
    // Ensure the heap-dump volume dir exists so the JVM's HeapDumpOnOutOfMemoryError
    // (hard-OOM path) and the watchdog (death-spiral path) both have somewhere to write.
    try java.nio.file.Files.createDirectories(java.nio.file.Paths.get(wiring.heapDumpDir))
    catch { case e: Throwable => logger.warn(s"Could not create heap-dump dir ${wiring.heapDumpDir}: ${e.getMessage}") }

    val done = new CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      logger.info("Worker received shutdown signal — draining the enrichment cascade…")
      try workerMetrics.stop() // process-level samplers, once
      catch { case e: Throwable => logger.warn(s"Metrics stop error on shutdown: ${e.getMessage}") }
      wirings.foreach { w =>
        try w.stop()
        catch { case e: Throwable => logger.warn(s"Drain error on shutdown: ${e.getMessage}") }
      }
      // Close the shared client last — each wiring's `stop()` closed only its own
      // (per-country) resources; the borrowed client is owned here.
      try sharedClient.foreach(_.close())
      catch { case e: Throwable => logger.warn(s"Mongo client close error on shutdown: ${e.getMessage}") }
      finally {
        health.stop(0)
        done.countDown()
      }
    }))
    done.await()
  }

  /** The countries this worker instance runs, from `KINOWO_COUNTRIES` (comma-
   *  separated codes), defaulting to just [[Country.default]] so a single-country
   *  deploy needs no new env var. Unknown codes are logged and skipped; an empty
   *  or all-unknown list falls back to the default so the worker never boots with
   *  zero countries. */
  private def resolveCountries(): Seq[Country] = {
    val codes = Env.get("KINOWO_COUNTRIES")
      .map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList)
      .filter(_.nonEmpty)
      .getOrElse(List(Country.default.code))
    val resolved = codes.flatMap { code =>
      Country.byCode(code).orElse {
        logger.warn(s"Unknown country code '$code' in KINOWO_COUNTRIES — skipping.")
        None
      }
    }.distinct
    if (resolved.isEmpty) Seq(Country.default) else resolved
  }

  private def startHealthServer(port: Int): HttpServer = {
    val server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 0)
    server.createContext("/health", exchange => {
      val alive = livenessProbe()
      val body  = (if (alive) "ok" else "wedged").getBytes("UTF-8")
      exchange.sendResponseHeaders(if (alive) 200 else 503, body.length.toLong)
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

  /** Worker task-pipeline metrics for the VictoriaMetrics scrape (the `[[metrics]]`
   *  block in fly.worker.toml). Registered on the SAME HttpServer as /health, AFTER
   *  WorkerWiring is up since it reads the live queue + metrics. Served from a
   *  [[services.metrics.MetricsSnapshotCache]] so the scrape never blocks on the
   *  Mongo reads taskMetrics.scrape performs — see that class for why. */
  private def addMetricsEndpoint(server: HttpServer, workerMetrics: services.metrics.WorkerMetrics, wirings: Seq[WorkerWiring]): Unit = {
    // Render the exposition OFF the scrape request path. The per-country scrape
    // reads each country's queue depth + staging counts from Mongo (a find + three
    // countDocuments + a full staging scan, each a 10s Await); doing that inside the
    // handler made a momentarily-slow Mongo blow VictoriaMetrics' 10s scrape_timeout
    // → up=0 → every kinowo_worker_* panel blank for that window. The cache refreshes
    // on a daemon thread and the handler just returns the last rendered bytes.
    //
    // One render over the SHARED registry covers every country (each wiring supplies
    // its own country-tagged queue sample) plus the process-level JVM/native series.
    val snapshot = new services.metrics.MetricsSnapshotCache(render = () =>
      workerMetrics.taskSeries.scrape(
        wirings.map(w => services.metrics.WorkerTaskMetrics.CountryQueueSample(
          w.country.code, w.taskQueue.monitor(MetricsActiveLimit),
          w.stagingReaper.stepCounts(), w.throttleSignal.isThrottled)),
        Instant.now()))
    snapshot.start()
    server.createContext("/metrics", exchange => {
      val body = snapshot.current()
      if (body.isEmpty) {
        exchange.sendResponseHeaders(503, -1) // only before the first refresh completes
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
