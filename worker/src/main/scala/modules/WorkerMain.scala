package modules

import com.sun.net.httpserver.HttpServer
import models.Country
import org.mongodb.scala.MongoClient
import play.api.Logging
import services.MongoConnection
import tools.{Env, ExecutionBudget}

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
    val liveness = new BootLiveness
    val health   = startHealthServer(port, liveness)
    logger.info(s"Worker health up on :$port/health — booting scrape/enrich…")

    // The countries this worker runs (KINOWO_COUNTRIES, default just the default
    // country). One shared background concurrency budget + one shared MongoClient
    // are built ONCE here and injected into every country's wiring, so all
    // countries draw run permits from one cap and reuse one connection pool /
    // monitor-thread set — each country still keeps its OWN event bus and its OWN
    // per-country database view on that shared client.
    // The process's config — env vars, `.env.local`, and (once each wiring's
    // EnvConfigService starts) the admin overrides. Built ONCE here and handed to
    // every country's wiring, so a flip reaches all of them.
    val env          = Env.fromProcess()
    val countries    = resolveCountries(env)
    // Refuse a configuration this process cannot normalise correctly, before any
    // wiring touches the corpus.
    unsupportedCountries(countries).foreach { why =>
      logger.error(why)
      health.stop(0)
      sys.exit(1)
    }
    val sharedBudget: ExecutionBudget = WorkerWiring.backgroundBudgetFrom(env)
    val sharedClient: Option[MongoClient] = MongoConnection.sharedClientFromEnv(env)
    // ONE metrics bundle for the whole JVM: a single Prometheus registry + one set
    // of metric objects (each tagged with a `country` label), shared by every
    // country's wiring. This is what fixes the earlier "primary country's registry
    // only, others headless" gap — every country writes its own `country="…"` slice
    // and ALL of them surface on the single /metrics endpoint below.
    val workerMetrics = new services.metrics.WorkerMetrics(
      countries.map(_.code), env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4))
    // ONE poster-shrink gate for the JVM: the vips child it bounds shares the pod's
    // memory cgroup with every country's renders.
    val posterShrinkGate = services.sharecards.VipsPosterShrinker.newGate()
    logger.info(s"Worker running countries: ${countries.map(_.code).mkString(", ")}")

    val wirings =
      try {
        val ws = countries.map(c => new WorkerWiring(c, sharedBudget, sharedClient, Some(workerMetrics), posterShrinkGate, env))
        // Open (and so claim — `MongoConnection.forCountry`) every country's database
        // before any wiring starts, so a mismatch refuses the whole boot up front.
        ws.foreach(_.mongoConnection)
        ws.foreach(_.start())
        workerMetrics.start() // process-level JVM/native samplers, once
        // Process-wide secrets, so reported once rather than per country: gauge + WARN for
        // any integration a missing one has quietly switched off.
        val integrations = modules.wiring.WorkerIntegrations.features(env.get)
        workerMetrics.envGatedFeatures.recordIntegrations(integrations)
        services.metrics.EnvGatedFeature.disabledWarning("integration", integrations).foreach(w => logger.warn(w))
        ws
      } catch {
        case e: Throwable =>
          logger.error(s"Worker failed to start — shutting down: ${e.getMessage}", e)
          sharedClient.foreach(_.close())
          health.stop(0)
          sys.exit(1)
      }
    // /health is a property of the MACHINE, not of any one country, so it folds
    // across every wiring — see [[WorkerFleet]]. Metrics likewise cover all countries
    // (shared registry, `country` label).
    val fleet = new WorkerFleet(wirings.map(_.livenessWatchdog))
    // Process-wide config (KINOWO_HEAP_DUMP_DIR), so it reads the same on every
    // wiring — one dump dir per machine, not per country.
    val heapDumpDir = wirings.head.heapDumpDir
    logger.info("Worker up — scraping/enriching")

    // Register /metrics now that every country's queue + metrics are live: one
    // scrape renders the shared registry with all countries' series.
    addMetricsEndpoint(health, workerMetrics, wirings)
    logger.info(s"Worker metrics up on :$port/metrics")

    addHeapDumpEndpoint(health, heapDumpDir)

    // Now that the heartbeat + watchdog are running, let /health report real
    // liveness: it goes 503 (and the watchdog restarts the process) only once a
    // heartbeat pulse has been stale for minutes — a wedged JVM, not a slow boot.
    // ANY country going stale wedges the machine they share.
    liveness.becomes(() => fleet.isAlive)
    // Ensure the heap-dump volume dir exists so the JVM's HeapDumpOnOutOfMemoryError
    // (hard-OOM path) and the watchdog (death-spiral path) both have somewhere to write.
    try java.nio.file.Files.createDirectories(java.nio.file.Paths.get(heapDumpDir))
    catch { case e: Throwable => logger.warn(s"Could not create heap-dump dir $heapDumpDir: ${e.getMessage}") }

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
  private def resolveCountries(env: Env): Seq[Country] = {
    val codes = env.get("KINOWO_COUNTRIES")
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

  /** Why this worker must NOT boot with the countries it was given, or None when
   *  the configuration is safe.
   *
   *  Introduced because the title normalizer used to resolve ONE rule set per
   *  process from the environment, so a multi-country worker keyed every country
   *  with Poland's rules — the German "Minions & Monster" stored as
   *  `minionsimonster`, a key its own web tier could never produce. That cause is
   *  gone: every component now takes its country's normalizer from its wiring
   *  (`CorpusWiring.titleNormalizer`), and `NoDefaultTitleNormalizerSpec` keeps an
   *  environment-resolved default from coming back. The refusal stays until
   *  running several countries in one JVM is a deliberate deploy decision. */
  private[modules] def unsupportedCountries(countries: Seq[Country]): Option[String] =
    Option.when(countries.sizeIs > 1)(
      s"KINOWO_COUNTRIES names ${countries.size} countries (${countries.map(_.code).mkString(", ")}), but a " +
        s"worker runs one country per process: give each country its own worker.")

  private def startHealthServer(port: Int, liveness: BootLiveness): HttpServer = {
    val server = HttpServer.create(new InetSocketAddress("0.0.0.0", port), 0)
    addHealthEndpoint(server, liveness)
    // A tiny daemon pool (not the default single caller-runs executor) so a
    // /metrics scrape — which reads the queue depth from Mongo and can block up
    // to its Await timeout if Mongo is slow — can't delay the /health check.
    server.setExecutor(Executors.newFixedThreadPool(2, (r: Runnable) => {
      val t = new Thread(r, "worker-http"); t.setDaemon(true); t
    }))
    server.start()
    server
  }

  /** `/health`: 200 while `liveness` holds, 503 once it doesn't. */
  private[modules] def addHealthEndpoint(server: HttpServer, liveness: BootLiveness): Unit = {
    server.createContext("/health", exchange => {
      val alive = liveness.isAlive
      val body  = (if (alive) "ok" else "wedged").getBytes("UTF-8")
      exchange.sendResponseHeaders(if (alive) 200 else 503, body.length.toLong)
      val os = exchange.getResponseBody
      try os.write(body) finally os.close()
    })
    ()
  }

  /** On-demand HPROF dump of the LIVE heap, written to the Fly volume.
   *
   *  `-XX:+HeapDumpOnOutOfMemoryError` only fires at the hard OOM, which is exactly
   *  when you have already lost the machine — and a worker that is merely running hot
   *  never produces one at all. That left the only way to inspect a suspicious heap
   *  being to wait for it to die. The box is JRE-only (no jcmd/jmap), so nothing can
   *  attach from outside either; [[tools.HeapDumper]] could already do this from
   *  inside the process, it just had no trigger but the wedged-watchdog.
   *
   *  Same exposure as /metrics: port 9000 is reachable on the cluster's
   *  private network (Prometheus scrapes it over a NodePort), never publicly.
   *
   *    kubectl -n kinowo port-forward deploy/worker-uk 9000:9000 &
   *    curl -X POST localhost:9000/heapdump
   *
   *  POST-only: a dump stops the world for the length of a full GC and writes a few
   *  hundred MB to the volume, so it must not be reachable by a stray GET from a
   *  health-checker or a link-prefetch. `dump` is injected so the endpoint is testable
   *  without dumping the test JVM's own heap. */
  private[modules] def addHeapDumpEndpoint(server: HttpServer, dir: String,
                                           dump: String => Option[String] = tools.HeapDumper.dump(_)): Unit = {
    server.createContext("/heapdump", exchange => {
      val (status, text) =
        if (exchange.getRequestMethod != "POST") (405, "POST to take a heap dump (it stops the world)")
        else dump(dir).fold((500, "heap dump failed — see the worker log"))(p => (200, s"wrote $p"))
      val body = text.getBytes("UTF-8")
      exchange.sendResponseHeaders(status, body.length.toLong)
      val os = exchange.getResponseBody
      try os.write(body) finally os.close()
    })
    ()
  }

  private val MetricsActiveLimit = 1000

  /** Worker task-pipeline metrics for the VictoriaMetrics scrape (the `[[metrics]]`
   *  block in each worker overlay). Registered on the SAME HttpServer as /health, AFTER
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
        wirings.map(w => services.metrics.WorkerTaskMetrics.CountryQueueSample.read(
          w.country.code, w.taskQueue.monitor(MetricsActiveLimit),
          w.stagingReaper.stepCounts(), w.movieRepository.changeStreamLiveness)),
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
