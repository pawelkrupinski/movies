package modules

import com.sun.net.httpserver.HttpServer
import play.api.Logging

import java.net.InetSocketAddress
import java.util.concurrent.CountDownLatch

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

    val wiring = new WorkerWiring()
    wiring.start()

    val port   = Option(System.getenv("PORT")).map(_.toInt).getOrElse(9000)
    val health = startHealthServer(port)
    logger.info(s"Worker up — scraping/enriching; health on :$port/health")

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
    server.setExecutor(null) // default executor — health traffic is trivial
    server.start()
    server
  }
}
