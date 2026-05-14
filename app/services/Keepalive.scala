package services

import play.api.Logging

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}

/**
 * Periodic self-ping that keeps the Fly.io machine awake.
 *
 * Fly's free-tier machines suspend after a few minutes of HTTP inactivity; the
 * first request after suspension pays a cold-start hit (~5–10s) which is
 * noticeable for users hitting the site directly. A 60s heartbeat is well below
 * the suspension threshold and adds negligible egress.
 *
 * Lifecycle: caller invokes `start()` to schedule the heartbeat and `stop()`
 * to shut down the scheduler. The class itself doesn't decide when to run or
 * register itself anywhere — see `AppLoader` for the wiring.
 */
class Keepalive extends Logging {

  private val Url = "https://kinowo.fly.dev/"

  private val httpClient = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(10))
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  private val scheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "keepalive")
    t.setDaemon(true)
    t
  }

  /** Schedule the heartbeat. 30s initial delay so the first ping doesn't race
   *  the cache warm-up; then every 60s thereafter. Idempotent in spirit but
   *  not designed to be called twice. */
  def start(): Unit = {
    scheduler.scheduleAtFixedRate(() => ping(), 30L, 60L, TimeUnit.SECONDS)
    logger.info(s"Keepalive enabled — pinging $Url every 60s")
  }

  def stop(): Unit = scheduler.shutdown()

  private def ping(): Unit = {
    val t0 = System.currentTimeMillis()
    try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create(Url))
        .timeout(Duration.ofSeconds(20))
        .header("User-Agent", "kinowo-keepalive/1.0")
        .GET()
        .build()
      val response = httpClient.send(request, HttpResponse.BodyHandlers.discarding())
      val elapsed  = System.currentTimeMillis() - t0
      if (response.statusCode() / 100 != 2)
        logger.warn(s"Keepalive ping returned ${response.statusCode()} after ${elapsed}ms")
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        logger.warn(s"Keepalive ping failed after ${elapsed}ms: ${e.getMessage}")
    }
  }
}
