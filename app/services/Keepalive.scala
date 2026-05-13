package services

import play.api.{Environment, Logging, Mode}
import play.api.inject.ApplicationLifecycle

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

/**
 * Periodic self-ping that keeps the Fly.io machine awake.
 *
 * Fly's free-tier machines suspend after a few minutes of HTTP inactivity; the
 * first request after suspension pays a cold-start hit (~5–10s) which is
 * noticeable for users hitting the site directly. A 60s heartbeat is well below
 * the suspension threshold and adds negligible egress.
 *
 * Disabled outside production so dev runs (and tests) don't make network calls
 * on boot.
 */
@Singleton
class Keepalive @Inject()(lifecycle: ApplicationLifecycle, env: Environment) extends Logging {

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

  if (env.mode == Mode.Prod) {
    // 30s initial delay so the first ping doesn't race the cache warm-up; then
    // every 60s thereafter.
    scheduler.scheduleAtFixedRate(() => ping(), 30L, 60L, TimeUnit.SECONDS)
    logger.info(s"Keepalive enabled — pinging $Url every 60s")
  }

  lifecycle.addStopHook(() => Future.successful(scheduler.shutdown()))

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
