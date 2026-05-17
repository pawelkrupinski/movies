package tools

import play.api.Logging

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.time.Duration
import java.util.concurrent.CompletableFuture

class RealHttpFetch extends HttpFetch with Logging {
  // Connect + per-request timeouts so a hung upstream (MC search HTML
  // sometimes streams forever, Filmweb soft-blocks by holding the socket
  // open, …) can't pin a worker thread indefinitely. Production was
  // tolerating this — the daemon-thread pool would just have a stuck
  // thread that nobody noticed. The recording script can't: one stuck
  // request blocks `fullySyncOne`'s sequential loop and the rest of the
  // 200+ rows never run.
  private val ConnectTimeout = Duration.ofSeconds(10)
  private val RequestTimeout = Duration.ofSeconds(30)

  private val underlying = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(ConnectTimeout)
    .build()

  override def get(url: String): String = sendLogged("GET", url, underlying.send(buildRequest(url), HttpResponse.BodyHandlers.ofString()))

  override def getAsync(url: String): CompletableFuture[String] =
    underlying.sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofString())
      .handle[String] { (response, throwable) =>
        if (throwable != null) {
          logFailure("GET", url, unwrap(throwable))
          throw throwable
        }
        checkStatus("GET", url, response)
      }

  override def post(url: String, body: String, contentType: String = "application/json"): String = {
    val req = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(RequestTimeout)
      .header("Content-Type", contentType)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    sendLogged("POST", url, underlying.send(req, HttpResponse.BodyHandlers.ofString()))
  }

  // ── Logging hooks ─────────────────────────────────────────────────────────
  //
  // Every HTTP client in the app (TMDB, Filmweb, MC, RT, IMDb, every cinema
  // scraper) routes through this class, so failure visibility added here
  // surfaces in every caller at once. Two failure shapes worth seeing:
  //
  //   - **Network / timeout** — the service didn't respond at all (DNS,
  //     connection refused, socket timeout, request timeout). Always WARN —
  //     this is what masks the symptom users hit (e.g. Kino Muza's burst
  //     limiter stalling our detail fetches: no logs, just silently empty
  //     synopses).
  //   - **5xx response** — the service returned an error status. Also WARN.
  //   - **4xx response** — DEBUG only. Several callers (MetacriticClient's
  //     slug probe, RT's URL probe) deliberately use 404 as the "this
  //     slug doesn't exist" signal; WARN-ing every probe miss would flood
  //     the logs with hundreds of expected 404s per refresh tick.

  private def sendLogged(method: String, url: String, send: => HttpResponse[String]): String = {
    val resp = try send catch {
      case ex: Throwable =>
        logFailure(method, url, ex)
        throw ex
    }
    checkStatus(method, url, resp)
  }

  private def checkStatus(method: String, url: String, resp: HttpResponse[String]): String = {
    val code = resp.statusCode()
    if (code >= 200 && code < 300) resp.body()
    else {
      if (code >= 500) logger.warn(s"HTTP $code from $method $url (server-side)")
      else             logger.debug(s"HTTP $code from $method $url")
      throw new RuntimeException(s"HTTP $code for $method $url")
    }
  }

  private def logFailure(method: String, url: String, ex: Throwable): Unit = ex match {
    case _: HttpTimeoutException =>
      logger.warn(s"HTTP $method $url timed out after ${RequestTimeout.toSeconds}s (service not responding)")
    case _ =>
      logger.warn(s"HTTP $method $url failed: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
  }

  // Async failures come wrapped in `CompletionException`; unwrap so the
  // log line names the real cause (`HttpTimeoutException`, …) rather than
  // the framework wrapper.
  private def unwrap(ex: Throwable): Throwable = ex match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }

  private def buildRequest(url: String): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(RequestTimeout)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .GET()

    decorateBuilder(builder, url).build()
  }

  protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder = builder
}
