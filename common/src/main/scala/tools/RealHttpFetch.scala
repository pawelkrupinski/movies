package tools

import play.api.Logging

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{CookieManager, CookiePolicy}
import java.nio.charset.StandardCharsets
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
  // Connect timeout bounds only the TCP handshake — a live host completes it
  // in well under a second, so 5s is generous headroom. Keeping it low matters
  // for the fan-out scrapers: ParallelDetailFetch runs just 2 fetches at once,
  // so a dead/refusing host that hangs the connect pins HALF the budget for the
  // whole timeout. At 10s a couple of unreachable detail URLs stalled a wave for
  // 10s each (most of Kinoteka's ~57s scrape); at 5s the slot is freed twice as
  // fast to fetch the films that *are* reachable. The request timeout below is
  // separate — it bounds reading the response, where a slow upstream legitimately
  // needs longer — so trimming connect doesn't cut off slow-but-alive servers.
  private val ConnectTimeout = Duration.ofSeconds(5)
  private val RequestTimeout = Duration.ofSeconds(30)

  // `CookieManager` makes the client a well-behaved HTTP citizen: any
  // `Set-Cookie` header lands in the in-memory store and is sent back on
  // subsequent requests to the same domain. Multikino's direct path
  // depends on it — the homepage hands out a session cookie that the
  // API call must carry — and other clients are unaffected (cookies
  // are domain-scoped, and the APIs we talk to are otherwise stateless).
  private val underlying = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(ConnectTimeout)
    // Trust the JDK defaults PLUS the Certum root that OpenJDK's cacerts omits,
    // so the Certum-rooted cinema sites (Kinomuzeum/artmuseum.pl, Kino
    // Muranów/kinomuranow.pl, sdk.waw.pl) stop failing PKIX path building. See
    // TlsTrust — touching it here also enables AIA intermediate fetching.
    .sslContext(TlsTrust.augmentedContext)
    .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    .build()

  override def get(url: String): String = get(url, Map.empty)

  override def get(url: String, headers: Map[String, String]): String =
    sendLogged("GET", url, underlying.send(buildRequest(url, headers), HttpResponse.BodyHandlers.ofByteArray()))

  override def getAsync(url: String): CompletableFuture[String] =
    underlying.sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofByteArray())
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
      // Decoded transparently in `decodeBody` below. See the matching
      // header on `buildRequest` for the GET path.
      .header("Accept-Encoding", "gzip")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    sendLogged("POST", url, underlying.send(req, HttpResponse.BodyHandlers.ofByteArray()))
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

  private def sendLogged(method: String, url: String, send: => HttpResponse[Array[Byte]]): String = {
    val resp = try send catch {
      case ex: Throwable =>
        logFailure(method, url, ex)
        throw ex
    }
    checkStatus(method, url, resp)
  }

  private def checkStatus(method: String, url: String, resp: HttpResponse[Array[Byte]]): String = {
    val code = resp.statusCode()
    if (code >= 200 && code < 300) decodeBody(resp.body())
    else {
      if (code >= 500) logger.warn(s"HTTP $code from $method $url (server-side)")
      else             logger.debug(s"HTTP $code from $method $url")
      throw new RuntimeException(s"HTTP $code for $method $url")
    }
  }

  /** Decode the raw response bytes to a UTF-8 string. `Gunzip.decode`
   *  loops while the bytes still start with the gzip magic prefix
   *  `1F 8B` — TMDB (or its CDN) ships some responses as
   *  `gzip(gzip(json))` under a single `Content-Encoding: gzip` header,
   *  so one round of decompression isn't enough. Non-gzip and
   *  single-gzip bodies are handled by the same call. */
  private def decodeBody(bytes: Array[Byte]): String =
    new String(Gunzip.decode(bytes), StandardCharsets.UTF_8)

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

  private def buildRequest(url: String, extraHeaders: Map[String, String] = Map.empty): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(RequestTimeout)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      // Advertise gzip — `decodeBody` decompresses transparently. Without
      // this header some CDNs still gzip based on the Chrome-shaped UA we
      // send, so the decode path runs regardless; advertising it makes the
      // negotiation explicit and shrinks the wire payload.
      .header("Accept-Encoding", "gzip")
      .GET()
    extraHeaders.foreach { case (k, v) => builder.header(k, v) }

    decorateBuilder(builder, url).build()
  }

  protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder = builder
}
