package tools

import play.api.Logging

import java.io.ByteArrayInputStream
import java.net.URI
import java.net.http.{HttpClient, HttpHeaders, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{CookieManager, CookiePolicy}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.CompletableFuture
import java.util.zip.GZIPInputStream

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
    .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    .build()

  override def get(url: String): String =
    sendLogged("GET", url, underlying.send(buildRequest(url), HttpResponse.BodyHandlers.ofByteArray()))

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
    if (code >= 200 && code < 300) decodeBody(resp.headers(), resp.body())
    else {
      if (code >= 500) logger.warn(s"HTTP $code from $method $url (server-side)")
      else             logger.debug(s"HTTP $code from $method $url")
      throw new RuntimeException(s"HTTP $code for $method $url")
    }
  }

  /** Decode the raw response bytes to a UTF-8 string. If the body starts
   *  with the gzip magic prefix `1F 8B`, gunzip it regardless of headers
   *  — some servers (notably TMDB's `/movie/<id>/external_ids` endpoint
   *  as of mid-2026) ship gzip bytes without the `Content-Encoding`
   *  header. If the decompress fails (truncated, corrupt), fall back to
   *  the raw bytes so the caller sees a String it can investigate
   *  rather than a black-box `IOException`.
   *
   *  Identity-encoded responses (no magic prefix) pass through
   *  unchanged. */
  private def decodeBody(headers: HttpHeaders, bytes: Array[Byte]): String = {
    val ceHeader = headers.firstValue("Content-Encoding").orElse("")
    val gzipByHeader = ceHeader.equalsIgnoreCase("gzip")
    val gzipByMagic  = bytes.length >= 2 && (bytes(0) & 0xFF) == 0x1F && (bytes(1) & 0xFF) == 0x8B
    val isGzip       = gzipByHeader || gzipByMagic
    // Diagnostic: any response whose first byte is in the binary range
    // (< 0x20, excluding tab/LF/CR) gets a one-line dump of the headers
    // and first 8 bytes. Lets the next prod failure tell us exactly what
    // reaches the decoder — header values, byte values, length. WARN so
    // it surfaces in default-level logging; cheap because the predicate
    // is false for every JSON / HTML response (those start with `{` or
    // `<`).
    val first = if (bytes.length > 0) bytes(0) & 0xFF else -1
    if (first >= 0 && first < 0x20 && first != 0x09 && first != 0x0A && first != 0x0D) {
      val hex = bytes.take(8).map(b => f"${b & 0xFF}%02X").mkString(" ")
      logger.warn(
        s"decodeBody: binary-looking response (first8=$hex len=${bytes.length}) " +
        s"Content-Encoding='$ceHeader' gzipByHeader=$gzipByHeader gzipByMagic=$gzipByMagic"
      )
    }
    val decoded =
      if (isGzip) {
        try {
          val gz = new GZIPInputStream(new ByteArrayInputStream(bytes))
          try gz.readAllBytes() finally gz.close()
        } catch {
          case ex: Throwable =>
            logger.warn(
              s"gzip decode failed (${ex.getClass.getSimpleName}: ${ex.getMessage}); " +
              s"first 4 bytes=${bytes.take(4).map(b => f"${b & 0xFF}%02X").mkString(" ")}, " +
              s"length=${bytes.length} — passing raw bytes through"
            )
            bytes
        }
      } else bytes
    new String(decoded, StandardCharsets.UTF_8)
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
      // Advertise gzip — `decodeBody` decompresses transparently. Without
      // this header some CDNs still gzip based on the Chrome-shaped UA we
      // send, so the decode path runs regardless; advertising it makes the
      // negotiation explicit and shrinks the wire payload.
      .header("Accept-Encoding", "gzip")
      .GET()

    decorateBuilder(builder, url).build()
  }

  protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder = builder
}
