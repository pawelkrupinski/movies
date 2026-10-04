package integration

import tools.{HttpFetch, HttpStatusException, RealHttpFetch}

import java.nio.file.{Files, Path}

/** The signal-combination experiment's answers (`<cache>/<host>/<sha256 of "METHOD url body">`, `{status, body}`), else a
 *  live read — kept on disk under `target/agreement-live` so a re-run asks nothing twice, at most [[PerHost]] at a time per
 *  host, a 429 or 503 retried after a back-off. For the resolver-only replay's agreement measure only. */
final class ExperimentCacheFetch(cache: Path) extends HttpFetch {
  private val real  = new RealHttpFetch()
  private val slots = new java.util.concurrent.ConcurrentHashMap[String, java.util.concurrent.Semaphore]()
  private val Live  = java.nio.file.Paths.get("target", "agreement-live")
  private val PerHost = 4

  private def hex(id: String) = java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(id.getBytes("UTF-8")))
  private def answer(method: String, url: String, body: String)(read: => String): String = {
    val host   = java.net.URI.create(url).getHost
    // the experiment wrote a list's `|` bare where a live URL must encode it
    val cached = Seq(url, url.replace("%7C", "|")).map(u => cache.resolve(host).resolve(hex(s"$method $u $body"))).find(Files.exists(_))
      .getOrElse(cache.resolve(host).resolve(hex(s"$method $url $body")))
    val live   = Live.resolve(hex(s"$method $url $body"))
    if (Files.exists(cached)) {
      val js = play.api.libs.json.Json.parse(Files.readString(cached))
      (js \ "status").as[Int] match {
        case 200 => (js \ "body").as[String]
        case 204 => ""
        case code => throw new HttpStatusException(code, method, url, None)
      }
    } else if (Files.exists(live)) Files.readString(live)
    else {
      val slot = slots.computeIfAbsent(host, _ => new java.util.concurrent.Semaphore(PerHost))
      @scala.annotation.tailrec def attempt(n: Int): String =
        scala.util.Try { slot.acquire(); try read finally slot.release() } match {
          case scala.util.Success(text) => text
          case scala.util.Failure(e: HttpStatusException) if (e.code == 429 || e.code == 503) && n < 5 => Thread.sleep(5000L * (n + 1)); attempt(n + 1)
          case scala.util.Failure(e) => throw e
        }
      val text = attempt(0)
      Files.createDirectories(Live); Files.writeString(live, text); text
    }
  }
  override def get(url: String): String = answer("GET", url, "")(real.get(url))
  override def get(url: String, headers: Map[String, String]): String = answer("GET", url, "")(real.get(url, headers))
  override def post(url: String, body: String, contentType: String): String = answer("POST", url, body)(real.post(url, body, contentType))
}
