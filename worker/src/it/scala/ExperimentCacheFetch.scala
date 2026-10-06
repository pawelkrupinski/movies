package integration

import tools.{HttpFetch, HttpStatusException, RealHttpFetch}

import java.nio.file.{Files, Path}

/** The signal-combination experiment's answers (`<cache>/<host>/<sha256 of "METHOD url body">`, `{status, body}`), else a
 *  live read — kept on disk under `target/agreement-live` so a re-run asks nothing twice, at most `perHost` at a time per
 *  host ([[tools.HostPacing]]: halved on a 429 or 503, retried after a back-off). For the resolver-only replay's
 *  agreement measure and the unmatched-cluster capture only. */
final class ExperimentCacheFetch(cache: Path,
                                 perHost: settings.IdentityLivePerHost = settings.ProcessConfiguration.resolve().identityLivePerHost) extends HttpFetch {
  private val real   = new RealHttpFetch()
  private val pacing = new tools.HostPacing(perHost.value, retries = 5)
  private val Live   = java.nio.file.Paths.get("target", "agreement-live")

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
      val text = pacing(url)(read)
      tools.AtomicFiles.writeString(live, text); text
    }
  }
  override def get(url: String): String = answer("GET", url, "")(real.get(url))
  override def get(url: String, headers: Map[String, String]): String = answer("GET", url, "")(real.get(url, headers))
  override def post(url: String, body: String, contentType: String): String = answer("POST", url, body)(real.post(url, body, contentType))
}
