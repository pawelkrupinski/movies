package tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

/** Fetches raw poster image bytes for the server-side OG card.
 *
 *  A separate concern from the scraper's [[HttpFetch]] for one reason:
 *  timeouts. `RealHttpFetch` runs a deliberately tight 5s connect budget so a
 *  dead host can't pin a fan-out slot — but several cinema poster origins
 *  (kinobulgarska19.pl, kinomuza.pl) have a cold TCP+TLS connect of ~6-7s, so
 *  that budget kills the fetch and the card renders posterless. This is a
 *  one-shot, request-time fetch, not a fan-out, so it can afford a generous
 *  connect budget. We also fetch the origin directly rather than via weserv,
 *  which is fast from a browser IP but throttles our datacenter IP server-side.
 */
trait PosterFetch {
  /** The image bytes at `url`, or None on any failure (timeout, non-2xx, IO).
   *  Never throws — a missing poster degrades to a text-only card. */
  def bytes(url: String): Option[Array[Byte]]
}

class HttpPosterFetch extends PosterFetch {
  // Generous connect budget for slow cinema origins (~6-7s cold), and a bounded
  // read budget so a hung origin fails fast to text-only instead of pinning the
  // request thread. TlsTrust adds the Certum root some cinema sites need.
  private val client = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .sslContext(TlsTrust.augmentedContext)
    .build()

  def bytes(url: String): Option[Array[Byte]] =
    try {
      val req = HttpRequest.newBuilder()
        .uri(URI.create(url))
        .timeout(Duration.ofSeconds(20))
        .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
        .GET()
        .build()
      val resp = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
      if (resp.statusCode() / 100 == 2) Some(resp.body()) else None
    } catch { case _: Throwable => None }
}
