package services.cinemas

import tools.{Env, FallbackHttpFetch, HttpFetch}

import java.net.http.HttpClient
import java.time.Duration

/**
 * Builds the `HttpFetch` for a cinema whose site WAF blocks our datacenter IP:
 * Zyte (residential ASN) primary → `direct` fallback. Zyte is included only
 * when `ZYTE_API_KEY` is set, so local dev and the fixture-replay test wiring
 * — neither of which carries the key — collapse the chain to `direct` alone.
 *
 * `cookieSource` is threaded straight to [[ZyteFetch]]: `Some(homepage)` for
 * upstreams with a session-cookie wall (Multikino), `None` for stateless pages
 * (biletyna). Extracted from `MultikinoClient.fetchFor` once a second caller
 * (Kino Kameralne) needed the same chain.
 */
object ZyteFallback {

  def fetchFor(direct: HttpFetch, cookieSource: Option[String] = None): HttpFetch = {
    val zyte = Env.get("ZYTE_API_KEY").map { k =>
      "zyte" -> (new ZyteFetch(new ZyteClient(httpClient, k), cookieSource): HttpFetch)
    }
    val chain = zyte.toSeq :+ ("direct" -> direct)
    if (chain.size == 1) chain.head._2 else new FallbackHttpFetch(chain)
  }

  private lazy val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .build()
}
