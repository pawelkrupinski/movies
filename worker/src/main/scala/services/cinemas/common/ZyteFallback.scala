package services.cinemas.common

import tools.{CountingHttpFetch, Env, FallbackHttpFetch, HttpFetch, HttpOutcomeRecorder}

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
 *
 * `apiKey` defaults to the `ZYTE_API_KEY` env read but is a parameter so tests
 * pin both branches deterministically — `Env` reads `System.getenv`, which a
 * test can't unset, and CI does set the key, so reading it inside here would
 * make the "no key → direct" path untestable.
 */
object ZyteFallback {

  def fetchFor(
    direct:       HttpFetch,
    cookieSource: Option[String] = None,
    apiKey:       Option[String] = Env.get("ZYTE_API_KEY"),
    meter:        HttpOutcomeRecorder = HttpOutcomeRecorder.noop
  ): HttpFetch =
    chain(apiKey.filter(_.nonEmpty).map(k => new ZyteFetch(new ZyteClient(httpClient, k), cookieSource)), direct, meter)

  /** Zyte (when there is a Zyte leg) → `direct`, with every Zyte attempt's
   *  outcome going to `meter` — the paid-egress counter; `direct` is free and is
   *  not metered here. Split from [[fetchFor]] so the composition is testable
   *  without a key or a network. */
  def chain(zyte: Option[HttpFetch], direct: HttpFetch, meter: HttpOutcomeRecorder): HttpFetch =
    zyte.fold(direct)(z => new FallbackHttpFetch(Seq("zyte" -> new CountingHttpFetch(z, meter), "direct" -> direct)))

  private lazy val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .build()
}
