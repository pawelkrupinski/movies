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
 * The key (`ZYTE_API_KEY`) and the session TTL come from the [[tools.Env]] the
 * caller hands in — the composition root's in production, a fixed `Env.of(…)` in
 * a spec, so both branches are testable even where CI sets the key.
 *
 * `zyteHttp` is the JDK client the Zyte API calls go through — built by the
 * composition root ([[newHttpClient]]) and handed in, by name, so it is only built
 * when there is a key to use it with.
 */
object ZyteFallback {

  def fetchFor(
    direct:       HttpFetch,
    zyteHttp:     => HttpClient,
    env:          Env,
    cookieSource: Option[String] = None,
    meter:        HttpOutcomeRecorder = HttpOutcomeRecorder.noop
  ): HttpFetch =
    chain(env.get("ZYTE_API_KEY").map(k =>
      new ZyteFetch(new ZyteClient(zyteHttp, k), cookieSource, ZyteFetch.sessionTtlFrom(env))), direct, meter)

  /** Zyte (when there is a Zyte leg) → `direct`, with every Zyte attempt's
   *  outcome going to `meter` — the paid-egress counter; `direct` is free and is
   *  not metered here. Split from [[fetchFor]] so the composition is testable
   *  without a key or a network. */
  def chain(zyte: Option[HttpFetch], direct: HttpFetch, meter: HttpOutcomeRecorder): HttpFetch =
    zyte.fold(direct)(z => new FallbackHttpFetch(Seq("zyte" -> new CountingHttpFetch(z, meter), "direct" -> direct),
                                                  endsChain = FallbackHttpFetch.OriginAnswered))

  /** The client a wiring builds once and passes to every Zyte chain it composes. */
  def newHttpClient(): HttpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .build()
}
