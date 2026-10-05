package services.cinemas.common

import tools.{CountingHttpFetch, FallbackHttpFetch, HttpFetch, HttpOutcomeRecorder}

import java.net.http.HttpClient
import java.time.Duration

/**
 * Builds the `HttpFetch` for a cinema whose origin firewall blocks both our
 * datacenter IP and the Decodo proxy: Zyte (residential ASN) primary → `direct`
 * fallback. Zyte is included only when there is an API key, so local dev and
 * the fixture-replay test wiring — neither of which carries the key — collapse
 * the chain to `direct` alone.
 *
 * `zyteHttp` is the JDK client the Zyte API calls go through — built by the
 * composition root ([[newHttpClient]]) and handed in, by name, so it is only built
 * when there is a key to use it with.
 */
object ZyteFallback {

  def fetchFor(
    direct:  HttpFetch,
    zyteHttp: => HttpClient,
    apiKey:  Option[settings.ZyteApiKey],
    meter:   HttpOutcomeRecorder = HttpOutcomeRecorder.noop
  ): HttpFetch =
    chain(apiKey.map(key => new ZyteFetch(new ZyteClient(zyteHttp, key))), direct, meter)

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
