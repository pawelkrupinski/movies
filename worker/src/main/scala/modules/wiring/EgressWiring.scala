package modules.wiring

import modules.WorkerWiring
import services.cinemas.common.ZyteFallback
import services.cinemas.pl.MultikinoClient
import services.cinemas.uk.OdeonAuthHarvester
import services.metrics.PaidEgressMetrics
import tools.{CountingHttpFetch, FallbackHttpFetch, HostCircuitBreakerHttpFetch, HttpFetch, HttpOutcomeRecorder, RealHttpFetch, ResidentialProxy, SessionWarmingHttpFetch, StickyShardHttpFetch}

/** Cinema-site egress routes: the residential-proxy chains the Cloudflare-blocked
 *  venues scrape through (plus the one Zyte route for a venue the proxy cannot
 *  reach), each a seam the fixture wirings collapse back onto `httpFetch`. */
trait EgressWiring { self: WorkerWiring =>
  import EgressWiring.ResidentialProxyService

  // Residential-proxy egress (Decodo static-ISP, PL Netia) for the cinema sites
  // that Cloudflare-block our Fly datacenter IP. Non-secret host+ports come from
  // the committed residential-proxy.properties; the DECODO_PROXY_USER/PASS secrets
  // come from Env (env -> .env.local). Set only when both are present — absent in
  // local/test, where the chain collapses to the direct path. See the
  // `reference_decodo_isp_proxy` memory.
  // One RealHttpFetch per Decodo pool IP (each pinned, own cookie jar), built
  // once and shared by the proxied clients; None where the DECODO_PROXY_* secrets
  // aren't set (local/CI/fixture-replay → direct). Sharing the shards means
  // each IP warms its Multikino session at most once and reuses it across the
  // venues routed there.
  private lazy val proxyShards: Option[IndexedSeq[HttpFetch]] = residentialProxyShards

  /** Where [[proxyShards]] comes from — a seam so the test wirings can refuse the proxy
   *  whatever environment they are handed: `TestWiring` answers None. */
  protected def residentialProxyShards: Option[IndexedSeq[HttpFetch]] =
    EgressWiring.residentialShards(ResidentialProxy.fromConfiguration(configuration), tlsContext)

  // Proxy primary → `fallback` (direct), so a proxy IP that's ever unreachable/burned
  // rolls over to the direct fetch rather than failing outright. There is no paid
  // leg behind the proxy any more: Zyte was dropped as its fallback on 2026-10-05.
  //
  // StickyShardHttpFetch fans each client's venues across all the pool IPs, keyed
  // by venue URL so a given venue always egresses via the same IP while different
  // venues spread across the pool. This keeps Multikino off a single pinned IP —
  // notably it avoids getting stuck on `ports.head`, which is a M247 *datacenter*
  // IP that Multikino's Cloudflare blocks — and lets each IP hold its own warmed
  // Multikino session. (It is NOT what cleared the 2026-06-16 "Limit: 3" outage:
  // that was a Decodo auth rejection — the worker's egress IP fell off the
  // whitelist on a machine recreate — fixed account-side, not in this code.)
  //
  // `warmUrl` wraps EACH shard in its own SessionWarmingHttpFetch — for Multikino,
  // whose films API 401s on a cold call from the proxy IP until the homepage warms
  // a session cookie (verified 2026-06-16). Per-venue stickiness means the warm
  // and the API retry share an IP, and each IP warms once then reuses the cookie.
  // Stateless venues (biletyna, ck105) pass None.
  //
  // `keyOf` chooses the sticky-shard key. The default (host+path) spreads venues
  // across IPs — right for stateless per-venue scrapes (flicks, Cineworld). Vue
  // needs HOST-only stickiness instead: its token cookie is minted by a POST to
  // `/auth/token` and spent by a GET to `/…/films` — different PATHS — so host+path
  // would split them onto different IPs and lose the cookie. Host-only funnels all
  // of a brand's traffic onto one IP+cookie jar; fine at Vue's ~88-venue/420-min
  // volume, well under Decodo's concurrent-auth cap.
  //
  // The proxy leg is circuit-broken (EgressWiring.breakerGuarded), unlike every
  // other leg of this chain: it is the only one built from raw RealHttpFetch
  // shards with none of HttpWiring's protective wrapping (no
  // HostCircuitBreakerHttpFetch, no per-host pacing). Without it, a Decodo
  // account-wide outage (every tunnel 503ing, 2026-09-10) makes EVERY venue call
  // pay the full connect/request timeout on a dead tunnel before FallbackHttpFetch
  // even tries the next leg — with only 4 worker threads, that alone starved
  // throughput across every task type sharing the pool, not just the proxied
  // scrapers, and surfaced as `Worker task queue head-of-line age high` (UK
  // climbed past 2600s). The breaker opens per destination host after a few
  // consecutive tunnel failures and fast-fails (~0ms) for the cooldown, so the
  // chain falls through to `fallback` almost immediately instead of queuing
  // behind a doomed proxy attempt on every single call.
  private def proxyPrimary(fallback: HttpFetch, warmUrl: Option[String] = None,
                           keyOf: String => String = StickyShardHttpFetch.hostAndPath): HttpFetch =
    proxyShards.fold(fallback)(EgressWiring.proxyPrimary(_, fallback, clock, warmUrl, keyOf, decodoMeter, recordProxyOutcome, decodoBreakerMeter))

  // Meter the residential-proxy leg to /uptime: a green "Residential proxy" bar
  // means the proxy served, a red one means it failed and we fell back to direct
  // (which, for a Cloudflare-blocked origin, usually fails too). Aggregated across
  // all proxied cinemas into one row. Only the "proxy" leg is metered.
  private def recordProxyOutcome(backend: String, error: Option[String]): Unit =
    if (backend == "proxy") error match {
      case None        => uptimeMonitor.recordSuccess(ResidentialProxyService)
      case Some(label) => uptimeMonitor.recordFailure(ResidentialProxyService, label)
    }

  // Each paid leg's per-request outcome, for `PaidEgressFailing` (see PaidEgressMetrics).
  private lazy val zyteMeter: HttpOutcomeRecorder =
    workerMetrics.paidEgress.recorderFor(country.code, PaidEgressMetrics.Provider.Zyte)
  private lazy val decodoMeter: HttpOutcomeRecorder =
    workerMetrics.paidEgress.recorderFor(country.code, PaidEgressMetrics.Provider.Decodo)
  private lazy val decodoBreakerMeter: tools.CircuitBreakerMeter =
    workerMetrics.httpBreakers.meterFor(country.code, services.metrics.HttpBreakerMetrics.Leg.Decodo)

  // The one JDK client the Zyte route below calls the Zyte API through. Lazy, and
  // handed on by name, so a wiring without ZYTE_API_KEY never builds it.
  lazy val zyteHttpClient: java.net.http.HttpClient = ZyteFallback.newHttpClient()

  /** The key the paid Zyte uses below are built with — None means this wiring has no
   *  Zyte leg anywhere (`zyteFetch` collapses to direct, the Odeon harvester mints no
   *  token). A seam so the test wirings can refuse Zyte whatever environment they are
   *  handed: `TestWiring` answers None. */
  protected def zyteApiKey: Option[settings.ZyteApiKey] = configuration.zyteApiKey

  lazy val multikinoFetch: HttpFetch = proxyPrimary(httpFetch, warmUrl = Some(MultikinoClient.HomeUrl))
  // The same route for Multikino's share-card POSTERS, but NOT metered to the "Residential proxy"
  // /uptime row: that row says how often the SCRAPES fall off the proxy, and a poster the origin
  // refuses through the proxy is not the proxy failing. Its own breaker too, so poster failures
  // never open the scrapes'. The paid-egress counters still see it: it is paid for.
  lazy val multikinoPosterFetch: HttpFetch =
    proxyShards.fold(httpFetch)(EgressWiring.proxyPrimary(_, httpFetch, clock, Some(MultikinoClient.HomeUrl), meter = decodoMeter,
      breakerMeter = decodoBreakerMeter))
  // Zyte residential egress → direct fallback (Zyte only when ZYTE_API_KEY is set), for
  // the one venue whose origin blocks the Decodo proxy too (see CinemaScraperCatalog).
  // Never a fallback behind the proxy — see feedback_zyte_is_decodo_fallback_only.
  lazy val zyteFetch: HttpFetch = ZyteFallback.fetchFor(httpFetch, zyteHttpClient, zyteApiKey, zyteMeter)
  // biletyna.pl 403s our datacenter IP; residential proxy primary, direct fallback.
  lazy val biletynaFetch: HttpFetch = proxyPrimary(httpFetch)
  // www.flicks.co.uk 403s our datacenter IP behind Cloudflare (verified 2026-07-26
  // from kinowo-worker-uk: the identical GET returns 403 from Fly, 200 from a
  // residential IP; every Decodo pool IP returns 200 too). Flicks is the ONLY UK
  // source (and, via flicksUs, the AMC/Regal/Malco fallback for the US), so the
  // block took all ~843 UK venues red at once. Residential proxy primary; the direct
  // fallback is the very block the proxy exists to clear, so a Decodo-side outage
  // leaves these venues without a working path until it recovers.
  lazy val flicksFetch: HttpFetch = proxyPrimary(httpFetch)

  // Vue/CinemaxX films API is Cloudflare-403'd from our Fly IP (like flicks) AND
  // token-gated, so it egresses residential AND host-sticky (one IP+cookie for the
  // token POST + films GET — see proxyPrimary/keyOf) and falls back to direct if
  // the proxy is down. Cineworld reuses flicksFetch (GET-only, no cookie, so
  // per-venue stickiness is fine). Showcase/Everyman still reach their origins directly.
  lazy val vueFetch: HttpFetch = proxyPrimary(httpFetch, keyOf = StickyShardHttpFetch.hostOnly)

  // vwc.odeon.co.uk — Odeon's Vista ocapi backend — is Cloudflare-403'd too. It was
  // NOT when the client was written: it answered our Fly egress directly, and only
  // the www page needed a browser. The 2026-08-29 move to Hetzner changed the egress
  // IP and the identical GET now returns 403 (a Cloudflare "Attention Required" page)
  // from k3s-worker-1 while returning 401 — i.e. reaching the origin, just
  // unauthenticated — from a residential IP and from every Decodo pool port. That
  // took all 102 Odeon venues red at once, so the data fetch moves onto the proxy.
  // Per-venue (default host+path) stickiness, not host-only: Odeon carries its auth
  // in a header, not a cookie, so nothing has to share an IP, and the per-date
  // showtimes paths spread the sweep across the pool.
  lazy val odeonFetch: HttpFetch = proxyPrimary(httpFetch)

  // Harvests Odeon's ~12h Vista JWT via Zyte browserHtml (the estate-wide token
  // lives in the Cloudflare-gated www page; the ocapi DATA host is open). Lazy TTL
  // cache — ~2 browser fetches/day — so Odeon's ocapi pulls run over plain `http`.
  // No key (CI/local) → token() is None → Odeon venues ride the flicks fallback.
  lazy val odeonAuthHarvester: OdeonAuthHarvester =
    new OdeonAuthHarvester(() => OdeonAuthHarvester.zyteFetchPage(zyteApiKey, meter = zyteMeter))
}

object EgressWiring {
  /** The residential proxy first, `direct` behind it — and with no proxy, `direct` alone. */
  def paidEgressChain(proxyShards: Option[IndexedSeq[HttpFetch]], direct: HttpFetch,
                      clock: java.time.Clock, warmUrl: Option[String] = None): HttpFetch =
    proxyShards.fold(direct)(proxyPrimary(_, direct, clock, warmUrl))

  /** Multikino's chain for a recording or diagnostic tool: proxy (warmed on the homepage) → `direct`. */
  def multikinoChain(proxyShards: Option[IndexedSeq[HttpFetch]], direct: HttpFetch, clock: java.time.Clock): HttpFetch =
    paidEgressChain(proxyShards, direct, clock, Some(MultikinoClient.HomeUrl))

  /** The /uptime row the residential-proxy leg is metered under. */
  private val ResidentialProxyService = "Residential proxy"

  /** One [[RealHttpFetch]] per Decodo pool IP, each pinned with its own cookie
   *  jar — or None when the proxy isn't configured. */
  def residentialShards(config: Option[RealHttpFetch.ProxyConfig],
                        tls: javax.net.ssl.SSLContext): Option[IndexedSeq[RealHttpFetch]] =
    config.map(_.perPort.map(cfg => new RealHttpFetch(Some(cfg), tls)).toIndexedSeq)

  /** The residential proxy (sticky across `shards`, each warmed on `warmUrl`
   *  when given, metered and circuit-broken) with `fallback` behind it — the
   *  chain the trait's `proxyPrimary` explains, shared with `tools.RosterAudit`. */
  def proxyPrimary(shards: IndexedSeq[HttpFetch], fallback: HttpFetch, clock: java.time.Clock, warmUrl: Option[String] = None,
                   keyOf: String => String = StickyShardHttpFetch.hostAndPath,
                   meter: HttpOutcomeRecorder = HttpOutcomeRecorder.noop,
                   onOutcome: (String, Option[String]) => Unit = FallbackHttpFetch.NoOutcome,
                   breakerMeter: tools.CircuitBreakerMeter = tools.CircuitBreakerMeter.noop): HttpFetch = {
    val legs = warmUrl.fold(shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
    val proxyLeg = meteredProxyLeg(new StickyShardHttpFetch(legs, keyOf), meter, clock, breakerMeter)
    new FallbackHttpFetch(Seq("proxy" -> proxyLeg, "fallback" -> fallback), onOutcome = onOutcome,
                          endsChain = FallbackHttpFetch.OriginAnswered)
  }

  /** Wrap the sticky-shard proxy leg in a per-host circuit breaker, so a host
   *  whose Decodo tunnel starts failing (a pool-wide 503 spell, an account-level
   *  outage) stops paying the full connect/request budget on every call once it
   *  has opened. Extracted to a pure function — rather than inlined in
   *  `proxyPrimary` — so this composition is unit-testable without the rest of
   *  `WorkerWiring`. */
  private[wiring] def breakerGuarded(proxyLeg: HttpFetch, clock: java.time.Clock,
                                     breakerMeter: tools.CircuitBreakerMeter = tools.CircuitBreakerMeter.noop): HttpFetch =
    new HostCircuitBreakerHttpFetch(proxyLeg, meter = breakerMeter, clock = clock)

  /** [[breakerGuarded]] around the proxy leg with its paid-egress `meter` INSIDE
   *  the breaker: every attempt that reached Decodo is counted with its outcome,
   *  and a fast-fail from an open breaker — which sends nothing — is not, or an
   *  open breaker would read as the proxy failing at 100%. */
  private[wiring] def meteredProxyLeg(proxyLeg: HttpFetch, meter: HttpOutcomeRecorder, clock: java.time.Clock,
                                      breakerMeter: tools.CircuitBreakerMeter = tools.CircuitBreakerMeter.noop): HttpFetch =
    breakerGuarded(new CountingHttpFetch(proxyLeg, meter), clock, breakerMeter)
}
