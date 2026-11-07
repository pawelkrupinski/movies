package tools

/** The residential-proxy chains a Cloudflare-blocked venue is fetched through: the proxy (sticky across its shards,
 *  each warmed on `warmUrl` when given, metered and circuit-broken) with a fallback behind it. Built here, below both
 *  the worker's wiring (`EgressWiring`, which meters and records each leg) and the scrapers' diagnostic routes
 *  (`CinemaEgress.diagnostic`, the recording and replay tools'), so the two build one chain. */
object ProxyChain {

  /** The proxy leg's name in the chain — what its failures are reported under (`AllBackendsFailedException.failedLegs`). */
  val ProxyLeg = "proxy"

  /** The residential proxy first, `direct` behind it — and with no proxy, `direct` alone. */
  def paidEgressChain(proxyShards: Option[IndexedSeq[HttpFetch]], direct: HttpFetch,
                      clock: java.time.Clock, stopwatch: Stopwatch, warmUrl: Option[String] = None): HttpFetch =
    proxyShards.fold(direct)(proxyPrimary(_, direct, clock, stopwatch, warmUrl))

  /** The residential proxy (sticky across `shards`, each warmed on `warmUrl`
   *  when given, metered and circuit-broken) with `fallback` behind it — the
   *  chain `EgressWiring`'s `proxyPrimary` explains, shared with `tools.RosterAudit`. */
  def proxyPrimary(shards: IndexedSeq[HttpFetch], fallback: HttpFetch, clock: java.time.Clock, stopwatch: Stopwatch,
                   warmUrl: Option[String] = None,
                   keyOf: String => String = StickyShardHttpFetch.hostAndPath,
                   meter: HttpOutcomeRecorder = HttpOutcomeRecorder.noop,
                   onOutcome: (String, Option[String]) => Unit = FallbackHttpFetch.NoOutcome,
                   breakerMeter: CircuitBreakerMeter = CircuitBreakerMeter.noop): HttpFetch = {
    val legs = warmUrl.fold(shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
    val proxyLeg = meteredProxyLeg(new StickyShardHttpFetch(legs, keyOf), meter, clock, breakerMeter)
    new FallbackHttpFetch(Seq(ProxyLeg -> proxyLeg, "fallback" -> fallback), stopwatch, onOutcome = onOutcome,
                          endsChain = FallbackHttpFetch.OriginAnswered)
  }

  /** Wrap the sticky-shard proxy leg in a per-host circuit breaker, so a host
   *  whose Decodo tunnel starts failing (a pool-wide 503 spell, an account-level
   *  outage) stops paying the full connect/request budget on every call once it
   *  has opened. Extracted to a pure function — rather than inlined in
   *  `proxyPrimary` — so this composition is unit-testable on its own. */
  private[tools] def breakerGuarded(proxyLeg: HttpFetch, clock: java.time.Clock,
                                    breakerMeter: CircuitBreakerMeter = CircuitBreakerMeter.noop): HttpFetch =
    new HostCircuitBreakerHttpFetch(proxyLeg, meter = breakerMeter, clock = clock)

  /** [[breakerGuarded]] around the proxy leg with its paid-egress `meter` INSIDE
   *  the breaker: every attempt that reached Decodo is counted with its outcome,
   *  and a fast-fail from an open breaker — which sends nothing — is not, or an
   *  open breaker would read as the proxy failing at 100%. */
  private[tools] def meteredProxyLeg(proxyLeg: HttpFetch, meter: HttpOutcomeRecorder, clock: java.time.Clock,
                                     breakerMeter: CircuitBreakerMeter = CircuitBreakerMeter.noop): HttpFetch =
    breakerGuarded(new CountingHttpFetch(proxyLeg, meter), clock, breakerMeter)
}
