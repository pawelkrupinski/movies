package tools

/** What a residential-proxy chain ([[ProxyChain]]) reports to: `attempts`, each request that reached the proxy with its
 *  outcome (the paid-egress counters); `breaker`, its per-host circuit breaker's state; and `outcome`, the chain's
 *  per-request answer by leg (the `/uptime` "Residential proxy" row). Every one records nothing by default
 *  ([[ProxyMeters.None]]): a diagnostic route meters nothing. */
final case class ProxyMeters(attempts: HttpOutcomeRecorder = HttpOutcomeRecorder.noop,
                             breaker: CircuitBreakerMeter = CircuitBreakerMeter.noop,
                             outcome: (String, Option[String]) => Unit = FallbackHttpFetch.NoOutcome) {

  /** [[ProxyMeters.breakerGuarded]] around the proxy leg with the `attempts` meter INSIDE the breaker: every attempt
   *  that reached Decodo is counted with its outcome, and a fast-fail from an open breaker — which sends nothing — is
   *  not, or an open breaker would read as the proxy failing at 100%. */
  def meteredLeg(proxyLeg: HttpFetch, clock: java.time.Clock): HttpFetch =
    ProxyMeters.breakerGuarded(new CountingHttpFetch(proxyLeg, attempts), clock, breaker)
}

object ProxyMeters {
  /** Meters nothing. */
  val None: ProxyMeters = ProxyMeters()

  /** Wrap the sticky-shard proxy leg in a per-host circuit breaker, so a host
   *  whose Decodo tunnel starts failing (a pool-wide 503 spell, an account-level
   *  outage) stops paying the full connect/request budget on every call once it
   *  has opened. Extracted to a pure function — rather than inlined in
   *  `ProxyChain.proxyPrimary` — so this composition is unit-testable on its own. */
  private[tools] def breakerGuarded(proxyLeg: HttpFetch, clock: java.time.Clock,
                                    breakerMeter: CircuitBreakerMeter = CircuitBreakerMeter.noop): HttpFetch =
    new HostCircuitBreakerHttpFetch(proxyLeg, meter = breakerMeter, clock = clock)
}
