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
   *  when given, metered to `meters` and circuit-broken) with `fallback` behind it — the
   *  chain `EgressWiring`'s `proxyPrimary` explains, shared with `diagnostics.RosterAudit`. */
  def proxyPrimary(shards: IndexedSeq[HttpFetch], fallback: HttpFetch, clock: java.time.Clock, stopwatch: Stopwatch,
                   warmUrl: Option[String] = None,
                   keyOf: String => String = StickyShardHttpFetch.hostAndPath,
                   meters: ProxyMeters = ProxyMeters.None): HttpFetch = {
    val legs = warmUrl.fold(shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
    val proxyLeg = meters.meteredLeg(new StickyShardHttpFetch(legs, keyOf), clock)
    new FallbackHttpFetch(Seq(ProxyLeg -> proxyLeg, "fallback" -> fallback), stopwatch, onOutcome = meters.outcome,
                          endsChain = FallbackHttpFetch.SameOnEveryRoute)
  }

  /** The residential proxy with NOTHING behind it, for an origin that refuses the direct route outright: there a
   *  direct leg can only add a refusal to every proxy failure. Still a one-leg [[FallbackHttpFetch]], so its failures
   *  name the proxy leg (a venue failing on it is parked on the proxy's health, `ProxyParking`) and are logged
   *  once per incident like every chain's. */
  def proxyAlone(shards: IndexedSeq[HttpFetch], clock: java.time.Clock, stopwatch: Stopwatch,
                 keyOf: String => String = StickyShardHttpFetch.hostAndPath,
                 meters: ProxyMeters = ProxyMeters.None): HttpFetch =
    new FallbackHttpFetch(Seq(ProxyLeg -> meters.meteredLeg(new StickyShardHttpFetch(shards, keyOf), clock)),
                          stopwatch, onOutcome = meters.outcome, endsChain = FallbackHttpFetch.SameOnEveryRoute)
}
