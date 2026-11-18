package tools

/** What a [[FallbackHttpFetch]] makes of its legs' failures: which one ends the chain as the origin's own answer (the
 *  `endsChain` policies), and, once every leg has failed, what the caller is told. */
object LegFailures {

  /** For a chain of egress ROUTES to one origin: the origin's "not found" (404/410)
   *  through any route is final. Letting it fall through buried it — Odeon's ocapi
   *  404'd a business date through the proxy, the Zyte and direct legs then failed on
   *  their own blocks, and the composite failure read as transient, so the
   *  `ScrapeChunk` retried a permanent answer to exhaustion on paid egress (UK
   *  2026-09-21/22). An egress provider's OWN 404 is not an `HttpStatusException`
   *  (see `EgressProviderException`), so it still falls through. */
  val OriginAnswered: Throwable => Boolean = ReadOutcome.isAbsent

  /** For the residential-proxy chain ([[ProxyChain]]): [[OriginAnswered]], and the origin's 401 too -- it saying who
   *  we are NOT (no token, no session yet), which is the same through every route, unlike a 403, often an egress IP's
   *  block that the next route exists to clear. Not a "gone": the caller refreshes its token or session and asks
   *  again. Falling through, Vue's cold-token films GET warned "All 2 backends failed" once per venue
   *  (2026-10-29..11-12) on its way to the client's token refresh, which then read the venue fine. */
  val SameOnEveryRoute: Throwable => Boolean = failure => OriginAnswered(failure) || (failure match {
    case s: HttpStatusException => s.code == 401
    case _                      => false
  })

  /** The last leg's "not found", when it said so: an ANSWER, the caller's to see as one (only the last leg's counts:
   *  an earlier leg missing says nothing about the resource). */
  def notFound(failures: Seq[Throwable]): Option[Throwable] = failures.lastOption.filter(ReadOutcome.isAbsent)

  /** The rate limit asking for the longest wait, when any leg was rate limited: the origin telling us to slow down,
   *  which the gate outside the chain can only act on as the typed status. */
  def longestRateLimit(failures: Seq[Throwable]): Option[Throwable] =
    failures.collect { case s: HttpStatusException if HttpStatusException.isRateLimit(s) => s }
      .maxByOption(_.retryAfter.fold(0L)(_.toMillis))

  /** Did a leg find its host's breaker open? Then the failure is the host's, not the URL's. */
  def hostWide(failures: Seq[Throwable]): Boolean = failures.exists(_.isInstanceOf[CircuitOpenException])
}
