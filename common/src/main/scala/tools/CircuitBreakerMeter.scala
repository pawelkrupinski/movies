package tools

/**
 * Where a [[HostCircuitBreakerHttpFetch]] reports itself, so an open breaker is a METRIC and not
 * only a log line: an upstream outage the breaker absorbs (every call fast-failing for minutes)
 * otherwise shows on no panel at all.
 *
 * Keyed by the breaker's leg (a bounded set — never by host, of which there are thousands): the
 * breaker hands over how many of its hosts are open now ([[watch]]) and says each time one opens.
 */
trait CircuitBreakerMeter {
  /** Read `openHosts` whenever the metric is sampled — called once, as the breaker is built. */
  def watch(openHosts: () => Int): Unit
  /** A host's breaker opened (closed → open; a failed half-open probe is not a new opening). */
  def opened(): Unit
}

object CircuitBreakerMeter {
  val noop: CircuitBreakerMeter = new CircuitBreakerMeter {
    def watch(openHosts: () => Int): Unit = ()
    def opened(): Unit                    = ()
  }
}
