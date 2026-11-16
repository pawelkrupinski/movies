package tools

import java.time.Instant

/**
 * Where a [[HostCircuitBreakerHttpFetch]] keeps the hosts it has seen fail and not seen answer since, so a
 * process that restarts still knows them.
 *
 * WHY (2026-11-16). The breaker lived in memory alone, and the workers restart on every deploy -- 62 times in
 * the week to 11-16, every one an image rollout (kube_pod_container_status_restarts_total 0 for every
 * worker; process_start_time_seconds changed 62 times in lockstep across the five countries, once per
 * `main-<utc>-<sha>` worker image). Each boot forgot every host that was down: the
 * `kinowo_worker_http_circuit_open{host}` series ended until the host failed four more times (for a parked
 * venue its next hourly scrape), so one two-day PKIX outage of kinoikar.mok-jar.pl paged seven times, and every
 * boot spent four more timeouts per dead host finding out again. Remembered, a booting worker starts each such
 * host half-open: its first call is the probe, the series carries on, and an answer clears it.
 *
 * The store only keeps what it is told; what to remember and for how long is the breaker's
 * ([[HostCircuitBreakerHttpFetch.RememberFor]]).
 */
trait UnrecoveredHosts {
  /** Every host remembered, with the failure it last failed with and when. */
  def load(): Seq[UnrecoveredHosts.Host]
  /** `host` failed (it opened, or an open host's probe failed): remember it, or refresh it. */
  def failed(host: UnrecoveredHosts.Host): Unit
  /** `host` answered: forget it. */
  def recovered(host: String): Unit
}

object UnrecoveredHosts {
  final case class Host(host: String, lastFailure: Option[String], failedAt: Instant)

  /** Remembers nothing: a breaker over it forgets its hosts with the process, as one did before this existed. */
  val none: UnrecoveredHosts = new UnrecoveredHosts {
    def load(): Seq[Host]          = Seq.empty
    def failed(host: Host): Unit   = ()
    def recovered(host: String): Unit = ()
  }
}
