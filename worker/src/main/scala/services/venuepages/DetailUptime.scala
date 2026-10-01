package services.venuepages

import services.UptimeMonitor
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}

/** A venue page read, on /uptime: a read page is a success, a failed or gone one a failure — under the
 *  cinema's own "<cinema>|enrichment" row, or the network-level service a chain names. */
object DetailUptime {
  def record(uptime: UptimeMonitor, enricher: DetailEnricher, label: String, outcome: DetailFetchOutcome): Unit = {
    val service = enricher.enrichmentServiceOverride.getOrElse(UptimeMonitor.enrichmentService(enricher.cinema.displayName))
    outcome match {
      case DetailFetchOutcome.Fetched(_)  => uptime.recordSuccess(service)
      case DetailFetchOutcome.Failed      => uptime.recordFailure(service, s"detail fetch returned nothing for $label")
      case DetailFetchOutcome.Gone(code)  => uptime.recordFailure(service, s"detail page gone (HTTP $code) for $label")
    }
  }
}
