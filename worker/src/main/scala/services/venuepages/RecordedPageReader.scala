package services.venuepages

import models.EnrichmentUptimeService
import services.UptimeRecorder
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}

/** Every venue page a task reads ([[VenuePageReader]]), recorded on /uptime: a read page is a success, a failed or gone
 *  one a failure — under the cinema's own "<cinema>|enrichment" row, or the network-level service a chain names. */
final class RecordedPageReader(reader: VenuePageReader, uptime: UptimeRecorder) {

  /** `page` of `enricher`'s venue read, and recorded under `label` (what the failure names: a film, a page). */
  def read(enricher: DetailEnricher, page: String, label: String): VenuePageRead = {
    val read    = reader.read(enricher, page)
    val service = enricher.enrichmentServiceOverride.getOrElse(EnrichmentUptimeService.forCinema(enricher.cinema.displayName))
    read.outcome match {
      case DetailFetchOutcome.Fetched(_)  => uptime.recordSuccess(service)
      case DetailFetchOutcome.Failed      => uptime.recordFailure(service, s"detail fetch returned nothing for $label")
      case DetailFetchOutcome.Gone(code)  => uptime.recordFailure(service, s"detail page gone (HTTP $code) for $label")
    }
    read
  }
}
