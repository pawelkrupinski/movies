package models

/** Per-cinema detail-enrichment health is recorded under a synthetic
 *  "<cinema displayName>|enrichment" uptime service, distinct from the scrape service
 *  (the bare cinema name). The worker names it ([[forCinema]]); the `/uptime` page parses
 *  the suffix ([[matches]]) to render the enrichment bar grouped under its cinema rather
 *  than as a standalone row. */
object EnrichmentUptimeService {
  val Suffix: String = "|enrichment"
  def forCinema(cinemaDisplayName: String): String = cinemaDisplayName + Suffix
  def matches(service: String): Boolean = service.endsWith(Suffix)
}
