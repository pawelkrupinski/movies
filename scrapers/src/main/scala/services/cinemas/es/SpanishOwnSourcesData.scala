package services.cinemas.es

import models.GeneratedTable

/** Which Spanish roster venues are scraped off a source other than SensaCine, keyed by SensaCine theaterId: the
 *  tables `data/spain/scripts/generate_own_sources.py` writes under `generated/es-own-sources/` (`taquilla.tsv`:
 *  theaterId, venue id, path, name; `artesiete.tsv`: theaterId, page, name — the name is for the table's readers). */
private[cinemas] object SpanishOwnSourcesData {
  private val Directory = "generated/es-own-sources"

  /** SensaCine theaterId -> the venue on taquilla.com. */
  val taquilla: Map[String, TaquillaVenue] =
    GeneratedTable.rows(s"$Directory/taquilla.tsv").map(row => row(0) -> TaquillaVenue(row(1), row(2))).toMap

  /** SensaCine theaterId -> the venue's page on its own Artesiete subdomain. */
  val artesiete: Map[String, String] =
    GeneratedTable.rows(s"$Directory/artesiete.tsv").map(row => row(0) -> row(1)).toMap

  /** Whether the venue is read off a Spanish own source rather than SensaCine. */
  def ownSourced(theaterId: String): Boolean =
    taquilla.contains(theaterId) || artesiete.contains(theaterId)
}
