package models

/** Spain's roster as the tables `data/spain/scripts/generate_roster.py` writes under `generated/es-roster/`
 *  (`pages.tsv`, `cinemas.tsv` keyed by page slug, `retired.tsv`, `seasonal.tsv`), read through
 *  [[GeneratedTable]] and materialised into City/Cinema objects by [[SpanishRoster]]. */
private[models] object SpanishRosterData {
  private val Directory = "generated/es-roster"

  // (displayName, pillName, SensaCine theaterId, Ocine ticketing server) — a venue
  // SensaCine does not list has no theaterId and is scraped off its own server
  type C = (String, String, Option[String], Option[String])
  // (slug, slug qualified with its autonomous community, name, province, lat, lon,
  //  zoneId, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])

  val pages: Seq[R] = {
    val cinemasByPage: Map[String, Seq[C]] =
      GeneratedTable.rows(s"$Directory/cinemas.tsv").groupMap(_(0)) { row =>
        // The pill reads the display name: the table keeps it once.
        (row(1), row(1), GeneratedTable.option(row(2)), GeneratedTable.option(row(3)))
      }
    GeneratedTable.rows(s"$Directory/pages.tsv").map { row =>
      (row(0), row(1), row(2), row(3), row(4).toDouble, row(5).toDouble, row(6), row(7).toBoolean,
        GeneratedTable.list(row(8)), cinemasByPage.getOrElse(row(0), Seq.empty))
    }
  }

  /** Pages that no longer exist — the provinces Spain's pages were until 2026-10
   *  among them — and the page now holding most of their venues: (slug, slug
   *  qualified with its autonomous community, the page's slug). */
  val retired: Seq[(String, String, String)] =
    GeneratedTable.rows(s"$Directory/retired.tsv").map(row => (row(0), row(1), row(2)))

  /** The venues that screen only in season — open-air, summer and drive-in screens, by name
   *  (data/scripts/seasonal_venues.py): their display names. */
  val seasonal: Set[String] = GeneratedTable.rows(s"$Directory/seasonal.tsv").map(_(0)).toSet
}
