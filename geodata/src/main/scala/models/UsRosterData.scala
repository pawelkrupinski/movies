package models

/** The full US cinema roster: 1,995 pages over 55 states and territories / 4,999 cinemas (Flicks,
 *  www.flicks.us), as the tab-separated tables `data/us/scripts/generate_roster.py` writes from
 *  data/us/pages.json under `generated/us-roster/` (read through [[GeneratedTable]]). Regenerate after
 *  re-harvesting or re-clustering; see data/us/README.md.
 *
 *  Tables: `pages.tsv` (one [[R]] a row, its towns a list cell), `cinemas.tsv` (the [[C]]s, each keyed by its
 *  page's slug, in page order), `venue-zones.tsv`, `retired.tsv`, `seasonal.tsv`. */
private[models] object UsRosterData {
  // (displayName, pillName, flicks cinema slug, the venue's state, the venue's own town)
  type C = (String, String, String, String, String)
  // (slug, slug qualified with its state, name, state, lat, lon, zoneId, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])

  val pages: Seq[R] = {
    val cinemasByPage: Map[String, Seq[C]] =
      GeneratedTable.rows("generated/us-roster/cinemas.tsv")
        .groupMap(_(0))(cells => (cells(1), cells(2), cells(3), cells(4), cells(5)))
    GeneratedTable.rows("generated/us-roster/pages.tsv").map { cells =>
      (cells(0), cells(1), cells(2), cells(3), cells(4).toDouble, cells(5).toDouble, cells(6), cells(7).toBoolean,
        GeneratedTable.list(cells(8)), cinemasByPage.getOrElse(cells(0), Seq.empty))
    }
  }

  /** (displayName, zoneId) — the venues whose own clock is not their page's. */
  val venueZones: Seq[(String, String)] =
    GeneratedTable.rows("generated/us-roster/venue-zones.tsv").map(cells => (cells(0), cells(1)))

  /** Pages that no longer exist — the metros the US pages were until 2026-10 among
   *  them — and the page holding the town each was named after, else most of its
   *  venues: (slug, slug qualified with its state, the page's slug). */
  val retired: Seq[(String, String, String)] =
    GeneratedTable.rows("generated/us-roster/retired.tsv").map(cells => (cells(0), cells(1), cells(2)))

  /** The venues that screen only in season — open-air, summer and drive-in screens, by name
   *  (data/scripts/seasonal_venues.py): their display names. */
  val seasonal: Set[String] = GeneratedTable.rows("generated/us-roster/seasonal.tsv").map(_(0)).toSet
}
