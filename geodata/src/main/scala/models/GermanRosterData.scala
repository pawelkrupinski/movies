package models

/** The German cinema roster: the tables `data/germany/scripts/generate_roster.py` writes under
 *  `generated/de-roster/` (tab-separated classpath resources, read through [[GeneratedTable]]) —
 *  pages, each page's cinemas in order, the retired slugs and the seasonal venues' names.
 *  Regenerate after adding venues or re-clustering; see data/germany/README.md. */
private[models] object GermanRosterData {
  // (displayName, pillName, venue key -- its Filmstarts theaterId, or the feed that found a venue
  //  Filmstarts does not list ("cineamo:997") --, kinoprogramm.com fallback path, town)
  type C = (String, String, String, Option[String], String)
  // (slug, slug qualified with its Bundesland, name, Bundesland, lat, lon, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, Boolean, Seq[String], Seq[C])

  private def table(name: String) = GeneratedTable.rows(s"generated/de-roster/$name.tsv")

  val pages: Seq[R] = {
    val cinemasByPage: Map[String, Seq[C]] = table("cinemas").groupMap(_(0)) { row =>
      (row(1), row(2), row(3), GeneratedTable.option(row(4)), row(5))
    }
    table("pages").map { row =>
      (row(0), row(1), row(2), row(3), row(4).toDouble, row(5).toDouble, row(6).toBoolean,
        GeneratedTable.list(row(7)), cinemasByPage.getOrElse(row(0), Seq.empty))
    }
  }

  /** Pages that no longer exist — the regions Germany's pages were until 2026-10
   *  among them — and the page now holding most of their venues: (slug, slug
   *  qualified with its Bundesland, the page's slug). */
  val retired: Seq[(String, String, String)] = table("retired").map(row => (row(0), row(1), row(2)))

  /** The venues that screen only in season — open-air, summer and drive-in screens, by name
   *  (data/scripts/seasonal_venues.py): their display names as generated. */
  val seasonal: Set[String] = table("seasonal").map(_(0)).toSet
}
