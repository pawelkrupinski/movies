package models

/** What the roster generators (`data/<country>/scripts/generate_roster.py`) write under a roster's directory, read
 *  through [[GeneratedTable]]: `pages.tsv`, one page a row, joined with `cinemas.tsv`, each venue a row keyed by its
 *  page's slug (column 0) in page order. */
private[models] object RosterTables {

  /** A page of a roster whose pages each carry a time zone: (slug, qualified slug, name, region, lat, lon, zoneId,
   *  multiTown, towns, cinemas). */
  type ZonedPage[C] = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])

  /** Every row of `directory`'s pages table, made by `page` with the venues `cinema` makes of its cinemas rows. */
  def pages[C, R](directory: String)(cinema: IndexedSeq[String] => C)(page: (IndexedSeq[String], Seq[C]) => R): Seq[R] = {
    val cinemasByPage: Map[String, Seq[C]] = GeneratedTable.rows(s"$directory/cinemas.tsv").groupMap(_(0))(cinema)
    GeneratedTable.rows(s"$directory/pages.tsv").map(row => page(row, cinemasByPage.getOrElse(row(0), Seq.empty)))
  }

  /** [[pages]] for a roster whose pages table holds a [[ZonedPage]]'s nine cells before its cinemas. */
  def zonedPages[C](directory: String)(cinema: IndexedSeq[String] => C): Seq[ZonedPage[C]] =
    pages(directory)(cinema) { (row, cinemas) =>
      (row(0), row(1), row(2), row(3), row(4).toDouble, row(5).toDouble, row(6), row(7).toBoolean,
        GeneratedTable.list(row(8)), cinemas)
    }
}
