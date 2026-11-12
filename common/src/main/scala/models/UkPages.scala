package models

/** Which UK listing page every venue is on, read from the generated tables under
 *  `generated/uk-pages/` (`data/uk/scripts/generate_roster.py`, from `data/uk/pages.json`).
 *
 *  A major city (a place of 100,000+) lists only the venues inside it; every
 *  other venue is on a page of its own town (3+ venues) or of a cluster of small
 *  towns within a short drive, named after its biggest — never more than 40 km
 *  from it (see `data/uk/scripts/build_pages.py`). London is hand-declared in
 *  `City` for its compass areas and reads [[london]]; every other page is a
 *  [[Row]] that `City.ukPages` builds. */
private[models] object UkPages {

  /** A page. `multiTown` pages are named "X area"; `towns` is the town the page
   *  is named after, then the rest of its towns. */
  final case class Row(slug: String, name: String, lat: Double, lon: Double, nation: String, county: String,
                       multiTown: Boolean, towns: Seq[String], cinemas: Seq[Cinema]) {
    /** The page's name: the town, or "<town> area" for a cluster of towns. */
    def nominative: String = if (multiTown) s"$name area" else name
  }

  private val Dir = "generated/uk-pages"

  /** The nations in the order every picker shows them. */
  val nations: Seq[String] = GeneratedTable.rows(s"$Dir/nations.tsv").map(_.head)

  /** Every page, London first; a venue key the roster lacks fails here ([[UkRoster.apply]]). */
  private val pages: Seq[Row] = GeneratedTable.rows(s"$Dir/pages.tsv").map {
    case Seq(slug, name, lat, lon, nation, county, multiTown, towns, cinemas) =>
      Row(slug, name, lat.toDouble, lon.toDouble, nation, county, multiTown.toBoolean, GeneratedTable.list(towns),
          GeneratedTable.list(cinemas).map(UkRoster(_)))
    case row => throw new IllegalStateException(s"$Dir/pages.tsv: a row of ${row.size} cells, not 9: ${row.mkString(" | ")}")
  }

  val london: Row = pages.headOption.filter(_.slug == "london")
    .getOrElse(throw new IllegalStateException(s"$Dir/pages.tsv does not start with London"))

  val rows: Seq[Row] = pages.tail

  /** Pages that no longer exist → the page holding the town each was named
   *  after, or — where none was — the page now holding most of its venues. */
  val retired: Map[String, String] = GeneratedTable.lookup(s"$Dir/retired.tsv")

  /** Page slug → the names of the retired pages that now redirect to it, which
   *  the picker's search still finds it by ("West Yorkshire" → Leeds). */
  val formerNames: Map[String, Seq[String]] =
    GeneratedTable.rows(s"$Dir/former-names.tsv").map(row => row(0) -> GeneratedTable.list(row(1))).toMap
}
