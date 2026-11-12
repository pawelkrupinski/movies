package models

/** Which Polish listing page every venue is on.
 *
 *  A major city lists only the venues inside it; every venue outside one is on
 *  a page of its own town (3+ venues) or of a cluster of small towns within a
 *  short drive, named after its biggest (see `data/pl/scripts/build_pages.py`).
 *  The 41 major cities are hand-declared in `City` and read [[majorCinemas]];
 *  every other page is a [[Row]] that `City.polishPages` builds.
 *
 *  The tables are the GENERATED resources the `.tsv` tables under `generated/pl-pages`
 *  (`data/pl/scripts/generate_polish_pages.py`, from `data/pl/pages.json`), read through
 *  [[GeneratedTable]]. A venue is named there by its Scala object name. */
private[models] object PolishPages {
  private val Directory = "generated/pl-pages"

  /** A page that is not a major city. `multiTown` pages are named "X i okolice". */
  final case class Row(slug: String, name: String, locative: String, lat: Double, lon: Double,
                       voivodeship: String, multiTown: Boolean, cinemas: Seq[Cinema]) {
    /** The page's name: the town, or "<town> i okolice" for a cluster of towns. */
    def nominative: String = if (multiTown) s"$name i okolice" else name
  }

  /** The venue behind a generated table's object name. Resolved by reflection on the top-level `case object`
   *  rather than through `Cinema.byDisplayName`, which is itself built from these pages. A venue renamed
   *  or deleted without regenerating fails the load: it must not silently vanish from its page. */
  private[models] def cinemaNamed(objectName: String): Cinema =
    try Class.forName(s"models.$objectName$$").getField("MODULE$").get(null).asInstanceOf[Cinema]
    catch {
      case cause @ (_: ClassNotFoundException | _: NoSuchFieldException | _: ClassCastException) =>
        throw new IllegalStateException(
          s"$Directory names the cinema '$objectName', which is not a `models` Cinema object: " +
            "regenerate with data/pl/scripts/generate_polish_pages.py", cause)
    }

  private def cinemasIn(cell: String): Seq[Cinema] = GeneratedTable.list(cell).map(cinemaNamed)

  val rows: Seq[Row] = {
    val cinemasBySlug = GeneratedTable.lookup(s"$Directory/cinemas.tsv")
    GeneratedTable.rows(s"$Directory/pages.tsv").map { cells =>
      Row(cells(0), cells(1), cells(2), cells(3).toDouble, cells(4).toDouble, cells(5), cells(6).toBoolean,
        cinemasIn(cinemasBySlug.getOrElse(cells(0),
          throw new IllegalStateException(s"$Directory/cinemas.tsv has no row for page ${cells(0)}"))))
    }
  }

  private val majorRows = GeneratedTable.rows(s"$Directory/major-cinemas.tsv")

  /** Each major city's own venues — the ones inside the city itself. */
  val majorCinemas: Map[String, Seq[Cinema]] = majorRows.map(cells => cells(0) -> cinemasIn(cells(2))).toMap

  /** The voivodeship each major city's page is grouped under in the picker. */
  val majorVoivodeship: Map[String, String] = majorRows.map(cells => cells(0) -> cells(1)).toMap

  /** Pages that no longer exist → the page now holding their town. */
  val retired: Map[String, String] = GeneratedTable.lookup(s"$Directory/retired.tsv")
}
