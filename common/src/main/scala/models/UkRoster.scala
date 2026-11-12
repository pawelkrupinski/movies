package models

/** Every UK venue, one [[UkCinema]] each, built once from the generated table
 *  `generated/uk-roster/venues.tsv` (`data/uk/scripts/generate_roster.py`, from `data/uk/roster.json`)
 *  and reused everywhere: the page lists ([[UkPages]]), `Cinema.byCity` and the scrape catalog all hold
 *  the same instance. A venue is addressed by its key, roster.json's `object` and the `toString` it
 *  kept from its hand-written `case object` days: `UkRoster("ActOneActon")`. */
object UkRoster {
  private val Table = "generated/uk-roster/venues.tsv"

  /** Every UK venue, in roster.json's order. */
  val all: Seq[UkCinema] = GeneratedTable.rows(Table).map {
    case Seq(key, displayName, pillName, opening) => new UkCinema(displayName, pillName, key, openingOf(key, opening))
    case row => throw new IllegalStateException(s"$Table: a row of ${row.size} cells, not 4: ${row.mkString(" | ")}")
  }

  private def openingOf(key: String, cell: String): Opening = cell match {
    case ""         => Opening.YearRound
    case "seasonal" => Opening.Seasonal(SeasonWindow.UnitedKingdom)
    case other      => throw new IllegalStateException(s"$Table: $key opens '$other', neither all year nor seasonal")
  }

  private val byKey: Map[String, UkCinema] = {
    val keyed = all.groupBy(_.toString)
    keyed.collect { case (key, venues) if venues.size > 1 => key }.toSeq.sorted match {
      case Seq()      => keyed.view.mapValues(_.head).toMap
      case duplicated => throw new IllegalStateException(s"$Table: keys on more than one row: ${duplicated.mkString(", ")}")
    }
  }

  /** The venue keyed `key`. An unknown key fails loudly: it names a venue roster.json does not have. */
  def apply(key: String): UkCinema =
    byKey.getOrElse(key, throw new NoSuchElementException(s"no UK venue '$key' in $Table"))
}
