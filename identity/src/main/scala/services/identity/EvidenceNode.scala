package services.identity

/** A node: the listings sharing one evidence. Named by its smallest listing's sort key. */
private[identity] final class EvidenceNode(val evidence: Evidence, val listings: Seq[Listing]) {
  val id: String          = listings.head.sortKey
  val weight: Int         = listings.size
  val venue: String       = listings.head.venue
  val venues: Set[String] = listings.map(_.venue).toSet
  /** The title's words, once per node: `namesOnlyItsVenue` reads them against every candidate. */
  lazy val titleWords: Seq[String] = services.movies.TitleContainment.tokens(evidence.title)
  /** The country (ISO-3166-1) of its venue — what [[ReleaseVeto]] asks a namesake's releases of. */
  lazy val country: Option[String] = EvidenceNode.countryOf(listings.head.cinema)
  /** The people its listings' own text names, all of them ([[VenueNames]]): the `cast` measure's listing side. */
  lazy val names: VenueNames = listings.iterator.map(_.names).foldLeft(VenueNames.None)(_ ++ _)
  /** The evidence as text: title, stated year, directors. */
  def name: String        = s"'${evidence.title}'${evidence.statedYear.fold("")(year => s" [$year]")}" +
    (if (evidence.directors.nonEmpty) s" {${evidence.directors.mkString(", ")}}" else "")
  /** [[name]] and how many listings share it — for a log line, never a stored trace (the count drifts tick to tick). */
  def label: String       = s"$name ×$weight"
}

private[identity] object EvidenceNode {
  /** The country (ISO-3166-1) of `cinema`. */
  def countryOf(cinema: models.Cinema): Option[String] =
    models.City.forCinema(cinema).map(_.country.language.getCountry).filter(_.nonEmpty)

  /** A venue's own name and its city's, as words: what a title piece naming the venue spells. */
  def placesOf(cinema: models.Cinema): Seq[Seq[String]] =
    (Seq(cinema.displayName) ++ models.City.forCinema(cinema).map(_.labels.nominative))
      .map(services.movies.TitleContainment.tokens).filter(_.nonEmpty)
}
