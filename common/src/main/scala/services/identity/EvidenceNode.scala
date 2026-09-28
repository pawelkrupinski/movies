package services.identity

/** A node: the listings sharing one evidence. Named by its smallest listing's sort key. */
private[identity] final class EvidenceNode(val evidence: Evidence, val listings: Seq[Listing]) {
  val id: String          = listings.head.sortKey
  val weight: Int         = listings.size
  val venue: String       = listings.head.venue
  val venues: Set[String] = listings.map(_.venue).toSet
  def label: String       = s"'${evidence.title}'${evidence.statedYear.fold("")(y => s" [$y]")}" +
    (if (evidence.directors.nonEmpty) s" {${evidence.directors.mkString(", ")}}" else "") + s" ×$weight"
}

private[identity] object EvidenceNode {
  /** A venue's own name and its city's, as words: what a title piece naming the venue spells. */
  def placesOf(cinema: models.Cinema): Seq[Seq[String]] =
    (Seq(cinema.displayName) ++ models.City.forCinema(cinema).map(_.labels.nominative))
      .map(services.movies.TitleContainment.tokens).filter(_.nonEmpty)
}
