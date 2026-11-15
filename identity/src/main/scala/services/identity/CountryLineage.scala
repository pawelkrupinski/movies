package services.identity

/** The states that ceased to exist and those that succeeded them (`identity-country-lineage.tsv`, hand-kept): TMDB codes a
 *  film by the state it was made in, a venue by the country it knows today, so Chytilová's "Daisies" (TMDB: XC,
 *  Czechoslovakia) read as made somewhere other than MAC Birmingham's "Czechia". A country and its predecessor or
 *  successor are one country to the listing-film `country` measure ([[YearRuntimeMeasures.countryRelation]]); two
 *  successors of one state are not (Czechia is not Slovakia). The table also names the spellings venues write a
 *  defunct state in ("Czechoslovakia", "ZSRR", "DDR"), which the JDK's country names have no entry for. */
object CountryLineage {

  /** One dissolved state: its code, its successors' codes, the spellings a venue writes it in. */
  final case class State(code: String, successors: Set[String], spellings: Seq[String])

  val Resource = "identity-country-lineage.tsv"

  /** Lines `<defunct code>\t<successor codes, comma-separated>[\t<spellings, comma-separated>]`; `#` starts a comment. */
  def parse(lines: Iterator[String]): Seq[State] =
    ClasspathResource.rows(lines).map(_.split("\t", -1).map(_.trim)).collect {
      case Array(code, successors, rest*) if code.nonEmpty =>
        State(code, list(successors).toSet, rest.headOption.fold(Seq.empty[String])(list))
    }.toSeq

  private def list(column: String): Seq[String] = column.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq

  /** The table on the classpath. */
  lazy val table: Seq[State] = parse(ClasspathResource.lines(Resource).iterator)

  /** Each code with the others it is one country with: its successors if it was dissolved, the states it succeeded. */
  private lazy val lineage: Map[String, Set[String]] =
    table.flatMap(s => (s.code -> s.successors) +: s.successors.toSeq.map(_ -> Set(s.code)))
      .groupMapReduce(_._1)(_._2)(_ ++ _)

  /** Does any of `listing`'s countries name one of `film`'s, or its predecessor or successor? */
  def share(listing: Set[String], film: Set[String]): Boolean =
    listing.exists(code => film.contains(code) || lineage.get(code).exists(_.exists(film)))

  /** The defunct state a spelling already turned into a comparison key names. */
  def codeOfKey(spellingKey: String): Option[String] = byKey.get(spellingKey)

  private lazy val byKey: Map[String, String] =
    table.flatMap(s => s.spellings.map(name => IdentityMeasures.key(name) -> s.code)).toMap
}
