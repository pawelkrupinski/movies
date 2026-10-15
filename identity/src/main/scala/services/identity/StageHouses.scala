package services.identity

/** The stage houses billed under more than one name (`identity-stage-houses.tsv`, hand-kept): each house's names, as
 *  the words a banner spells them in. A house renamed — Covent Garden's Royal Opera House, Royal Ballet and Opera since
 *  2024 — is billed under both names and both initials ("Tosca (ROH)" beside TMDB's "Royal Ballet & Opera 2026/27:
 *  Tosca"), which no word of the banner ties together. */
object StageHouses {
  val Resource = "identity-stage-houses.tsv"

  /** Lines `<name>|<name>…`, one house each: every name's words. */
  def parse(lines: Iterator[String]): Seq[Seq[Seq[String]]] =
    ClasspathResource.rows(lines)
      .map(_.split('|').toSeq.map(name => services.movies.TitleContainment.tokens(name).toSeq).filter(_.nonEmpty)).filter(_.sizeIs >= 2).toSeq

  /** Every house's names on the classpath. */
  lazy val houses: Seq[Seq[Seq[String]]] = parse(ClasspathResource.lines(Resource).iterator)

  /** Words a house's initials skip ("RBO" of "The Royal Ballet and Opera"). */
  val UninitialledWords: Set[String] = Set("the", "and", "und", "of")

  /** `name`'s initials, its numbers and [[UninitialledWords]] skipped: "Royal Ballet & Opera" → "rbo". */
  def initialsOf(name: Seq[String]): String =
    name.filterNot(word => word.forall(_.isDigit) || UninitialledWords(word)).flatMap(_.headOption).mkString

  /** The initials of every house name of three initialled words or more ("rbo", "roh"), derived from the names, never
   *  listed: two letters ("MO" of "Metropolitan Opera") are any title's word. */
  def initialsOfHouses(houses: Seq[Seq[Seq[String]]]): Set[String] =
    houses.flatten.map(initialsOf).filter(_.length >= 3).toSet

  /** [[initialsOfHouses]] of the classpath's houses. */
  lazy val initials: Set[String] = initialsOfHouses(houses)

  /** Each house's names, its names' [[initials]] among them: "ROH" and "Royal Ballet & Opera" name one house. */
  lazy val namedWithInitials: Seq[Seq[Seq[String]]] =
    houses.map(names => names ++ names.map(initialsOf).filter(initials).distinct.map(Seq(_)))
}
