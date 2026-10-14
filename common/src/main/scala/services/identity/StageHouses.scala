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
}
