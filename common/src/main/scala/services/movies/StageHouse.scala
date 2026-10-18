package services.movies

import scala.io.Source

/** An opera or ballet HOUSE as a title bills it: the word table's house words ([[TitleWords.Concept.StageHouse]]), the
 *  houses billed under more than one name (`stage-houses.tsv`, hand-kept: Covent Garden's Royal Opera House, Royal Ballet
 *  and Opera since 2024, billed under both names and both initials), and every name's initials, derived from it ("RBO",
 *  "ROH"; never listed). Read by the identity model (a relay's house, its banner) and by a venue's slots (two seasons of
 *  one staging are two productions; a programme's two seasons are not), so both read one house. */
object StageHouse {
  val Resource = "stage-houses.tsv"

  /** Lines `<name>|<name>…`, one house each: every name's words. */
  def parse(lines: Iterator[String]): Seq[Seq[Seq[String]]] =
    lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(_.split('|').toSeq.map(name => TitleContainment.tokens(name).toSeq).filter(_.nonEmpty)).filter(_.sizeIs >= 2).toSeq

  /** Every house's names on the classpath. */
  lazy val houses: Seq[Seq[Seq[String]]] =
    Option(getClass.getClassLoader.getResourceAsStream(Resource)).fold(Seq.empty[Seq[Seq[String]]]) { in =>
      try parse(Source.fromInputStream(in, "UTF-8").getLines()) finally in.close()
    }

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

  /** Words naming a house or its relay, as their own tokens ("Dreigroschenoper" or "Operation" is no house): the table's
   *  and the houses' initials. */
  lazy val Words: Set[String] = TitleWords.tokens(TitleWords.Concept.StageHouse) ++ initials

  /** `name` with each run of a house's INITIALS that punctuation split into words joined back into one: DE "RB+O" folds to
   *  "rb" and "o", the initials "rbo" of Royal Ballet & Opera. A run is two words or more of two letters at most, one of
   *  them a single letter — "de la" are words, never initials. */
  def initialsJoined(name: Seq[String]): Seq[String] = {
    def short(word: String) = word.length <= 2 && word.forall(_.isLetter)
    def isInitials(run: Seq[String]) = run.sizeIs >= 2 && run.exists(_.length == 1)
    name.foldLeft(Vector.empty[Vector[String]]) { (runs, word) =>
      runs.lastOption match {
        case Some(run) if short(word) && run.forall(short) => runs.init :+ (run :+ word)
        case _                                             => runs :+ Vector(word)
      }
    }.flatMap(run => if (run.forall(short) && isInitials(run)) Seq(run.mkString) else run)
  }

  /** Do `words` hold a house's initials that punctuation split into words ("RB&O" read as "rb", "o")? */
  def splitInitials(words: Seq[String]): Boolean =
    initialsJoined(words).exists(word => initials(word) && !words.contains(word))

  /** A house's initials as a word of its own: no letter or digit beside them, nor a hyphened word ("Tosca (ROH)", never
   *  "Jin-Roh"). */
  private lazy val StandaloneInitials = Option.when(initials.nonEmpty)(
    initials.toSeq.sorted.map(java.util.regex.Pattern.quote).mkString("""(?iu)(?<![\p{L}\d]-?)(?:""", "|", """)(?!-?[\p{L}\d])""").r)

  /** Does one of `titles` name an opera or ballet house ([[Words]])? A house's initials only standing alone, or split by
   *  punctuation ([[splitInitials]]: UK Penrith's "RB&O 26/27: Carmen", DE "Carmen (RB+O 2026/27)"): DE "Jin-Roh - Die
   *  Wolfsbrigade" is Okiura's anime, no Royal Opera House relay, whatever its hyphen splits off. */
  def billed(titles: Seq[String]): Boolean = titles.exists { title =>
    val words = TitleContainment.tokens(title)
    words.exists(word => Words(word) && !initials(word)) || StandaloneInitials.exists(_.findFirstIn(title).isDefined) || splitInitials(words)
  }
}
