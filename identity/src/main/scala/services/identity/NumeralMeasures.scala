package services.identity

import services.movies.TitleContainment

import IdentityMeasures.*

/** How a listing's numerals (Roman or Arabic, glued or apart) and a film's number one instalment of a series.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object NumeralMeasures {
  /** A Roman numeral of the size a series numbers its instalments by (I to XXXIX), by its grammar,
   *  never a list of values: tens, then units. The larger letters (L, C, D, M) spell words far more
   *  often than instalments ("M", "Mix", "DC"). */
  private[identity] val RomanNumeral = "(?=[xvi])(x{0,3})(ix|iv|v?i{0,3})".r
  private val RomanDigit = Map('i' -> 1, 'v' -> 5, 'x' -> 10)
  /** A title word that is a Roman numeral ([[RomanNumeral]]) of two letters or more — "i" is the Polish "and". */
  private[identity] def numbersAnInstalment(word: String): Boolean = word.length > 1 && RomanNumeral.pattern.matcher(word).matches()
  /** A title word that is a year of the last two centuries ("2024" of "Miłość 2024"). */
  private[identity] val TitleYear = "(?:19|20)\\d\\d".r
  private def romanValue(t: String): Option[Int] =
    Option.when(RomanNumeral.matches(t))(t.map(RomanDigit).foldRight((0, 0)) { case (v, (sum, max)) =>
      if (v < max) (sum - v, max) else (sum + v, v) }._1)

  /** The NUMBERS a title writes as words of their own: an Arabic numeral of at most three digits
   *  anywhere (four digits are a year, "1917", "2001", "Blade Runner 2049", as the listing's other
   *  measures read them), or a Roman numeral closing a delimited piece of it ("Rocky II", "Part
   *  III: …", "Star Wars: Episode IV - …"; mid-piece "i" is a word, the Polish "and"). "Part 2",
   *  "2" and "II" are all 2. Seasons and bracketed years are dropped first. With the title's
   *  other words, in order: the series it names. */
  private[identity] final case class Numbered(words: Seq[String], numbers: Set[Int]) {
    /** The words run together, once: [[sameSeries]] compares it for every (shape, title) pair. */
    lazy val joined: String = words.mkString
  }
  private[identity] val ColonBreak         = java.util.regex.Pattern.compile(":\\s")
  private val NumberedPieceBreak = java.util.regex.Pattern.compile("""[:|/()\[\]–—,.;!?]|\s-\s""")
  /** A word with its number run into it — "Worlds25", "Rambo3" — as the word and the number; a number inside a word
   *  ("Se7en", "M3GAN", "R2D2") is its name, kept whole. */
  private val GluedNumber = """(\p{L}{2,})(\d{1,3})""".r
  private def unglued(token: String): Seq[String] = token match {
    case GluedNumber(word, number) => Seq(word, number)
    case _                         => Seq(token)
  }
  private[identity] def numbered(title: String): Numbered = {
    val pieces = NumberedPieceBreak.split(withoutYears(title)).map(p => TitleContainment.tokens(p).flatMap(unglued)).filter(_.nonEmpty).toSeq
    val arabic = (t: String) => t.lengthIs <= 3 && t.forall(c => c >= '0' && c <= '9')
    val numbers = pieces.flatMap(p => p.filter(arabic).map(_.toInt) ++ romanValue(p.last)).toSet
    val words = pieces.flatMap(p => p.zipWithIndex.filterNot { case (t, i) => arabic(t) || (i == p.size - 1 && romanValue(t).isDefined) }.map(_._1))
    Numbered(words, numbers)
  }

  /** Does one title SPELL the other's series once each drops its numbers — the same words, or
   *  one's words a token run along an edge of the other's ([[TitleContainment.isTokenRun]]): "The
   *  Texas Chainsaw Massacre 2" and "The Texas Chain Saw Massacre", "Toy Story" and "Toy Story 3". */
  private def sameSeries(a: Numbered, b: Numbered): Boolean =
    a.words.nonEmpty && b.words.nonEmpty &&
      (a.joined == b.joined || TitleContainment.isTokenRun(a.words, b.words) || TitleContainment.isTokenRun(b.words, a.words))

  /** The NUMBERS the listing's title and the film's carry, where the two name one series
   *  ([[sameSeries]]): `same` when a reading of the listing (a title shape) and a title of the
   *  film number themselves alike — "The Texas Chainsaw Massacre 2" and "… Part 2", "Mortal
   *  Kombat 2" and "… II"; else the instalment one side numbers and the other does not
   *  (`listing_only`: "Toy Story 2" beside "Toy Story"; `film_only`), or numbers otherwise
   *  (`different`). Missing when neither numbers itself (`none`: nothing to compare — and a
   *  decoration's number, "Cineworld 30: The Matrix", never counts while a reading without it
   *  names the film) or when no reading names the film's series (`unrelated`: the title relation
   *  weighs that). A remake, or a title whose number IS its name ("1917", "Se7en", "Ocean's
   *  Eleven"), has nothing to compare; nor has a house's SEASON record, whose relay a venue numbers by its season
   *  (`season`: "RBO: Tosca 27" is "Royal Ballet & Opera 2026/27: Tosca"). An original title that is but a spelling of
   *  the billing ([[Listing.originalStandsApart]]) never numbers it the film's way where the billing numbers itself
   *  otherwise: a feed copies a search hit's title, number and all — DE UFA Palast's "League of Legends Worlds 26 -
   *  Finals in Cinema" carried "League of Legends Worlds25 - Finals in Cinema", the 2025 final's record. */
  def numeralRelation(l: Listing, f: Film): Measure = {
    val relation = numeralRelation(l.numberedShapes, f)
    if (relation == Category("same") && (l.numberedBilledShapes ne l.numberedShapes)) {
      val billed = numeralRelation(l.numberedBilledShapes, f)
      if (billed == Category("different")) billed else relation
    } else relation
  }
  private def numeralRelation(shapes: Seq[Numbered], f: Film): Measure = {
    lazy val related = for (a <- shapes; b <- f.numberedTitles if sameSeries(a, b)) yield (a.numbers, b.numbers)
    if (f.season.isDefined) Missing("season")
    else if (related.isEmpty) Missing("unrelated")
    else if (related.exists { case (a, b) => a.nonEmpty && a == b }) Category("same")
    else if (related.exists { case (a, b) => a.isEmpty && b.isEmpty }) Missing("none")
    else related.head match {
      case (_, b) if b.isEmpty => Category("listing_only")
      case (a, _) if a.isEmpty => Category("film_only")
      case _                   => Category("different")
    }
  }

  /** The numeral relations under which the listing numbers ANOTHER instalment than the film. */
  val OtherInstalment: Set[String] = Set("listing_only", "film_only", "different")

  /** Does the listing's title NAME the film: a naming title relation ([[NamingRelations]]), and not
   *  another instalment of its series ([[numeralRelation]]) — "The Texas Chainsaw Massacre 2"
   *  carries the whole of "The Texas Chainsaw Massacre" and names its sequel. */
  def namesFilm(l: Listing, f: Film, houses: StageRelays.Houses = StageRelays.Houses.Unknown): Boolean =
    names(titleRelation(l, f, houses).value, l, f)
  /** [[namesFilm]] on a title relation already measured. */
  def names(relation: String, l: Listing, f: Film): Boolean =
    NamingRelations(relation) && !numbersAnotherInstalment(l, f)
  def numbersAnotherInstalment(l: Listing, f: Film): Boolean = numeralRelation(l, f) match {
    case Category(c) => OtherInstalment(c)
    case _           => false
  }
}
