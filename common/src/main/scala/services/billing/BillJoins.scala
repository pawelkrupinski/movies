package services.billing

import services.movies.{TitleContainment, TitleWords}

/**
 * How a billing JOINS several works into one programme, read off its text alone ([[BillingParse.billPieces]],
 * [[BillingParse.slashPieces]], [[BillingParse.marker]], [[BillingParse.billedOne]]): a bill's "+", a spaced "//", a word
 * billing several films (a double or triple feature, a trilogy, a marathon, a block of shorts, parts numbered together),
 * a programme word annotating one title. Whether a film is one of the works, or named by the word, is
 * `services.identity.MultiFilmBill`'s question.
 *
 * Read on the title's folded words ([[TitleContainment.tokens]]), the same lexicon in every country, as a venue bills
 * its programme in whatever language. A marker counts only beside other words: a title that is nothing but the marker
 * ("Marathon", "Double Feature") is a film's own name.
 */
object BillJoins {

  private val Markers: Seq[scala.util.matching.Regex] = Seq(
    // a double or triple bill, as each language bills one (the word table's, services.movies.TitleWords)
    TitleWords.foldedPattern(TitleWords.Concept.DoubleBill),
    // "Triple" after a franchise's name — DE CinemaxX's "Lord Of The Rings Triple (Extended Version - English)" — never
    // heading a title, as a film's own name does ("Triple Frontier", "Triple 9")
    """(?<=\w\s)triple\b""",
    // a franchise's films together
    """\btrilog(?:y|ie|ia)\b|\btrylogi\w*""",
    // a night of films
    """\bmarath?on\w*""",
    """\bback\s+to\s+back\b""",
    """\bnoc\s+(?:\w+\s+)?film\w*""",
    // a block of shorts: "Blok filmów krótkometrażowych", "Pokaz shortów", "Shorts Programme", "Kurzfilmprogramm"
    """\bblok\w*\s+(?:\w+\s+)?krotkometrazow\w*|\bpokaz\s+short\w*""",
    """\bshorts?\s+(?:films?\s+)?(?:programmes?|programs?|blocks?|nights?|showcase)\b""",
    // "Shorts" beside a block's name, and any "Kurzfilm…" compound: DE "SHORTS ATTACK! THE KIDS ARE ALRIGHT" and
    // "Kurzfilmtag Oktober: The Kids Are Alright 2026", touring blocks titled like The Who's 1979 film
    """\bshorts\b|\bshortfilm\w*|\bshort\s+films\b""",
    """\bkurzfilm\w+""",
    """\b(?:programa|sesion|noche)\s+de\s+cortos?\w*|\bcortometrajes\b""",
    // parts numbered together: "Parts 1-3", "Teil 1 & 2", "Vol. 1 & 2", "części 1 i 2"
    """\b(?:parts?|teile?|czesci|partes?|vol|volumes?)\s+(?:\d|i{1,3}|iv)\s+(?:(?:and|und|i|y|et)\s+)?(?:\d|i{1,3}|iv)\b"""
  ).map(_.r)

  /** "Triple" or "Double" heading a title as its banner, a colon after it and a title after that — DE Filmpalast's
   *  "Triple: Der Herr der Ringe - Extended Versions" (Jackson's three back to back) — read on the title as billed, since
   *  the folded words lose the colon that tells the banner from a film's own name ("Triple 9", "Double Trouble"). */
  private val HeadingBanner = """(?i)^\s*(triple|double|quadruple)\s*:\s*\p{L}""".r

  /** The banner word heading `title` as a bill's ([[HeadingBanner]]), lower case. */
  def headingBanner(title: String): Option[String] =
    HeadingBanner.findFirstMatchIn(title).map(_.group(1).toLowerCase(java.util.Locale.ROOT))

  /** The marker by which `title` bills several films as its folded words read ("triple feature", "trilogy", "maraton"),
   *  when it does beside other words. */
  def foldedMarker(title: String): Option[String] = {
    val words = folded(title)
    Markers.iterator.flatMap(_.findFirstMatchIn(words)).find { m =>
      val rest = (words.substring(0, m.start) + " " + words.substring(m.end)).split(' ')
      rest.exists(word => word.length > 1 && word.exists(_.isLetter))
    }.map(_.matched)
  }

  /** A spaced dash between a title and its annotation. */
  private val DashPiece = """\s+[-\u2013\u2014]\s+""".r

  /** The one title a programme word annotates after a dash — "Inna Mamusia - maraton" — when the word and nothing else
   *  follows it, and the title joins no works by a "+". A programme word heading the title ("Maraton Horrorów") names no
   *  title. */
  def billedOne(title: String): Option[String] =
    DashPiece.split(title.trim).toSeq match {
      case Seq(head, tail) if !head.contains('+') && head.exists(_.isLetter) &&
        Markers.exists(_.pattern.matcher(folded(tail)).matches()) => Some(head.trim)
      case _ => None
    }

  /** The pieces a spaced "//" joins in `raw`: one when it joins none. */
  def slashPieces(raw: String): Seq[String] = SlashJoin.split(raw).toSeq
  private val SlashJoin = """\s//\s""".r

  private def folded(title: String): String = TitleContainment.tokens(title).mkString(" ")

  private val BillJoin = """\s\+\s""".r
  private val TightJoin = """(?<=\p{L}{2})\+(?=\p{L}{2})""".r
  /** The pieces a bill's "+" joins in `title`: a spaced "+" always, an unspaced one only between two pieces of two words
   *  or more — UK Odeon's "We're Going on a Bear Hunt+Tiger Who Came to..." — never inside one title's own words
   *  ("Love+War"), a talk's ("Q+A") or a house's name ("RB+O 2026/27"). One piece when it joins none. */
  def billPieces(title: String): Seq[String] =
    if (title.indexOf('+') < 0) Seq(title) // every rule asks, and almost no title holds a "+"
    else BillJoin.split(title).toSeq.flatMap { piece =>
      val parts = TightJoin.split(piece).toSeq
      def words(part: String) = TitleContainment.tokens(part).count(word => word.length > 1 && word.exists(_.isLetter))
      if (parts.sizeIs == 2 && parts.forall(words(_) >= 2)) parts else Seq(piece)
    }
  /** Does `title` join works by a bill's "+" ([[billPieces]])? */
  def joinsWorks(title: String): Boolean = title.indexOf('+') >= 0 && (BillJoin.findFirstIn(title).isDefined || billPieces(title).sizeIs > 1)
}
