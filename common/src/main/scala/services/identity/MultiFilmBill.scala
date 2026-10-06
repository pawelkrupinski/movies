package services.identity

import services.movies.TitleContainment

/**
 * A listing billing SEVERAL films as one programme by a word that says so — a double or triple feature, a trilogy, a
 * marathon, a block of shorts, parts numbered together — is none of its films (user rule: zero wrong matches). US Frida
 * Cinema's "Triple Feature: Lord of the Rings" (all three of Jackson's films back to back) took "The Return of the King"
 * pooled at 61.2%; UK Showcase's "The Dark Knight Trilogy" ×17 took "The Dark Knight" by its director; PL "Maraton
 * Horrorów" ×5 (three horrors a night, none of them "Whistle") took "Whistle" by a title it shares.
 *
 * Read on the title's folded words ([[TitleContainment.tokens]]), the same lexicon in every country, as a venue bills
 * its programme in whatever language. A marker counts only beside other words: a title that is nothing but the marker
 * ("Marathon", "Double Feature") is a film's own name. And a film whose OWN title (or original title) carries the marker
 * is no bill of it — "Marathon Man", "Trilogy of Terror", "National Theatre Live: The Lehman Trilogy" — which
 * [[namedBy]] answers for the film a rule would take. An alternative title is not its own: "Whistle" is filed under
 * "Maraton horrorów" somewhere, and that is how the marathon took it.
 *
 * The "+" joining two whole works ([[IdentityMeasures.billsTwoWholeWorks]]) is the structural half of the same rule.
 */
object MultiFilmBill {

  private val Markers: Seq[scala.util.matching.Regex] = Seq(
    // a double or triple bill, as each language bills one
    """\b(?:double|triple|quadruple)\s+(?:features?|bills?)\b""",
    """\bdoppel(?:vorstellung|pack|programm|feature)\w*""",
    """\b(?:sesion|programa)\s+(?:doble|triple)\b|\bdoble\s+sesion\b""",
    // not "zestaw" (a set): a set of a series' episodes is one compilation record ("Niesamowite przygody skarpetek 4. Do
    // roboty! – zestaw" is TMDB's 1735319)
    """\bpodwojn\w*\s+seans\w*""",
    // a franchise's films together
    """\btrilog(?:y|ie|ia)\b|\btrylogi\w*""",
    // a night of films
    """\bmarath?on\w*""",
    """\bback\s+to\s+back\b""",
    """\bnoc\s+(?:\w+\s+)?film\w*""",
    // a block of shorts: "Blok filmów krótkometrażowych", "Pokaz shortów", "Shorts Programme", "Kurzfilmprogramm"
    """\bblok\w*\s+(?:\w+\s+)?krotkometrazow\w*|\bpokaz\s+short\w*""",
    """\bshorts?\s+(?:films?\s+)?(?:programmes?|programs?|blocks?|nights?|showcase)\b""",
    """\bkurzfilm(?:programm|abend|nacht|rolle)\w*|\bkurzfilme\b""",
    """\b(?:programa|sesion|noche)\s+de\s+cortos?\w*|\bcortometrajes\b""",
    // parts numbered together: "Parts 1-3", "Teil 1 & 2", "Vol. 1 & 2", "części 1 i 2"
    """\b(?:parts?|teile?|czesci|partes?|vol|volumes?)\s+(?:\d|i{1,3}|iv)\s+(?:(?:and|und|i|y|et)\s+)?(?:\d|i{1,3}|iv)\b"""
  ).map(_.r)

  /** The marker by which one of `titles` bills several films, as its folded words read ("triple feature", "trilogy",
   *  "maraton"), when one does beside other words. */
  def marker(titles: Seq[String]): Option[String] =
    titles.iterator.map(folded).flatMap { words =>
      Markers.iterator.flatMap(_.findFirstMatchIn(words)).find { m =>
        val rest = (words.substring(0, m.start) + " " + words.substring(m.end)).split(' ')
        rest.exists(word => word.length > 1 && word.exists(_.isLetter))
      }.map(_.matched)
    }.nextOption()

  /** A spaced dash between a title and its annotation. */
  private val DashPiece = """\s+[-\u2013\u2014]\s+""".r

  /** The one title a programme word annotates after a dash — "Inna Mamusia - maraton" — when the word and nothing else
   *  follows it, and the title joins no works by a "+": a marathon of one title, which is one film's unless another
   *  film's title extends it (a franchise: "Piraci z Karaibów - maraton"), as [[Acceptance]] reads it beside its
   *  candidates. A programme word heading the title ("Maraton Horrorów") names no title. */
  def billedOne(titles: Seq[String]): Option[String] =
    titles.iterator.flatMap { title =>
      DashPiece.split(title.trim).toSeq match {
        case Seq(head, tail) if !head.contains('+') && head.exists(_.isLetter) &&
          Markers.exists(_.pattern.matcher(folded(tail)).matches()) => Some(head.trim)
        case _ => None
      }
    }.nextOption()

  /** Does `film`'s own title, or its original title, carry `marker` — the film named by it, not a bill of others? */
  def namedBy(marker: String, film: IdentityMeasures.Film): Boolean =
    (film.title +: film.originalTitle.toSeq).exists(title => s" ${folded(title)} ".contains(s" $marker "))

  /** Does a listing titled `titles` bill several films, `film` not named by its marker? */
  def billsBeside(titles: Seq[String], film: IdentityMeasures.Film): Boolean = marker(titles).exists(!namedBy(_, film))

  private def folded(title: String): String = TitleContainment.tokens(title).mkString(" ")
}
