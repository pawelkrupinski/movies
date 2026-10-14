package services.identity

import services.movies.{TitleContainment, TitleWords}

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
 * The "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]) is the structural half of the same rule.
 */
object MultiFilmBill {

  private val Markers: Seq[scala.util.matching.Regex] = Seq(
    // a double or triple bill, as each language bills one (the word table's, services.movies.TitleWords)
    services.movies.TitleWords.foldedPattern(services.movies.TitleWords.Concept.DoubleBill),
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

  /** The marker by which one of `titles` bills several films, as its folded words read ("triple feature", "trilogy",
   *  "maraton"), when one does beside other words. */
  def marker(titles: Seq[String]): Option[String] =
    titles.iterator.flatMap(title => HeadingBanner.findFirstMatchIn(title).map(_.group(1).toLowerCase(java.util.Locale.ROOT))).nextOption().orElse(
    titles.iterator.map(folded).flatMap { words =>
      Markers.iterator.flatMap(_.findFirstMatchIn(words)).find { m =>
        val rest = (words.substring(0, m.start) + " " + words.substring(m.end)).split(' ')
        rest.exists(word => word.length > 1 && word.exists(_.isLetter))
      }.map(_.matched)
    }.nextOption())

  /** "Triple" or "Double" heading a title as its banner, a colon after it and a title after that — DE Filmpalast's
   *  "Triple: Der Herr der Ringe - Extended Versions" (Jackson's three back to back) — read on the title as billed, since
   *  the folded words lose the colon that tells the banner from a film's own name ("Triple 9", "Double Trouble"). */
  private val HeadingBanner = """(?i)^\s*(triple|double|quadruple)\s*:\s*\p{L}""".r

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

  private val BillJoin = """\s\+\s""".r
  private val TightJoin = """(?<=\p{L}{2})\+(?=\p{L}{2})""".r
  /** The pieces a bill's "+" joins in `title`: a spaced "+" always, an unspaced one only between two pieces of two words
   *  or more — UK Odeon's "We're Going on a Bear Hunt+Tiger Who Came to..." — never inside one title's own words
   *  ("Love+War"), a talk's ("Q+A") or a house's name ("RB+O 2026/27"). One piece when it joins none. */
  private[identity] def billPieces(title: String): Seq[String] =
    if (title.indexOf('+') < 0) Seq(title) // every rule asks, and almost no title holds a "+"
    else BillJoin.split(title).toSeq.flatMap { piece =>
      val parts = TightJoin.split(piece).toSeq
      def words(part: String) = TitleContainment.tokens(part).count(word => word.length > 1 && word.exists(_.isLetter))
      if (parts.sizeIs == 2 && parts.forall(words(_) >= 2)) parts else Seq(piece)
    }
  /** Does `title` join works by a bill's "+" ([[billPieces]])? */
  private[identity] def joinsWorks(title: String): Boolean = title.indexOf('+') >= 0 && (BillJoin.findFirstIn(title).isDefined || billPieces(title).sizeIs > 1)

  /** What a double bill joins LAST with a "+" ([[billPieces]]: "… + The Tiger Who Came to Tea") — a film, or a talk. */
  def billedSecondTitle(l: IdentityMeasures.Listing): Option[String] = billPieces(l.decorations.withoutTail(l.title)).lastOption.filter(_ => billsTwoWorks(l)).map(_.trim)

  /** Does the listing bill two works with a "+" ([[billPieces]]) — a double bill, whose facts are one of its films'? Not when
   *  what it bills last is a learned event tail ([[TitleDecorations.withoutTail]]): "Punku + spotkanie z reżyserem". */
  def billsTwoWorks(l: IdentityMeasures.Listing): Boolean = l.ownTitles.exists(t => joinsWorks(l.decorations.withoutTail(t)))
  /** Does the listing bill two WHOLE WORKS — two of the pieces its "+" joins ([[billPieces]]) naming no event, whatever a database
   *  holds of either? A talk, a screening note or a format joined to one film ("+ prelekcja", "+ spotkanie z reżyserem",
   *  "+ napisy EN", a bracketed "(AD + CC + PJM)") is no second work. PL Kino Pałacowe "… | Historia kina w Popielawach +
   *  Pruska kultura" bills a 1908 short TMDB does not hold: no candidate stood for it, yet the bill is neither film. */
  def billsTwoWholeWorks(l: IdentityMeasures.Listing): Boolean =
    l.ownTitles.exists { t =>
      val pieces = billPieces(l.decorations.withoutTail(Bracketed.replaceAllIn(t, " ").trim))
      // the programme the last work runs into after a spaced dash is the bill's ("… + Kocia Szajka - Festiwal TAURON
      // Młode Horyzonty"): the work is what precedes it
      (pieces.init :+ ProgrammeDash.pattern.split(pieces.last, 2).head).count(namesAWork) >= 2
    }
  private val Bracketed     = """\([^()]*\)|\[[^\[\]]*\]""".r
  private val ProgrammeDash = """\s[-–—]\s""".r
  private def namesAWork(piece: String): Boolean = {
    // a one-letter word names nothing: "Q&A" is no work, and the "a" of "We're Going on a Bear Hunt" no event; a title
    // of numbers is one ("2 x 2 = 4", billed after "Zakazane piosenki")
    val all   = TitleContainment.tokens(piece)
    val words = all.filter(_.length > 1)
    (words.exists(_.exists(_.isLetter)) || all.sizeIs >= 3) &&
      !words.exists(w => EventTailWords(w) || services.movies.FormatTags.FormatToken.contains(w))
  }
  /** What a piece a bill joins after a film names when it names an event, not a work: a talk, a lecture, a workshop, a
   *  quiz, a gala, an exhibition or its opening, a concert, a (poster) tour, a signing — PL, EN, DE, ES — beside the
   *  segment model's event words. "OPĘTANIE + TRASA PLAKATOWA" (Światowid) is one film and a poster tour. */
  private val EventTailWords: Set[String] = DecorationWords.EventWords ++ TitleWords.tokens(TitleWords.Concept.SideEvent)
  /** The works a DOUBLE BILL joins with a spaced "+" ("Basia. Humor w paski mam + Kocia Szajka"),
   *  each searched on its own: the database has no record of the bill, so without them the only
   *  candidates are what a credited director's filmography walks to. Searched, not shapes: a bill
   *  is neither of its works, which is why its family keys leave them out. */
  private[identity] def billedWorks(l: IdentityMeasures.Listing): Seq[String] =
    l.ownTitles.map(t => billPieces(l.decorations.withoutTail(t)).map(_.trim).filter(_.nonEmpty)).filter(_.sizeIs > 1).flatten
}
