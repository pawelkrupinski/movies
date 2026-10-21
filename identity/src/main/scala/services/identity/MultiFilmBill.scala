package services.identity

import services.billing.{BillingParse, DecorationWords}
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

  /** The marker by which one of `titles` bills several films ([[services.billing.BillingParse.marker]]): a "Triple:" or
   *  "Double:" banner heading any of them first, else the first one's word as its folded words read ("triple feature",
   *  "trilogy", "maraton"), when it bills one beside other words. */
  def marker(titles: Seq[String]): Option[String] =
    titles.iterator.flatMap(BillingParse(_).headingBanner).nextOption().orElse(titles.iterator.flatMap(BillingParse(_).foldedMarker).nextOption())

  /** The one title a programme word annotates after a dash — "Inna Mamusia - maraton" ([[BillingParse.billedOne]]): a
   *  marathon of one title, which is one film's unless another film's title extends it (a franchise: "Piraci z Karaibów -
   *  maraton"), as [[Acceptance]] reads it beside its candidates. */
  def billedOne(titles: Seq[String]): Option[String] = titles.iterator.flatMap(BillingParse(_).billedOne).nextOption()

  /** Does `film`'s own title, or its original title, carry `marker` — the film named by it, not a bill of others? */
  def namedBy(marker: String, film: IdentityMeasures.Film): Boolean =
    (film.title +: film.originalTitle.toSeq).exists(title => s" ${folded(title)} ".contains(s" $marker "))

  /** Does a listing titled `titles` bill several films, `film` not named by its marker? */
  def billsBeside(titles: Seq[String], film: IdentityMeasures.Film): Boolean = marker(titles).exists(!namedBy(_, film))

  /** Does the billing join pieces with a spaced "//" as two works, `film` one of them? A "//" joins one film's titles in
   *  two languages — DE Central im Bürgerbräu's "DIE VENUS VON PARIS // LA VÉNUS ÉLECTRIQUE", "DOWN AND OUT IN PARIS // À
   *  PIED D'ŒUVRE", Ostpassagen's "La Haine // Hass", one piece its original title — or two works: Filmhaus Kino Köln's
   *  "SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE" (judged 10-13, a double bill) is no 2021 "片袖の魚", which neither piece
   *  titles as its own and whose titles hold no word of "Shinjukuy Boys". Read against the film, as only its titles tell
   *  the two shapes apart: neither piece is its own or original title, and one is none of its titles at all. A piece
   *  names a title when every word of it (of two letters or more) is one of the title's. */
  def slashesAnotherWork(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean = {
    val raw = listing.rawTitle.getOrElse(listing.title)
    raw.contains("//") && {
      val pieces = slashPieces(raw).map(piece => TitleContainment.tokens(piece).filter(_.length > 1)).filter(_.nonEmpty)
      def names(piece: Seq[String], titles: Seq[String]) = titles.exists(title => piece.forall(TitleContainment.tokens(title).toSet))
      pieces.sizeIs >= 2 && !pieces.exists(names(_, film.ownTitles)) && pieces.exists(piece => !names(piece, film.titles))
    }
  }
  /** The pieces a spaced "//" joins in `raw` ([[BillingParse.slashPieces]]). */
  private[identity] def slashPieces(raw: String): Seq[String] = BillingParse(raw).slashPieces

  private def folded(title: String): String = TitleContainment.tokens(title).mkString(" ")

  /** The pieces a bill's "+" joins in `title` ([[BillingParse.billPieces]]). */
  private[identity] def billPieces(title: String): Seq[String] = BillingParse(title).billPieces
  /** Does `title` join works by a bill's "+" ([[BillingParse.joinsWorks]])? */
  private[identity] def joinsWorks(title: String): Boolean = BillingParse(title).joinsWorks

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
  private[identity] def namesAWork(piece: String): Boolean = {
    // a one-letter word names nothing: "Q&A" is no work, and the "a" of "We're Going on a Bear Hunt" no event; a title
    // of numbers is one ("2 x 2 = 4", billed after "Zakazane piosenki")
    val all   = TitleContainment.tokens(piece)
    val words = all.filter(_.length > 1)
    // an event's whole phrase counts as its words do: "Alex Cox Q&A" is a talk, whose single letters no word set holds
    (words.exists(_.exists(_.isLetter)) || all.sizeIs >= 3) &&
      !words.exists(w => EventTailWords(w) || services.movies.FormatTags.FormatToken.contains(w)) && !DecorationWords.namesAnEvent(all)
  }
  /** What a piece a bill joins after a film names when it names an event, not a work: a talk, a lecture, a workshop, a
   *  quiz, a gala, an exhibition or its opening, a concert, a (poster) tour, a signing, an introduction, a party, a drink,
   *  a choir — PL, EN, DE, ES — beside the
   *  segment model's event words. "OPĘTANIE + TRASA PLAKATOWA" (Światowid) is one film and a poster tour. */
  private val EventTailWords: Set[String] = DecorationWords.EventWords ++ TitleWords.tokens(TitleWords.Concept.SideEvent)
  /** The works a DOUBLE BILL joins with a spaced "+" ("Basia. Humor w paski mam + Kocia Szajka"),
   *  each searched on its own: the database has no record of the bill, so without them the only
   *  candidates are what a credited director's filmography walks to. Searched, not shapes: a bill
   *  is neither of its works, which is why its family keys leave them out. */
  private[identity] def billedWorks(l: IdentityMeasures.Listing): Seq[String] =
    l.ownTitles.map(t => billPieces(l.decorations.withoutTail(t)).map(_.trim).filter(_.nonEmpty)).filter(_.sizeIs > 1).flatten
}
