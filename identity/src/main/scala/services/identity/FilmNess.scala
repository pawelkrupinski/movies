package services.identity

/** A billing's FILM-NESS from its own shape (experiment `film-ness`): the words of an event's vocabulary it bills — a
 *  gig, a party, a tour, a festival, a show — in a billing that states no film's fact (no year, running time or
 *  director), names no relay and attaches no screening. A record a search found by those words alone, sharing none of
 *  the billing's other words, is the namesake of the event's genre, not what the venue screens: DE Casablanca Zwickau's
 *  "Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer" took "Number_i LIVE TOUR 2024", Kino im Druckluft's "Italo
 *  Cinema Festival 2026" "The Cure - Maia North Festival 2026". */
private[identity] object FilmNess {

  /** Words whole: a token that IS one. */
  private val Whole = Set("live", "tour", "show", "band", "gala", "slam", "quiz", "night", "nacht", "stage", "disco", "open", "experience",
    "abend", "noc", "wieczor")
  /** Stems a compound carries ("Halloweenparty", "Wohnzimmerkonzerte", "Kinokonzert"). */
  private val Stems = Seq("konzert", "concert", "koncert", "festival", "festiwal", "party", "comedy", "musical", "tribute", "karaoke",
    "lesung", "poetry", "signierstunde", "kabarett", "kabaret", "cabaret", "soulnight", "tournee")

  private val Bracketed = """[(\[][^)\]]*[)\]]""".r

  def isEventWord(word: String): Boolean = Whole(word) || Stems.exists(word.contains)

  /** The billing's event words — none when it states a fact of a film, names a relay or attaches a screening. */
  def eventWords(listing: IdentityMeasures.Listing): Seq[String] = {
    val raw = listing.rawTitle.getOrElse(listing.title)
    // V3c: no fact condition — a feed's page copies the found record's year and running time onto a gig's listing
    // (kinoheld's "Tanzwut - Live Tour 2026" carries the Number_i film's 108 minutes), so a stated fact proves nothing here
    if (agreement.NonFilmEvents.eventBilled(raw).isEmpty ||
        // a relay by its broadcast's name or its stage work — never by a year alone, which a gig's "Tour 2026" bills too
        services.cinemas.pl.NonMovieEventClassifier.isNamedBroadcast(raw) ||
        StageRelays.billsStageWork(listing.ownTitles.distinct, raw)) Nil
    // V3e: an event word inside a bracketed decoration decorates a film billed before it ("ELF (Pyjama Party)")
    else IdentityMeasures.countedWords(Bracketed.replaceAllIn(raw, " ")).filter(isEventWord)
  }

  /** The billing's words that are no event's: what a record of the event itself must share. */
  def otherWords(listing: IdentityMeasures.Listing): Set[String] =
    IdentityMeasures.countedWords(listing.rawTitle.getOrElse(listing.title)).filterNot(w => isEventWord(w) || w.forall(_.isDigit)).toSet
}
