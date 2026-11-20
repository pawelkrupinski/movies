package services.identity

/** A billing's FILM-NESS from its own shape (experiment `film-ness`): the words of an event's vocabulary it bills — a
 *  gig, a party, a tour, a festival, a show — in a billing that states no film's fact (no year, running time or
 *  director), names no relay and attaches no screening. A record a search found by those words alone, sharing none of
 *  the billing's other words, is the namesake of the event's genre, not what the venue screens: DE Casablanca Zwickau's
 *  "Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer" took "Number_i LIVE TOUR 2024", Kino im Druckluft's "Italo
 *  Cinema Festival 2026" "The Cure - Maia North Festival 2026". */
private[identity] object FilmNess {

  /** Words whole: a token that IS one. */
  private val Whole = Set("live", "tour", "show", "band", "gala", "slam", "quiz", "night", "nacht", "stage", "disco", "experience",
    "abend", "noc", "wieczor")
  /** Stems a compound carries ("Halloweenparty", "Wohnzimmerkonzerte", "Kinokonzert"). */
  private val Stems = Seq("konzert", "concert", "koncert", "festival", "festiwal", "party", "comedy", "musical", "tribute", "karaoke",
    "lesung", "poetry", "signierstunde", "kabarett", "kabaret", "cabaret", "soulnight", "tournee")

  private val Bracketed = """[(\[][^)\]]*[)\]]""".r

  // V3f: never a compound naming film ("Pornfilmfestival", "Filmkonzert"): a festival of films screens one
  def isEventWord(word: String): Boolean = !word.contains("film") && (Whole(word) || Stems.exists(word.contains))

  /** A film screened with the event — the event its companion, the film still the families' to name — or a festival's
   *  numbered edition billing one of its films ("11. UFF - Teatr weteranów" is a documentary). */
  private[identity] val FilmAttached = """\+|\bpokaz\b|\bseans|\bfilm(?!ow)|^\d+\.\s""".r

  /** "… joined with …": what follows is the companion of what the title bills before it — a performance named only
   *  there is a screening's ("DYRYGENT - POŁĄCZONY Z KONCERTEM MUZYKI NA ŻYWO", Wajda's film), a performance billed
   *  before it is still one ("Koncert jazzowy połączony z degustacją wina"). */
  private val JoinedWith = """(?iu)\s*\bpołączon\p{L}*\s+ze?\b""".r

  /** What `raw` bills as the event itself — what precedes "joined with" ([[JoinedWith]]), its companion following — or
   *  `None` when the billing attaches a screening to it ([[FilmAttached]]): the film is the evening then, the event its
   *  companion. */
  private[identity] def eventBilled(raw: String): Option[String] =
    Option.when(FilmAttached.findFirstIn(raw.toLowerCase(java.util.Locale.ROOT)).isEmpty)(JoinedWith.split(raw).head)

  /** The billing's event words — none when it states a fact of a film, names a relay or attaches a screening. */
  def eventWords(listing: MeasuredListing): Seq[String] = {
    val raw = listing.rawTitle.getOrElse(listing.title)
    // V3c: no fact condition — a feed's page copies the found record's year and running time onto a gig's listing
    // (kinoheld's "Tanzwut - Live Tour 2026" carries the Number_i film's 108 minutes), so a stated fact proves nothing here
    if (eventBilled(raw).isEmpty ||
        // a relay by its broadcast's name or its stage work — never by a year alone, which a gig's "Tour 2026" bills too
        services.titles.NonMovieEventClassifier.isNamedBroadcast(raw) ||
        StageRelays.billsStageWork(listing.ownTitles.distinct, raw)) Nil
    // V3e: an event word inside a bracketed decoration decorates a film billed before it ("ELF (Pyjama Party)")
    else IdentityMeasures.countedWords(Bracketed.replaceAllIn(raw, " ")).filter(isEventWord)
  }

  /** The billing's words that are no event's: what a record of the event itself must share. */
  def otherWords(listing: MeasuredListing): Set[String] =
    IdentityMeasures.countedWords(listing.rawTitle.getOrElse(listing.title)).filterNot(w => isEventWord(w) || w.forall(_.isDigit)).toSet
}
