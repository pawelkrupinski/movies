package services.identity

import services.cinemas.pl.NonMovieEventClassifier
import services.movies.TitleContainment

/**
 * The ONE place a listing's SHAPE is read off its billing — what kind of screening it bills, before any film is in
 * hand: a house's relay of a stage work, a bill of several works, an event no film database holds
 * ([[agreement.NonFilmEvents]], which reads the relay test below), and whose facts it carries (its venue's, or a
 * catalogue's). Every rule that asks one of these questions asks it here: the agreement's guards, the poster evidence,
 * the catalogue take, the broadcast join's withdrawal, the unified fill's guards.
 *
 * Read per call, never kept: a listing is asked a handful of times per apply, and a shape held per listing would cost
 * the worker's heap a field per listing for what costs microseconds to read.
 */
object ListingShape {

  // ── a stage relay ──────────────────────────────────────────────────────────────────────────

  /** Does the listing name a stage work ([[StageWorks]]) — an opera or ballet a house's relay bills? The films its
   *  families find are the work's screen namesakes ("ReTransmisje Met: Così fan tutte" → Tinto Brass's 1992 "Così fan
   *  tutte", replay 2026-10-04), never the relay, whose record TMDB alone keeps. */
  def stagesAWork(listing: Listing): Boolean =
    IdentityMeasures.billsStageWork(Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct, listing.rawTitle)

  /** Words a billing names a stage house, a relay or a stage show by. */
  private val HouseWords = Set("opera", "opery", "oper", "operze", "met", "metropolitan", "ballet", "balet", "baletu", "bolshoi", "bolszoj",
    "royal", "teatr", "teatru", "theatre", "theater", "nt", "live", "retransmisja", "retransmisje", "transmisja", "relay", "season", "sezon",
    "musical", "stage", "scena", "rbo", "roh", "hd", "glyndebourne", "scala", "staatsoper")

  /** Does the listing bill a house, a relay or a season ("OPERA-MAKBET - retransmisja", "Met Opera 2026/27: …")? */
  def billsAHouse(listing: Listing): Boolean = {
    val titles = Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct
    IdentityMeasures.seasonYear(titles).isDefined || titles.exists(title => TitleContainment.tokens(title).exists(HouseWords))
  }

  /** A concert FILM, recorded where a cinema relays it from: event cinema TMDB may hold ("Hauser symfonicznie z Royal
   *  Albert Hall"). */
  private val ConcertFilm = """royal\s+albert\s+hall|\blive\s+(in|at)\b""".r

  /** Does the listing RELAY a broadcast — a stage work, a house's season, a screened broadcast, a concert film
   *  ("Balet z Opery Paryskiej 2026-2027: Bajadera", "ReTransmisje Met: Na żywo w HD - Così fan tutte")? Cinema the
   *  broadcast take names, never an event however its vocabulary reads. `title`: its raw title in lower case. */
  def relays(listing: Listing, title: String): Boolean =
    NonMovieEventClassifier.isScreenedBroadcast(title) || ConcertFilm.findFirstIn(title).isDefined || stagesAWork(listing) ||
      IdentityMeasures.seasonYear(Seq(listing.rawTitle)).isDefined

  // ── a bill of several works ────────────────────────────────────────────────────────────────

  private val Quoted = """[„"“][^"”„]+["”]""".r
  /** A set ("zestaw") — of a series' episodes, often one compilation record the model may still take ([[MultiFilmBill]]
   *  leaves it out), but no family's agreement, poster or catalogue id stands for one of its pieces. */
  private val SetOfWorks = """(?i)\bzestaw\b""".r

  /** Does the listing bill several works — a "+" joining two whole works ([[IdentityMeasures.billsTwoWholeWorks]]: an
   *  event joined to the film, "11. UFF - Gala otwarcia + Demony", "… pokaz filmu + dyskusja", is none), a word billing
   *  a programme of films ([[MultiFilmBill]]: a double bill, a trilogy, a marathon, a block of shorts), a set, or two
   *  quoted titles? Read where no film is in hand: a film whose own title carries the word is let through only by
   *  [[billsSeveralBeside]]. */
  def billsSeveral(listing: Listing): Boolean =
    MultiFilmBill.marker(titlesOf(listing)).isDefined || billsSeveralBySigns(listing)

  /** [[billsSeveral]] for `film`: a programme word its own title carries ("Marathon Man") bills no other. */
  def billsSeveralBeside(listing: Listing, film: IdentityMeasures.Film): Boolean =
    MultiFilmBill.billsBeside(titlesOf(listing), film) || billsSeveralBySigns(listing)

  private def titlesOf(listing: Listing): Seq[String] = Seq(listing.rawTitle, listing.title).distinct
  private def billsSeveralBySigns(listing: Listing): Boolean =
    SetOfWorks.findFirstIn(listing.rawTitle).isDefined || Quoted.findAllIn(listing.rawTitle).size >= 2 ||
      IdentityMeasures.billsTwoWholeWorks(Evidence.of(listing, None).measured)

  // ── a programme slot ───────────────────────────────────────────────────────────────────────

  /** A programme SLOT a festival or season bills by its place in the programme rather than by a film's title. */
  private val ProgrammeSlot = ("""(?iu)(?:opening|closing)\s+(?:night|gala|film)|gala|shorts?\s+programme|shorts?\s+program|""" +
    """secret\s+screening|surprise\s+(?:film|screening)|mystery\s+(?:film|screening)|""" +
    """gala\s+(?:otwarcia|zamknięcia)|(?:film|pokaz)\s+(?:otwarcia|zamknięcia|niespodzianka)|seans\s+niespodzianka|""" +
    """eröffnungs(?:film|gala|abend)|abschluss(?:film|gala|abend)|überraschungs(?:film|vorstellung)|""" +
    """sesión\s+(?:inaugural|de\s+clausura)|gala\s+(?:inaugural|de\s+inauguración|de\s+clausura)|película\s+sorpresa""").r
  /** A banner naming a festival, a season, a series or an edition — what a slot is billed under. */
  private val ProgrammeBanner =
    """(?iu)(?<!\p{L})(?:festival|festivals|fest|festiwal\p{L}*|filmfest\p{L}*|festspiele|season|sezon\p{L}*|series|ciclo|muestra|semana|week|tydzień|edition|edycja|edycji)(?!\p{L})""".r

  /** The PROGRAMME SLOT the listing bills in place of a film — a festival's or season's banner and a slot of its programme,
   *  and nothing else: UK "Unrestricted View Horror Film Festival 2026: Opening Night" is the festival's opening night,
   *  not the 2016 "Opening Night" its search ranks first. A slot billed with a film's own title beside it ("Ars Independent
   *  Festival 2026: Gala otwarcia + „Czarna godzina”") is that film's screening; a bare slot ("Opening Night", Cassavetes'
   *  film) bills no banner. `None` unless every other piece of the title is such a banner. */
  def programmeSlotOf(rawTitle: String): Option[String] =
    // no banner, no slot: read for every node of every resolve, so the title is split only when one is billed
    if (!ProgrammeBanner.pattern.matcher(rawTitle).find()) None
    else {
      val pieces = IdentityMeasures.pieces(rawTitle)
      pieces.find(piece => ProgrammeSlot.pattern.matcher(piece).matches()).filter(slot =>
        pieces.sizeIs >= 2 && pieces.forall(piece => (piece eq slot) || ProgrammeBanner.pattern.matcher(piece).find()))
    }

  // ── whose facts ────────────────────────────────────────────────────────────────────────────

  /** Are the listing's facts all its VENUE's own — no feed catalogue's ([[CatalogueSources.feedStated]]), and no
   *  listings site's catalogue entry linked as its page ([[CatalogueSources.catalogueEntry]]: Flicks' film page, which
   *  links a new relay to an old production's page), whether or not the entry's facts were read onto it? Broader than
   *  `!factsFromCatalogue`, which marks only facts read off such a page. */
  def venueStated(listing: Listing): Boolean =
    !listing.factsFromCatalogue && !listing.page.exists(CatalogueSources.catalogueEntry)

}
