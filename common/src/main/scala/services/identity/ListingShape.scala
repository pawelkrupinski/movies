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

  // ── a silent film ──────────────────────────────────────────────────────────────────────────

  /** Does the listing bill its film as a SILENT one ("Stummfilm mit Live-Musik", "film niemy", "cine mudo", "film
   *  muet")? Then it is a film of the silent era ([[SilentEraEnd]]), never a sound-era namesake: DE "STUMMFILM MIT TOBIAS
   *  RANK:NOSFERATU" is Murnau's, not Eggers' 2024 film the families' searches rank first. Read on the folded words. */
  def billsASilentFilm(listing: Listing): Boolean =
    Seq(listing.title, listing.rawTitle).distinct.exists(title => SilentFilm.findFirstIn(TitleContainment.tokens(title).mkString(" ")).isDefined)

  /** The last year a film billed as silent was made in: Chaplin's "Modern Times" (1936), the sound era's last. */
  val SilentEraEnd = 1936

  private val SilentFilm =
    ("""\bstummfilm\w*|\bsilent\s+(?:films?|movies?|cinema|classics?)\b|\bfilm\w*\s+niem\w*|\bniem\w*\s+film\w*|\bkin[oa]\s+niem\w*|""" +
      """\bcine\s+mudo\b|\bpeliculas?\s+mudas?\b|\bfilms?\s+muets?\b""").r

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
    relaysBilled(title, Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct, listing.rawTitle)

  /** [[relays]] for a venue's raw title alone, where no listing is in hand (the review app's fact check). */
  def relaysTitle(rawTitle: String): Boolean = relaysBilled(rawTitle.toLowerCase(java.util.Locale.ROOT), Seq(rawTitle), rawTitle)

  private def relaysBilled(lowerTitle: String, titles: Seq[String], rawTitle: String): Boolean =
    NonMovieEventClassifier.isScreenedBroadcast(lowerTitle) || relaysBesideLive(lowerTitle, titles, rawTitle)

  /** [[relays]] by what names a broadcast — a broadcast's own name, a concert film's place, a stage work, a season — and
   *  not by the bare word "live", which a performer billed in person carries too ("Jan Logemann LIVE: …"). */
  def relaysNamed(listing: Listing, title: String): Boolean =
    NonMovieEventClassifier.isNamedBroadcast(title) || relaysBesideLive(title, Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct, listing.rawTitle)

  private def relaysBesideLive(lowerTitle: String, titles: Seq[String], rawTitle: String): Boolean =
    ConcertFilm.findFirstIn(lowerTitle).isDefined || IdentityMeasures.billsStageWork(titles, rawTitle) ||
      IdentityMeasures.seasonYear(Seq(rawTitle)).isDefined

  // ── a bill of several works ────────────────────────────────────────────────────────────────

  private val Quoted = """[„"“]([^"”„]+)["”]""".r
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
  /** Does the venue's own title quote two works or more ("…: „Bestia”, reż. Hertz (1917), „Cud nad Wisłą”, …")? Read
   *  off the raw title: a country's title rules may cut the billing down to its first quote. */
  def quotesSeveral(rawTitle: String): Boolean = Quoted.findAllIn(rawTitle).size >= 2
  /** [[quotesSeveral]] where two of the quoted works are dated ("(1917)", "(1921)"): a programme of DATED works — a
   *  quote naming a programme beside one dated work ('„10 na 10”. Przegląd filmów Andrzeja Wajdy: „Pan Tadeusz”
   *  (1999)') bills that work. */
  def quotesSeveralDated(rawTitle: String): Boolean = quotesSeveral(rawTitle) && DatedWork.findAllIn(rawTitle).size >= 2
  private val DatedWork = """\((?:18|19|20)\d\d\)""".r
  /** Does the venue's own title join two quoted works by a conjunction — a DOUBLE BILL, neither film (user rule): PL
   *  "„Mieszkanie” i „Owoce rajskich drzew spożywamy”", 'MAF: "Obcy" oraz "Milcząca Przyjaciółka"'? Undated, unlike
   *  [[quotesSeveralDated]]: the conjunction says the two are billed as one programme. The two close the billing: words
   *  after them say what they are ('"Kumotry" i "All The Diamonds": wystawa i film w dialogu', an exhibition beside the
   *  film Kumotry). */
  def quotesTwoJoined(rawTitle: String): Boolean = JoinedQuotes.findFirstIn(rawTitle).isDefined
  private val JoinedQuotes = """(?iu)[„"“][^"”„]+["”]\s*(?:i|oraz|und|and|&|et|y|e)\s+[„"“][^"”„]+["”]\W*$""".r
  private def billsSeveralBySigns(listing: Listing): Boolean =
    SetOfWorks.findFirstIn(listing.rawTitle).isDefined || quotesSeveral(listing.rawTitle) ||
      IdentityMeasures.billsTwoWholeWorks(Evidence.of(listing, None).measured)

  // ── a piece billed beside the film ─────────────────────────────────────────────────────────

  /** The PIECE of the billing beside the film's title that bills another thing — a subtitle the film does not carry, a
   *  performer, a show, a strand — so the film's title names only ONE piece of a longer billing, not the billing: DE
   *  "Femme - Frauen heilen die Welt" is a documentary, not the 2023 "Femme"; "Giorgia Fumo - OUT OF OFFICE" a comedian's
   *  show, not the 2022 "Out of Office"; UK "FBF: Freud and Gale: Making It Up" a programme's last piece. `None` when the
   *  film's title is no run of the billing's whole pieces, or every piece beside it is the venue's own wording around a
   *  film:
   *  - a banner in the banner's slot, the leading piece a colon or a pipe closes ("Somali Film Festival: Lifeline"), or
   *    every piece of a segment ahead of the film's that names a screening programme or event (a strand's banner);
   *  - an event, format or edition word ([[DecorationSegments.EventWords]], "IT FOLLOWS - HALLOWEEN SPECIAL"), a
   *    screening programme or its audience ([[ScreeningProgramme]]: "Fatherland - Film Club", "KÖNIG DROSSELBART - EIN
   *    ADVENTSMÄRCHEN FÜR KINDER"), a note in brackets ("Geliefert (ver.di)") or one a number carries ("Digger (15) 129
   *    mins", "ABO 10": one word of its own beside it), the film's own titles and director;
   *  - the listing's own original title ("Hacer Mucho con Poco - Do More With Less");
   *  - the film quoted: the quotes name the work, what is around them is the evening's (PL "Festiwal Korelacje: „Zemsta”
   *    z autorską narracją Artura Andrusa").
   *  A piece holding a word of its own, three letters at least ("Athanor - Startblock", a drama school's showcase; "50 na
   *  51" holds none), is another thing's. Measured on the unmatched clusters' agreed takes (2026-10-13). */
  def billedBeside(listing: Listing, films: Seq[IdentityMeasures.Film]): Option[String] = {
    val raw = listing.rawTitle
    // a silent film's billing is its programme's: an accompanist, an anniversary ("Dni Kina Niemego - 100 lat temu: …")
    if (films.isEmpty || billsASilentFilm(listing) || Quote.findFirstIn(raw).exists(_ => quotesAFilm(raw, films))) None
    else {
      val parts  = PieceBreak.split(Bracketed.replaceAllIn(raw, " | ")).toIndexedSeq
      val breaks = PieceBreak.findAllIn(Bracketed.replaceAllIn(raw, " | ")).toIndexedSeq.map(_.trim)
      val words  = parts.map(TitleContainment.tokens)
      val titles = films.flatMap(_.titles).map(t => withoutArticles(TitleContainment.tokens(t))).filter(_.nonEmpty).distinct
      val theirs = films.flatMap(f => f.titles ++ f.directors.getOrElse(Nil)).flatMap(TitleContainment.tokens).toSet
      val original = listing.originalTitle.map(t => withoutArticles(TitleContainment.tokens(t))).filter(_.nonEmpty)
      def own(word: String) = word.length >= 3 && !word.exists(_.isDigit) && !theirs(word) && !billed(word)
      // the film's run of whole pieces, the first one
      val run = (for { from <- words.indices.iterator; to <- (from + 1 to words.size).iterator
                       if titles.contains(withoutArticles(words.slice(from, to).flatten)) } yield (from, to)).nextOption()
      // the banner segment each piece is in: a colon or a pipe closes one
      val segment = parts.indices.map(k => breaks.take(k).count(b => b == ":" || b == "|"))
      def programmeWords(piece: Seq[String]) = piece.exists(word => billed(word) || ScreeningProgramme.findFirstIn(word).isDefined)
      run.flatMap { case (from, to) =>
        // a strand's banner: a segment closed ahead of the film's that names a screening programme or an event in one of
        // its pieces is the venue's, all of it (DE "SPECIAL: 2. Dangerous Encounters – Hong Kong im Filmrausch: MADE IN
        // HONG KONG", a numbered strand of the Filmrausch's)
        def strand(k: Int) = segment(k) < segment(from) && words.indices.exists(j => segment(j) == segment(k) && programmeWords(words(j)))
        words.indices.iterator.filterNot(k => k >= from && k < to).filter(k => words(k).nonEmpty).filterNot(strand)
          // the banner's slot: the leading piece a colon or a pipe closes, the film after it
          .filterNot(k => k == 0 && from > 0 && breaks.headOption.exists(b => b == ":" || b == "|"))
          .map(words).find(piece => !original.contains(withoutArticles(piece)) && piece.exists(own) && !numberedNote(piece, own) &&
            !programmeWords(piece))
          .map(piece => s"'${piece.mkString(" ")}' is billed beside '${films.head.title}', no word of it the venue's around a film")
      }
    }
  }

  /** A NOTE a number carries — a running time, a certificate, a subscription's or an anniversary's number — one word of
   *  its own beside the number at most: "129 mins", "Cert 18", "ABO 10", "50 Jahre". */
  private def numberedNote(piece: Seq[String], own: String => Boolean): Boolean =
    piece.exists(_.exists(_.isDigit)) && piece.count(own) <= 1

  /** An event, format or edition word: what a venue bills around a film. */
  private def billed(word: String): Boolean =
    DecorationSegments.EventWords(word) || DecorationSegments.EditionWords(word) || services.movies.FormatTags.FormatToken.contains(word)
  /** A word naming a screening programme or its audience, as a stem a compound carries ("Filmmittwoch", "Kinderkino"). */
  private val ScreeningProgramme = """film|kin[oaie]|cine|movie|klub|club|kinder|kids|children|dzieci|familie|family|senior""".r
  private val PieceBreak = """\s*[:|•+]\s*|\s+[-–—]\s+|\s*[„”"“«»]\s*""".r
  private val Bracketed  = """\([^)]*\)|\[[^\]]*]""".r
  private val Quote      = """[„”"“«»]""".r
  private def quotesAFilm(raw: String, films: Seq[IdentityMeasures.Film]): Boolean = {
    val titles = films.flatMap(_.titles).map(t => withoutArticles(TitleContainment.tokens(t))).toSet
    Quoted.findAllMatchIn(raw).exists(m => titles(withoutArticles(TitleContainment.tokens(m.group(1)))))
  }
  private def withoutArticles(words: Seq[String]): Seq[String] =
    if (words.sizeIs >= 2 && IdentityMeasures.LeadingArticles(words.head)) words.tail else words

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
