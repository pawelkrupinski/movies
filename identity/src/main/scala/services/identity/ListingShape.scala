package services.identity

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.IdentityMeasures.withoutLeadingArticle
import services.movies.{TitleContainment, TitleSeason, TitleWords}

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
    StageRelays.billsStageWork(spellingsOf(listing), listing.rawTitle)

  // ── a silent film ──────────────────────────────────────────────────────────────────────────

  /** Does the listing bill its film as a SILENT one ("Stummfilm mit Live-Musik", "film niemy", "cine mudo", "film
   *  muet")? Then it is a film of the silent era ([[SilentEraEnd]]), never a sound-era namesake: DE "STUMMFILM MIT TOBIAS
   *  RANK:NOSFERATU" is Murnau's, not Eggers' 2024 film the families' searches rank first. Read on the folded words. */
  def billsASilentFilm(listing: Listing): Boolean =
    Seq(listing.title, listing.rawTitle).distinct.exists(title => SilentFilm.findFirstIn(TitleContainment.tokens(title).mkString(" ")).isDefined)

  /** The last year a film billed as silent was made in: Chaplin's "Modern Times" (1936), the sound era's last. */
  val SilentEraEnd = 1936

  /** Does a billing as a silent film ([[billsASilentFilm]]: `billedSilent`) rule out a film of `year`, one of the sound
   *  era? The model reads it for a node, any listing of it billing silent; the agreement stage per listing. */
  def silentBillingDenies(year: Option[Int], billedSilent: Boolean): Boolean = billedSilent && year.exists(_ > SilentEraEnd)

  private val SilentFilm =
    ("""\bstummfilm\w*|\bsilent\s+(?:films?|movies?|cinema|classics?)\b|\bfilm\w*\s+niem\w*|\bniem\w*\s+film\w*|\bkin[oa]\s+niem\w*|""" +
      """\bcine\s+mudo\b|\bpeliculas?\s+mudas?\b|\bfilms?\s+muets?\b""").r

  /** Words a billing names a stage house, a relay or a stage show by. */
  private val HouseWords = {
    import TitleWords.Concept.*
    TitleWords.tokens(StageHouse, Stage, Relay, HouseBanner)
  }

  /** Words a billing names a stage show by — a theatre, a musical, a stage ([[TitleWords.Concept.Stage]]). */
  val StageWords: Set[String] = TitleWords.tokens(TitleWords.Concept.Stage)

  /** Does the listing bill a house, a relay or a season ("OPERA-MAKBET - retransmisja", "Met Opera 2026/27: …"), or a
   *  house's initials as [[StageRelays.billsAStageHouse]] reads them ("Tosca (ROH)", never "Jin-Roh")? */
  def billsAHouse(listing: Listing): Boolean = {
    val titles = spellingsOf(listing)
    TitleSeason.of(titles, listing.screenings.season).isDefined || titles.exists(title => TitleContainment.tokens(title).exists(HouseWords)) ||
      StageRelays.billsAStageHouse(titles)
  }

  /** A concert FILM, recorded where a cinema relays it from: event cinema TMDB may hold ("Hauser symfonicznie z Royal
   *  Albert Hall"). */
  private val ConcertFilm = """royal\s+albert\s+hall|\blive\s+(in|at)\b""".r

  /** Does the listing RELAY a broadcast — a stage work, a house's season, a screened broadcast, a concert film
   *  ("Balet z Opery Paryskiej 2026-2027: Bajadera", "ReTransmisje Met: Na żywo w HD - Così fan tutte")? Cinema the
   *  broadcast take names, never an event however its vocabulary reads. `title`: its raw title in lower case. */
  def relays(listing: Listing, title: String): Boolean =
    relaysBilled(title, spellingsOf(listing), listing.rawTitle, listing.screenings.season)

  /** [[relays]] for a venue's raw title alone, where no listing is in hand (the review app's fact check). */
  def relaysTitle(rawTitle: String): Boolean = relaysBilled(rawTitle.toLowerCase(java.util.Locale.ROOT), Seq(rawTitle), rawTitle, None)

  private def relaysBilled(lowerTitle: String, titles: Seq[String], rawTitle: String, screenedIn: Option[Int]): Boolean =
    NonMovieEventClassifier.isScreenedBroadcast(lowerTitle) || relaysBesideLive(lowerTitle, titles, rawTitle, screenedIn)

  /** [[relays]] by what names a broadcast — a broadcast's own name, a concert film's place, a stage work, a season — and
   *  not by the bare word "live", which a performer billed in person carries too ("Jan Logemann LIVE: …"). */
  def relaysNamed(listing: Listing, title: String): Boolean =
    NonMovieEventClassifier.isNamedBroadcast(title) || relaysBesideLive(title, spellingsOf(listing), listing.rawTitle, listing.screenings.season)

  /** Every spelling the listing carries of its title: as shown, cleaned, and as the venue published it. */
  private def spellingsOf(listing: Listing): Seq[String] = Seq(listing.title, listing.cleanTitle, listing.rawTitle).distinct

  private def relaysBesideLive(lowerTitle: String, titles: Seq[String], rawTitle: String, screenedIn: Option[Int]): Boolean =
    ConcertFilm.findFirstIn(lowerTitle).isDefined || StageRelays.billsStageWork(titles, rawTitle) ||
      TitleSeason.of(Seq(rawTitle), screenedIn).isDefined

  // ── a bill of several works ────────────────────────────────────────────────────────────────

  private val Quoted = """[„"“]([^"”„]+)["”]""".r
  /** A set ("zestaw") — of a series' episodes, often one compilation record the model may still take ([[MultiFilmBill]]
   *  leaves it out), but no family's agreement, poster or catalogue id stands for one of its pieces. */
  private val SetOfWorks = """(?i)\bzestaw\b""".r

  /** Does the listing bill several works — a "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]: an
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
      MultiFilmBill.billsTwoWholeWorks(Evidence.of(listing, None).measured)

  // ── a piece billed beside the film ─────────────────────────────────────────────────────────

  /** The PIECE of the billing beside the film's title that bills another thing — a subtitle the film does not carry, a
   *  performer, a show, a strand — so the film's title names only ONE piece of a longer billing, not the billing: DE
   *  "Femme - Frauen heilen die Welt" is a documentary, not the 2023 "Femme"; "Giorgia Fumo - OUT OF OFFICE" a comedian's
   *  show, not the 2022 "Out of Office"; UK "FBF: Freud and Gale: Making It Up" a programme's last piece. `None` when the
   *  film's title is no run of the billing's whole pieces, or every piece beside it is the venue's own wording around a
   *  film:
   *  - a banner in the banner's slot, the leading piece a colon or a pipe closes ("Somali Film Festival: Lifeline"), or
   *    every piece of a segment ahead of the film's that names a screening programme or event (a strand's banner);
   *  - an event, format or edition word ([[DecorationWords.EventWords]], "IT FOLLOWS - HALLOWEEN SPECIAL"), a
   *    screening programme or its audience ([[ScreeningProgramme]]: "Fatherland - Film Club", "KÖNIG DROSSELBART - EIN
   *    ADVENTSMÄRCHEN FÜR KINDER"), a note in brackets ("Geliefert (ver.di)") or one a number carries ("Digger (15) 129
   *    mins", "ABO 10": one word of its own beside it), a screening's note ([[ScreeningNote]]: "+ Panel Discussion"), the
   *    film's own titles and director (a word of them misspelt by a letter: "Living - Einma wirklich leben");
   *  - what a "+" adds after the film ("OPĘTANIE + TRASA PLAKATOWA"), tags piped after it one of which names a programme
   *    ("Królestwo Kensuke | EKO-KINO | ZIELONE INSTYTUCJE KULTURY"), anything around a silent-era film a programme bills;
   *  - the listing's own original title ("Hacer Mucho con Poco - Do More With Less");
   *  - the film quoted: the quotes name the work, what is around them is the evening's (PL "Festiwal Korelacje: „Zemsta”
   *    z autorską narracją Artura Andrusa").
   *  A piece holding a word of its own, three letters at least ("Athanor - Startblock", a drama school's showcase; "50 na
   *  51" holds none), is another thing's. Measured on the unmatched clusters' agreed takes (2026-10-13).
   *  `venues`: what else the caller knows a piece's words to be the venue's (the resolver: a banner the corpus bills
   *  beside other titles). */
  def billedBeside(listing: Listing, films: Seq[IdentityMeasures.Film], venues: Seq[String] => Boolean = _ => false): Option[String] = {
    val raw = listing.rawTitle
    // a silent film's billing is its programme's: an accompanist, an anniversary ("Dni Kina Niemego - 100 lat temu: …")
    if (films.isEmpty || billsASilentFilm(listing) || Quote.findFirstIn(raw).exists(_ => quotesAFilm(raw, films))) None
    else {
      val parts  = PieceBreak.split(Bracketed.replaceAllIn(raw, " | ")).toIndexedSeq
      val breaks = PieceBreak.findAllIn(Bracketed.replaceAllIn(raw, " | ")).toIndexedSeq.map(_.trim)
      val words  = parts.map(TitleContainment.tokens)
      val titles = films.flatMap(_.titles).map(t => withoutLeadingArticle(TitleContainment.tokens(t))).filter(_.nonEmpty).distinct
      // each title as its letters run together: a piece spelling it without its stops ("AI" of Spielberg's "A.I.") is it
      val keys   = films.flatMap(_.titles).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
      val theirs = films.flatMap(f => f.titles ++ f.directors.getOrElse(Nil)).flatMap(TitleContainment.tokens).toSet
      val original = listing.originalTitle.map(t => withoutLeadingArticle(TitleContainment.tokens(t))).filter(_.nonEmpty)
      // a piece crediting the film's director, as an inflected language bills the name ("Poniedziałki z Konwickim: pisarz –
      // scenarzysta – reżyser"): the whole billing is the venue's programme of the director's films
      val names  = films.flatMap(_.directors.getOrElse(Nil)).flatMap(TitleContainment.tokens).filter(_.length >= DirectorStem)
      def own(word: String) = word.length >= 3 && !word.exists(_.isDigit) && !theirs.exists(spells(word, _)) && !billed(word)
      def credits(word: String) = names.exists(word.startsWith)
      // the film's run of whole pieces, the first one
      val run = (for { from <- words.indices.iterator; to <- (from + 1 to words.size).iterator
                       if titles.contains(withoutLeadingArticle(words.slice(from, to).flatten)) || keys.contains(words.slice(from, to).flatten.mkString) }
                   yield (from, to)).nextOption()
      // the banner segment each piece is in: a colon or a pipe closes one
      val segment = parts.indices.map(k => breaks.take(k).count(b => b == ":" || b == "|"))
      def programme(piece: Seq[String]) = piece.exists(word => ScreeningProgramme.findFirstIn(word).isDefined)
      def programmeWords(piece: Seq[String]) = programme(piece) || piece.exists(billed) || DecorationWords.namesAnEvent(piece)
      // a film of the silent era under a screening programme: the billing is the programme's, its accompanist's (DE
      // "Film+Musikfest: Greta Garbo: The Temptress", 1926), as one billing it silent is
      // the film's own tagline, its subtitle ("Frauen bildet Banden - Eine Spurensuche zur Geschichte der Roten Zora")
      def tagline(piece: Seq[String]) = piece.nonEmpty && films.exists(_.taglineKey == IdentityMeasures.Film.taglineKeyOf(piece.mkString(" ")))
      def silent = films.forall(_.year.exists(_ <= SilentEraEnd)) && words.exists(programme)
      // one of the film's own titles of three pieces or more, piece for piece, one piece re-worded: the venue's spelling of
      // that piece ("Exhibition on Screen: Vermeer - Die Blockbuster-Ausstellung" of its "Exhibition on Screen: Vermeer -
      // The Greatest Exhibition"); two pieces alike are no coincidence a namesake's two-piece title can be
      def reworded = films.flatMap(_.titles).exists { title =>
        val theirs = PieceBreak.split(title).toIndexedSeq.map(piece => withoutLeadingArticle(TitleContainment.tokens(piece)))
        theirs.sizeIs >= 3 && theirs.size == words.size && theirs.indices.count(k => theirs(k) != withoutLeadingArticle(words(k))) == 1
      }
      // the billing opens with one of the film's own titles of two pieces or more, word for word, its last word maybe glued
      // to what follows ("LA BOUM - Die FeteZum 60. Geb. von Sophie Marceau!" of "La Boum - Die Fete"): the rest is a note
      def opens = films.flatMap(_.titles).exists { title =>
        val theirs = TitleContainment.tokens(title)
        val ours   = words.flatten
        PieceBreak.split(title).count(_.trim.nonEmpty) >= 2 && theirs.nonEmpty && ours.sizeIs >= theirs.size &&
          ours.take(theirs.size - 1) == theirs.init && ours(theirs.size - 1).startsWith(theirs.last)
      }
      // a film titled only by a screening note's words ("Q & A", "Talk"): the billing's note, not a film, beside the
      // billing's other pieces (UK The Garden Cinema's "Köln Tracks: The Legend Of Keith Jarrett’s Köln Concert + Q&A")
      val noted = run.filter { case (from, to) =>
        films.forall(f => { val title = TitleContainment.tokens(f.title); title.nonEmpty && (title.forall(NoteWords) || NotePhrase.matches(title.mkString(" "))) }) &&
          words.indices.exists(k => (k < from || k >= to) && words(k).nonEmpty)
      }.map(_ => s"'${films.head.title}' is a screening note's words, billed beside the billing's own pieces")
      noted orElse run.filterNot { case (from, to) => silent || reworded || opens || words.indices.exists(k => (k < from || k >= to) && words(k).exists(credits)) }.flatMap { case (from, to) =>
        // a strand's banner: a segment closed ahead of the film's that names a screening programme or an event in one of
        // its pieces is the venue's, all of it (DE "SPECIAL: 2. Dangerous Encounters – Hong Kong im Filmrausch: MADE IN
        // HONG KONG", a numbered strand of the Filmrausch's)
        def strand(k: Int) = segment(k) < segment(from) && words.indices.exists(j => segment(j) == segment(k) && programmeWords(words(j)))
        // the pieces piped after the film, one after another: the venue's tags, when one of them names a programme (PL
        // "Królestwo Kensuke | EKO-KINO | ZIELONE INSTYTUCJE KULTURY")
        val piped = (to until words.size).takeWhile(k => breaks.lift(k - 1).contains("|"))
        val tagged = piped.exists(k => programme(words(k)))
        words.indices.iterator.filterNot(k => k >= from && k < to).filter(k => words(k).nonEmpty).filterNot(strand)
          // the banner's slot: the leading piece a colon or a pipe closes, the film after it
          .filterNot(k => k == 0 && from > 0 && breaks.headOption.exists(b => b == ":" || b == "|"))
          // what the screening adds after the film, a "+" closing it (PL "OPĘTANIE + TRASA PLAKATOWA", a poster tour)
          .filterNot(k => k >= to && breaks.lift(to - 1).contains("+"))
          .filterNot(k => tagged && piped.contains(k))
          .map(words).find(piece => !original.contains(withoutLeadingArticle(piece)) && piece.exists(own) && !numberedNote(piece, own) &&
            !programmeWords(piece) && !piece.exists(ScreeningNote.matches) && !venues(piece) && !tagline(piece))
          .map(piece => s"'${piece.mkString(" ")}' is billed beside '${films.head.title}', no word of it the venue's around a film")
      }
    }
  }

  /** The fewest letters of a director's name an inflected form of it starts with. */
  private val DirectorStem = 5

  /** Is `word` the film's own `theirs`, or it misspelt by one letter dropped, added or changed — four letters and more
   *  ("Living - Einma wirklich leben" of "Living - Einmal wirklich leben")? */
  private def spells(word: String, theirs: String): Boolean =
    word == theirs || (word.length >= 4 && theirs.length >= 4 && math.abs(word.length - theirs.length) <= 1 && {
      val (short, long) = if (word.length <= theirs.length) (word, theirs) else (theirs, word)
      if (short.length == long.length) short.indices.count(i => short(i) != long(i)) <= 1
      else { val at = short.indices.find(i => short(i) != long(i)).getOrElse(short.length); short.substring(at) == long.substring(at + 1) }
    })

  /** A NOTE a number carries — a running time, a certificate, a subscription's or an anniversary's number — one word of
   *  its own beside the number at most: "129 mins", "Cert 18", "ABO 10", "50 Jahre". */
  private def numberedNote(piece: Seq[String], own: String => Boolean): Boolean =
    piece.exists(_.exists(_.isDigit)) && piece.count(own) <= 1

  /** The words of a screening's note — a talk, a Q&A, the screening as an event ([[TitleWords]]): a title of these alone
   *  names the note. */
  private val NoteWords = TitleWords.tokens(TitleWords.Concept.Talk, TitleWords.Concept.Screening)
  /** One whole phrase of a screening's note, folded — "Q & A", whose letters [[NoteWords]] holds no word of. */
  private val NotePhrase = TitleWords.foldedPattern(TitleWords.Concept.Talk, TitleWords.Concept.Screening).r
  /** An event, format or edition word: what a venue bills around a film. */
  private def billed(word: String): Boolean =
    DecorationWords.EventWords(word) || DecorationWords.EditionWords(word) || services.movies.FormatTags.FormatToken.contains(word)
  /** A word naming a screening programme or its audience, as a stem a compound carries ("Filmmittwoch", "Kinderkino"), or
   *  a festival's ("SPECIAL: Jazzfest Community Week: SOUNDTRACK TO A COUP D’ETAT"). */
  private val ScreeningProgramme = """film|kin[oaie]|cine|movie|klub|club|dkf|kinder|kids|children|dzieci|familie|family|senior|fest$""".r
  /** A word of what a screening holds beside its film — a talk, a panel, a matinee, its audience's slot, its running time:
   *  UK "Judas and the Black Messiah + Panel Discussion", "The Incomer (12A) PARENT & BABY", "Wicker (15) 105 mins *MATINEE*", PL "Klątwa doliny węży – z autorską narracją
   *  Łony", "Zaproszenie | Zobacz przedpremierowo", DE "Live - Vortrag: Portugal - Der Wanderfilm", "BANKOMATT – Schweizer
   *  Filmwoche – mit Regisseur!". */
  private val ScreeningNote = """discussion|introduction|panel|matinee|parent|baby|relaxed|mins|minutes|narracj\w*|narration|przedpremier\w*|premierow\w*|deluxe|vortrag\w*|regisseur\w*""".r
  /** Is the billing two pieces a dash joins, and nothing else ("LÍDA BAAROVÁ – DOOMED BEAUTY")? */
  def dashedPair(raw: String): Boolean = {
    val breaks = PieceBreak.findAllIn(raw).toSeq.map(_.trim)
    breaks.sizeIs == 1 && Dash.matches(breaks.head) && PieceBreak.split(raw).count(_.trim.nonEmpty) == 2
  }
  /** Is the billing two pieces a dash joins ([[dashedPair]]), the second a title of `film`'s, whole ("LÍDA BAAROVÁ –
   *  DOOMED BEAUTY": its subject, then the film)? */
  def titledAfterPiece(raw: String, film: IdentityMeasures.Film): Boolean = dashedPair(raw) && {
    val titles = film.titles.map(t => withoutLeadingArticle(TitleContainment.tokens(t))).toSet
    PieceBreak.split(raw).lastOption.exists(piece => titles.contains(withoutLeadingArticle(TitleContainment.tokens(piece))))
  }
  private val Dash = "[-–—]".r
  /** Does a piece of the billing list three works or more by name, a comma between each, every one capitalised ("Retro -
   *  An Undiscovered World, Running High, Carrying Dreams: The Weight Between", a programme of shorts)? */
  def listsWorks(raw: String): Boolean = PieceBreak.split(raw).exists { piece =>
    val works = piece.split(",").map(_.trim).toSeq
    works.sizeIs >= ListedWorks && works.forall(work => work.headOption.exists(_.isUpper) && TitleContainment.tokens(work).sizeIs >= 2)
  }
  private val ListedWorks = 3
  private val PieceBreak = """\s*[:|•+]\s*|\s+[-–—]\s+|\s*[„”"“«»]\s*""".r
  private val Bracketed  = """\([^)]*\)|\[[^\]]*]""".r
  /** Any quotation mark a title may carry. */
  private[identity] val Quote = """[„”"“«»]""".r
  private def quotesAFilm(raw: String, films: Seq[IdentityMeasures.Film]): Boolean = {
    val titles = films.flatMap(_.titles).map(t => withoutLeadingArticle(TitleContainment.tokens(t))).toSet
    Quoted.findAllMatchIn(raw).exists(m => titles(withoutLeadingArticle(TitleContainment.tokens(m.group(1)))))
  }

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

  private val FilmFestival    = services.movies.TitleWords.foldedPattern(services.movies.TitleWords.Concept.FilmFestival).r
  private val FestivalEdition = services.movies.TitleWords.foldedPattern(services.movies.TitleWords.Concept.FestivalEdition).r

  /** Does the title bill a FILM FESTIVAL by its own name and nothing else — a piece naming a film festival
   *  ([[services.movies.TitleWords.Concept.FilmFestival]]), its edition's year or number and its touring edition's name
   *  ([[services.movies.TitleWords.Concept.FestivalEdition]]: "Cinema Edition") aside? That is the festival's programme,
   *  never one film: DE "Alpen Film Festival 2026" is no 13-minute short a feed links it to (whose TMDB record carries the
   *  billing as an alternative title), "Freeride Filmfestival 2026 - Cinema Edition" no documentary whose record does so
   *  too. A work billed under the festival's banner ("Alpen Film Festival 2026: Mont Blanc") or beside it ("Fargo -
   *  Purbeck Film Festival") is the work, and a festival that is no film festival may be a film's own name ("Festival
   *  Express"). */
  def billsAFilmFestival(rawTitle: String): Boolean = {
    def folded(text: String) = services.movies.TitleContainment.tokens(text).mkString(" ")
    // what an edition adds: its year or number, its touring edition's name
    def editionOnly(words: String) = FestivalEdition.replaceAllIn(words, " ").trim.split(' ').forall(_.forall(_.isDigit))
    IdentityMeasures.pieces(rawTitle).filter(_.exists(_.isLetter)) match {
      case Seq(festival, rest*) if rest.forall(piece => editionOnly(folded(piece))) =>
        val words = folded(festival)
        val found = FilmFestival.pattern.matcher(words)
        found.find() && {
          val (before, after) = (words.substring(0, found.start), words.substring(found.end))
          // the festival's name before its phrase ("Alpen Film Festival 2026"), or the phrase first and the name after it
          // ("Festiwal Filmowy Kino Lato", "Festival de Cine de Sitges"); a sentence about a festival ("How I Started A Film
          // Festival", a short) bills an article, which no festival's name does
          (editionOnly(after) || editionOnly(before)) && !before.trim.split(' ').exists(IdentityMeasures.LeadingArticles)
        }
      case _ => false
    }
  }

  // ── whose facts ────────────────────────────────────────────────────────────────────────────

  /** Are the listing's facts all its VENUE's own — no feed catalogue's ([[SourceReliability.feedStated]]), and no
   *  listings site's catalogue entry linked as its page ([[SourceReliability.listingsSiteEntry]]: Flicks' film page, which
   *  links a new relay to an old production's page), whether or not the entry's facts were read onto it? Broader than
   *  `!factsFromCatalogue`, which marks only facts read off such a page. */
  def venueStated(listing: Listing): Boolean =
    SourceReliability.venueOnly(listing)

}
