package services.identity.measures

import services.identity.measures.IdentityMeasures.withoutLeadingArticle
import services.titles.NonMovieEventClassifier
import services.titles.billing.{BillingParse, BillingPieces, Credits}
import services.titles.{TitleContainment, TitleSeason, TitleWords}

/**
 * The ONE place a listing's SHAPE is read off its billing — what kind of screening it bills, before any film is in
 * hand: a house's relay of a stage work, a bill of several works, an event no film database holds
 * ([[NonFilmEvents]], which reads the relay test below), and whose facts it carries (its venue's, or a
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

  // ── an author's credit ─────────────────────────────────────────────────────────────────────

  /** A billing's WORK and the author it credits it to ([[Credits.AuthorCredit]]). */
  type AuthorCredit = Credits.AuthorCredit
  val AuthorCredit = Credits.AuthorCredit


  /** Does a billing crediting its work to an author ([[BillingParse.author]]) rule `film` out: the film its work's title names
   *  word for word, made two seasons or more before the one the billing screens in (`screenedIn`), which the author did
   *  not direct, and whose own titles do not carry the credit ("Mary Shelley's Frankenstein")? The billing stages the
   *  author's work — a play's capture or relay, its current record — not an older namesake film (DE Cineplex "Fallen
   *  Angels by Noël Coward", the 2026 capture of Coward's play, took Wong Kar-wai's 1995 "Fallen Angels"). A billing
   *  crediting a host or a score ("Blow-Up + Intro by …", "Nosferatu with Live Score by …") names no work its film is. */
  def creditDenies(credit: AuthorCredit, film: Film, screenedIn: Option[Int]): Boolean =
    film.year.exists(year => screenedIn.exists(_ - year >= CreditedNamesakeSeasons)) && {
      val work     = IdentityMeasures.key(credit.work)
      val credited = IdentityMeasures.key(credit.person)
      val titles   = film.titles.map(IdentityMeasures.key)
      titles.contains(work) && !titles.exists(_.contains(credited)) &&
        film.directors.exists(directors => directors.nonEmpty && FactRelations.otherPeople(Seq(credit.person), directors))
    }

  /** How many seasons before its screening a film credited to another author is an older namesake. */
  private val CreditedNamesakeSeasons = 2

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

  /** Words a billing names a stage show by — a theatre, a musical, a stage ([[TitleWords.Concept.Stage]]). */
  val StageWords: Set[String] = TitleWords.tokens(TitleWords.Concept.Stage, TitleWords.Concept.StageShow)
  /** The words naming a staged show itself ("CBeebies Panto 2026: Treasure Island"): a staging wherever it is billed, the
   *  banner's slot too — unlike a stage word a strand names itself by ("Musical Monday: Grease"). */
  val ShowWords: Set[String] = TitleWords.tokens(TitleWords.Concept.StageShow)
  /** Is `film` the staged show's OWN record — titled as a staged show ([[ShowWords]]) where the billing names one, a word
   *  `venueWord` says is not the venue's own name: the show's, an event's, whatever facts the two share, no film the
   *  venue screens (UK "CBeebies Panto: Treasure Island" beside TMDB's "Cbeebies Panto: Treasure Island", 1785679; the
   *  user's review 10-21). Read by the acceptance rules and [[BilledPiece]] alike. */
  def showsOwnRecord(billed: String, film: Film, venueWord: String => Boolean = _ => false): Boolean =
    film.titles.exists(title => TitleContainment.tokens(title).exists(ShowWords)) &&
      TitleContainment.tokens(billed).exists(word => ShowWords(word) && !venueWord(word))

  /** Does the listing bill a house, a relay or a season ("OPERA-MAKBET - retransmisja", "Met Opera 2026/27: …"), or a
   *  house's initials as [[StageRelays.billsAStageHouse]] reads them ("Tosca (ROH)", never "Jin-Roh")? */
  def billsAHouse(listing: Listing): Boolean = {
    val titles = spellingsOf(listing)
    TitleSeason.of(titles, listing.screenings.season).isDefined || StageRelays.namesAHouseOrRelay(titles)
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

  /** Does the listing bill several works — a "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]: an
   *  event joined to the film, "11. UFF - Gala otwarcia + Demony", "… pokaz filmu + dyskusja", is none), a word billing
   *  a programme of films ([[MultiFilmBill]]: a double bill, a trilogy, a marathon, a block of shorts), a set, or two
   *  quoted titles? Read where no film is in hand: a film whose own title carries the word is let through only by
   *  [[billsSeveralBeside]]. */
  def billsSeveral(listing: Listing): Boolean =
    MultiFilmBill.marker(titlesOf(listing)).isDefined || billsSeveralBySigns(listing)

  /** [[billsSeveral]] for `film`: a programme word its own title carries ("Marathon Man") bills no other. */
  def billsSeveralBeside(listing: Listing, film: Film): Boolean =
    MultiFilmBill.billsBeside(titlesOf(listing), film) || billsSeveralBySigns(listing)

  private def titlesOf(listing: Listing): Seq[String] = Seq(listing.rawTitle, listing.title).distinct
  private def billsSeveralBySigns(listing: Listing): Boolean =
    BillingPieces.billsASet(listing.rawTitle) || BillingPieces.quotesSeveral(listing.rawTitle) ||
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
   *  - an event, format or edition word ([[services.titles.billing.DecorationWords.EventWords]], "IT FOLLOWS - HALLOWEEN SPECIAL"), a
   *    screening programme or its audience ([[BilledBeside]]'s ScreeningProgramme: "Fatherland - Film Club", "KÖNIG DROSSELBART - EIN
   *    ADVENTSMÄRCHEN FÜR KINDER"), a note in brackets ("Geliefert (ver.di)") or one a number carries ("Digger (15) 129
   *    mins", "ABO 10": one word of its own beside it), a screening's note ([[BilledBeside]]'s ScreeningNote: "+ Panel Discussion"), the
   *    film's own titles and director (a word of them misspelt by a letter: "Living - Einma wirklich leben");
   *  - what a "+" adds after the film ("OPĘTANIE + TRASA PLAKATOWA"), tags piped after it one of which names a programme
   *    ("Królestwo Kensuke | EKO-KINO | ZIELONE INSTYTUCJE KULTURY"), anything around a silent-era film a programme bills;
   *  - the listing's own original title ("Hacer Mucho con Poco - Do More With Less"); an animation studio's name
   *    ([[BilledBeside]]'s StudioTags: "Kikis kleiner Lieferservice- Studio Ghibli");
   *  - the film quoted: the quotes name the work, what is around them is the evening's (PL "Festiwal Korelacje: „Zemsta”
   *    z autorską narracją Artura Andrusa").
   *  A piece holding a word of its own, three letters at least ("Athanor - Startblock", a drama school's showcase; "50 na
   *  51" holds none), is another thing's. Measured on the unmatched clusters' agreed takes (2026-10-13).
   *  `venues`: what else the caller knows a piece's words to be the venue's (the resolver: a banner the corpus bills
   *  beside other titles). `bannerSlot`: whether the banner's slot is the venue's whatever it says — not where it names a
   *  staged show ([[ShowWords]], [[BilledPiece]]): a pantomime heading its work is a staging of it, not a venue's banner
   *  (UK "CBeebies Panto 2026: Treasure Island"). */
  def billedBeside(listing: Listing, films: Seq[Film], venues: Seq[String] => Boolean = _ => false,
                   bannerSlot: Boolean = true): Option[String] = {
    // a box office's certificate note is the showing's, never a piece of what it bills (UK "Family Cinema: Paddington - Cert PG")
    val raw = BillingParse(listing.rawTitle).uncertified
    // a silent film's billing is its programme's: an accompanist, an anniversary ("Dni Kina Niemego - 100 lat temu: …")
    if (films.isEmpty || billsASilentFilm(listing) || Quote.findFirstIn(raw).exists(_ => quotesAFilm(raw, films))) None
    else new BilledBeside(raw, listing, films, venues, bannerSlot).piece
  }

  /** The fewest letters of a director's name an inflected form of it starts with. */
  private[identity] val DirectorStem = 5

  /** Is the billing two pieces a dash joins ([[BillingParse.dashedPair]]), the second a title of `film`'s, whole ("LÍDA BAAROVÁ –
   *  DOOMED BEAUTY": its subject, then the film)? */
  def titledAfterPiece(raw: String, film: Film): Boolean = BillingPieces.dashedPair(raw) && {
    val titles = film.titles.map(t => withoutLeadingArticle(TitleContainment.tokens(t))).toSet
    BillingPieces.SegmentBreak.split(raw).lastOption.exists(piece => titles.contains(withoutLeadingArticle(TitleContainment.tokens(piece))))
  }
  /** Any quotation mark a title may carry. */
  private[identity] val Quote = """[„”"“«»]""".r
  private def quotesAFilm(raw: String, films: Seq[Film]): Boolean = {
    val titles = films.flatMap(_.titles).map(t => withoutLeadingArticle(TitleContainment.tokens(t))).toSet
    BillingPieces.Quoted.findAllMatchIn(raw).exists(m => titles(withoutLeadingArticle(TitleContainment.tokens(m.group(1)))))
  }

  // ── a programme slot ───────────────────────────────────────────────────────────────────────



  /** Does a bare billing under a FILM FESTIVAL's banner name `film` as an old catalogue film it cannot be told from? The
   *  banner a colon or a pipe closes names a film festival ([[services.titles.TitleWords.Concept.FilmFestival]]), the
   *  listing states no year, director or running time, and the film was released [[FestivalCatalogueYears]] years or
   *  more before the season its screenings fall in, and the billing names none of its directors. A festival bills its retrospectives' films with their facts or by a
   *  title no newer film shares; a bare title under its banner names its programme's entry, and a catalogue namesake fits
   *  it by the title alone: DE Pornfilmfestival Berlin's "The more the merrier" (a shorts programme) is not Stevens' 1943
   *  comedy, its "No Mercy" (Willinger's 2026 documentary) not Pearce's 1986 thriller — beside "Cyberfuck", the 2026 film
   *  at the same 39.1% on the same bare title. */
  def festivalsCatalogueNamesake(listing: MeasuredListing, film: Film): Boolean =
    film.year.exists(year => listing.dating.screeningsSeason.exists(_ - year >= FestivalCatalogueYears)) && festivalsBareBilling(listing, film)

  /** Is the listing a bare billing under a FILM FESTIVAL's banner that names none of `film`'s directors — no year,
   *  director or running time stated, the banner a colon or a pipe closes naming a film festival? Its title names the
   *  programme's entry, which no fact singles out among its namesakes ([[festivalsCatalogueNamesake]],
   *  [[festivalsOlderNamesake]]). */
  def festivalsBareBilling(listing: MeasuredListing, film: Film): Boolean = {
    val raw = listing.rawTitle.getOrElse(listing.title)
    // a programme naming the film's director is the festival's homage to them: "Hommage Rosa von Praunheim 2: Stadt der
    // verlorenen Seelen" is his 1983 film
    def creditsIt = {
      val billed = TitleContainment.tokens(raw).toSet
      film.directors.getOrElse(Nil).flatMap(TitleContainment.tokens).exists(name => name.length >= DirectorStem && billed(name))
    }
    listing.year.isEmpty && listing.directors.isEmpty && listing.runtime.isEmpty && BillingPieces.underAFilmFestival(raw) && !creditsIt
  }

  /** Is `film` OLDER than `newer`, a film the same bare billing under a film festival's banner names alike
   *  ([[festivalsBareBilling]])? A festival screens its year's entries: of two namesakes no fact tells apart, the newer
   *  is its programme's, and the older only the database's popularity ranking picks — DE Pornfilmfestival Berlin's
   *  "The Dating Game" took Netflix's 2024 "Woman of the Hour" (TMDB's German alternative title "The Dating Game") at
   *  50.7% beside Violet Du Feng's 2025 documentary "The Dating Game". The catalogue rule's years
   *  ([[FestivalCatalogueYears]]) deny an old namesake outright; this one only keeps the pooled vote from taking the
   *  older on its ranking. */
  def festivalsOlderNamesake(listing: MeasuredListing, film: Film, newer: Film): Boolean =
    festivalsBareBilling(listing, film) && film.year.exists(year => newer.year.exists(_ > year))

  /** How many years before its screenings a film released is a catalogue film, no festival's entry of its year. */
  val FestivalCatalogueYears = 10


  // ── whose facts ────────────────────────────────────────────────────────────────────────────

  /** Are the listing's facts all its VENUE's own — no feed catalogue's ([[SourceReliability.feedStated]]), and no
   *  listings site's catalogue entry linked as its page ([[SourceReliability.listingsSiteEntry]]: Flicks' film page, which
   *  links a new relay to an old production's page), whether or not the entry's facts were read onto it? Broader than
   *  `!factsFromCatalogue`, which marks only facts read off such a page. */
  def venueStated(listing: Listing): Boolean =
    SourceReliability.venueOnly(listing)

}
