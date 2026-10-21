package services.identity

import services.identity.IdentityMeasures.Measure

/** Why a film a billing's cluster took — the model's, or the one the families agree on — is none of the billing's: it
 *  names the film as ONE PIECE beside another thing ([[ListingShape.billedBeside]]) — DE "Jens Wawrczeck - Immer Ärger mit Harry" is an actor's reading, not
 *  Hitchcock's 1955 film; UK "SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY" an amateur company's musical. Not where
 *  what is around the film is the venue's after all:
 *  - its cluster's listings — its own, or those its title joins it to — are backed by [[IdentityResolver.PieceCorroboration]] other venues
 *    or more stating the film's year or crediting its director (`venues.corroborating`, `corroborated`): a film widely
 *    screened, whose local title, subtitle or event a venue bills beside it (PL "Rozważna i romantyczna | DZIEŃ
 *    PRZEPIĘKNYCH KOBIET", joined to the plain title 75 venues bill);
 *  - the piece beside it RECURS beside two of the corpus's whole titles or more, whatever families they fell in
 *    ([[CorpusContext.repeatedSegment]], read by `recurs` on the title's shapes): a programme's banner or a venue's offer ("Family Cinema: Paddington - Cert
 *    PG", PL Kino Kosmos' "Nakarmić kruki | Złote gody" beside "Wszyscy ludzie prezydenta | Złote gody"); or is a one-word
 *    tag in capitals beside a title that is not ([[IdentityMeasures.capitalisedTags]]: "Camino dla opornych | CHKF") or
 *    one word of consonants alone, a programme's initials ("Kopnięci w czasie|Fks"); or
 *    dates an edition after the film ("Dismember the Alamo 2026 - Chicago"); or names its venue (PL Kino
 *    Amondo's "Punku | Peruwiańskie pożegnanie lata w Amondo") — not only its town, a subject the film is not (DE
 *    Trifthof Kinocenter's "Gegen das Vergessen - Weilheim in der Zeit des Nationalsozialismus", a Weilheim documentary);
 *  - the piece is another title of the film: its title search's first film, or one IMDb lists under it in some language
 *    ([[CandidateQuery.ImdbTitled]]: DE "Épines du Sahel - Die Dornen der Sahelzone");
 *  - the listing states a fact of the film's own: a year, a director or an original title ([[IdentityMeasures.ownAgreement]]),
 *    a running time within [[IdentityResolver.PieceRuntimeAgreement]] minutes, a feed's TMDB link ("Notting Hill - Lady's Night" [1999]);
 *  - the film is a current release, of its screenings' year or the one before ([[IdentityResolver.CurrentRelease]]), or a foreign film
 *    TMDB knows by its original title alone, billed first ("Ekmek parası - Geld für's Brot"): the venue's subtitle or
 *    translation beside it — the namesakes a reading or a show is taken for are older catalogue films.
 *  A piece naming a STAGING beside the film — a theatre, a musical, a pantomime ([[ListingShape.StageWords]]); a pantomime even
 *  in the banner's slot (UK "CBeebies Panto 2026: Treasure Island", not the 2027 film) — outweighs all but a
 *  stated fact: the widely screened film, the current release and the venue's town are no answer to a staging of its work.
 *  `measures`: the billing's against the film; `tmdbId`: the film's TMDB id, when it has one —
 *  never a family's own number, so the feed's link ([[FeedLink.linked]]) is read as TMDB's: a family's resolve decides no
 *  piece (`IdentityResolver.resolveScored`), and [[ofFamilies]] is given the agreed record's TMDB id. Measured on the five convergence legs'
 *  whole-corpus takes (2026-10-14). */
private[identity] object BilledPiece {
  /** Why the film is one piece of the billing; `staging`: the piece beside it names a staging of its work. */
  final case class Piece(why: String, staging: Boolean)

  /** Which keeps a stage reads. [[Keeps.All]]: the model's own candidate, its facts weighed against the billing's. The
   *  record the families agree on is their best namesake of the PIECE, which every keep but the billing's facts would take
   *  (DE "Femme - Frauen heilen die Welt": the 2023 British "Femme", untranslated; UK "CBeebies Panto 2026: Treasure
   *  Island": a 2027 "Treasure Island", current; DE "40 Jahre Lumière - Die Geburtstagsfeier" at the Lumière: the 1961
   *  film beside the venue's own anniversary), so the agreement stage reads [[Keeps.Facts]]: a fact the billing states,
   *  or other venues stating the film's. A FILL's film is no agreed record but the model's lean the families lean to too,
   *  and reads [[Keeps.FactsOrCurrent]]: those, or a current release the venue bills a note beside (DE "ROYA - Eintritt
   *  frei mit Seebrücke Würzburg e.V.", the 2026 film with free entry from a charity). */
  enum Keeps { case All, Facts, FactsOrCurrent }

  def of(evidence: Evidence, listings: Seq[Listing], country: Option[String], film: IdentityMeasures.Film, tmdbId: Option[Int],
         measures: Map[String, Measure], corroborated: Double, recurs: String => Boolean,
         answered: CandidateQuery => Seq[Hit], keeps: Keeps = Keeps.All): Option[Piece] = {
    def screened = corroborated >= IdentityResolver.PieceCorroboration
    // a staged show ([[ListingShape.ShowWords]]) dates itself by its season, which its work's film of the next year stays a
    // year off: only its film's exact year is a fact of the film's (UK "CBeebies Panto 2026: Treasure Island", Hyde Park
    // Picture House publishing 2026, is not Ridley Scott's 2027 "Treasure Island")
    def exactYear = Seq("year.distance", "titleYear.delta").exists(name => measures.get(name).contains(IdentityMeasures.Number(0)))
    def show      = services.movies.TitleContainment.tokens(evidence.rawTitle).exists(ListingShape.ShowWords)
    def agreed    = IdentityMeasures.ownAgreement(measures)._1.filter(fact => fact != "year" || exactYear || !show)
    def stated    = agreed.nonEmpty ||
      IdentityMeasures.runtimeGap(measures).exists(_ <= IdentityResolver.PieceRuntimeAgreement) ||
      FeedLink.linked(listings).exists(link => tmdbId.contains(link._2))
    // a current release — of its screenings' year or the one before, or one TMDB dates no release of yet — whose subtitle
    // or translation its venue bills beside it ("Über Unterbiberger – Vom Woid in die Welt", 2026, ten venues): the
    // namesakes a reading or a show is taken for are the catalogue's older films ("Immer Ärger mit Harry", 1955)
    def current = film.year.fold(film.released.isEmpty)(year =>
      listings.flatMap(_.screenings.first).map(_.getYear).minOption.exists(year >= _ - IdentityResolver.CurrentRelease))
    // a foreign film TMDB knows by no title in the venue's language — its local title is its original — billed first, or
    // under one of its titles after one other piece: what the venue bills beside it is its own translation, subtitle or
    // subject (DE "Ekmek parası - Geld für's Brot", "Épines du Sahel - Die Dornen der Sahelzone", "LÍDA BAAROVÁ – DOOMED
    // BEAUTY"); a film of a country speaking the venue's language — its own, or another this deployment serves in that
    // language — carries its title, so its title being its original says nothing (UK "The Wizard Of Oz - Hot Rock", a
    // touring pantomime, is not the 1939 US film untranslated); nor does a title in the venue's own language, whichever
    // country TMDB files the film under (DE "Gegen das Vergessen - Weilheim in der Zeit des Nationalsozialismus", a local
    // documentary, is not Toscano's 2015 portraits, a US record of that German original title)
    def untranslated = film.originalTitle.exists(IdentityMeasures.key(_) == IdentityMeasures.key(film.title)) &&
      !film.countries.exists(_.exists(filmCountry => country.exists(venue => IdentityMeasures.sharesLanguage(venue, filmCountry)))) &&
      !country.flatMap(IdentityMeasures.languageOf).exists(tools.TextLanguage.readsAs(film.title, _)) &&
      (services.movies.TitleContainment.tokens(evidence.rawTitle).startsWith(services.movies.TitleContainment.tokens(film.title)) ||
        ListingShape.titledAfterPiece(evidence.rawTitle, film))
    import services.movies.TitleContainment.tokens
    // a word of every listing's venue's name or city (`places`), inflected as it may be ("Kino w Kadrze": "Kadr Non-Fiction")
    def placed(word: String, places: models.Cinema => Seq[Seq[String]]) = word.length >= IdentityMeasures.StemLetters &&
      listings.forall(listing => places(listing.cinema).flatten
        .exists(place => place.length >= IdentityMeasures.StemLetters && (word.startsWith(place) || place.startsWith(word))))
    def venues(word: String) = placed(word, EvidenceNode.placesOf)
    def venueNamed(word: String) = placed(word, cinema => Seq(tokens(cinema.displayName)))
    val listing = listings.head.copy(rawTitle = evidence.rawTitle, title = evidence.title, originalTitle = evidence.originalTitle)
    // a piece naming a stage show — a theatre, a musical ([[ListingShape.StageWords]]), not the venue's own name — beside the
    // film: the billing is a staging of the work, however widely the film is screened or whichever town the piece names
    // (UK "SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY", four venues billing the 1992 film plainly)
    def staging(piece: Seq[String]) = piece.exists(word => ListingShape.StageWords(word) && !venues(word))
    lazy val filmWords = film.titles.flatMap(tokens).toSet
    lazy val staged = Option.when(staging(tokens(evidence.rawTitle).filterNot(filmWords)))(
      ListingShape.billedBeside(listing, Seq(film), piece => !staging(piece),
        bannerSlot = !tokens(evidence.rawTitle).exists(ListingShape.ShowWords))).flatten
      .map(Piece(_, staging = true))
    if (stated) None
    // a billing listing several works by name is a programme, whatever its first piece's film (DE "Retro - An Undiscovered
    // World, Running High, Carrying Dreams: The Weight Between", four shorts): no keep reads it as one film's
    else if (screened || (keeps == Keeps.All && !ListingShape.listsWorks(evidence.rawTitle) && (current || untranslated))) staged
    else if (keeps == Keeps.FactsOrCurrent && !ListingShape.listsWorks(evidence.rawTitle) && current) staged
    else if (keeps != Keeps.All) staged.orElse(ListingShape.billedBeside(listing, Seq(film)).map(Piece(_, staging = false)))
    else {
      lazy val banners = evidence.published.shapes.filter(recurs).map(tokens)
      lazy val tags    = IdentityMeasures.capitalisedTags(evidence.published).map(tokens).filter(_.sizeIs == 1)
      // one word of consonants alone: a programme's initials ("Kopnięci w czasie|Fks", a seniors' film club)
      def acronym(piece: Seq[String]) = piece.sizeIs == 1 && !piece.head.exists(c => "aeiouy".contains(c) || c.isDigit)
      def dated(word: String) = word.length == 4 && word.forall(_.isDigit) && film.year.exists(_ < word.toInt)
      // another title of the film itself: its search's first film, or one IMDb lists under it in some language (DE
      // "Épines du Sahel - Die Dornen der Sahelzone", the German release title TMDB's record does not carry)
      def titles(piece: Seq[String]) = evidence.published.shapes.exists(shape => tokens(shape) == piece &&
        tmdbId.exists(id => answered(CandidateQuery.Title(shape)).headOption.exists(_.tmdbId == id) || answered(CandidateQuery.ImdbTitled(shape)).exists(_.tmdbId == id)))
      staged.orElse(ListingShape.billedBeside(listing, Seq(film), piece =>
        piece.exists(word => dated(word) || venueNamed(word)) || acronym(piece) || tags.contains(piece) || banners.contains(piece) || titles(piece))
        .map(Piece(_, staging = false)))
    }
  }

  /** [[of]] for a film the FAMILIES found — the one they agree on ([[Keeps.Facts]]: the billing's stated facts against
   *  the record, and the other venues stating them), or one a fill rule takes from their leans and the model's
   *  ([[Keeps.FactsOrCurrent]]). The families' best match for a billing is their best namesake of the
   *  piece it names, whichever stage takes it (UK "Song Sung Blue- The Music of Neil Diamond", a tribute concert, filled as
   *  the 2025 film from the families' leans). */
  def ofFamilies(listings: Seq[Listing], film: IdentityMeasures.Film, tmdbId: Option[Int], keeps: Keeps = Keeps.Facts): Option[Piece] = {
    val read = listings.map { listing =>
      val evidence = Evidence.of(listing, None)
      val country  = EvidenceNode.countryOf(listing.cinema)
      (listing, evidence, country, measuresOf(evidence, film, country))
    }
    val stating = read.collect { case (listing, _, _, measures) if IdentityMeasures.ownAgreement(measures)._1.nonEmpty => listing.cinema }.toSet
    read.iterator.flatMap { case (listing, evidence, country, measures) =>
      of(evidence, Seq(listing), country, film, tmdbId, measures, (stating - listing.cinema).size, _ => false, _ => Nil, keeps)
    }.nextOption()
  }

  /** A billing's measures against `film` for a stage that scored no candidate (the agreement stage): its stated facts. */
  def measuresOf(evidence: Evidence, film: IdentityMeasures.Film, country: Option[String]): Map[String, Measure] =
    IdentityMeasures.listingFilm(evidence.measured, film, None, 0, 0, country = country)
}
