package services.identity.measures

import services.identity.vocabulary.VenueNames

/**
 * The CAST signal, a tie-break between namesakes: of the candidates a billing names alike, the one whose top-billed TMDB
 * cast the venue's own text names two whole names of ([[VenueNames]]), no other's twice. Measured offline 2026-10-06 over 244 labelled clusters of the
 * five corpora (the venue pages fetched, every candidate's TMDB credits): 24 decided, 24 right, 0 wrong, 11 of them
 * clusters the resolver left unmatched (PL Kino CK Lublin's "Lalka": Dorociński, Urzędowska and Kondrat name Kawalski's
 * 2026 film). ONE name is no evidence — it took 2 wrong films there: performers sing and act in several productions
 * (US "MetOpera: Medea (2022–23)" names Sondra Radvanovsky, the Naples Medea's lead too). A double bill naming each
 * film's cast names more than one candidate: nothing. A candidate whose cast is not known might be the one named:
 * nothing either. Narrowed to the tie-break 2026-10-19 (docs/identity-experiments.md, `cast-tiebreak`): once the
 * captures held casts, the wider rule took a tribute named after a film ("Straight Talk: A Tribute to Dolly Parton") as
 * the film; the tie-break took 6 UK no-director ties right, none wrong.
 */
object CastEvidence {
  /** The whole names a venue's text must name of one candidate's cast. */
  val Names = 2

  /** The TIE-BREAK: of the candidates the billing names alike (`sameTitled`, each with its
   *  top-billed cast, `None` while not known), the one whose cast the venue's text names [[Names]] whole names of, no
   *  other doing so — a preference among namesakes, never a take of a lone candidate. A rival named by one name does not
   *  stand in its way (sequels and remakes share an actor), nor is a rival named by none held against it: only two names
   *  decide. Nothing while fewer than two candidates tie, or one's cast is not known: it might be the one named. */
  def tieBreak[K](venue: VenueNames, sameTitled: Seq[(K, Option[Seq[String]])]): Option[(K, Seq[String])] =
    if (venue.isEmpty || sameTitled.sizeIs < 2 || sameTitled.exists(_._2.isEmpty)) Option.empty
    else sameTitled.map { case (candidate, names) => candidate -> names.get.filter(venue.names).distinct }.filter(_._2.sizeIs >= Names) match {
      case Seq(only) => Some(only)
      case _         => Option.empty
    }

  /** The MISMATCH: why the venues' own cast FIELDS rule `film` out, if they do — each
   *  listing listing a cast ([[ListedCast]]) names none of the film's top-billed cast ([[FilmCast]]), and no listing states
   *  a fact of its own that agrees with the film (a year within one, a director the film credits): then the cast is
   *  another film's. Read only where the cast is the film's: never beside a stage work, a relay, a performance (a
   *  tribute's cast field lists its performers) or a bill of several works, only for a film of the venues' own language
   *  (`language`) — a dub's cast field lists its voices, a foreign film's its transliterations — and only for a film whose
   *  genres are known and say its cast is on screen ([[onScreenCast]]). Measured 2026-10-19 (recording 38713274617): ES
   *  "La maleta" ×14 cured (Objetos 2022 → Therón's 2026 film), 9 right gained, nothing else moved. */
  def mismatch(listings: Seq[Listing], film: Film, language: Option[String], castIsTheFilms: => Boolean): Option[String] = {
    val listing = listings.filter(_.listedCast.nonEmpty)
    if (listing.isEmpty || film.cast.isEmpty || language.isEmpty || film.originalLanguage != language || !onScreenCast(film) || !castIsTheFilms) None
    else {
      def ownFact(l: Listing) = l.year.exists(y => film.year.exists(f => math.abs(f - y) <= 1)) ||
        (l.directors.nonEmpty && FactRelations.samePerson(l.directors, film.directors.getOrElse(Nil)))
      Option.when(listing.forall(l => film.cast.get.namedBy(l.listedCast) == 0) && !listings.exists(ownFact))(
        "its venue's own cast field names none of its top-billed cast, and no fact the venue states agrees with it")
    }
  }

  /** TMDB's genres whose cast is no on-screen cast of the film: Animation (a dub's voices), Documentary (whoever appears). */
  val VoiceOrContributorGenres: Seq[Int] = Seq(16, 99)

  /** Is the film's TMDB cast the people a venue's cast field would list — its genres KNOWN (a record filed before records
   *  kept them reads as unknown, and the mismatch stands down) and none of [[VoiceOrContributorGenres]]? */
  def onScreenCast(film: Film): Boolean = film.genreBits != 0 && !VoiceOrContributorGenres.exists(film.hasGenre)

  /** Is what the listings' cast fields list the film's cast — none billing a stage work, a relay, a performance or
   *  several works ([[mismatch]])? */
  def castIsTheFilms(listings: Seq[Listing]): Boolean = !listings.exists { l =>
    ListingShape.stagesAWork(l) || ListingShape.relays(l, l.rawTitle.toLowerCase(java.util.Locale.ROOT)) || ListingShape.billsSeveral(l) ||
      services.titles.NonMovieEventClassifier.isPerformance(l.rawTitle)
  }
}
