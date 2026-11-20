package services.identity.measures

import services.titles.{SearchTitles, TitleContainment, TitleSeason}
import services.identity.vocabulary.FilmCast

/** What TMDB says about a candidate film. `directors`/`countries` are `None` when the film's
 *  details were not fetched, which is not the same as TMDB crediting nobody. `countries` are
 *  ISO 3166-1 alpha-2 codes. */
final case class Film(title: String, originalTitle: Option[String] = None, alternativeTitles: Seq[String] = Nil,
                      year: Option[Int] = None, runtime: Option[Int] = None, directors: Option[Seq[String]] = None,
                      countries: Option[Seq[String]] = None, popularity: Option[Double] = None,
                      /** IMDb's title number for the film (tt0064570 → 64570, [[IdentityMeasures.imdbNumber]]), 0 when
                       *  TMDB names none: an Int, not the id, so a model of every candidate's record carries no string. */
                      imdbNumber: Int = 0,
                      /** The day TMDB dates its release: a broadcast's air date ([[agreement.Broadcast]]). */
                      released: Option[java.time.LocalDate] = None,
                      /** The countries TMDB dates a release of it in, their ISO-3166-1 codes run together in order
                       *  ("ATDEPL", [[TmdbFilmRecord.releaseCountries]]); `None` when its release dates were not fetched. One
                       *  string, not a set: every candidate of every family holds it, and a veto asks one country of it. */
                      releaseCountries: Option[String] = None,
                      /** The running times TMDB states for the film besides [[runtime]]: the other translations' it was fetched
                       *  in. TMDB keeps a runtime per translation — "Once Upon a Time in America" runs 229 minutes in pl-PL and
                       *  de-DE, 139 (the US theatrical cut) in en-US — and a venue screening either cut screens the film. */
                      alternativeRuntimes: Seq[Int] = Nil,
                      /** The cinema releases TMDB dates — premieres, re-releases, editions — run together
                       *  ([[TmdbFilmRecord.releases]]); `None` when its release dates were not fetched. One string, as
                       *  [[releaseCountries]] is. */
                      releases: Option[String] = None,
                      /** Its localised tagline's words as one key ([[Film.taglineKeyOf]]), 0 when TMDB states none: the
                       *  subtitle a venue may bill beside its title ("Frauen bildet Banden - Eine Spurensuche zur Geschichte
                       *  der Roten Zora"). A key, not the text: every candidate of every family holds it, and one question is
                       *  asked of it ([[ListingShape.billedBeside]]). */
                      taglineKey: Int = 0,
                      /** The certificates TMDB records for it in the countries a rating scale is read for, the least
                       *  restrictive one each ("GB=18,US=R", [[Certificates.ofReleaseDates]]); `None` when none is known.
                       *  One string, as [[releases]] is. */
                      certificates: Option[String] = None,
                      /** TMDB's `original_language`, its ISO 639-1 code ("en", "pl"), interned: every record holds one of a
                       *  few dozen shared strings, a reference each and no copy; `None` for a record filed before records
                       *  kept it, or one TMDB names none for. What a showing billed as subtitled into a language asks of the
                       *  film: is that language its own? */
                      originalLanguage: Option[String] = None,
                      /** TMDB's genres of it, one bit each of [[Film.Genres]] (an Int, no collection per record): what the
                       *  cast evidence asks of a candidate — is its cast a voice cast (Animation) or its contributors'
                       *  (Documentary)? 0 for a record filed before records kept them, or one TMDB files under none. */
                      genreBits: Int = 0,
                      /** Its top-billed cast as the cast mismatch reads it ([[FilmCast]]); `None` where not fetched, or
                       *  in other letters than a venue's. */
                      cast: Option[FilmCast] = None) {
  /** TMDB's genre ids of the film ([[genreBits]]). */
  def genreIds: Set[Int] = Film.genreIdsOf(genreBits)
  /** Does TMDB file the film under the genre of TMDB id `genreId`? */
  def hasGenre(genreId: Int): Boolean = (genreBits & Film.genreBitsOf(Seq(genreId))) != 0
  /** The film as a line names it: `'Lalka' (1968)`, its year left out where none is known. */
  def titled: String = s"'$title'${year.fold("")(y => s" ($y)")}"
  /** Every running time TMDB states for the film, [[runtime]] first: what a listing's runtime is compared with. */
  def runtimes: Seq[Int] = (runtime.toSeq ++ alternativeRuntimes).filter(_ > 0)
  /** The cinema releases TMDB dates in `country` (the venue's, ISO-3166-1) — or in any country when it dates none there,
   *  or no country is given: a venue screens its own country's release of the film. */
  private[identity] def releasesIn(country: Option[String]): Seq[TmdbFilmRecord.Release] = releases.fold(Seq.empty[TmdbFilmRecord.Release]) { codes =>
    val all  = TmdbFilmRecord.Release.all(codes)
    val here = country.fold(Seq.empty[TmdbFilmRecord.Release])(c => all.filter(_.country == c))
    if (here.nonEmpty) here else all
  }
  /** The year of the film's — its original [[year]], or a dated cinema release's ([[releasesIn]]): a re-release's, an
   *  edition's — closest to `stated`, the original on a tie. "Apocalypse Now" billed 2019 is TMDB's 1979 record, through
   *  its 2019 Final Cut release. */
  private[identity] def closestYear(stated: Int, country: Option[String]): Option[Int] =
    year.map(original => releasesIn(country).iterator.map(_.year).foldLeft(original)((best, y) =>
      if (math.abs(stated - y) < math.abs(stated - best)) y else best))
  /** Is `stated` the year of a release of the film whose note names an EDITION ("Final Cut", "Redux") — and not its
   *  original year: a listing of that year screens the edition, which can run longer than any runtime TMDB states. */
  private[identity] def editionYear(stated: Int, country: Option[String]): Boolean =
    !year.contains(stated) && releasesIn(country).exists(release => release.edition && release.year == stated)
  /** Does TMDB date a release of the film in `country` (ISO-3166-1)? `None` when its release dates are unknown. */
  def releasedIn(country: String): Option[Boolean] = Option.when(releaseCountries.isDefined)(knownReleasedIn(country, dated = true))
  /** Are the film's release dates known, and does TMDB date (`dated`) — or not date — a release in `country`? What
   *  [[releasedIn]] answers, read by the resolver per candidate without an option. */
  private[identity] def knownReleasedIn(country: String, dated: Boolean): Boolean = releaseCountries.isDefined && {
    val codes = releaseCountries.get
    var i = 0
    while (i + 1 < codes.length && !codes.regionMatches(i, country, 0, 2)) i += 2
    (i + 1 < codes.length) == dated
  }
  /** Its title, original title and alternative titles, in that order: every derived form below reads these. */
  private[identity] def titles: Seq[String] = Seq(title) ++ originalTitle ++ alternativeTitles
  /** Its own title, then its original title: the record's titles less the alternatives TMDB or venues file it under. */
  def ownTitles: Seq[String] = Seq(title) ++ originalTitle
  /** Its own titles' non-empty [[IdentityMeasures.key]]s, each once, title first: what a record is filed under by title. */
  def ownTitleKeys: Seq[String] = ownTitles.map(IdentityMeasures.key).filter(_.nonEmpty).distinct
  /** Its titles' [[IdentityMeasures.key]]s — worked out once, not per listing weighed against it ([[searchGroups]]): a
   *  film's alternative titles can run to dozens, keyed again for every listing of every family it is a candidate of. */
  private[identity] lazy val titleKeys: Set[String] = titles.map(IdentityMeasures.key).toSet
  /** Its own title's and original title's tokens: what a director credited by a house name is read against. */
  private[identity] lazy val ownTitleTokens: Seq[Seq[String]] = ownTitles.map(TitleContainment.tokens)
  /** Every word of its titles, the stage works and houses they name, and their keys ([[StageRelays.Relay]]), once per record:
   *  a stage house's relay reads them of every candidate ([[StageRelays.namesAnotherWork]]). */
  private[identity] lazy val titleWords: Set[String] = titles.flatMap(TitleContainment.tokens).toSet
  /** Is it a CONCERT's record crediting no director: TMDB fetched its credits and they name none, and it files it under
   *  Music or a title calls it a concert? Such a record's country is usually its uploader's or its cinema distributor's,
   *  not the production's (PL "André Rieu. Pada śnieg!", billed Holandia: TMDB's 1707785 dates the Dutch concert in the
   *  US), so a country mismatch against it weighs for less ([[ConcertMismatch]]). A stage relay's record names no
   *  concert: its country is the house's, which tells look-alike productions apart. */
  private[identity] lazy val concertRecord: Boolean =
    directors.exists(_.isEmpty) && (hasGenre(Film.Music) || titleWords.exists(w => Film.ConcertStems.exists(w.contains)))
  private[identity] lazy val stageWorkIds: Set[String] =
    StageRelays.stageWorks(this) ++ StageRelays.stageWorksBilled(titles, seasonNamed = true)
  private[identity] lazy val wholeKeys: Set[String] = titles.map(StageRelays.wholeKey).toSet
  private[identity] lazy val houseNames: Seq[Seq[String]] = StageRelays.Relay.of(titles).houses
  /** Its own title's and original title's yearless tokens. */
  private[identity] lazy val ownYearlessTitles: Seq[Seq[String]] = ownTitles.map(IdentityMeasures.yearlessTokens)
  /** The film's titles and their delimited pieces as yearless tokens, once per record (`StageRelays.billing`). */
  private[identity] lazy val billedTitles: Seq[Seq[String]] =
    titles.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty).distinct
  /** Each of the film's title and original title's [[IdentityMeasures.Qualifiers.split]] alone, once per record:
   *  every family whose pool holds the record learns its qualifiers from them. */
  private[identity] lazy val ownSplits: Seq[(Seq[String], Seq[String], Boolean)] =
    ownTitles.distinct.flatMap(t => IdentityMeasures.Qualifiers.split(t, IdentityMeasures.shapes(Seq(t))))
  private[identity] lazy val billedWorks: Set[Seq[String]] =
    titles.flatMap(SearchTitles.candidates(_, None)).map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
  /** [[billedWorks]] but those only how a broadcast is screened ([[StageRelays.billings]]), once per record: every
   *  listing scored against the record asked the label test of each work it shares with it (a twelfth of what kept
   *  spellings added to a UK resolve, JFR 2026-11-07). */
  private[identity] lazy val billedWorksBesideLabels: Set[Seq[String]] = billedWorks.filterNot(StageRelays.broadcastLabel)
  /** The title, original title and alternative titles, in that order, as comparison forms once
   *  per record (`titleRelation`). */
  private[identity] lazy val forms: Seq[IdentityMeasures.TitleForm] =
    titles.map(IdentityMeasures.TitleForm(_))
  /** The season its titles name, once per record: every node scoring it asks (`evidenceDenies`). */
  private[identity] lazy val season: Option[Int] = TitleSeason.of(titles)
  /** The credited directors, parsed once per record (`directorRelation`); `None` when not fetched. */
  private[identity] lazy val directorCredits: Option[CreditRelation.Credits] = directors.map(new CreditRelation.Credits(_))
  /** The last word of each credited director's name, [[IdentityMeasures.StemLetters]] letters at least ("Fritz Lang:
   *  M"), once per record: the surname a billing names a director by. ONE rule for a banner over the film's own title
   *  ([[IdentityMeasures.bannerNamesItsDirector]]) and over a namesake's ([[agreement.Agreement.billsAnothersDirector]]).
   *  A surname that is also a word of the film's titles still names him in a banner ("Hitchcock & Chabrol: Spellbound",
   *  which TMDB also titles "Alfred Hitchcock's Spellbound"). */
  private[identity] lazy val directorSurnames: Set[String] =
    directors.getOrElse(Nil).flatMap(name => IdentityMeasures.words(name).lastOption).filter(_.length >= IdentityMeasures.StemLetters).toSet
  /** Each credited director's whole name of two words or more, as a key, once per record: the credit a title closing
   *  on it names ([[IdentityMeasures.bannerNamesItsDirector]]). */
  private[identity] lazy val directorNameKeys: Set[String] =
    directors.getOrElse(Nil).map(IdentityMeasures.words).filter(_.sizeIs >= 2).map(_.mkString).toSet
  /** The titles, trimmed and non-empty, as comparison forms once per record: the other side of
   *  every node's `originalTitleRelation` to this film. */
  private[identity] lazy val trimmedForms: Seq[IdentityMeasures.TitleForm] =
    titles.map(_.trim).filter(_.nonEmpty).map(IdentityMeasures.TitleForm(_))
  /** The film's titles as series and numbers (`numeralRelation`). */
  private[identity] lazy val numberedTitles: Seq[IdentityMeasures.Numbered] =
    titles.map(_.trim).filter(_.nonEmpty).distinct.map(IdentityMeasures.numbered)
}
object Film {
  /** TMDB's Music genre id. */
  private[identity] val Music = 10402
  /** The stems a concert's title carries ("Christmas Concert", "Konzert", "Koncert noworoczny"). */
  private[identity] val ConcertStems: Seq[String] = Seq("concert", "konzert", "koncert", "concierto")
  /** TMDB's movie genre ids (`/genre/movie/list`), each a bit of [[Film.genreBits]] by its place here: an id TMDB adds
   *  later is not kept until it is added (at the end — a stored record's bits name the places). */
  val Genres: IndexedSeq[Int] = IndexedSeq(28, 12, 16, 35, 80, 99, 18, 10751, 14, 36, 27, 10402, 9648, 10749, 878, 10770, 53, 10752, 37)
  /** TMDB's English name of each of [[Genres]]: what a screen lists for a film's genres. */
  val GenreNames: Map[Int, String] = Map(28 -> "Action", 12 -> "Adventure", 16 -> "Animation", 35 -> "Comedy", 80 -> "Crime",
    99 -> "Documentary", 18 -> "Drama", 10751 -> "Family", 14 -> "Fantasy", 36 -> "History", 27 -> "Horror", 10402 -> "Music",
    9648 -> "Mystery", 10749 -> "Romance", 878 -> "Science Fiction", 10770 -> "TV Movie", 53 -> "Thriller", 10752 -> "War",
    37 -> "Western")
  private val bitOf: Map[Int, Int] = Genres.zipWithIndex.map { case (id, place) => id -> (1 << place) }.toMap
  /** The bits of `ids` (an unknown id none). */
  def genreBitsOf(ids: Iterable[Int]): Int = ids.foldLeft(0)((bits, id) => bits | bitOf.getOrElse(id, 0))
  /** The ids `bits` hold. */
  def genreIdsOf(bits: Int): Set[Int] = Genres.indices.filter(place => (bits & (1 << place)) != 0).map(Genres).toSet
  /** A tagline's (or a billing piece's) words as one key: [[Film.taglineKey]]; 0 for none. */
  def taglineKeyOf(text: String): Int = {
    val words = services.titles.TitleContainment.tokens(text)
    if (words.isEmpty) 0 else words.mkString(" ").hashCode
  }
}
