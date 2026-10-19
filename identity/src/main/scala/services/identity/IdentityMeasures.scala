package services.identity

import services.movies.{TitleContainment, TitleSeason, TitleWords}
import services.resolution.{SearchTitles, YearWindow}

/**
 * The raw MEASUREMENTS the calibrated identity score reads: what one listing's own evidence says
 * about a film (a TMDB candidate), or about another listing. Each is a general agreement measure
 * (title shape, year, director, runtime, country, where the candidate ranked, how many rivals it
 * has, how many other venues' own facts back it); none names a title, venue, chain or franchise.
 *
 * This object only MEASURES. It never says what a measurement is worth: the bins, the weight of
 * each bin, the calibration and the cannot-link rules are data in `identity-weights.json`
 * ([[IdentityCalibration]]), fitted from corroborated films by `scripts.IdentityCalibrate`
 * (docs/design/identity-resolver.md §14). Both the calibration and the resolver call
 * these functions, so the fitted weights and the scored values are one definition.
 *
 * Missing evidence is a value of its own, and says WHICH side is missing ("missing:listing",
 * "missing:film"): a venue that prints no year and a candidate whose details were never fetched
 * are different facts, and neither may read as agreement or as disagreement.
 */
object IdentityMeasures {
  export TitleTextMeasures.*
  export CreditMeasures.*
  export TitleShapeMeasures.*
  export WorkMeasures.*
  export TitleRelationMeasures.*
  export NumeralMeasures.*
  export YearRuntimeMeasures.*
  export SearchMeasures.*

  /** What one venue published about one film. `decorations`: the venue decorations learned around
   *  titles (`TitleDecorations`), which the title shapes read — not a fact the venue published. */
  final case class Listing(title: String, rawTitle: Option[String] = None, originalTitle: Option[String] = None,
                           year: Option[Int] = None, runtime: Option[Int] = None, directors: Seq[String] = Nil,
                           countries: Seq[String] = Nil, yearCredits: Option[Seq[String]] = None,
                           decorations: TitleDecorations = TitleDecorations.None, searchTitles: Seq[String] = Nil,
                           /** The season a stage relay billing neither its season nor a year screens in
                            *  ([[services.identity.Listing.broadcastSeason]]): searched with its work, never measured. */
                           broadcastSeason: Option[Int] = None,
                           /** The season its screenings fall in, whatever it states ([[services.identity.Listing.screenedSeason]]):
                            *  what picks among a house's records of its work's seasons, never searched. */
                           screenedSeason: Option[Int] = None,
                           /** The season its first screening falls in, unconditionally ([[services.identity.ScreeningDays.season]]):
                            *  the seasons its title can name ([[TitleSeason.near]]). */
                           screeningsSeason: Option[Int] = None,
                           /** Its screening days, held only where its stated year is more than a season before the season
                            *  it screens in ([[services.identity.Listing.airDays]]): what the air-day test reads. */
                           airDays: Seq[java.time.LocalDate] = Nil,
                           /** The age certificate the venue bills it with ([[services.identity.Listing.certificate]]): read
                           *  against the film's certificate in the venue's country by the weighted `certificate` measure. */
                           certificate: Option[String] = None) {
    private def titles: Seq[String] = rawTitle.toSeq :+ title
    /** Its title, then the raw title the venue published — title first, as the title relations read them. */
    def ownTitles: Seq[String] = Seq(title) ++ rawTitle
    /** Does the original title the venue publishes stand apart from its billing — the billing itself or one of its whole
     *  delimited or decorated pieces (US "Legend (1986)", published as "Legend"; DE Goli's "The Piano Tuner
     *  (Dokumentarfilm)"), or a title sharing none of its words and no near-spelling of it, as the work's title in another
     *  language is (PL "Podróżnik", published as "Mossafer")? An original title that is a mere part of the billing or a
     *  near-spelling of it is what a search for the billing hit, copied onto the page with that record's credits: DE
     *  kinoheld's "Tino Bomelino" (a comedian) as "Tino", "Punkabend #93" as "'93", "Ganes" (a band) as "Ganges". A
     *  shared word counts when it is a number or longer than an article. */
    private[identity] lazy val originalStandsApart: Boolean = originalTitle.forall { original =>
      IdentityMeasures.shapes(titles).exists(shape => IdentityMeasures.key(shape) == IdentityMeasures.key(original)) ||
        originalSharesNothingBilled
    }
    /** Does the original title share NONE of the billing's words (a number, or one longer than an article) and is it no
     *  near-spelling of it: another title altogether, not a spelling of the billed one? Either the work's title in another
     *  language ([[originalStandsApart]]) or, where a feed copied it, what a search for the billing hit: DE kinoheld's
     *  "Abdul Kader Chahin" (a concert) carried the original title of "5 Centimeters per Second", "Dirty Deeds - A
     *  Tribute To AC/DC" that of "Salaam-E-Ishq" (`FamilyScope.namedOnlyByCopiedTitle`). */
    private[identity] lazy val originalSharesNothingBilled: Boolean = originalTitle.forall { original =>
      titles.forall { billed =>
        !IdentityMeasures.shareAWord(original, billed) && !tools.EditDistance.within(IdentityMeasures.key(original), IdentityMeasures.key(billed), 1)
      }
    }
    /** The pieces of the event its billing bills ([[agreement.NonFilmEvents.eventBilled]]), each with its counted words
     *  (a piece with none left out) and whether it bills a performance ([[services.cinemas.pl.NonMovieEventClassifier.isPerformance]]) — none unless the
     *  whole event bills one, as most billings bill none. Read once per listing, whichever films it is weighed against
     *  (`Acceptance.notPerformedBeside`). */
    private[identity] lazy val performancePieces: Seq[(String, Seq[String], Boolean)] =
      agreement.NonFilmEvents.eventBilled(rawTitle.getOrElse(title)).filter(services.cinemas.pl.NonMovieEventClassifier.isPerformance(_)).toSeq
        .flatMap(IdentityMeasures.pieces).map(piece => (piece, IdentityMeasures.countedWords(piece), services.cinemas.pl.NonMovieEventClassifier.isPerformance(piece)))
        .filter(_._2.nonEmpty)
    /** The billing's event words and its other words ([[FilmNess]]), once per listing. */
    private[identity] lazy val eventWords: Seq[String] = FilmNess.eventWords(this)
    private[identity] lazy val nonEventWords: Set[String] = if (eventWords.isEmpty) Set.empty else FilmNess.otherWords(this)
    /** How many words the billing shares with `title` at most, over its titles ([[IdentityMeasures.sharedWords]]). */
    private[identity] def wordsBilledOf(title: String): Int = titles.map(IdentityMeasures.sharedWords(title, _)).max
    /** The directors credited beside the published `year` — one listing's own, unless a pooled
     *  read took its year and its credits from different listings (`yearCredits`). */
    def creditedBesideYear: Seq[String] = yearCredits.getOrElse(directors)
    /** The season the title names ("2026/27"), by its first year. */
    lazy val seasonYear: Option[Int] = TitleSeason.of(titles, screeningsSeason)
    /** The word the title bills several films by ([[MultiFilmBill.marker]]), once per listing: every rule asks. */
    private[identity] lazy val billMarker: Option[String] = MultiFilmBill.marker(titles)
    /** The one title a marathon billed after a dash names ([[MultiFilmBill.billedOne]]), once per listing. */
    private[identity] lazy val billedOne: Option[String] = MultiFilmBill.billedOne(titles)
    /** A year the venue put in its title as a delimited annotation ("(2026)"), outside any season. */
    lazy val titleYear: Option[Int] = IdentityMeasures.titleYearOf(titles)
    /** The venue's own year: its field, else the one its title brackets. */
    def statedYear: Option[Int] = year.orElse(titleYear)
    /** A running time the venue put in its title as a bracketed annotation ("(97’)", "[97 min]"). */
    lazy val titleRuntime: Option[Int] = IdentityMeasures.bracketedRuntime(titles)
    /** The venue's own runtime: its field, else the one its title brackets — a running time no film has
     *  ([[services.movies.FilmRuntime.plausible]]) is none. */
    def statedRuntime: Option[Int] = runtime.filter(services.movies.FilmRuntime.plausible).orElse(titleRuntime)
    /** Each of the listing's own titles' shapes alone ([[IdentityMeasures.shapes]] of the one title, no learned
     *  decorations), split once per listing: the scorer asks them of its billed title for every candidate it weighs. */
    private lazy val ownShapes: Map[String, Seq[String]] = titles.distinct.map(t => t -> IdentityMeasures.shapes(Seq(t))).toMap
    private[identity] def shapesOfOwn(t: String): Seq[String] = ownShapes.getOrElse(t, IdentityMeasures.shapes(Seq(t)))
    /** The shapes its title can name a film by: the whole title, its raw form, and each programme-banner segment
     *  (`SearchTitles.candidates`: `|`, ` - `, a first `: `, …), once per listing: every title relation and billing reads them. */
    lazy val shapes: Seq[String] = IdentityMeasures.shapesOf(this)
    /** What a title relation reads of this listing, hashed once: `FamilyScope` shares relations by it. */
    private[identity] lazy val titleInputs: FamilyScope.TitleInputs = new FamilyScope.TitleInputs(this)
    /** The title and raw title as comparison forms, and the shapes' keys, once per listing: the
     *  resolver relates every listing to every film of its family's pool (`titleRelation`). */
    private[identity] lazy val ownForms: Seq[IdentityMeasures.TitleForm] = ownTitles.map(IdentityMeasures.TitleForm(_))
    /** The own forms' non-empty keys, once per listing: every pool candidate's `titleRelation` asks. */
    private[identity] lazy val ownKeys: Set[String] = (ownForms.map(_.key) ++ printless.map(IdentityMeasures.key)).filter(_.nonEmpty).toSet
    /** Its titles without the print a repertory screening bills ([[IdentityMeasures.billsAPrint]]), format tags and all:
     *  "Tschaikowsky - Genie und Wahnsinn (35mm)" is Ken Russell's film's whole title, not a piece of it. */
    private[identity] lazy val printless: Seq[String] =
      titles.filter(IdentityMeasures.billsAPrint).map(services.movies.FormatTags.stripFormatTags).filter(_.nonEmpty)
    /** How many years an anniversary the title or original title bills celebrates ("30th Anniversary"), once per listing. */
    private[identity] lazy val anniversary: Option[Int] = IdentityMeasures.anniversaryOf(ownTitles ++ originalTitle)
    /** The stage works its titles bill ([[StageRelays.stageWorksBilled]]), no season named. */
    private[identity] def billedStageWorks: Set[String] = StageRelays.stageWorksBilled(titles, seasonNamed = false)
    /** Does the title name an opera or ballet house ([[StageRelays.screenNamesake]]), once per listing: every candidate asks. */
    private[identity] lazy val billsAStageHouse: Boolean = StageRelays.billsAStageHouse(titles)
    /** Its title read as a stage house's relay ([[StageRelays.Relay]]), once per listing: every candidate asks. */
    private[identity] lazy val relay: StageRelays.Relay =
      if (billsAStageHouse) StageRelays.Relay.of(titles) else StageRelays.Relay.Unbilled
    /** The title's words, once per listing: a banner word before the title naming a director is read against every candidate. */
    private[identity] lazy val titleWords: Seq[String] = ownForms.head.words
    /** The title cut after each of its first words, as the key of what follows and the words before it, once per
     *  listing: what [[IdentityMeasures.bannerNamesItsDirector]] reads against every candidate. */
    private[identity] lazy val bannerCuts: Seq[(String, Seq[String])] =
      (1 until titleWords.size).map(k => (titleWords.drop(k).mkString, titleWords.take(k)))
    /** The title cut before each of its last words but one, as the key of what precedes and of the two words or more
     *  after: a credit closing it, read by [[IdentityMeasures.bannerNamesItsDirector]] against every candidate. */
    private[identity] lazy val closingCuts: Seq[(String, String)] =
      (1 to titleWords.size - 2).map(k => (titleWords.take(k).mkString, titleWords.drop(k).mkString))
    private[identity] lazy val shapeKeys: Seq[String] = shapes.map(IdentityMeasures.key)
    private[identity] lazy val shapeWords: Seq[Seq[String]] = shapes.map(IdentityMeasures.words)
    /** The shapes only the learned decorations leave, once per listing (usually none): a relation's scoring reads them
     *  per candidate, and each read derived the undecorated copy's shapes again. */
    private[identity] lazy val decoratedOnlyShapes: Seq[String] =
      if (decorations == TitleDecorations.None) Nil else (shapes.toSet -- copy(decorations = TitleDecorations.None).shapes).toSeq.sorted
    /** The title with a learned programme decoration stripped ("Horror Season 2026 …"), as words:
     *  what the venue's banner leaves of it. Empty when no learned decoration applies. */
    private[identity] lazy val undecoratedWords: Seq[Seq[String]] =
      ownTitles.map(t => IdentityMeasures.creditedWork(t).getOrElse(t)).flatMap(decorations.strip)
        .map(IdentityMeasures.words).filter(_.nonEmpty).distinct
    /** The title and raw title, each also as ingest cleans it ([[services.movies.FormatTags]]), as yearless tokens
     *  (`StageRelays.billing`): a box office's certificate is no part of the house's billing. UK "NT LIVE: Golden Boy (15)" billed
     *  "Golden Boy" under no banner, its work not at the title's edge, and measured an `overlap` of "Golden Boy (NT Live)". */
    private[identity] lazy val billedTitles: Seq[Seq[String]] =
      ownTitles.flatMap(t => Seq(t, services.movies.FormatTags.stripFormatTags(t))).map(IdentityMeasures.yearlessTokens).distinct
    private[identity] lazy val billedWorks: Set[Seq[String]] = shapes.map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
    /** Does the venue publish a FACT beside its title — a year (field, bracket or season), a
     *  running time, a director, a country, or an original title that is not its title again
     *  ([[IdentityMeasures.repeatsItsTitle]])? */
    def publishesAFact: Boolean =
      statedYear.isDefined || seasonYear.isDefined || statedRuntime.isDefined || directors.exists(_.trim.nonEmpty) ||
        countries.nonEmpty || (originalTitle.exists(_.trim.nonEmpty) && !IdentityMeasures.repeatsItsTitle(this))
    /** The shapes as series and numbers (`numeralRelation`). */
    private[identity] lazy val numberedShapes: Seq[IdentityMeasures.Numbered] = shapes.map(IdentityMeasures.numbered)
    /** The shapes of the BILLING alone, as series and numbers: without an original title that is but a spelling of it
     *  ([[originalStandsApart]]), the copy whose number `numeralRelation` never takes over the billing's own. */
    private[identity] lazy val numberedBilledShapes: Seq[IdentityMeasures.Numbered] =
      if (originalStandsApart) numberedShapes else copy(originalTitle = None).numberedShapes
    /** The directors, and those credited beside the year, parsed once per listing (`directorRelation`). */
    private[identity] lazy val directorCredits: CreditRelation.Credits   = new CreditRelation.Credits(directors)
    /** Its search titles' [[IdentityMeasures.key]]s, in order: worked out once, not per film it is weighed against. */
    private[identity] lazy val searchKeys: Seq[String] = searchTitles.map(IdentityMeasures.key)
    /** Each credited director's name as title tokens, in order. */
    private[identity] lazy val directorTokens: Seq[Seq[String]] = directors.map(TitleContainment.tokens)
    private[identity] lazy val creditsBesideYear: CreditRelation.Credits =
      if (yearCredits.isEmpty) directorCredits else new CreditRelation.Credits(creditedBesideYear)
    /** The original title, trimmed, as a comparison form, once per listing (`originalTitleRelation`). */
    private[identity] lazy val originalForm: Option[IdentityMeasures.OriginalForm] =
      originalTitle.map(_.trim).filter(_.nonEmpty).map(IdentityMeasures.OriginalForm(_))
  }

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
    /** TMDB's movie genre ids (`/genre/movie/list`), each a bit of [[Film.genreBits]] by its place here: an id TMDB adds
     *  later is not kept until it is added (at the end — a stored record's bits name the places). */
    val Genres: IndexedSeq[Int] = IndexedSeq(28, 12, 16, 35, 80, 99, 18, 10751, 14, 36, 27, 10402, 9648, 10749, 878, 10770, 53, 10752, 37)
    private val bitOf: Map[Int, Int] = Genres.zipWithIndex.map { case (id, place) => id -> (1 << place) }.toMap
    /** The bits of `ids` (an unknown id none). */
    def genreBitsOf(ids: Iterable[Int]): Int = ids.foldLeft(0)((bits, id) => bits | bitOf.getOrElse(id, 0))
    /** The ids `bits` hold. */
    def genreIdsOf(bits: Int): Set[Int] = Genres.indices.filter(place => (bits & (1 << place)) != 0).map(Genres).toSet
    /** A tagline's (or a billing piece's) words as one key: [[Film.taglineKey]]; 0 for none. */
    def taglineKeyOf(text: String): Int = {
      val words = services.movies.TitleContainment.tokens(text)
      if (words.isEmpty) 0 else words.mkString(" ").hashCode
    }
  }

  /** One measurement: a category, a number, or missing (with which side is missing). */
  sealed trait Measure
  final case class Category(value: String) extends Measure
  final case class Number(value: Double) extends Measure
  final case class Missing(side: String) extends Measure

  val MissingListing: Missing = Missing("listing")
  /** The director relation naming one person on both sides ([[directorRelation]]). */
  val SamePersonMeasure: Category = Category("same_person")
  val MissingFilm: Missing    = Missing("film")

  /** Scopes of a measurement set, as the artefact's cannot-link rules name them. */
  val ListingFilm    = "listing-film"
  val ListingListing = "listing-listing"

  /** The listing-film measures that are the film DATABASE'S ranking of its search, not anything
   *  a listing published: where the film ranked, how popular it is, how many same-titled rivals
   *  it has. */
  val RankingPriors: Set[String] = Set("search.rank", "popularity.log2", "rivals")

  /** The listing-film measure that counts OTHER venues' evidence (the family's pool), not
   *  anything this listing published. */
  val PooledMeasures: Set[String] = Set("venues.corroborating")

  /** The listing-film measures read from a GUARD that weigh as well: the venue's certificate against the film's in its
   *  country ([[Certificates.below]]). They weigh in the calibrated probability only: no fact a rule counts
   *  ([[FactMeasures]]), nothing the listing's own-evidence sums read ([[EvidenceWeights]]), so every guard and rule
   *  reading those stays as it was. */
  val GuardMeasures: Set[String] = Set("certificate")

  /** The title relations under which a listing's certificate is its film's: the billing names the film as a WHOLE title
   *  (its own, original or an alternative). Beside a film one piece of the billing names, the certificate rates the
   *  programme — UK Hyde Park's "The Tiger Who Came for Tea and We're Going on a Bear Hunt" (U) bills two shorts. */
  val CertifiedRelations: Set[String] = Set("exact", "original", "alternative")

  /** The venue's certificate against the film's in the venue's country, on that country's scale ([[Certificates.Ladders]]):
   *  `match` on one step, `near` within [[Certificates.StepsBelow]], `below` / `above` that far or further the venue's
   *  under / over the film's. `missing:unrated` where the country has no scale (Poland), `missing:piece` where the
   *  billing names the film by a piece only ([[CertifiedRelations]]), else the side that states none. */
  def certificateRelation(l: Listing, f: Film, country: Option[String], title: Category): Measure =
    if (l.certificate.isDefined && !CertifiedRelations(title.value)) Missing("piece")
    else country.flatMap(c => Certificates.Ladders.get(c).map(c -> _)).fold[Measure](Missing("unrated")) { case (c, ladder) =>
      l.certificate.flatMap(ladder.step).fold[Measure](MissingListing) { own =>
        Certificates.in(f.certificates, c).flatMap(ladder.step).fold[Measure](MissingFilm) { rated =>
          val gap = rated - own
          Category(if (gap == 0) "match" else if (math.abs(gap) < Certificates.StepsBelow) "near" else if (gap > 0) "below" else "above")
        }
      }
    }

  /** The listing-film measures that read the TITLE itself — how it names the film, and the
   *  instalment numbers it carries: a title alone never vetoes, so none of them is a fact. */
  val TitleMeasures: Set[String] = Set("title", "numeral")

  /** The listing-film measures that compare a FACT the listing published beside its title — a
   *  year (field, bracket or season), a director, a runtime, a country, an original title: every
   *  measure but the title measures, the ranking priors and the pooled count. Derived from
   *  [[listingFilm]] itself, so a new measure is a fact unless it is classified otherwise. */
  lazy val FactMeasures: Set[String] =
    listingFilm(Listing(""), Film(""), None, 0, 0).keySet -- RankingPriors -- PooledMeasures -- TitleMeasures -- GuardMeasures

  /** The listing-listing measure that says WHERE the two listings are (one venue or two), not
   *  anything either published about its film. */
  val PlacementMeasures: Set[String] = Set("venue")

  /** The listing-listing measures that compare a FACT both listings published beside their titles —
   *  a year (field, bracket or season), a director, a runtime, an original title, a chain's id:
   *  every measure but the title relation and the placement. Derived from [[listingListing]]
   *  itself, as [[FactMeasures]] is from [[listingFilm]]. */
  lazy val ListingFactMeasures: Set[String] =
    listingListing(Listing(""), Listing(""), sameVenue = false, sharedChainId = None).keySet -- PlacementMeasures - "title"

  /** Does this measurement set of `scope` compare at least one fact (a fact measure that is not
   *  missing on either side)? When it does not, the only evidence against the pair is how the two
   *  titles relate (and, for two listings, where they are) — a score, never a veto. */
  def comparesAFact(scope: String, m: Map[String, Measure]): Boolean = comparedFacts(scope, m).nonEmpty

  /** The facts this measurement set of `scope` compares: its fact measures missing on neither side. */
  def comparedFacts(scope: String, m: Map[String, Measure]): Map[String, Measure] = {
    val facts = if (scope == ListingListing) ListingFactMeasures else FactMeasures
    m.filter { case (name, v) => facts(name) && !v.isInstanceOf[Missing] }
  }

  /** The title relations under which one title carries the other's WHOLE — the same title, a
   *  delimited piece of it, or a token run along one edge — so the two differ only by what one
   *  adds around the other, never by words each has that the other lacks (`overlap`, `none`). */
  val ContainingRelations: Set[String] = Set("exact", "segment", "decorated", "fragment")

  /** Measures that only ever AGREE with a film, never deny it: a year in a title is as often a
   *  re-release's screening year ("Gone With The Wind (2026)") as the film's, so the label rule
   *  ([[ownAgreement]]) reads it only when it agrees — and a veto reads it the same way. The table
   *  still weighs it both ways in the probability. */
  val AgreesOnly: Set[String] = Set("titleYear.delta")

  /** A listing's published year against the film's. Beside the SAME director it never denies the
   *  film (`listingFilm` reads it as absent there): a year decades off then dates the screening (a
   *  re-release, a retrospective), not another film — and that director's other film of the title,
   *  when the pool has it, still wins on the year's score (Helios RePlay's 2026 "Diabły" is Ken
   *  Russell's 1971 film). */
  val PublishedYear: Set[String] = Set("year.delta", "year.distance")
  /** Minutes off at which a listing's stated runtime is its own fact against the film, not a
   *  venue's rounding or trailers. */
  val RuntimeContradiction: Int = 30
  def runtimeContradicts(m: Map[String, Measure]): Boolean = runtimeGap(m).exists(_ >= RuntimeContradiction)
  /** How many minutes a listing's stated runtime is off its film's, either way: the size of the
   *  listing-film `runtime.delta`, which is SIGNED — the venue's minutes less the record's — so its
   *  table can weigh a venue billing more (an interval, an introduction, a short before the feature)
   *  apart from one billing less. The rules that read a runtime gap read its size: the hand-written ones
   *  here, the learned cannot-links as [[RuntimeGap]] — a venue's minutes off its film either way are what
   *  rules out a film, while which way they are off is the table's to weigh. */
  def runtimeGap(m: Map[String, Measure]): Option[Double] =
    ruleMeasure(m, RuntimeGap).collect { case Number(d) => d }
  /** The measure a learned cannot-link's [[RuntimeGap]] condition reads. */
  val RuntimeGap: String = "runtime.gap"
  /** What a learned cannot-link's condition on `signal` reads from a pair's measures: the measure itself, or —
   *  [[RuntimeGap]] — the size of `runtime.delta`. The calibration's rule search and the resolver's rule
   *  evaluation both read it (`IdentityCalibration.Condition`), so a rule is learned on what it is applied to. */
  def ruleMeasure(m: Map[String, Measure], signal: String): Option[Measure] =
    if (signal == RuntimeGap) m.get("runtime.delta").map { case Number(d) => Number(math.abs(d)); case other => other }
    else m.get(signal)
  def sameDirector(m: Map[String, Measure]): Boolean = m.get("director").contains(SamePersonMeasure)
  /** Does the listing's original title only repeat its own title — the whole of it (a venue
   *  filling the field with the display title, "Cellar Door x ThoughtBubble Presents: Terminator 2:
   *  Judgment Day") or a run cut off one edge of it ("BTS World Tour 'ARIRANG' In Buenos Aires:
   *  Live" beside "…: LIVE VIEWING")? Then it carries nothing the title does not: it is the title
   *  again, not a second fact. Read by [[originalTitleRelation]] against the listing's own titles
   *  (`match` or `fragment`), so one definition of "carries" serves both. An original title that
   *  adds to the listing's — a `decorated` one, or a `segment` one that holds the title as one
   *  delimited piece ("Royal Ballet and Opera: Romeo and Juliet (INACTIVE)") — says more than the
   *  title, and stays a fact. */
  def repeatsItsTitle(l: Listing): Boolean =
    originalTitleRelation(l.originalTitle, l.rawTitle.toSeq :+ l.title) match {
      case Category(c) => CopiesOfTheTitle(c)
      case _           => false
    }
  private val CopiesOfTheTitle: Set[String] = Set("match", "fragment")

  /** Categories whose evidence cannot weaken as the listing carries more of the other side, per
   *  measure, strongest first: a decoration carries the film's whole title, an overlap some of its
   *  words, `none` nothing. The calibration fits their weights under this order
   *  (`IdentityCalibrate.inOrder`) — an ORDER, never a weight; categories it leaves out are placed by
   *  the data alone. */
  val EvidenceOrder: Map[String, Seq[String]] = Map("title" -> Seq("decorated", "overlap", "none"))

  /** Which way a numeric measure's evidence runs, by what it measures: more venues corroborating a
   *  film can only back it more, a lower search rank and a smaller runtime gap only name it more
   *  closely. The calibration fits their bins under this direction (`IdentityCalibrate.monotone`) —
   *  a DIRECTION, never a weight — so a thin bin (0 same-film and 4 different-film units at 153-155
   *  venues) cannot weigh more evidence below less. A `Peaked` measure is signed and names the film
   *  most closely at 0: rising up to it, falling past it, each side fitted on its own counts — the
   *  listing-film runtime (the venue's minutes less the record's), where a venue billing 20 minutes
   *  more is often the film with an interval or an introduction and one billing 20 less another cut.
   *  Other signed measures (a year's difference) and ones with no direction of their own are left to
   *  the data. */
  enum EvidenceDirection { case Rising, Falling, Peaked }
  val NumericDirection: Map[String, EvidenceDirection] = Map(
    "venues.corroborating" -> EvidenceDirection.Rising,
    "search.rank"          -> EvidenceDirection.Falling,
    "runtime.delta"        -> EvidenceDirection.Peaked)

  /** Title relations that name a film: the listing's title is (a spelling of) the film's. */
  val NamingRelations: Set[String] = Set("exact", "original", "alternative", "segment", "decorated")
  /** The naming relations under which the listing's title IS one of the film's titles — whole, or
   *  as one delimited piece of it — not merely carrying it along one edge (`decorated`), where the
   *  rest may bill another work. */
  val TitledRelations: Set[String] = NamingRelations - "decorated"

  /** What a listing's own measurements say about a film by themselves: the corroborators that
   *  agree (a year within one, the same director, the original title) and those that deny it. */
  def ownAgreement(m: Map[String, Measure]): (Set[String], Set[String]) = {
    val agree = Set.newBuilder[String]; val deny = Set.newBuilder[String]
    // A published year agrees or denies; a year in the title only agrees — a bracket is as often
    // a re-release's year as the film's.
    m.get("year.distance").foreach { case Number(d) => if (d <= YearWindow.PublishedAdjacency) agree += "year" else deny += "year"; case _ => }
    m.get("titleYear.delta").foreach { case Number(d) if FactRelations.nearDelta(d) => agree += "year"; case _ => }
    m.get("director").foreach { case Category(c) => if (c == SamePersonMeasure.value) agree += "director" else if (c == "different") deny += "director"; case _ => }
    m.get("originalTitle").foreach { case Category(c) => if (c == "match") agree += "originalTitle" else if (c == "disjoint") deny += "originalTitle"; case _ => }
    (agree.result(), deny.result())
  }

  // ── the measurement sets ─────────────────────────────────────────────────────────────

  /**
   * A listing against a candidate film.
   *
   * @param searchRank 1-based rank of the film in the listing's OWN title search, if it was there
   * @param rivals     how many OTHER candidates of the listing's pool its title names as closely
   *                   (`exact`, `original` or `alternative`)
   * @param corroboratingVenues how many OTHER venues listing the same title publish this film's
   *                   exact year or credit its director — the family's pooled evidence
   * @param houses     the houses the listings' banners were learned to be ([[StageRelays.Houses.learn]])
   * @param qualifiers the title pieces learned to be qualifiers, not works ([[Qualifiers.learn]])
   */
  def listingFilm(l: Listing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int,
                  houses: StageRelays.Houses = StageRelays.Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown, country: Option[String] = None): Map[String, Measure] =
    listingFilmTitled(l, f, searchRank, rivals, corroboratingVenues, titleRelation(l, f, houses, qualifiers), country)

  /** [[listingFilm]] with the title relation already read — `titleRelation(l, f, houses, qualifiers)` —
   *  by a caller that relates the listing to the film anyway (`FamilyScope.score`). `country`: the venue's (ISO-3166-1),
   *  whose cinema releases of the film its year is read against first ([[Film.closestYear]]). */
  def listingFilmTitled(l: Listing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int,
                        title: Category, country: Option[String] = None): Map[String, Measure] = {
    // Each measure in its slot ([[ListingFilmMeasures]]), worked out in the order the map's entries were.
    val slots = new Array[Measure](ListingFilmMeasures.Keys.length)
    slots(0)  = title
    slots(1)  = numeralRelation(l, f)
    slots(2)  = ownOriginalTitle(l, f, title)
    // the listing's year against the film's closest release year: its original, a re-release's, an edition's
    val released = if (f.releases.isEmpty || l.year.isEmpty) f.year else f.closestYear(l.year.get, country)
    slots(3)  = delta(l.year, released)
    slots(4)  = absDelta(l.year, released)
    slots(5)  = filmMinus(f.year, l.titleYear)
    slots(6)  = filmMinus(f.year, l.seasonYear)
    slots(7)  = {
                  // Each name's and title's tokens read once per listing and film, not per pair ([[Listing.directorTokens]]).
                  val persons = if (l.directors.isEmpty) l.directors else
                    l.directors.iterator.zip(l.directorTokens).collect { case (name, words) if !namesItsHouse(name, words, f) => name }.toSeq
                  f.directorCredits.fold[Measure](if (persons.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
                    CreditRelation.of(if (persons.size == l.directors.size) l.directorCredits else new CreditRelation.Credits(persons), _))
                }
    slots(8)  = runtimeDelta(l, f, country)
    slots(9)  = countryRelation(l.countries, f.countries)
    slots(10) = searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble))
    slots(11) = f.popularity.fold[Measure](MissingFilm)(p => Number(PopularityBucket.of(p).toDouble))
    slots(12) = Number(rivals.toDouble)
    slots(13) = Number(corroboratingVenues.toDouble)
    slots(14) = certificateRelation(l, f, country, title)
    premiereYear(l, f, anniversaryYear(l, f, screeningYearAbsent(l, f, title, new ListingFilmMeasures(slots))))
  }

  /** `m` with the published year absent when it predates the season the listing's screenings broadcast its stage work
   *  in ([[Listing.screenedSeason]]), against the house's record of that season ([[StageRelays.screenedSeasonRecord]]): the year dates
   *  the staging's premiere, not the broadcast. UK Aldeburgh's "The Royal Ballet: Romeo & Juliet" [2025], screening on 25
   *  May 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet", aired that day — two years off its 2027, its title an
   *  overlap, it was denied, and the denial withdrew the record from every relay title-linked to it. Against any other
   *  record — the 2021/22 season's, the house's 2013 recording — the year stays the venue's fact. */
  private def premiereYear(l: Listing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (l.year.isDefined && StageRelays.screenedSeasonRecord(l, f)) m ++ PublishedYear.map(_ -> MissingListing) else m

  /** `m` with the published year absent when it is the year of an ANNIVERSARY the title bills: ES "Drácula. 30
   *  Aniversario" (original title "Bram Stoker's Dracula 30th Anniversary"), dated 2022, is Coppola's 1992 film — its
   *  year that many years before the stated one, within a year. Any other year stays the listing's fact, and so does one
   *  a re-release TMDB dates already agrees with (DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)" [2026], released
   *  again in 2026). */
  private def anniversaryYear(l: Listing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (l.anniversary.isEmpty || l.year.isEmpty || f.year.isEmpty || !FactRelations.yearsNear(l.year.get - l.anniversary.get, f.year.get) ||
        m("year.delta").match { case Number(d) => FactRelations.nearDelta(d); case _ => false }) m
    else m ++ PublishedYear.map(_ -> MissingListing)
  /** Is `a` dated by the screening of an anniversary it bills, `b` by the film's own year that many years before: two
   *  spellings of one re-release, neither year the other's fact (DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)" dated
   *  2026 at one venue and 1991 at another). */
  private def anniversaryDated(a: Listing, b: Listing): Boolean =
    a.anniversary.isDefined && a.year.isDefined && b.year.exists(FactRelations.yearsNear(a.year.get - a.anniversary.get, _))
  /** How many years an anniversary one of `titles` bills celebrates ("30th Anniversary": 30), the first that bills one. */
  private[identity] def anniversaryOf(titles: Seq[String]): Option[Int] =
    titles.iterator.flatMap(t => AnniversaryYears.findFirstMatchIn(t)).map(_.group(1).toInt).nextOption()
  /** "30th Anniversary", "30 Aniversario", "35-jähriges Jubiläum", "35. rocznica", "35e anniversaire": how many years a
   *  re-release celebrates — the number, its ordinal ending, and an anniversary word of a language venues bill in. */
  private[identity] val AnniversaryYears =
    ("""(?iU)\b(\d{1,3})(?:st|nd|rd|th|e|ème|°)?\.?(?:\s*-\s*|\s+)(?:jährige[smn]?\s+)?""" +
      TitleWords.rawPattern(TitleWords.Concept.Anniversary) + """\b""").r

  /** `m` with the published year absent when it dates a screening ([[PublishedYear]]): when the
   *  listing's title is the film's title ([[TitledRelations]]) and the same director is credited, a year
   *  that denies the film is the year the venue shows it (Kinoteka's 2026 on Ken Russell's 1971
   *  "Diabły") or releases it, not another film's — in the score as in a veto. A title that is
   *  NOT the film's leaves the year a fact: then it tells that director's films apart
   *  (KINOMUZEUM's 2026 "Błotem w twarz" is Jaak Kilmi's 2026 film, not his 2017 "Sangarid"), and
   *  a title that only starts or ends with the film's (`decorated`) may bill another work beside it
   *  (Nowe Horyzonty's 2026 double bill "Basia. Humor w paski mam + Kocia Szajka…" is not
   *  Wasilewski's 2018 "Basia").
   *  "Credited" is by the listing that published the year ([[Listing.creditedBesideYear]]), never
   *  borrowed from a sibling's credit. A re-release is the same cut: a runtime that contradicts
   *  the film ([[runtimeContradicts]]) makes the year another production's — DE's 2027 "MET Opera
   *  Live im Kino: Manon" at 264 minutes is the Met's revival of Laurent Pelly's staging, not his
   *  232-minute 2019 recording. */
  private def screeningYearAbsent(l: Listing, f: Film, title: Category, m: Map[String, Measure]): Map[String, Measure] =
    if (TitledRelations(title.value) && ownAgreement(m)._2("year") && !runtimeContradicts(m) &&
        f.directorCredits.exists(CreditRelation.onePerson(l.creditsBesideYear, _)))
      m ++ PublishedYear.map(_ -> MissingListing)
    else m

  /** The listing's original title against the film's titles — unless it only repeats the
   *  listing's own title ([[repeatsItsTitle]]) and that title names the film ([[NamingRelations]]):
   *  then it is the title again, and the title relation already says the film is named, so it
   *  counts only where it agrees (the film carries it whole, `match`) and is otherwise absent —
   *  in the score and a veto alike. Counted, a truncated copy ("…: Live") read as a fragment of the
   *  very record the title names exactly. Beside a film the title does NOT name, the repeat stays
   *  what it measures: Everyman's "Dracula (4K Restoration)" is not The Mummy its cinematographer
   *  directed. */
  private def ownOriginalTitle(l: Listing, f: Film, title: Category): Measure =
    originalFormRelation(l.originalForm, f.trimmedForms, f.year) match {
      case m if NamingRelations(title.value) && repeatsItsTitle(l) && m != Category("match") => MissingListing
      case m                                                                                 => m
    }

  /** Titles a listing names itself by, as the other side of a listing-listing comparison. */
  private def asFilm(l: Listing): Film =
    Film(l.title, l.originalTitle, l.rawTitle.toSeq, l.statedYear, l.statedRuntime, Some(l.directors).filter(_.exists(_.trim.nonEmpty)),
      None, None)

  /**
   * Two listings: are they one film? `sharedChainId` is whether the two venues' chains published
   * an id in a common namespace, and whether it was the same id (`None` when there is no common
   * namespace).
   */
  def listingListing(a: Listing, b: Listing, sameVenue: Boolean, sharedChainId: Option[Boolean]): Map[String, Measure] = {
    // Two listings name each other the same way whichever is measured first: when one's title is a
    // whole delimited piece of the other's ("Lalka" and "Astra Seniora - Lalka", in either order)
    // the pair is a `segment`, as the resolver's title-segment must-link reads it — measured one
    // way only, the plain spelling first read as a `fragment` or an `overlap` of its decorated one.
    val title = (titleRelation(a, asFilm(b), None, Qualifiers.Unknown), titleRelation(b, asFilm(a), None, Qualifiers.Unknown)) match {
      case (Category("original") | Category("alternative"), _) => Category("exact")
      case (forward, backward) if forward != Category("exact") && backward == Category("segment") => backward
      case (forward, _)                                        => forward
    }
    Map(
      "title"         -> title,
      "originalTitle" -> ((a.originalTitle.filter(_.trim.nonEmpty), b.originalTitle.filter(_.trim.nonEmpty)) match {
                           case (Some(_), Some(ob)) => originalTitleRelation(a.originalTitle, Seq(ob))
                           case (None, _)           => MissingListing
                           case (_, None)           => MissingFilm
                         }),
      "year.delta"    -> (if (anniversaryDated(a, b) || anniversaryDated(b, a)) MissingListing else absDelta(a.year, b.year)),
      "titleYear.delta" -> absDelta(a.titleYear, b.titleYear),
      "season.delta"  -> absDelta(a.seasonYear, b.seasonYear),
      "director"      -> directorRelation(a.directors, b.directors),
      "runtime.delta" -> absDelta(a.statedRuntime, b.statedRuntime),
      "venue"         -> Category(if (sameVenue) "same" else "different"),
      "chainId"       -> sharedChainId.fold[Measure](Missing("no-shared-namespace"))(s => Category(if (s) "same" else "different"))
    )
  }
}

/**
 * A listing's measures against a film ([[IdentityMeasures.listingFilmTitled]]) as a map of its fixed keys, each value in a
 * slot of one array: built for every listing/film pair the resolver weighs, the 14-entry `HashMap` and its entries' tuples
 * were among worker-pl's identity model's largest allocations (JFR 2026-10-05). A map like any other to read, compare and
 * hash; a change to one of its keys is another such map, any other change an ordinary one.
 */
final class ListingFilmMeasures private[identity] (private val slots: Array[IdentityMeasures.Measure])
    extends scala.collection.immutable.AbstractMap[String, IdentityMeasures.Measure] {
  import ListingFilmMeasures.{Keys, slotOf}

  def get(key: String): Option[IdentityMeasures.Measure] = { val i = slotOf(key); if (i < 0) None else Some(slots(i)) }
  override def getOrElse[V1 >: IdentityMeasures.Measure](key: String, default: => V1): V1 = { val i = slotOf(key); if (i < 0) default else slots(i) }
  override def contains(key: String): Boolean = slotOf(key) >= 0
  override def apply(key: String): IdentityMeasures.Measure = { val i = slotOf(key); if (i < 0) default(key) else slots(i) }
  def iterator: Iterator[(String, IdentityMeasures.Measure)] = Keys.indices.iterator.map(i => Keys(i) -> slots(i))
  override def keysIterator: Iterator[String]                    = Keys.iterator
  override def valuesIterator: Iterator[IdentityMeasures.Measure] = slots.iterator
  override def size: Int      = Keys.length
  override def knownSize: Int = Keys.length
  override def isEmpty: Boolean = false

  def updated[V1 >: IdentityMeasures.Measure](key: String, value: V1): Map[String, V1] = (slotOf(key), value) match {
    case (i, m: IdentityMeasures.Measure) if i >= 0 => val next = slots.clone(); next(i) = m; new ListingFilmMeasures(next)
    case _                                         => scala.collection.immutable.HashMap.from[String, V1](this).updated(key, value)
  }
  def removed(key: String): Map[String, IdentityMeasures.Measure] =
    if (slotOf(key) < 0) this else scala.collection.immutable.HashMap.from(this).removed(key)
}

object ListingFilmMeasures {
  /** The keys, by slot. */
  val Keys: IndexedSeq[String] = IndexedSeq("title", "numeral", "originalTitle", "year.delta", "year.distance", "titleYear.delta",
    "season.delta", "director", "runtime.delta", "country", "search.rank", "popularity.log2", "rivals", "venues.corroborating",
    "certificate")
  private val Slots = { val m = new java.util.HashMap[String, Integer](Keys.length * 2); Keys.zipWithIndex.foreach { case (k, i) => m.put(k, i) }; m }
  private def slotOf(key: String): Int = { val i = Slots.get(key); if (i == null) -1 else i.intValue }
}
