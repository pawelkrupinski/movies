package services.identity

import services.titles.billing.{BillingParse, BillingPieces}
import services.titles.{TitleContainment, TitleSeason}

/** What one venue published about one film. `decorations`: the venue decorations learned around
 *  titles (`TitleDecorations`), which the title shapes read — not a fact the venue published. */
final case class MeasuredListing(title: String, rawTitle: Option[String] = None, originalTitle: Option[String] = None,
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
                                 certificate: Option[String] = None,
                                 /** The latest year its film can be from ([[services.identity.Listing.yearUpperBound]]): what
                                  *  weighs against a film released after it as a stated year would ([[ListingYearReadings.yearBound]]), never for one. */
                                 yearUpperBound: Option[Int] = None,
                                 /** The year of the performance a stage relay's synopsis names ([[services.identity.Listing.performanceYear]]):
                                  *  what dates an encore's relay ([[RelayStage.of]]), never measured. */
                                 performanceYear: Option[Int] = None,
                                 /** The year its first screening falls in, where its title bills an anniversary
                                  *  ([[services.identity.Listing.anniversaryScreeningYear]]): what [[anniversaryFilmYear]] dates the film by. */
                                 anniversaryScreeningYear: Option[Int] = None) {
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
  /** Is the original title a word-for-word cognate of a billed one ([[TitleTextMeasures.cognateOf]]): the billing's own
   *  title translated, not another title a search hit? */
  private[identity] lazy val originalTranslatesBilled: Boolean =
    originalTitle.exists(original => titles.exists(billed => IdentityMeasures.cognateOf(original, billed)))
  /** The pieces of the event its billing bills ([[NonFilmEvents.eventBilled]]), each with its counted words
   *  (a piece with none left out) and whether it bills a performance ([[services.titles.NonMovieEventClassifier.isPerformance]]) — none unless the
   *  whole event bills one, as most billings bill none. Read once per listing, whichever films it is weighed against
   *  (`Acceptance.notPerformedBeside`). */
  private[identity] lazy val performancePieces: Seq[(String, Seq[String], Boolean)] =
    NonFilmEvents.eventBilled(rawTitle.getOrElse(title)).filter(services.titles.NonMovieEventClassifier.isPerformance(_)).toSeq
      .flatMap(BillingPieces.pieces).map(piece => (piece, IdentityMeasures.countedWords(piece), services.titles.NonMovieEventClassifier.isPerformance(piece)))
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
  /** Does the title name a season further back than a season's record is encored ([[services.identity.Listing.SeasonEncoreLag]])
   *  before the one it screens in — a page kept from that season, its own facts as old as its title ([[Listing.airDays]])? */
  lazy val billsAStaleSeason: Boolean =
    seasonYear.exists(named => screeningsSeason.exists(_ - named > services.identity.Listing.SeasonEncoreLag))
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
   *  ([[models.FilmRuntime.plausible]]) is none. */
  def statedRuntime: Option[Int] = runtime.filter(models.FilmRuntime.plausible).orElse(titleRuntime)
  /** Each of the listing's own titles' shapes alone ([[IdentityMeasures.shapes]] of the one title, no learned
   *  decorations), split once per listing: the scorer asks them of its billed title for every candidate it weighs. */
  private lazy val ownShapes: Map[String, Seq[String]] = titles.distinct.map(t => t -> IdentityMeasures.shapes(Seq(t))).toMap
  private[identity] def shapesOfOwn(t: String): Seq[String] = ownShapes.getOrElse(t, IdentityMeasures.shapes(Seq(t)))
  /** The shapes its title can name a film by: the whole title, its raw form, and each programme-banner segment
   *  (`SearchTitles.candidates`: `|`, ` - `, a first `: `, …), once per listing: every title relation and billing reads them. */
  lazy val shapes: Seq[String] = IdentityMeasures.shapesOf(this)
  /** What a title relation reads of this listing, hashed once: `FamilyScope` shares relations by it. */
  private[identity] lazy val titleInputs: MeasuredListing.TitleInputs = new MeasuredListing.TitleInputs(this)
  /** The title and raw title as comparison forms, and the shapes' keys, once per listing: the
   *  resolver relates every listing to every film of its family's pool (`titleRelation`). */
  private[identity] lazy val ownForms: Seq[IdentityMeasures.TitleForm] = ownTitles.map(IdentityMeasures.TitleForm(_))
  /** The own forms' non-empty keys, once per listing: every pool candidate's `titleRelation` asks. */
  private[identity] lazy val ownKeys: Set[String] =
    (ownForms.map(_.key) ++ printless.map(IdentityMeasures.key) ++ versionless.map(IdentityMeasures.key)).filter(_.nonEmpty).toSet
  /** Its titles without the closing version tag they bill ([[BillingParse.versionless]]): "Der Herr der Ringe: Die zwei
   *  Türme EXTENDED VERSION" names "Der Herr der Ringe: Die zwei Türme" whole, as a cut of that film. */
  private[identity] lazy val versionless: Seq[String] = titles.flatMap(BillingParse(_).versionless)
  /** Its titles without the print a repertory screening bills ([[IdentityMeasures.billsAPrint]]), format tags and all:
   *  "Tschaikowsky - Genie und Wahnsinn (35mm)" is Ken Russell's film's whole title, not a piece of it. */
  private[identity] lazy val printless: Seq[String] =
    titles.filter(IdentityMeasures.billsAPrint).map(BillingParse(_).formatless).filter(_.nonEmpty)
  /** How many years an anniversary the title or original title bills celebrates ("30th Anniversary"), once per listing. */
  private[identity] lazy val anniversary: Option[Int] = IdentityMeasures.anniversaryOf(ownTitles ++ originalTitle)
  /** The year the film of an anniversary the title bills was released, when the listing states no year of its own: the
   *  year it screens in less the anniversary's — US "The Host: 20th Anniversary", screening in 2026, bills Bong Joon-ho's
   *  2006 film. Of the matched anniversary billings stating no year in prod (2026-11-15), 1238 of 1239 took a film within
   *  a year of it. */
  private[identity] lazy val anniversaryFilmYear: Option[Int] =
    if (statedYear.isDefined) None else anniversary.zip(anniversaryScreeningYear).map((years, screened) => screened - years)
  /** The stage works its titles bill ([[StageRelays.stageWorksBilled]]), no season named. */
  private[identity] def billedStageWorks: Set[String] = StageRelays.stageWorksBilled(titles, seasonNamed = false)
  /** Does the title name an opera or ballet house ([[StageRelays.screenNamesake]]), once per listing: every candidate asks. */
  private[identity] lazy val billsAStageHouse: Boolean = StageRelays.billsAStageHouse(titles)
  /** Its title read as a stage house's relay ([[StageRelays.Relay]]), once per listing: every candidate asks. */
  private[identity] lazy val relay: StageRelays.Relay =
    if (billsAStageHouse) StageRelays.Relay.of(titles) else StageRelays.Relay.Unbilled
  /** Its title read as a house's relay of a work in a season ([[RelayStage.of]]), once per listing: every candidate asks. */
  private[identity] lazy val relayBilled: Option[RelayStage.Billed] = RelayStage.of(this)
  /** The production its title bills, read as data ([[RelayStage.production]]: house, work, season, encore, live), once per
   *  listing: every pair of a family's nodes asks it. */
  private[identity] lazy val production: Option[RelayStage.Production] = RelayStage.production(this)
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
  /** The title and raw title, each also as ingest cleans it ([[services.titles.FormatTags]]), as yearless tokens
   *  (`StageRelays.billing`): a box office's certificate is no part of the house's billing. UK "NT LIVE: Golden Boy (15)" billed
   *  "Golden Boy" under no banner, its work not at the title's edge, and measured an `overlap` of "Golden Boy (NT Live)". */
  private[identity] lazy val billedTitles: Seq[Seq[String]] =
    ownTitles.flatMap(t => Seq(t, BillingParse(t).formatless)).map(IdentityMeasures.yearlessTokens).distinct
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
  /** The original title as the `originalTitle` measures read it: without the screen-format and language-version tags
   *  a feed bills in it -- the tags that are a showing's badge ([[services.titles.FormatTags.extractFormatTags]]'s
   *  tokens: OmU, OmeU, OV, 3D, IMAX, …), read off as the title's own are. DE Moviemento's "Die Blutgräfin - The Blood
   *  Countess (OmeU)" bills the showing's English-subtitled version, no word of the film's title; read with it, the
   *  original was a `segment` of Ottinger's "Die Blutgräfin" and the cannot-link cut denied the film. A note that is
   *  no badge stays the field's text: Viktoria's "DAS BOOT (DIRECTOR'S CUT)(WA:2023)" re-release mark, read off, left
   *  a copy of the title and the stub record of the cut (1794076) took it. Searches still ask the original as
   *  published. */
  private[identity] lazy val measuredOriginal: Option[String] =
    originalTitle.map(_.trim).filter(_.nonEmpty).flatMap { original =>
      services.titles.FormatTags.extractFormatTags(original) match {
        case (clean, tokens) if tokens.nonEmpty => Some(clean.trim).filter(_.nonEmpty)
        case _                                  => Some(original)
      }
    }
  /** [[measuredOriginal]] as a comparison form, once per listing (`originalTitleRelation`). */
  private[identity] lazy val originalForm: Option[IdentityMeasures.OriginalForm] = measuredOriginal.map(IdentityMeasures.OriginalForm(_))
}

object MeasuredListing {
  /** What a title relation reads of a listing ([[FamilyScope]]'s relations, the qualifiers' memo). The decorations by identity:
   *  one resolve's listings share its instance, and its structural hash was itself a cost. */
  final class TitleInputs(listing: MeasuredListing) {
    private val title         = listing.title
    private val rawTitle      = listing.rawTitle
    private val originalTitle = listing.originalTitle
    private val searchTitles  = listing.searchTitles
    private val decorations   = listing.decorations
    override val hashCode: Int = (title, rawTitle, originalTitle, searchTitles).## * 31 + System.identityHashCode(decorations)
    override def equals(other: Any): Boolean = other match {
      case that: TitleInputs => (that.decorations eq decorations) && that.title == title && that.rawTitle == rawTitle &&
        that.originalTitle == originalTitle && that.searchTitles == searchTitles
      case _ => false
    }
  }
}
