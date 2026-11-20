package services.identity

import services.titles.{TitleWords, YearWindow}
import services.identity.vocabulary.PopularityBucket

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
  import Measure.{Category, Missing, Number}

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
  def certificateRelation(l: MeasuredListing, f: Film, country: Option[String], title: Category): Measure =
    if (l.certificate.isDefined && !CertifiedRelations(title.value)) Missing("piece")
    else country.flatMap(c => Certificates.Ladders.get(c).map(c -> _)).fold[Measure](Missing("unrated")) { case (c, ladder) =>
      l.certificate.flatMap(ladder.step).fold[Measure](MissingListing) { own =>
        Certificates.in(f.certificates, c).flatMap(ladder.step).fold[Measure](MissingFilm)(rated => certificateGap(rated - own))
      }
    }

  /** A film rated `gap` steps above the venue's certificate, as [[certificateRelation]] names it. */
  private def certificateGap(gap: Int): Category =
    Category(if (gap == 0) "match" else if (math.abs(gap) < Certificates.StepsBelow) "near" else if (gap > 0) "below" else "above")

  /** The listing-film measures that read the TITLE itself — how it names the film, and the
   *  instalment numbers it carries: a title alone never vetoes, so none of them is a fact. */
  val TitleMeasures: Set[String] = Set("title", "numeral")

  /** The listing-film measures that compare a FACT the listing published beside its title — a
   *  year (field, bracket or season), a director, a runtime, a country, an original title: every
   *  measure but the title measures, the ranking priors and the pooled count. Derived from
   *  [[listingFilm]] itself, so a new measure is a fact unless it is classified otherwise. */
  lazy val FactMeasures: Set[String] =
    listingFilm(MeasuredListing(""), Film(""), None, 0, 0).keySet -- RankingPriors -- PooledMeasures -- TitleMeasures -- GuardMeasures

  /** The listing-listing measure that says WHERE the two listings are (one venue or two), not
   *  anything either published about its film. */
  val PlacementMeasures: Set[String] = Set("venue")

  /** The listing-listing measures that compare a FACT both listings published beside their titles —
   *  a year (field, bracket or season), a director, a runtime, an original title, a chain's id:
   *  every measure but the title relation and the placement. Derived from [[listingListing]]
   *  itself, as [[FactMeasures]] is from [[listingFilm]]. */
  lazy val ListingFactMeasures: Set[String] =
    listingListing(MeasuredListing(""), MeasuredListing(""), sameVenue = false, sharedChainId = None).keySet -- PlacementMeasures - "title"

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
  def repeatsItsTitle(l: MeasuredListing): Boolean =
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
    def tell(verdict: Option[(String, Boolean)]): Unit = verdict.foreach { case (name, agrees) => if (agrees) agree += name else deny += name }
    tell(publishedYearVerdict(m)); tell(titleYearVerdict(m)); tell(directorVerdict(m)); tell(originalTitleVerdict(m))
    (agree.result(), deny.result())
  }
  /** A published year agrees or denies; a year in the title only agrees ([[titleYearVerdict]]) — a bracket is as often a
   *  re-release's year as the film's. Each verdict: the corroborator, and whether it agrees (else it denies). */
  private def publishedYearVerdict(m: Map[String, Measure]): Option[(String, Boolean)] =
    m.get("year.distance").collect { case Number(d) => "year" -> (d <= YearWindow.PublishedAdjacency) }
  private def titleYearVerdict(m: Map[String, Measure]): Option[(String, Boolean)] =
    m.get("titleYear.delta").collect { case Number(d) if FactRelations.nearDelta(d) => "year" -> true }
  private def directorVerdict(m: Map[String, Measure]): Option[(String, Boolean)] =
    m.get("director").collect { case Category(c) if c == SamePersonMeasure.value || c == "different" => "director" -> (c == SamePersonMeasure.value) }
  private def originalTitleVerdict(m: Map[String, Measure]): Option[(String, Boolean)] =
    m.get("originalTitle").collect { case Category(c) if c == "match" || c == "disjoint" => "originalTitle" -> (c == "match") }

  // ── the measurement sets ─────────────────────────────────────────────────────────────

  /**
   * A listing against a candidate film.
   *
   * @param searchRank 1-based rank of the film in the listing's OWN title search, if it was there
   * @param rivals     how many OTHER candidates of the listing's pool its title names as closely
   *                   (`exact`, `original` or `alternative`)
   * @param corroboratingVenues how many OTHER venues listing the same title publish this film's
   *                   exact year or credit its director — the family's pooled evidence
   * @param houses     the houses the listings' banners were learned to be ([[BannerHouses.learn]])
   * @param qualifiers the title pieces learned to be qualifiers, not works ([[Qualifiers.learn]])
   */
  def listingFilm(l: MeasuredListing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int,
                  houses: BannerHouses = BannerHouses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown, country: Option[String] = None): Map[String, Measure] =
    listingFilmTitled(l, f, searchRank, rivals, corroboratingVenues, titleRelation(l, f, houses, qualifiers), country)

  /** The `country` measure's category against a concert's director-less record ([[Film.concertRecord]]): its own
   *  calibrated weight, weaker than a mismatch's, and every learned rule reading a mismatch reads it too. */
  val ConcertMismatch: Category = Category("concert-mismatch")

  private def concertCountry(f: Film, country: Measure): Measure =
    if (country == Category("mismatch") && f.concertRecord) ConcertMismatch else country

  /** The listing's directors against the film's credits ([[CreditRelation.of]]), a name that is the film's own house
   *  left out ([[namesItsHouse]]): the listing-film `director` measure. */
  private def directorCreditRelation(l: MeasuredListing, f: Film): Measure = {
    // Each name's and title's tokens read once per listing and film, not per pair ([[MeasuredListing.directorTokens]]).
    val persons = if (l.directors.isEmpty) l.directors else
      l.directors.iterator.zip(l.directorTokens).collect { case (name, words) if !namesItsHouse(name, words, f) => name }.toSeq
    f.directorCredits.fold[Measure](if (persons.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
      CreditRelation.of(if (persons.size == l.directors.size) l.directorCredits else new CreditRelation.Credits(persons), _))
  }

  /** [[listingFilm]] with the title relation already read — `titleRelation(l, f, houses, qualifiers)` —
   *  by a caller that relates the listing to the film anyway (`FamilyScope.score`). `country`: the venue's (ISO-3166-1),
   *  whose cinema releases of the film its year is read against first ([[Film.closestYear]]). */
  def listingFilmTitled(l: MeasuredListing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int,
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
    slots(7)  = directorCreditRelation(l, f)
    slots(8)  = runtimeDelta(l, f, country, title)
    slots(9)  = concertCountry(f, countryRelation(l.countries, f.countries))
    slots(10) = searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble))
    slots(11) = f.popularity.fold[Measure](MissingFilm)(p => Number(PopularityBucket.of(p).toDouble))
    slots(12) = Number(rivals.toDouble)
    slots(13) = Number(corroboratingVenues.toDouble)
    slots(14) = certificateRelation(l, f, country, title)
    ListingYearReadings.read(l, f, country, corroboratingVenues, title, new ListingFilmMeasures(slots))
  }

  /** How many years an anniversary one of `titles` bills celebrates ("30th Anniversary": 30), the first that bills one. */
  private[identity] def anniversaryOf(titles: Seq[String]): Option[Int] =
    titles.iterator.flatMap(t => AnniversaryYears.findFirstMatchIn(t)).map(_.group(1).toInt).nextOption()
  /** "30th Anniversary", "30 Aniversario", "35-jähriges Jubiläum", "35. rocznica", "35e anniversaire": how many years a
   *  re-release celebrates — the number, its ordinal ending, and an anniversary word of a language venues bill in. */
  private[identity] val AnniversaryYears =
    ("""(?iU)\b(\d{1,3})(?:st|nd|rd|th|e|ème|°)?\.?(?:\s*-\s*|\s+)(?:jährige[smn]?\s+)?""" +
      TitleWords.rawPattern(TitleWords.Concept.Anniversary) + """\b""").r

  /** The listing's original title against the film's titles — unless it only repeats the
   *  listing's own title ([[repeatsItsTitle]]) and that title names the film ([[NamingRelations]]):
   *  then it is the title again, and the title relation already says the film is named, so it
   *  counts only where it agrees (the film carries it whole, `match`) and is otherwise absent —
   *  in the score and a veto alike. Counted, a truncated copy ("…: Live") read as a fragment of the
   *  very record the title names exactly. Beside a film the title does NOT name, the repeat stays
   *  what it measures: Everyman's "Dracula (4K Restoration)" is not The Mummy its cinematographer
   *  directed. A field that is the title with the showing's version tag ([[measuredOriginal]]: ZOO PALAST's "OmU: In
   *  Memoriam") is the title again beside a film the title names, agreeing or not: absent, its tag no more a mismatch
   *  than its copy an agreement (read as a match, it took the venue's "In Memoriam – Gegen ihren Willen" as Brasó's 1977
   *  "In memoriam"). Beside a film the title does not name it stays what it measures, as the plain repeat does:
   *  Metropolis Bochum's "… Starlight Express - Live on Stage (OmeU)" is no "SpongeBob Musical: Live on Stage!". */
  private def ownOriginalTitle(l: MeasuredListing, f: Film, title: Category): Measure =
    if (NamingRelations(title.value) && l.measuredOriginal != l.originalTitle.map(_.trim).filter(_.nonEmpty) &&
        originalTitleRelation(l.measuredOriginal, l.rawTitle.toSeq :+ l.title).match { case Category(c) => CopiesOfTheTitle(c); case _ => false }) MissingListing
    else originalFormRelation(l.originalForm, f.trimmedForms, f.year) match {
      case m if NamingRelations(title.value) && repeatsItsTitle(l) && m != Category("match") => MissingListing
      case m                                                                                 => m
    }

  /** Titles a listing names itself by, as the other side of a listing-listing comparison. */
  private def asFilm(l: MeasuredListing): Film =
    Film(l.title, l.originalTitle, l.rawTitle.toSeq, l.statedYear, l.statedRuntime, Some(l.directors).filter(_.exists(_.trim.nonEmpty)),
      None, None)

  /**
   * Two listings: are they one film? `sharedChainId` is whether the two venues' chains published
   * an id in a common namespace, and whether it was the same id (`None` when there is no common
   * namespace).
   */
  def listingListing(a: MeasuredListing, b: MeasuredListing, sameVenue: Boolean, sharedChainId: Option[Boolean]): Map[String, Measure] =
    Map(
      "title"         -> listingTitleRelation(a, b),
      "originalTitle" -> listingOriginalTitle(a, b),
      "year.delta"    -> (if (ListingYearReadings.anniversaryDated(a, b) || ListingYearReadings.anniversaryDated(b, a)) MissingListing else absDelta(a.year, b.year)),
      "titleYear.delta" -> absDelta(a.titleYear, b.titleYear),
      "season.delta"  -> absDelta(a.seasonYear, b.seasonYear),
      "director"      -> directorRelation(a.directors, b.directors),
      "runtime.delta" -> YearRuntimeMeasures.listingRuntimeDelta(a, b),
      "venue"         -> Category(if (sameVenue) "same" else "different"),
      "chainId"       -> sharedChainId.fold[Measure](Missing("no-shared-namespace"))(s => Category(if (s) "same" else "different"))
    )

  /** Two listings name each other the same way whichever is measured first: when one's title is a whole delimited piece
   *  of the other's ("Lalka" and "Astra Seniora - Lalka", in either order) the pair is a `segment`, as the resolver's
   *  title-segment must-link reads it — measured one way only, the plain spelling first read as a `fragment` or an
   *  `overlap` of its decorated one. */
  private def listingTitleRelation(a: MeasuredListing, b: MeasuredListing): Category =
    (titleRelation(a, asFilm(b), None, Qualifiers.Unknown), titleRelation(b, asFilm(a), None, Qualifiers.Unknown)) match {
      case (Category("original") | Category("alternative"), _) => Category("exact")
      case (forward, backward) if forward != Category("exact") && backward == Category("segment") => backward
      case (forward, _)                                        => forward
    }

  /** One listing's original title against the other's, each as the `originalTitle` measures read it. */
  private def listingOriginalTitle(a: MeasuredListing, b: MeasuredListing): Measure =
    (a.measuredOriginal, b.measuredOriginal) match {
      case (Some(oa), Some(ob)) => originalTitleRelation(Some(oa), Seq(ob))
      case (None, _)           => MissingListing
      case (_, None)           => MissingFilm
    }
}
