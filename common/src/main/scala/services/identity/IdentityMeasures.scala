package services.identity

import java.util.Locale

import services.movies.{EmbeddedYear, TitleContainment}
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

  /** What one venue published about one film. `decorations`: the venue decorations learned around
   *  titles (`TitleDecorations`), which the title shapes read — not a fact the venue published. */
  final case class Listing(title: String, rawTitle: Option[String] = None, originalTitle: Option[String] = None,
                           year: Option[Int] = None, runtime: Option[Int] = None, directors: Seq[String] = Nil,
                           countries: Seq[String] = Nil, yearCredits: Option[Seq[String]] = None,
                           decorations: TitleDecorations = TitleDecorations.None, searchTitles: Seq[String] = Nil,
                           /** The season a stage relay billing neither its season nor a year screens in
                            *  ([[services.identity.Listing.broadcastSeason]]): searched with its work, never measured. */
                           broadcastSeason: Option[Int] = None) {
    private def titles: Seq[String] = rawTitle.toSeq :+ title
    /** The directors credited beside the published `year` — one listing's own, unless a pooled
     *  read took its year and its credits from different listings (`yearCredits`). */
    def creditedBesideYear: Seq[String] = yearCredits.getOrElse(directors)
    /** The season the title names ("2026/27"), by its first year. */
    lazy val seasonYear: Option[Int] = IdentityMeasures.seasonYear(titles)
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
    /** `titleShapes`, once per listing: every title relation and billing reads them. */
    private[identity] lazy val shapes: Seq[String] = IdentityMeasures.shapesOf(this)
    /** What a title relation reads of this listing, hashed once: `FamilyScope` shares relations by it. */
    private[identity] lazy val titleInputs: FamilyScope.TitleInputs = new FamilyScope.TitleInputs(this)
    /** The title and raw title as comparison forms, and the shapes' keys, once per listing: the
     *  resolver relates every listing to every film of its family's pool (`titleRelation`). */
    private[identity] lazy val ownForms: Seq[IdentityMeasures.TitleForm] = (Seq(title) ++ rawTitle).map(IdentityMeasures.TitleForm(_))
    /** The own forms' non-empty keys, once per listing: every pool candidate's `titleRelation` asks. */
    private[identity] lazy val ownKeys: Set[String] = ownForms.map(_.key).filter(_.nonEmpty).toSet
    /** How many years an anniversary the title or original title bills celebrates ("30th Anniversary"), once per listing. */
    private[identity] lazy val anniversary: Option[Int] = (Seq(title) ++ rawTitle ++ originalTitle).iterator
      .flatMap(t => IdentityMeasures.AnniversaryYears.findFirstMatchIn(t)).map(_.group(1).toInt).nextOption()
    /** The title's words, once per listing: a director's surname heading it is read against every candidate. */
    private[identity] lazy val titleWords: Seq[String] = ownForms.head.words
    private[identity] lazy val shapeKeys: Seq[String] = shapes.map(IdentityMeasures.key)
    private[identity] lazy val shapeWords: Seq[Seq[String]] = shapes.map(IdentityMeasures.words)
    /** The shapes only the learned decorations leave, once per listing (usually none): a relation's scoring reads them
     *  per candidate, and each read derived the undecorated copy's shapes again. */
    private[identity] lazy val decoratedOnlyShapes: Seq[String] =
      if (decorations == TitleDecorations.None) Nil else (shapes.toSet -- copy(decorations = TitleDecorations.None).shapes).toSeq.sorted
    /** The title with a learned programme decoration stripped ("Horror Season 2026 …"), as words:
     *  what the venue's banner leaves of it. Empty when no learned decoration applies. */
    private[identity] lazy val undecoratedWords: Seq[Seq[String]] =
      (Seq(title) ++ rawTitle).map(t => IdentityMeasures.creditedWork(t).getOrElse(t)).flatMap(decorations.strip)
        .map(IdentityMeasures.words).filter(_.nonEmpty).distinct
    /** The title and raw title, and the shapes, as yearless tokens (`billing`). */
    private[identity] lazy val billedTitles: Seq[Seq[String]] = (Seq(title) ++ rawTitle).map(IdentityMeasures.yearlessTokens).distinct
    private[identity] lazy val billedWorks: Set[Seq[String]] = shapes.map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
    /** Does the venue publish a FACT beside its title — a year (field, bracket or season), a
     *  running time, a director, a country, or an original title that is not its title again
     *  ([[IdentityMeasures.repeatsItsTitle]])? */
    def publishesAFact: Boolean =
      statedYear.isDefined || seasonYear.isDefined || statedRuntime.isDefined || directors.exists(_.trim.nonEmpty) ||
        countries.nonEmpty || (originalTitle.exists(_.trim.nonEmpty) && !IdentityMeasures.repeatsItsTitle(this))
    /** The shapes as series and numbers (`numeralRelation`). */
    private[identity] lazy val numberedShapes: Seq[IdentityMeasures.Numbered] = shapes.map(IdentityMeasures.numbered)
    /** The directors, and those credited beside the year, parsed once per listing (`directorRelation`). */
    private[identity] lazy val directorCredits: IdentityMeasures.Credits   = new IdentityMeasures.Credits(directors)
    /** Its search titles' [[IdentityMeasures.key]]s, in order: worked out once, not per film it is weighed against. */
    private[identity] lazy val searchKeys: Seq[String] = searchTitles.map(IdentityMeasures.key)
    /** Each credited director's name as title tokens, in order. */
    private[identity] lazy val directorTokens: Seq[Seq[String]] = directors.map(TitleContainment.tokens)
    private[identity] lazy val creditsBesideYear: IdentityMeasures.Credits =
      if (yearCredits.isEmpty) directorCredits else new IdentityMeasures.Credits(creditedBesideYear)
    /** The original title, trimmed, as a comparison form, once per listing (`originalTitleRelation`). */
    private[identity] lazy val originalForm: Option[IdentityMeasures.OriginalForm] =
      originalTitle.map(_.trim).filter(_.nonEmpty).map(IdentityMeasures.OriginalForm(_))
  }

  /** A running time in brackets, marked as minutes by a prime or an apostrophe ("97’", "97'", "97′")
   *  or by "min" — the only way a number in a title says it is a duration. One value, or none. */
  private val BracketedRuntime = """(?i)[(\[]\s*(\d{2,3})\s*(?:['’′]|min\.?|mins\.?)\s*[)\]]""".r
  def bracketedRuntime(titles: Seq[String]): Option[Int] =
    titles.iterator.flatMap(BracketedRuntime.findAllMatchIn).map(_.group(1).toInt).filter(_ > 0).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }

  /** A SEASON written into a title — "2026/27", "2026-27", "2026/2027", "2026–2027": a year and the
   *  next, which is how a broadcast or programme series names its run, never a film's year. */
  private val Season = """(?<![\p{N}])((?:18|19|20)\d{2})\s*[/–—-]\s*(\d{4}|\d{2})(?![\p{N}])""".r
  private def seasonStart(m: scala.util.matching.Regex.Match): Option[Int] = {
    val start = m.group(1).toInt
    val end   = m.group(2)
    Option.when((end.length == 4 && end.toInt == start + 1) || (end.length == 2 && end.toInt == (start + 1) % 100))(start)
  }
  /** A season or a bracketed year needs an ASCII digit (`\d` is ASCII-only in Java): a title without
   *  one skips their regexes. */
  private def hasAsciiDigit(t: String): Boolean = t.exists(c => c >= '0' && c <= '9')
  def seasonYear(titles: Seq[String]): Option[Int] =
    titles.iterator.filter(hasAsciiDigit).flatMap(Season.findAllMatchIn).flatMap(seasonStart).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }
  /** The season a film's own titles name ("The Metropolitan Opera 2026/27: Macbeth"). */
  def filmSeason(f: Film): Option[Int] = f.season

  /** Does the film's own title name the listing's SEASON PRODUCTION: both name the same season,
   *  and they share a whole title segment outside it — the work. "Met Opera 2026-27: Samson et
   *  Dalila" and the film database's "The Metropolitan Opera 2026/27: Samson et Dalila" are one
   *  season's production of one work, however each spells the house. Segments are the listing's
   *  own delimiters (`SearchTitles.candidates`) on both sides; a segment carrying the season is
   *  the banner, never the work. */
  def namesSeasonProduction(l: Listing, f: Film): Boolean = {
    val shared = seasonWorks(l) intersect seasonWorks(f)
    shared.exists { case (work, _) => !work.startsWith("work:") } || (shared.nonEmpty && bannersMeet(l.title +: l.rawTitle.toSeq, f.titles))
  }

  /** Where a title's own delimiters cut it into pieces: a colon or pipe, or a dash or slash spaced on both sides. */
  private val PieceBreak = java.util.regex.Pattern.compile("""\s*[:|]\s*|\s+[-–—/]\s+""")
  /** A season production met only through a work named in two languages ([[StageWorks]]) is the record's only when
   *  the two titles' banners — the pieces naming no work — share a word: PL "Balet z Opery Paryskiej 2026-2027:
   *  Jezioro łabędzie" ×16 is the Paris Opera Ballet's Swan Lake, which TMDB has no record of, not "Royal Ballet & Opera
   *  2026/27: Swan Lake"; "Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów" shares "royal", "ballet",
   *  "opera" with its record. Words of four letters or more, so "the" or "and" meets nothing. */
  private def bannersMeet(listingTitles: Seq[String], filmTitles: Seq[String]): Boolean = {
    def bannerWords(titles: Seq[String]) = titles.flatMap(t => PieceBreak.split(withoutYears(t)).toSeq)
      .filter(piece => StageWorks.resolver.named(key(piece)).isEmpty)
      .flatMap(TitleContainment.tokens).filter(word => word.length >= 4 && !word.forall(_.isDigit)).toSet
    (bannerWords(listingTitles) intersect bannerWords(filmTitles)).nonEmpty
  }

  /** The (work, season) pairs a listing's title names: each whole segment outside its season, keyed,
   *  beside the season. [[namesSeasonProduction]] is exactly two sides' pairs meeting, so a record
   *  of the listing's season production can be found by pair, whoever searched it up. */
  def seasonWorks(l: Listing): Set[(String, Int)] =
    l.seasonYear.fold(Set.empty[(String, Int)])(season => seasonlessWorks(titleShapes(l)).map(_ -> season))

  /** A film's (work, season) pairs, as [[seasonWorks]] reads a listing's. */
  def seasonWorks(f: Film): Set[(String, Int)] =
    filmSeason(f).fold(Set.empty[(String, Int)])(season => seasonlessWorks(f.titles.flatMap(SearchTitles.candidates(_, None))).map(_ -> season))

  /** Each segment's key — and each stage work it names, in whatever language ([[StageWorks]]): PL's "Royal Ballet and
   *  Opera Sezon Kinowy 2026-27: Dziadek do orzechów" is the season's "Royal Ballet & Opera 2026/27: The Nutcracker". */
  private val SentenceStop = java.util.regex.Pattern.compile("""\.\s+""")
  private def seasonlessWorks(titles: Seq[String]): Set[String] = {
    val seasonless = titles.filter(t => seasonYear(Seq(t)).isEmpty)
    val keys = seasonless.map(key).filter(_.nonEmpty).toSet
    // a work's name may run on into a translated subtitle after a full stop: "Cosi fan tutte. Tak czynią wszystkie"
    val named = keys ++ seasonless.map(t => key(SentenceStop.split(t, 2).head)).filter(_.nonEmpty)
    keys ++ named.flatMap(StageWorks.resolver.named).map(work => s"work:$work")
  }

  /** A title read as a HOUSE BILLING A WORK: the work both titles carry as a whole delimited
   *  piece (a title shape of each), and what each title adds to it along one edge — its banner,
   *  "how the title spells its house" ("NT Live: Dr. Strangelove" and "National Theatre Live:
   *  Dr. Strangelove" bill `drstrangelove` as `ntlive` and `nationaltheatrelive`). Seasons and
   *  bracketed years are no part of a house's name, on either side. Keys, by [[key]]. */
  final case class Billing(listingWords: Seq[String], filmWords: Seq[String], work: String) {
    def listingHouse: String = listingWords.mkString
    def filmHouse: String    = filmWords.mkString
  }

  /** How the listing and the film bill one work (the longest they share), when both titles add a
   *  banner to it. `None`: no shared work, or one title is the work alone. */
  def billing(l: Listing, f: Film): Option[Billing] = billings(l, f).headOption

  /** EVERY way the listing and the film bill the longest work they share under banners, in
   *  [[billing]]'s order: a record carrying its broadcast title beside an alternative streaming
   *  title ("National Theatre at Home: …") bills the work under both. */
  def billings(listing: Listing, film: Film): Seq[Billing] = {
    val works = (listing.billedWorks intersect film.billedWorks).toSeq.sortBy(work => (-work.length, work.mkString(" ")))
    works.iterator.map(work => billedUnder(listing, work, film, work, work.mkString)).find(_.nonEmpty).getOrElse(Nil)
  }

  /** The banners a title puts on a work it carries — its words before or after the work. */
  private def bannerOf(title: Seq[String], work: Seq[String]): Option[Seq[String]] =
    Option.when(title.lengthIs > work.length)(
      if (title.endsWith(work)) Some(title.dropRight(work.length)) else if (title.startsWith(work)) Some(title.drop(work.length)) else None
    ).flatten

  /** Every way the listing bills `listingWork` and the film `filmWork` under banners, as the one work `work`. */
  private def billedUnder(listing: Listing, listingWork: Seq[String], film: Film, filmWork: Seq[String], work: String): Seq[Billing] =
    (for {
      listingHouse <- listing.billedTitles.flatMap(bannerOf(_, listingWork))
      filmHouse    <- film.billedTitles.flatMap(bannerOf(_, filmWork))
    } yield Billing(listingHouse, filmHouse, work))
      .sortBy(billed => (billed.listingHouse, billed.filmHouse, billed.listingWords.mkString(" "), billed.filmWords.mkString(" ")))

  /** How the listing and the film bill one STAGE WORK under banners, each in its own language ([[StageWorks]]), when no
   *  work of theirs is the same words: PL "Makbet | metropolitan opera: live in hd 2026/27" and the Met's "The
   *  Metropolitan Opera 2026/27: Macbeth". What a season's banner is learned from ([[Houses.evidence]]): billed by its
   *  literal works alone, Kino 1410's Met banner met only Royal Ballet & Opera's records of "Carmen" and "Così fan
   *  tutte", and was learned as RBO's — whose season productions it then took. */
  def stageBilling(listing: Listing, film: Film): Option[Billing] = {
    def staged(works: Set[Seq[String]]) = works.toSeq.flatMap(work => StageWorks.resolver.named(key(work.mkString(" "))).map(_ -> work))
    val filmWorks = staged(film.billedWorks).groupMap(_._1)(_._2)
    staged(listing.billedWorks).sortBy { case (id, work) => (-work.length, id, work.mkString(" ")) }.iterator
      .map { case (id, listingWork) => filmWorks.getOrElse(id, Nil).sortBy(_.mkString(" ")).flatMap(billedUnder(listing, listingWork, film, _, s"work:$id")) }
      .find(_.nonEmpty).flatMap(_.headOption)
  }

  /** Does the film's record bill the listing's work under the listing's OWN house — the banner the
   *  listing puts on the work, spelt as the record spells it or learned to be it (`Houses.same`)?
   *  "NT Live: The Misanthrope" and "National Theatre Live: The Misanthrope" do — by any of the
   *  record's titles, its "National Theatre at Home" alternative aside ([[billings]]); a record titled
   *  the work alone ("The Misanthrope") bills no house at all. A listing naming a season takes only a
   *  record naming none — one naming a season is `namesSeasonProduction`'s to read — whose year the
   *  season spans (`ListingConstraints.seasonsApart` denies the rest): TMDB's US title of the Met's
   *  2026 Così is "The Metropolitan Opera: Così fan tutte", which US "Met Opera 2026-27: Così fan
   *  tutte" ×356 is. A season's record is a season-free listing's only when the
   *  listing's banner spells the house ([[spellsItsHouse]]): TMDB filing the Paris Opera's works only
   *  under the Met's 2026/27 records teaches the Paris banner to be the Met. And not a banner numbering its edition otherwise than the record's: a
   *  house's name carries no number ("League of Legends Worlds 26" is not "… Worlds25"). */
  def billsUnderItsHouse(listing: Listing, film: Film, houses: Houses): Boolean =
    (listing.seasonYear.isEmpty || filmSeason(film).isEmpty) &&
      billings(listing, film).exists(billed => houses.same(billed) && numbersIn(billed.listingHouse) == numbersIn(billed.filmHouse) &&
        (filmSeason(film).isEmpty || spellsItsHouse(billed)))
  /** Does the listing's banner SPELL the record's house — two of its words or more ("Royal Ballet and
   *  Opera" of "Royal Ballet & Opera")? A season-free listing takes a season's record only then: the
   *  Paris Opera's banner, learned as the Met from TMDB's filing, shares only "opera". */
  private def spellsItsHouse(billed: Billing): Boolean = (billed.listingWords.toSet intersect billed.filmWords.toSet).sizeIs >= 2
  private val Digits = "\\d+".r
  private def numbersIn(house: String): Set[String] = Digits.findAllIn(house).toSet

  /** Which house each listing banner is, LEARNED from how the film records of its works bill them
   *  (`learn`): no house, banner or abbreviation is known in advance. */
  final case class Houses(of: Map[String, String]) {
    /** Do the listing and the film bill the work under one house — the same spelling, or the house
     *  the listing's banner was learned to be? */
    def same(b: Billing): Boolean = b.listingHouse == b.filmHouse || of.get(b.listingHouse).contains(b.filmHouse)
    /** Is the listing's banner known to be ANOTHER house than the film's? */
    def other(b: Billing): Boolean = !same(b) && of.contains(b.listingHouse)
  }
  object Houses {
    val Unknown: Houses = Houses(Map.empty)

    /** A banner's house, among the houses whose records bill its works: the one whose name shares
     *  the most of the banner's words ("metropolitan opera: live in hd" is the Metropolitan Opera,
     *  though Royal Ballet & Opera's records bill more of its works), else — on a tie in words —
     *  the one billing the most of its DISTINCT works, at least two: "RBO Cinema Season" is Royal
     *  Ballet & Opera because its Swan Lake and Alice are, though TMDB files its Manon only under
     *  the Met; "NT Live" is National Theatre Live because every play it bills is. One work is a
     *  coincidence — a programme banner showing a film a house also staged — not a house, unless the
     *  banner's words name it. A tie on both is no house. */
    def learn(billings: Iterable[Billing]): Houses =
      Houses(ranking(billings).flatMap { case (banner, ranked) => chosen(ranked).map(banner -> _.house) })

    /** One house a banner's billings name, with how many of the banner's words its name shares
     *  (of the `named` words its name has) and how many of the banner's DISTINCT works it bills. */
    final case class Contender(house: String, spelt: Int, works: Int, named: Int = 1) {
      /** Does the banner NAME the house — two of its words, or a one-word house's one? One word of more
       *  ("maastricht" of "Love in Maastricht") is a coincidence of vocabulary, as one work is of billing. */
      def spells: Boolean = spelt >= 2 || (spelt >= 1 && spelt == named)
      def render: String = s"$house (words $spelt, works $works)"
    }

    /** Each banner's contending houses, best first — what [[learn]] chooses among. */
    def ranking(billings: Iterable[Billing]): Map[String, Seq[Contender]] =
      billings.toSeq.distinct.groupBy(_.listingHouse).map { case (banner, bannerBillings) =>
        val words = bannerBillings.flatMap(_.listingWords).toSet
        banner -> bannerBillings.groupBy(_.filmHouse).toSeq.map { case (house, houseBillings) =>
          val houseWords = houseBillings.flatMap(_.filmWords).toSet
          Contender(house, (houseWords intersect words).size, houseBillings.map(_.work).distinct.size, houseWords.size)
        }.sortBy(contender => (-contender.spelt, -contender.works, contender.house))
      }

    /** The house [[learn]] takes from a banner's ranked contenders, if any. */
    def chosen(ranked: Seq[Contender]): Option[Contender] = {
      val best = ranked.head
      val next = ranked.lift(1)
      val spelt  = best.spells && next.forall(_.spelt < best.spelt)
      val billed = best.works >= 2 && next.forall(runnerUp => runnerUp.spelt < best.spelt || runnerUp.works < best.works)
      Option.when(spelt || billed)(best)
    }

    /** What a listing's candidates say about its banner: how each record of its work bills it — of
     *  its season, when the listing names one (another season's record says nothing about which
     *  house this season's broadcast is). */
    def evidence(l: Listing, films: Iterable[Film]): Iterable[Billing] =
      films.filter(f => l.seasonYear.isEmpty || namesSeasonProduction(l, f))
        .flatMap(f => billing(l, f).orElse(Option.when(l.seasonYear.isDefined)(stageBilling(l, f)).flatten))
  }

  /** Which pieces of a title are its QUALIFIER rather than its work, LEARNED from how the film
   *  records of one family's pool bill each piece, and on which SIDE of the work: TMDB bills
   *  "Director's Cut" after many works ("Chocolate - Director's Cut", "The Great War: Director's
   *  Cut", "The Promise (Director's Cut)") and "The Final Cut" after Pink Floyd and Straw Dogs, as
   *  "Dark City: Director's Cut" and "Michael Mann's Manhunter: The Final Cut" do — so there they
   *  are the listings' qualifiers. A franchise LEADS its sequels ("Obcy: Przymierze", "Obcy:
   *  Romulus"), so under a banner ("Tani wtorek: Obcy") it is still the work. `companions`: for
   *  each piece, by its yearless key and whether it trails, how many distinct other pieces a record
   *  title bills it beside ([[Qualifiers.split]]). No edition, cut or banner word is known in
   *  advance. */
  final case class Qualifiers(companions: Map[(String, Boolean), Int], works: Set[Seq[String]] = Set.empty) {
    private def count(piece: Seq[String], trails: Boolean): Int = companions.getOrElse((piece.mkString, trails), 0)
    /** The listing's qualifier pieces, by key: each piece its whole title adds another to along one
     *  edge, which records bill on the same side beside at least two works — one is a coincidence,
     *  as a house's is ([[Houses.learn]]) — and beside more than they bill the rest of the title on
     *  its side. A tie is no qualifier, and neither is a piece the listing publishes as its original
     *  title: the venue names it as the film ("Cineworld 30: The Dark Knight", originally "The Dark
     *  Knight", though TMDB also bills "Enter the World of Hans Zimmer: The Dark Knight"). Nor is a
     *  LEADING piece some record is titled whole when the rest carries no record's whole title: a
     *  work leads its sequels, so "Dracula (4K Restoration)" is Dracula however many TMDB bills after
     *  "Dracula" — nor a TRAILING one after a learned venue decoration, for the same reason: "KINO SENIORA |
     *  Primetime" is Primetime however many records bill "Primetime" after a work. Beside a work the piece stays a cut or an edition
     *  ("Dark City: Director's Cut"), whatever record carries it alone. */
    def of(l: Listing): Set[String] = memo.getOrElseUpdate(l.titleInputs, {
      val named = l.originalTitle.map(yearlessTokens(_).mkString)
      // A trailing piece only when what leads it is a learned venue decoration ("KINO SENIORA"): a rest
      // merely missing from these records may still be a work ("Dark City" of "Dark City: Director's Cut").
      lazy val undecorated = (Seq(l.title) ++ l.rawTitle).flatMap(l.decorations.strip).map(yearlessTokens).toSet
      def leavesNoWork(piece: Seq[String], rest: Seq[String], trails: Boolean) =
        works(piece) && !works.exists(work => rest.containsSlice(work)) && (!trails || undecorated(piece))
      (Seq(l.title) ++ l.rawTitle).flatMap(Qualifiers.split).collect {
        case (piece, rest, trails) if count(piece, trails) >= 2 && count(piece, trails) > count(rest, !trails) && !named.contains(piece.mkString) &&
            !leavesNoWork(piece, rest, trails) =>
          piece.mkString
      }.toSet
    })
    // By the titles it reads, hashed once per listing: a listing's own hash walks its decorations' whole learned set.
    private val memo = scala.collection.concurrent.TrieMap.empty[FamilyScope.TitleInputs, Set[String]]
  }
  object Qualifiers {
    val Unknown: Qualifiers = Qualifiers(Map.empty)

    /** A title's pieces, each with the rest of the title and whether it TRAILS the rest: every
     *  delimited piece of it ([[shapes]]) that is a token run along one of its edges, and the rest,
     *  both ways round, as yearless tokens ("Dark City: Director's Cut" → `director s cut` trailing
     *  `dark city`, and `dark city` leading `director s cut`). */
    def split(title: String): Seq[(Seq[String], Seq[String], Boolean)] = {
      val whole = yearlessTokens(title)
      shapesOfTitle(title).map(yearlessTokens).filter(p => TitleContainment.isTokenRun(p, whole)).flatMap { p =>
        val trails = !whole.startsWith(p)
        val rest   = if (trails) whole.dropRight(p.length) else whole.drop(p.length)
        Seq((p, rest, trails), (rest, p, !trails))
      }.distinct
    }

    /** Learn from the candidate records one family's listings searched up, by their titles and
     *  original titles. */
    def learn(records: Seq[Film]): Qualifiers = {
      val titles = records.flatMap(f => Seq(f.title) ++ f.originalTitle).distinct
      Qualifiers(titles.flatMap(split).map { case (p, r, trails) => ((p.mkString, trails), r.mkString) }.distinct.groupMapReduce(_._1)(_ => 1)(_ + _),
        titles.map(yearlessTokens).filter(_.nonEmpty).toSet)
    }
  }

  /** Is `edition` a record of `work` under a qualifier: a later record one of whose own titles
   *  carries one of the work's as a delimited piece or a token run along one edge ("Radiohead X
   *  Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu", whose record carries "Nosferatu: A
   *  Symphony of Horror")? A record with a title of the work's own is a namesake or a translation
   *  ("Die Puppe", 1975, originally "The Doll", is not an edition of Has's "Lalka", which TMDB
   *  also calls "The Doll"). Each title is read alone: an original title is a whole title, never
   *  a piece. */
  def editionOf(edition: Film, work: Film): Boolean = {
    val relations = (Seq(edition.title) ++ edition.originalTitle).distinct.map(t => titleRelation(Listing(t), work).value)
    edition.year.exists(y => work.year.exists(_ <= y)) &&
      relations.exists(Set("segment", "decorated")) && !relations.exists(Rivalling)
  }

  /** A year in brackets ("(2026)"): a screening's or a production's date, as a season is. */
  private val BracketedYear = """[(\[]\s*(?:18|19|20)\d{2}\s*[)\]]""".r
  /** `t` without its seasons and bracketed years: how a house bills a work, whatever it dates it by. */
  def withoutYears(t: String): String = if (!hasAsciiDigit(t)) t else BracketedYear.replaceAllIn(withoutSeasons(t), " ")
  private[identity] def yearlessTokens(t: String): Seq[String] = TitleContainment.tokens(withoutYears(t))

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def withoutSeasons(t: String): String =
    if (!hasAsciiDigit(t)) t else Season.replaceAllIn(t, m => if (seasonStart(m).isDefined) " " else scala.util.matching.Regex.quoteReplacement(m.matched))

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
                        releases: Option[String] = None) {
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
    /** Its titles' [[IdentityMeasures.key]]s — worked out once, not per listing weighed against it ([[searchGroups]]): a
     *  film's alternative titles can run to dozens, keyed again for every listing of every family it is a candidate of. */
    private[identity] lazy val titleKeys: Set[String] = titles.map(IdentityMeasures.key).toSet
    /** Its own title's and original title's tokens: what a director credited by a house name is read against. */
    private[identity] lazy val ownTitleTokens: Seq[Seq[String]] = (Seq(title) ++ originalTitle).map(TitleContainment.tokens)
    /** Its own title's and original title's yearless tokens. */
    private[identity] lazy val ownYearlessTitles: Seq[Seq[String]] = (Seq(title) ++ originalTitle).map(IdentityMeasures.yearlessTokens)
    /** The film's titles and their delimited pieces as yearless tokens, once per record (`billing`). */
    private[identity] lazy val billedTitles: Seq[Seq[String]] =
      titles.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty).distinct
    private[identity] lazy val billedWorks: Set[Seq[String]] =
      titles.flatMap(SearchTitles.candidates(_, None)).map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
    /** The title, original title and alternative titles, in that order, as comparison forms once
     *  per record (`titleRelation`). */
    private[identity] lazy val forms: Seq[IdentityMeasures.TitleForm] =
      titles.map(IdentityMeasures.TitleForm(_))
    /** The season its titles name, once per record: every node scoring it asks (`evidenceDenies`). */
    private[identity] lazy val season: Option[Int] = IdentityMeasures.seasonYear(titles)
    /** The credited directors, parsed once per record (`directorRelation`); `None` when not fetched. */
    private[identity] lazy val directorCredits: Option[IdentityMeasures.Credits] = directors.map(new IdentityMeasures.Credits(_))
    /** The titles, trimmed and non-empty, as comparison forms once per record: the other side of
     *  every node's `originalTitleRelation` to this film. */
    private[identity] lazy val trimmedForms: Seq[IdentityMeasures.TitleForm] =
      titles.map(_.trim).filter(_.nonEmpty).map(IdentityMeasures.TitleForm(_))
    /** The film's titles as series and numbers (`numeralRelation`). */
    private[identity] lazy val numberedTitles: Seq[IdentityMeasures.Numbered] =
      titles.map(_.trim).filter(_.nonEmpty).distinct.map(IdentityMeasures.numbered)
  }

  /** One measurement: a category, a number, or missing (with which side is missing). */
  sealed trait Measure
  final case class Category(value: String) extends Measure
  final case class Number(value: Double) extends Measure
  final case class Missing(side: String) extends Measure

  val MissingListing: Missing = Missing("listing")
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

  /** The listing-film measures that read the TITLE itself — how it names the film, and the
   *  instalment numbers it carries: a title alone never vetoes, so none of them is a fact. */
  val TitleMeasures: Set[String] = Set("title", "numeral")

  /** The listing-film measures that compare a FACT the listing published beside its title — a
   *  year (field, bracket or season), a director, a runtime, a country, an original title: every
   *  measure but the title measures, the ranking priors and the pooled count. Derived from
   *  [[listingFilm]] itself, so a new measure is a fact unless it is classified otherwise. */
  lazy val FactMeasures: Set[String] =
    listingFilm(Listing(""), Film(""), None, 0, 0).keySet -- RankingPriors -- PooledMeasures -- TitleMeasures

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

  // ── keys ─────────────────────────────────────────────────────────────────────────────

  /** A title or name as a comparison key: accents folded, lowercased, every non-letter and
   *  non-digit dropped. Script-preserving, rule-free: no title-specific canonicalisation. */
  def key(s: String): String =
    tools.TextNormalization.lettersAndDigitsOnly(tools.TextNormalization.deburr(withoutPossessives(s)).toLowerCase(Locale.ROOT))

  /** A possessive "'s" dropped: venues write "Andre Rieu 2026 Christmas Concert" for TMDB's "Andre
   *  Rieu's …" (UK Odeon ×76 read it as a different title and vetoed the concert). Only after an
   *  apostrophe, straight or curly: "Schindlers" stays another spelling. */
  private[identity] def withoutPossessives(s: String): String =
    // Most titles carry no apostrophe, and the pattern needs one: they skip the regex (every key and word split asks).
    if (s.indexOf('\'') < 0 && s.indexOf('\u2019') < 0) s else Possessive.matcher(s).replaceAll("")
  private val Possessive = java.util.regex.Pattern.compile("(?<=\\p{L})['\u2019][sS]\\b")

  /** [[key]] of the title spelt in Latin letters: Cyrillic transliterated letter by letter, as
   *  Polish venues list Ukrainian films ("Potyag Chervona ruta" for "Потяг «Червона Рута»"). Only an
   *  EQUAL transliteration names a title: one letter-by-letter scheme, no fuzzing. */
  def latinKey(s: String): String = {
    val lower = s.toLowerCase(Locale.ROOT)
    if (!lower.exists(Cyrillic.contains)) key(s)
    else key(lower.flatMap(c => Cyrillic.getOrElse(c, c.toString)))
  }
  private val Cyrillic: Map[Char, String] = Map(
    'а' -> "a", 'б' -> "b", 'в' -> "v", 'г' -> "g", 'ґ' -> "g", 'д' -> "d", 'е' -> "e", 'є' -> "ye", 'ё' -> "yo",
    'ж' -> "zh", 'з' -> "z", 'и' -> "y", 'і' -> "i", 'ї' -> "yi", 'й' -> "y", 'к' -> "k", 'л' -> "l", 'м' -> "m",
    'н' -> "n", 'о' -> "o", 'п' -> "p", 'р' -> "r", 'с' -> "s", 'т' -> "t", 'у' -> "u", 'ф' -> "f", 'х' -> "kh",
    'ц' -> "ts", 'ч' -> "ch", 'ш' -> "sh", 'щ' -> "shch", 'ъ' -> "", 'ы' -> "y", 'ь' -> "", 'э' -> "e", 'ю' -> "yu",
    'я' -> "ya")

  /** One title's comparison forms, each computed on first use: the same titles are compared across
   *  a whole family pool, and normalising them again per pair was nearly all of a resolve's time. */
  private[identity] final case class TitleForm(text: String) {
    lazy val key: String            = IdentityMeasures.key(text)
    lazy val words: Seq[String]     = IdentityMeasures.words(text)
    lazy val wordSet: Set[String]   = words.toSet
    lazy val yearlessWords: Seq[String] = yearlessTokens(text)
    lazy val yearless: String       = yearlessWords.mkString
    lazy val latinKey: String       = IdentityMeasures.latinKey(text)
    /** Its words after a leading English article, when it has one — what a venue that drops the article lists. */
    lazy val afterArticle: Option[Seq[String]] = Option.when(words.sizeIs >= 2 && LeadingArticles(words.head))(words.tail)
    /** The title before a trailing bracketed gloss, keyed — "TKT (T'inquiète)" is "TKT" too. */
    lazy val unglossedKey: Option[String] =
      TrailingGloss.findFirstMatchIn(text).map(m => IdentityMeasures.key(m.group(1))).filter(k => k.nonEmpty && k != key)
  }
  private val TrailingGloss = """^(.*\S)\s*\([^()]*\)\s*$""".r
  /** The English articles a venue drops from a title's head, as the old pipeline's IMDb match dropped them — English
   *  titles are listed everywhere; another language's articles are part of its titles ("La familia Dino"). */
  private val LeadingArticles = Set("the", "a", "an")

  private def words(s: String): Seq[String] = TitleContainment.tokens(withoutPossessives(s))

  private def credits(names: Iterable[String]): Seq[String] =
    names.iterator.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSeq

  private def latin(names: Iterable[String]): Boolean =
    names.exists(_.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** A name in Latin letters: ICU's general Any-Latin transliteration, then to ASCII. Pinyin for
   *  Han, ISO-style for Cyrillic, Greek, Georgian, …: no table of names. One instance per thread
   *  (an ICU transliterator is not documented as safe to share). */
  private val toLatin = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII"))
  private[identity] def latinized(name: String): String = toLatin.get.transliterate(name)
  private[identity] def isAscii(s: String): Boolean = { var i = 0; while (i < s.length && s.charAt(i) < 0x80) i += 1; i == s.length }

  /** Credit lists as the director relation compares them, each written form found once. */
  final class Credits(raw: Iterable[String]) {
    val names: Seq[String] = credits(raw)
    lazy val keys: Set[String]              = names.map(services.movies.PersonKey.of).filter(_.nonEmpty).toSet
    lazy val words: Seq[Seq[String]]        = names.map(TitleContainment.tokens).filter(_.nonEmpty)
    lazy val isLatin: Boolean               = latin(names)
    // ASCII names are their own Latin form ("Any-Latin; Latin-ASCII" leaves them as they are): no transliteration —
    // ICU's, per name, was ~150 MB of a PL hard-cluster run's resolve allocation, most of it on the Latin side.
    lazy val inLatin: Credits               = if (names.forall(IdentityMeasures.isAscii)) this else new Credits(names.map(latinized))
    def isEmpty: Boolean = keys.isEmpty

    /** Some credit names the same person as some credit of `other`: the same words in any order
     *  ([[services.movies.PersonKey]]) or the same letters split otherwise ([[sameLetters]]). */
    def samePerson(other: Credits): Boolean =
      (keys intersect other.keys).nonEmpty || words.exists(a => other.words.exists(b => sameLetters(a, b) || spelledAlike(a, b)))
  }

  /** Two credits of as many words, in some order each word the same or a letter apart where both
   *  spellings run to five letters: a transliteration's spelling ("Rajeesh Parmeswaran", TMDB's
   *  "Rajesh Parameswaran"). Short words are other names ("Jan"/"Jon"), not spellings. */
  private def spelledAlike(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs >= 2 && a.size == b.size && a.sizeIs <= MaxOrderedWords && a != b &&
      b.permutations.exists(p => a.zip(p).forall { case (x, y) => x == y || (x.length >= 5 && y.length >= 5 && editDistanceOne(x, y)) })

  /** The most words a credit may have for [[sameLetters]] to try its orders (it enumerates them). */
  private val MaxOrderedWords = 5

  /** Two credits are one written name whatever its ORDER, CASE and SPLITTING: some order of each
   *  one's words, written out, is the same letters ("Jungjae HA" and "Ha Jung-jae" are both
   *  "hajungjae"). Every letter must be accounted for, so "Jung Ha" is not "Ha Jung-jae". */
  private def sameLetters(a: Seq[String], b: Seq[String]): Boolean =
    a.sizeIs <= MaxOrderedWords && b.sizeIs <= MaxOrderedWords && a.mkString.sorted == b.mkString.sorted && {
      val written = b.permutations.map(_.mkString).toSet
      a.permutations.exists(p => written(p.mkString))
    }

  private def byName(a: Credits, b: Credits, disagree: String): Category =
    if (a.samePerson(b)) Category("same_person")
    else {
      val wa = a.keys.flatMap(_.split(" ")).filter(_.length >= 3)
      val wb = b.keys.flatMap(_.split(" ")).filter(_.length >= 3)
      Category(if ((wa intersect wb).nonEmpty) "shared_name" else disagree)
    }

  /** How two credit lists relate: one person in common — the same words or letters in any order,
   *  case or splitting (`same_person`) — a shared name word only (a surname, a transliteration), or
   *  nobody in common. Credits in different scripts (a venue's "Bi Gan", TMDB's "毕赣") are compared in Latin letters
   *  ([[latinized]]); nobody in common THERE is `different_script`, its own category, since a
   *  transliteration can miss a person a Latin spelling names (a Japanese reading of Kanji is not
   *  its pinyin). `incomparable` only when a side has no name left to compare in Latin letters. */
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = creditRelation(new Credits(a), new Credits(b))

  /** `measures`, the director read as the listing's own when its credits and the record's are in different
   *  scripts and no transliteration joins them, but the person TMDB finds by the listing's OWN spelling directed
   *  (or wrote) the film: "Wong Kar Wai" is TMDB's 王家衛, whose pinyin reads "Wang Jiawei" (film 843). */
  def creditedBySearch(measures: Map[String, Measure], directedBySpelling: Boolean): Map[String, Measure] =
    if (directedBySpelling && measures.get("director").exists(m => m == Category("different_script") || m == Category("incomparable")))
      measures + ("director" -> Category("same_person"))
    else measures

  /** `measures`, the director read as credited when the listing credits nobody but its title heads the film's title with
   *  the surname of one of its directors ("Konwicki Salto", "Konwicki: Salto" — PL Kino Fenix's retrospective of
   *  Tadeusz Konwicki): the rest of the title must be one of the film's titles whole, and the surname five letters at
   *  least. */
  def creditedByTitle(l: Listing, f: Film, measures: Map[String, Measure]): Map[String, Measure] =
    if (!measures.get("director").contains(MissingListing) || l.directors.nonEmpty) measures
    else {
      val words = l.titleWords
      val named = words.sizeIs >= 2 && words.head.length >= 5 && f.directors.exists(_.exists { director =>
        val surname = IdentityMeasures.words(director).lastOption
        surname.exists(s => s.length >= 5 && s == words.head) && f.titleKeys.contains(words.tail.mkString)
      })
      if (named) measures + ("director" -> Category("same_person")) else measures
    }

  /** Is a listing's credited "director" the film's HOUSE — a name two words or more long that runs inside the film's
   *  own title, and none of the people the film credits? UK venues credit "The Metropolitan Opera" for its 2026/27
   *  "The Metropolitan Opera: Così fan tutte" ×97, which read as a different director than Phelim McDermott and
   *  vetoed the film. "Guillermo del Toro" of "Guillermo del Toro's Pinocchio" directed it, and stays a person. */
  private def namesItsHouse(name: String, words: Seq[String], f: Film): Boolean =
    words.sizeIs >= 2 && f.ownTitleTokens.exists(_.containsSlice(words)) &&
      !f.directorCredits.exists(credits => creditRelation(new Credits(Seq(name)), credits) == Category("same_person"))

  /** [[directorRelation]] over credits parsed once: a listing's and a record's directors meet every
   *  pair of a family's pool, and re-parsing both per pair allocated the names again each time. */
  private def creditRelation(ca: Credits, cb: Credits): Measure = {
    if (ca.isEmpty) MissingListing
    else if (cb.isEmpty) MissingFilm
    else if (ca.isLatin == cb.isLatin) byName(ca, cb, "different")
    else {
      val (la, lb) = (ca.inLatin, cb.inLatin)
      if (la.isEmpty || lb.isEmpty) Category("incomparable") else byName(la, lb, "different_script")
    }
  }

  /** The shapes a listing's title can name a film by: the whole title, its raw form, and each
   *  programme-banner segment (`SearchTitles.candidates`: `|`, ` - `, a first `: `, …). */
  def titleShapes(l: Listing): Seq[String] = l.shapes

  /** The pieces of a title AS BILLED ([[shapes]]) that are written in capitals where the title is not — the case change
   *  a venue's programme tag makes beside a work's own spelling ("Alim | UFF", "DKF: Lalka"). None in a title billed
   *  all in capitals ("BEZ KOŃCA 2D PL LOLO"): its case says nothing. */
  def capitalisedTags(billed: String): Seq[String] =
    if (!billed.exists(_.isLower)) Nil
    else shapesOfTitle(billed).filter(piece => piece != billed.trim && piece.exists(_.isLetter) && !piece.exists(_.isLower))

  /** [[shapes]] of ONE title with no learned decorations, split once and remembered: the scorer asks it of
   *  the same billed titles for every candidate it weighs (`CandidateScoring.namesOnlyATag`, `Qualifiers.split`),
   *  ~6% of a US order-independence replay's CPU (run 37517196329). A pure function of the string; bounded. */
  private[identity] def shapesOfTitle(title: String): Seq[String] = SingleTitleShapes.get(title, t => {
    titleShapesSplit.incrementAndGet(); shapes(Seq(t))
  })
  private val SingleTitleShapes = tools.BoundedCache.ofSize(100_000).build[String, Seq[String]]()
  /** How many single titles [[shapesOfTitle]] has split rather than remembered — what the spec reads the memo by. */
  private[identity] val titleShapesSplit = new java.util.concurrent.atomic.AtomicLong(0)

  private def shapesOf(l: Listing): Seq[String] = {
    shapes(Seq(l.title) ++ l.rawTitle ++ l.searchTitles ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None)) ++ (Seq(l.title) ++ l.rawTitle).flatMap(creditedWork), l.decorations)
  }

  /** `titles` and every part a split leaves, each de-decorated in turn ("Throwback: Donnie Darko
   *  (25th Anniversary)" → "Donnie Darko (25th Anniversary)" → "Donnie Darko"; "(4DX Rewind) Shrek"
   *  → "Shrek" by a learned `decorations` run) and cut before a bracketed year ("Człowiek z żelaza
   *  (1981) 4K" → "Człowiek z żelaza"), to a fixpoint: every shape still a whole delimited or
   *  decorated piece of one of the titles. */
  private[identity] def shapes(titles: Seq[String], decorations: TitleDecorations = TitleDecorations.None): Seq[String] = {
    // A worklist, each shape split once: what one round adds is what its NEW shapes split into — the older shapes'
    // pieces are all held already — in the order a round over every shape would add them (by splitter, then shape).
    val held     = scala.collection.mutable.LinkedHashSet.empty[String]
    var frontier = titles.map(_.trim).filter(_.nonEmpty).distinct
    // a title quoting two works bills both: none of its spellings is cut to one quoted title
    val quotesOne = !frontier.exists(title => Quoted.findAllMatchIn(title).size >= 2)
    held ++= frontier
    while (frontier.nonEmpty) {
      // a learned decoration never cuts into an author's credit: its name is no film's title ("… by Noël Coward")
      frontier = (frontier.flatMap(SearchTitles.candidates(_, None)) ++ frontier.flatMap(t => decorations.strip(creditedWork(t).getOrElse(t))) ++ frontier.flatMap(beforeItsYear) ++
        frontier.flatMap(delimitedPieces(_, quotesOne))).map(_.trim).filter(shape => shape.nonEmpty && !held(shape)).distinct
      held ++= frontier
    }
    held.toSeq
  }

  /** The pieces more banner separators leave, beside the ones `SearchTitles` splits: a slash with a
   *  space on either side ("MISTYCZKA /film polski/", "Róża / Spotkanie Filozoficzne"), a dash with a
   *  space on one side ("Fregata dla seniorów- 500 Mil"), a code of up to three letters before a colon
   *  with no space ("MS:HOT SPOT"), a square-bracketed format and a title
   *  quoted at the head of its billing (`quotesOne`: none where a title quotes two), or at its tail after a banner's
   *  stop ('Kino Kobiet - KLAPS! "Jak żyć żeby nie zwariować"'); what an upper-case banner of two words or more runs
   *  into with an unspaced colon ("KINO NA NIEDZIELE:CAMINO DLA OPORNYCH"). A slash or colon inside a word is the
   *  title's own ("Face/Off", "AC/DC", "Star Wars:Episode I"); so is a sentence's stop: "Szkoła magicznych zwierząt.
   *  Tajemnica szkolnego podwórka" is the sequel, and its head the first film (whole-corpus dump, 2026-10-05: the
   *  head cut switched it and lost "Avengers: Koniec Gry. Dogrywka"). Here, not in `SearchTitles`, which the old
   *  pipeline also reads. */
  private def delimitedPieces(title: String, quotesOne: Boolean): Seq[String] = {
    val slashed = Seq(SpacedSlash, HalfSpacedDash).flatMap(separator =>
      if (separator.findFirstIn(title).isDefined) separator.split(title).toSeq.map(_.trim) else Nil)
    val coded   = CodeBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    val undated = ScreeningYearSuffix.findFirstMatchIn(title).map(_.group(1)).toSeq
    val format  = TrailingSquareBracket.findFirstMatchIn(title).map(_.group(1)).toSeq
    val quoted  = (if (quotesOne && Quoted.findAllMatchIn(title).size == 1)
      LeadingQuoted.findFirstMatchIn(title).orElse(TrailingQuoted.findFirstMatchIn(title)).map(_.group(1)) else None).toSeq
    val banner  = BannerBeforeColon.findFirstMatchIn(title).map(m => title.substring(m.end)).toSeq
    (slashed ++ coded ++ undated ++ format ++ quoted ++ banner).map(_.trim).filter(_.exists(_.isLetter)).filter(_ != title.trim)
  }
  /** A format or version tag in square brackets after the title ("Vivaldi i ja [2D LEKTOR]"); a round bracket is
   *  `SearchTitles`'s. */
  private val TrailingSquareBracket = """^(.*\p{L}.*?)\s*\[[^\]\[]*\]$""".r
  /** A title quoted at the head of a programme's billing ("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe"),
   *  the title's only quotes: two quoted titles bill two works, and a quote further in names what a talk, a concert or a
   *  show is about ('Wojciech Cejrowski i „Prawo Dżungli”', 'ZADUSZKI JAZZOWE GABA JANUSZ "NIEOBECNI"'). */
  private val Quoted        = """[„"“«][^"”„“«»]*["”“»]""".r
  private val LeadingQuoted = """^\s*[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]""".r
  private val SpacedSlash     = """\s+/\s*|\s*/\s+""".r
  /** A dash with a space on ONE side ("Fregata dla seniorów- 500 Mil"); both sides is `SearchTitles`'s. */
  private val HalfSpacedDash  = """(?<=\S)[-–—]\s+|\s+[-–—](?=\S)""".r
  private val CodeBeforeColon = """^\p{L}{1,3}:(?=\S)""".r
  private val TrailingQuoted  = """[:!\-–—]\s*[„"“«]([^"”„“«»]*\p{L}[^"”„“«»]*)["”“»]\s*$""".r
  private val BannerBeforeColon = """^[\p{Lu}\d]+(?:\s+[\p{Lu}\d]+)+:(?=\p{L})""".r
  /** A screening year after a title, unbracketed ("Ma to sens 2026"): 2020–2039 only, so a title that IS a
   *  number ("2046", "Blade Runner 2049") keeps it. A shape, never the old pipeline's lookup query. */
  private val ScreeningYearSuffix = """^(.*\p{L}.*?)\s+20[23]\d$""".r

  /** Two titles that are the same words but for ONE, a letter apart — a venue's typo ("The Beast of
   *  Mossy Botton", "Pradhama Drishtiya Kuttakkar") — where both spellings of that word run to five
   *  letters and neither is a number, in a title of two words or more: a sequel's numeral ("Scary Movie 3", "Mission: Impossible II")
   *  or a short word ("Hunt"/"Hurt") is a different title, not a typo. */
  private[identity] def oneTypoApart(a: Seq[String], b: Seq[String]): Boolean =
    // Two words at least: a one-word title a letter from another is another film ("Lalka"/"Lalkar").
    a.size >= 2 && a.size == b.size && a != b && {
      // The one index the two differ at, or -1 for more than one: counted in place, not as a vector of them per pair.
      val differing = { var at = -1; var i = 0; var n = 0
        while (i < a.size && n < 2) { if (a(i) != b(i)) { at = i; n += 1 }; i += 1 }
        if (n == 1) at else -1 }
      differing >= 0 && {
        val (x, y) = (a(differing), b(differing))
        x.length >= 5 && y.length >= 5 && !Seq(x, y).exists(w => w.exists(_.isDigit) || RomanNumeral.pattern.matcher(w).matches()) &&
          editDistanceOne(x, y)
      }
    }
  /** One insertion, deletion or substitution apart. */
  private def editDistanceOne(x: String, y: String): Boolean =
    if (math.abs(x.length - y.length) > 1) false
    else {
      val (s, l) = if (x.length <= y.length) (x, y) else (y, x)
      val i = s.indices.find(i => s(i) != l(i)).getOrElse(s.length)
      if (s.length == l.length) s.substring(i + 1) == l.substring(i + 1) else s.substring(i) == l.substring(i + 1)
    }

  /** The WORK a title's subtitle hangs off: what stands before its first dash ("Cirque du Soleil:
   *  Kurios" of "Cirque du Soleil: Kurios - Gabinet osobliwości"), when a subtitle follows it. */
  private val SubtitleDash = """\s+[-–—]\s+""".r
  private def workOf(t: String): Option[String] =
    SubtitleDash.findFirstMatchIn(t).map(m => t.substring(0, m.start).trim).filter(w => TitleContainment.tokens(w).sizeIs >= 2)
  /** Is the listing's whole title the film's WORK — its title before a subtitle, after a dash, a comma
   *  or a colon ("Leonas" of "Leonas, el instinto más salvaje", "BTS WORLD TOUR 'ARIRANG' IN BUENOS
   *  AIRES" of "…: Live Viewing")? The words of the work, or None. */
  def titleIsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy(Seq(l.title) ++ l.rawTitle, f)
  /** Is the original title the venue publishes the film's WORK, as [[titleIsWorkOf]] — DE "Ein Hund namens Quill",
   *  published as "Quill", of "Quill - Ein Freund für´s Leben"? */
  def originalTitleIsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy(l.originalTitle.toSeq, f)
  /** The words of the film's work (its title or original title before a subtitle) when one of `titles` is it. */
  private def workNamedBy(titles: Seq[String], f: Film): Option[Int] = {
    val own = titles.map(key).filter(_.nonEmpty).toSet
    (Seq(f.title) ++ f.originalTitle).flatMap(leadingWork).find(w => own(key(w))).map(w => TitleContainment.tokens(w).size)
  }

  /** Do the listing and the film bill the same WORK, each under its own subtitle — the title before a
   *  first comma, colon or dash ("BTS World Tour 'ARIRANG' In Buenos Aires: Live" and "…: Live
   *  Viewing")? The words of the work, or None. */
  def billsWorkOf(l: Listing, f: Film): Option[Int] = workNamedBy((Seq(l.title) ++ l.rawTitle).flatMap(leadingWork), f)

  private val WorkSubtitle = """\s*[,:]\s+|\s+[-–—]\s+""".r
  private def leadingWork(t: String): Option[String] = WorkSubtitle.findFirstMatchIn(t).map(m => t.substring(0, m.start).trim)

  /** Do the listing and the film bill the same work under different subtitles — a venue's translated
   *  subtitle ("Gabinet osobliwości" for "Cabinet des curiosités")? */
  def sharesWork(l: Listing, f: Film): Boolean = {
    val works = (Seq(f.title) ++ f.originalTitle).flatMap(workOf).map(key).toSet
    (Seq(l.title) ++ l.rawTitle).flatMap(workOf).map(key).exists(works)
  }

  /** How the listing's title names the film: its localised title exactly, its original title,
   *  an alternative title, one whole banner segment, the film's title as a token run along one edge
   *  of the listing's (`decorated`: "Ken Russell's The Devils"), the listing's title as a run along
   *  one edge of the film's (`fragment`: "It" beside "It Ends with Us"), some shared words, or
   *  nothing. The two containments are opposite evidence — a decoration names the film, a fragment
   *  names a shorter, often different one — so they are measured apart.
   *
   *  A film record of the listing's season production ([[namesSeasonProduction]]) is a
   *  `segment`: the listing's work segment is the record's, under the same season. Only a FILM's
   *  record says so — two listings' banners do not tell one house from another (the Met's and the
   *  Royal Opera's "Carmen" of one season), so `listingListing` does not read it.
   *
   *  So is a record BILLING the listing's work under the listing's house ([[billing]]): the same
   *  banner once seasons and bracketed years are dropped ("The Metropolitan Opera: Manon (2027)"
   *  and "The Metropolitan Opera 2026/27: Manon"), or the house the listing's banner was learned
   *  to be (`houses`: "NT Live" and "National Theatre Live"). */
  def titleRelation(l: Listing, f: Film, houses: Houses = Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown): Category =
    titleRelation(l, f, Some(houses), qualifiers)

  /** `houses`: `None` for two listings, whose banners name no house on record.
   *
   *  A record title that is only one of the listing's [[Qualifiers]] names nothing: "Director's
   *  Cut" (2016) shares the words of "Dark City: Director's Cut" but is not its film, so it is
   *  measured on the record's other titles, and as an `overlap` when it has none. */
  private def titleRelation(l: Listing, f: Film, houses: Option[Houses], qualifiers: Qualifiers): Category = {
    val ls  = l.ownForms
    val all = f.forms
    val own = l.ownKeys
    lazy val qualifying = qualifiers.of(l)
    val fs  = if (qualifiers.companions.isEmpty) all else all.filterNot(t => qualifying(t.yearless))
    val (titleForm, rest) = (all.head, all.tail)
    val (originalForms, alternativeForms) = rest.splitAt(f.originalTitle.size)
    // The film's title with the leading English article the venue dropped: "Brides of Dracula" is "The Brides of Dracula"
    // (US Metrograph's listing took Fisher's "Dracula" over a `fragment` of its own film). Three words left at least —
    // "Spookies" is no more "The Spookies" than any other — and never the other way: a listing's own article is its title's.
    // A plain title, not a banner's: "Royal Ballet: Swan Lake" is not thereby "The Royal Ballet: Swan Lake" (2024), the
    // house's earlier recording, over the 2026/27 broadcast its banner names.
    val articleless = (t: TitleForm) => !t.text.contains(':') && t.afterArticle.exists(rest => rest.sizeIs >= 3 &&
      ls.exists(a => !a.text.contains(':') && a.words == rest))
    if (own.contains(titleForm.key) || articleless(titleForm) || ls.exists(a => oneTypoApart(a.words, titleForm.words)) ||
        (titleForm.latinKey.nonEmpty && ls.exists(_.latinKey == titleForm.latinKey))) Category("exact")
    else if (originalForms.map(_.key).exists(own) || originalForms.exists(articleless)) Category("original")
    else if (alternativeForms.map(_.key).exists(own) || alternativeForms.exists(articleless)) Category("alternative")
    else (if (fs.isEmpty) None
          else containment(ls, l.shapeKeys, fs, houses.exists(h => namesSeasonProduction(l, f) || billing(l, f).exists(h.same)), l.shapeWords, l.undecoratedWords)).getOrElse(
      if (ls.exists(a => all.exists(b => a.wordSet.exists(b.wordSet)))) Category("overlap") else Category("none"))
  }

  /** How one side's titles name the other's once no whole title matches: a whole delimited piece
   *  of them (a banner segment, the title without a trailing bracket — `segment`, from `shapes`),
   *  the other's title as a token run along one edge of one of them (`decorated`: "Ken Russell's
   *  The Devils"), or one of them as a run along one edge of the other's (`fragment`: "It" beside
   *  "It Ends with Us"). `alsoSegment` is another reason to read a segment (the title relation's
   *  season production), asked only when no shape matches. ONE definition for the title and the
   *  original-title relations. */
  private def containment(own: Seq[TitleForm], shapeKeys: Seq[String], others: Seq[TitleForm], alsoSegment: => Boolean = false,
                          shapeWords: Seq[Seq[String]] = Nil, undecorated: Seq[Seq[String]] = Nil): Option[Category] = {
    val otherKeys = others.map(_.key).filter(_.nonEmpty).toSet
    // a title closing on an author's credit is read without it: the author's name is no edge a film's title decorates
    val ow = own.map(form => creditedWork(form.text).fold(form.words)(words)).filter(_.nonEmpty)
    val fw = others.map(_.words).filter(_.nonEmpty)
    // A shape one venue typo from the film's title is a segment too ("Pradhama Drishtiya Kuttakkar
    // (Malayalam)" of "Pradhama Drishtya Kuttakkar", `oneTypoApart`).
    if (shapeKeys.exists(otherKeys) || shapeWords.exists(sw => fw.exists(oneTypoApart(sw, _))) || alsoSegment) Some(Category("segment"))
    // The title with its learned banner stripped decorates the film too: "Horror Season 2026 Manhunter:
    // The Final Cut" leaves "Manhunter: The Final Cut", "Manhunter" and an edition label. Not any shape:
    // "Fanciulla Encore (2027)" without its year does not decorate a film called "Encore".
    else if ((ow ++ undecorated).exists(a => fw.exists(b => TitleContainment.isTokenRun(b, a)))) Some(Category("decorated"))
    else if (ow.exists(a => fw.exists(b => TitleContainment.isTokenRun(a, b)))) Some(Category("fragment"))
    else None
  }

  /** The pieces of the listing's title that NAME the film, as words: a title shape equal to one of
   *  the film's titles (the whole title, a banner segment), or one of the film's titles as a run
   *  along one edge of the listing's (a decoration). Empty when no piece names it. */
  def namingPieces(l: Listing, f: Film): Set[Seq[String]] = {
    // The record's and the listing's forms, each normalised once per instance: this runs for every
    // node against every candidate (`namesOnlyItsVenue`), a quarter of a resolve when it re-ran the regexes.
    val filmForms = f.forms.filter(_.key.nonEmpty)
    val keys      = filmForms.map(_.key).toSet
    val segments  = l.shapeKeys.zip(l.shapeWords).collect { case (shapeKey, shapeWords) if keys(shapeKey) => shapeWords }
    val edgeRuns  = for {
      own  <- l.ownForms.map(_.words)
      film <- filmForms.map(_.words) if TitleContainment.isTokenRun(film, own)
    } yield film
    (segments ++ edgeRuns).filter(_.nonEmpty).toSet
  }

  /** Does the listing's title name the two films by DISJOINT pieces — "Lalka (Dolly)" names
   *  Kawalski's "Lalka" by one word and Blackhurst's "Dolly" by the other, and neither by the
   *  whole? Then its title names both alike, and only what else it publishes can tell them apart.
   *  Two pieces are disjoint when they share no word, or when they sit at spans of the listing's
   *  title that do not overlap: the double bill "The Gruffalo + The Gruffalo's Child" names each
   *  film by its own part of the title, though both parts spell "The Gruffalo". Nested pieces are
   *  no such tie: "Joker: Folie à deux" names its own film by the whole title and "Joker" only by
   *  a piece of it, and a film whose own title joins two others' ("Romeo + Juliet") is named by
   *  the whole title, which every piece overlaps. */
  def namedApart(l: Listing, a: Film, b: Film): Boolean = {
    val (pa, pb) = (namingPieces(l, a), namingPieces(l, b))
    pa.nonEmpty && pb.nonEmpty && (
      pa.forall(x => pb.forall(y => (x.toSet intersect y.toSet).isEmpty)) ||
        (Seq(l.title) ++ l.rawTitle).map(words).distinct.exists(placedApart(pa.toSeq, pb.toSeq, _)))
  }
  /** Can every piece of `pa` and of `pb` be placed in `title` (one occurrence each) so that no word
   *  of the title is under a piece of both? Every piece must occur in it: "Tokyo Story (Tokyo
   *  Monogatari)" names Ozu's film by both of its titles, which leave "Tokyo" nowhere of its own. */
  private def placedApart(pa: Seq[Seq[String]], pb: Seq[Seq[String]], title: Seq[String]): Boolean = {
    def placements(pieces: Seq[Seq[String]]): Seq[Set[Int]] =
      pieces.foldLeft(Seq(Set.empty[Int])) { (covered, p) =>
        for { c <- covered; i <- title.indices if title.startsWith(p, i) } yield c ++ (i until i + p.size)
      }
    val (ca, cb) = (placements(pa), placements(pb))
    ca.exists(x => cb.exists(y => (x intersect y).isEmpty))
  }

  /** The titles VENUES publish for film records, beside the record's own: a listing whose original
   *  title names one record exactly publishes its own title as that record's title in its language
   *  — an alternative title the record may lack (TMDB titles André Rieu's 2026 Maastricht concert
   *  in English only; Multikino lists it as "Andre Rieu. Niech żyje Maastricht!", originally "Andre
   *  Rieu's 2026 Summer Concert: Viva Maastricht!"). Read so only when
   *  - every listing of the title that publishes a different original title publishes the same one:
   *    venues giving one title two originals ("La invitación" as "The Invitation" and "The Invite")
   *    name two films by it;
   *  - that original is the title or original title of exactly ONE of `films`: "Niebo nad
   *    Normandią", originally "Pressure", names one of nine "Pressure"s, and which one is the
   *    listing's facts' to tell, not a title the other eight gain; and
   *  - the title does not name the record already ([[NamingRelations]]): "Coraline (2009)",
   *    originally "Coraline", is a dated spelling of the record's title, not a translation.
   *  Keyed by film id, each title once (its smallest spelling), sorted: a function of the two sets. */
  def venueTitles(listings: Iterable[Listing], films: Iterable[(Int, Film)]): Map[Int, Seq[String]] = {
    val translated = listings.iterator.flatMap(l => l.originalTitle.map(o => (key(l.title), key(o), l.title)))
      .filter { case (t, o, _) => t.nonEmpty && o.nonEmpty && t != o }.toSeq
    val unanimous = translated.groupMap(_._1)(_._2).collect { case (t, os) if os.distinct.sizeIs == 1 => t -> os.head }
    val spelling  = translated.groupMapReduce(_._1)(_._3)((a, b) => if (a <= b) a else b)
    val filmsByKey = films.toSeq.flatMap { case (id, f) => (Seq(f.title) ++ f.originalTitle).map(key).filter(_.nonEmpty).distinct.map(_ -> (id, f)) }
      .groupMap(_._1)(_._2)
    val byOriginal = unanimous.toSeq.flatMap { case (t, o) =>
      filmsByKey.getOrElse(o, Nil).distinctBy(_._1) match {
        case Seq((id, f)) if !NamingRelations(titleRelation(Listing(spelling(t)), f).value) => Some(id -> spelling(t))
        case _                                                                               => None
      }
    }
    byOriginal.groupMap(_._1)(_._2).map { case (id, ts) => id -> ts.distinct.sorted }
  }

  /** The second way a venue publishes a record's title in its language — asked per title key, of that key's
   *  listings and the records their own searches and walks reached (so it is local to the key, as the live
   *  corpus keeps it): listings of one title whose own
   *  director and year single out ONE record of `films` (the director the same person, the year within
   *  one) — "Vincent. Legenda oceanu" [2025] {Reza Memari} is TMDB's English-only "The Last Whale Singer",
   *  so a sibling listing billing that title bare reads it as the record's alternative title, not an
   *  overlap. Read so only when every listing of the title whose facts single out a record single out the
   *  same one, no record of `films` carries the title whole already (as its title, original or
   *  alternative title — this one or another), the title is no piece of the record's own (a tour's name is
   *  not its São Paulo film's), contains no OTHER record its years do not rule out, and is no double bill's. */
  def titlesByFacts(listings: Iterable[Listing], films: Iterable[(Int, Film)]): Seq[(Int, String)] = {
    val pool = films.toSeq
    // A bill's title names two works: its facts may single out one, but the title is no record's own.
    val singled = listings.iterator.filter(l => l.directors.nonEmpty && l.statedYear.isDefined && BillJoin.findFirstIn(l.title).isEmpty).map { l =>
      val year = l.statedYear.get
      val ids  = pool.collect { case (id, f) if f.year.exists(FactRelations.yearsNear(_, year)) &&
        f.directorCredits.exists(creditRelation(l.directorCredits, _) == Category("same_person")) => id }.distinct
      (key(l.title), l.title, ids, l.statedYear)
    }.filter(_._1.nonEmpty).toSeq
    singled.groupBy(_._1).toSeq.flatMap { case (_, ls) =>
      // A listing whose facts single out no record here, or several, says nothing; one singling out another denies.
      ls.map(_._3).filter(_.sizeIs == 1).distinct match {
        // A title some record of the pool carries already is that record's to answer for ("Obcy" is Ozon's
        // 2025 film however a 2026 listing's facts lean): only a title NO record names is learned.
        // Nor a title pointing at ANOTHER record its facts do not rule out: "BTS World Tour 'ARIRANG' In Buenos
        // Aires: Live" credits the tour's director, whom only the São Paulo record carries, while its title
        // names the Buenos Aires one ("…: Live Viewing").
        case Seq(Seq(id)) =>
          val spelling = ls.map(_._2).min
          val years    = ls.flatMap(_._4).distinct
          def relation(f: Film) = titleRelation(Listing(spelling), f).value
          def ruledOut(f: Film) = f.year.exists(y => years.nonEmpty && years.forall(l => !FactRelations.yearsNear(l, y)))
          // A title that is a piece of the record's own ("BTS World Tour 'ARIRANG'" of its São Paulo concert
          // film) names the tour, not the record: only a title sharing no part of it, a translation, is learned.
          Option.when(pool.collectFirst { case (`id`, f) => !ContainingRelations(relation(f)) }.getOrElse(false) &&
            !pool.exists { case (_, f) => Rivalling(relation(f)) } &&
            !pool.exists { case (other, f) => other != id && ContainingRelations(relation(f)) && !ruledOut(f) })(id -> spelling)
        case _ => None
      }
    }
  }

  /** `f` with the titles venues publish for it ([[venueTitles]]) among its alternative titles. */
  def withVenueTitles(f: Film, titles: Seq[String]): Film =
    if (titles.isEmpty) f else f.copy(alternativeTitles = f.alternativeTitles ++ titles)

  /** Does a year the original title writes ("… (2024)") agree with the film's — within one — when the
   *  yearless comparison drops it? A title that writes no year drops nothing; one that does, against a
   *  film whose year is unknown, is not read as agreeing: nothing else measures that year. */
  private val WrittenYear = """(?<!\d)(?:18|19|20)\d{2}(?!\d)""".r
  private def yearAgrees(title: String, filmYear: Option[Int]): Boolean = {
    val written = WrittenYear.findAllIn(title).map(_.toInt).toSeq
    written.isEmpty || filmYear.exists(y => written.exists(FactRelations.yearsNear(_, y)))
  }

  /** The listing's own ORIGINAL title against every title of the other side: the same title, a
   *  decorated or delimited spelling of one ([[containment]], as the title relation reads it: "Your
   *  Name (re-release)" is `segment` of "Your Name."), a shared long word, or nothing. */
  def originalTitleRelation(original: Option[String], otherTitles: Seq[String], filmYear: Option[Int] = None): Measure =
    originalFormRelation(original.map(_.trim).filter(_.nonEmpty).map(OriginalForm(_)),
      otherTitles.map(_.trim).filter(_.nonEmpty).map(TitleForm(_)), filmYear)

  /** An original title's comparison forms: its own, and its shapes' keys. */
  private[identity] final case class OriginalForm(text: String) {
    val form: TitleForm = TitleForm(text)
    lazy val shapeKeys: Seq[String] = shapesOfTitle(text).map(key)
    /** Its delimited segments as words, read for a decoration: "Michael Mann's Manhunter: The Final
     *  Cut"'s "Michael Mann's Manhunter" ends in the film's title. */
    lazy val segmentWords: Seq[Seq[String]] =
      (shapesOfTitle(text) ++ ColonBreak.split(text).headOption.filter(_ != text)).filterNot(_ == text).map(words).filter(_.nonEmpty).distinct
    lazy val longWords: Set[String] = form.words.filter(_.length >= 4).toSet
  }

  /** [[originalTitleRelation]] over forms normalised once: a node's original title meets every
   *  film of its family's pool, and re-normalising both sides per pair was a sixth of a PL re-resolve. */
  private def originalFormRelation(original: Option[OriginalForm], others: Seq[TitleForm], filmYear: Option[Int]): Measure =
    original match {
      case None => MissingListing
      case Some(o) =>
        if (others.isEmpty) MissingFilm
        // The same title — or the film's without its trailing gloss ("TKT (T'inquiète)") — or once years
        // and seasons are dropped: "The Metropolitan Opera: Così fan tutte
        // (2026)" is "The Metropolitan Opera 2026/27: Così fan tutte" (the year is measured apart).
        else if (others.exists(t => t.key == o.form.key || t.unglossedKey.contains(o.form.key)) ||
                 others.exists(t => oneTypoApart(o.form.words, t.words)) ||
                 others.exists(_.latinKey == o.form.latinKey) ||
                 (yearAgrees(o.text, filmYear) && others.exists(t => t.yearlessWords.nonEmpty && t.yearlessWords == o.form.yearlessWords)))
          Category("match")
        else containment(Seq(o.form), o.shapeKeys, others, undecorated = o.segmentWords).getOrElse {
          if (others.exists(t => (t.words.filter(_.length >= 4).toSet intersect o.longWords).nonEmpty)) Category("overlap")
          else Category("disjoint")
        }
    }

  /** A Roman numeral of the size a series numbers its instalments by (I to XXXIX), by its grammar,
   *  never a list of values: tens, then units. The larger letters (L, C, D, M) spell words far more
   *  often than instalments ("M", "Mix", "DC"). */
  private val RomanNumeral = "(?=[xvi])(x{0,3})(ix|iv|v?i{0,3})".r
  private val RomanDigit = Map('i' -> 1, 'v' -> 5, 'x' -> 10)
  private def romanValue(t: String): Option[Int] =
    Option.when(RomanNumeral.matches(t))(t.map(RomanDigit).foldRight((0, 0)) { case (v, (sum, max)) =>
      if (v < max) (sum - v, max) else (sum + v, v) }._1)

  /** The NUMBERS a title writes as words of their own: an Arabic numeral of at most three digits
   *  anywhere (four digits are a year, "1917", "2001", "Blade Runner 2049", as the listing's other
   *  measures read them), or a Roman numeral closing a delimited piece of it ("Rocky II", "Part
   *  III: …", "Star Wars: Episode IV - …"; mid-piece "i" is a word, the Polish "and"). "Part 2",
   *  "2" and "II" are all 2. Seasons and bracketed years are dropped first. With the title's
   *  other words, in order: the series it names. */
  private[identity] final case class Numbered(words: Seq[String], numbers: Set[Int]) {
    /** The words run together, once: [[sameSeries]] compares it for every (shape, title) pair. */
    lazy val joined: String = words.mkString
  }
  private val ColonBreak         = java.util.regex.Pattern.compile(":\\s")
  private val NumberedPieceBreak = java.util.regex.Pattern.compile("""[:|/()\[\]–—,.;!?]|\s-\s""")
  private[identity] def numbered(title: String): Numbered = {
    val pieces = NumberedPieceBreak.split(withoutYears(title)).map(TitleContainment.tokens).filter(_.nonEmpty).toSeq
    val arabic = (t: String) => t.lengthIs <= 3 && t.forall(c => c >= '0' && c <= '9')
    val numbers = pieces.flatMap(p => p.filter(arabic).map(_.toInt) ++ romanValue(p.last)).toSet
    val words = pieces.flatMap(p => p.zipWithIndex.filterNot { case (t, i) => arabic(t) || (i == p.size - 1 && romanValue(t).isDefined) }.map(_._1))
    Numbered(words, numbers)
  }

  /** Does one title SPELL the other's series once each drops its numbers — the same words, or
   *  one's words a token run along an edge of the other's ([[TitleContainment.isTokenRun]]): "The
   *  Texas Chainsaw Massacre 2" and "The Texas Chain Saw Massacre", "Toy Story" and "Toy Story 3". */
  private def sameSeries(a: Numbered, b: Numbered): Boolean =
    a.words.nonEmpty && b.words.nonEmpty &&
      (a.joined == b.joined || TitleContainment.isTokenRun(a.words, b.words) || TitleContainment.isTokenRun(b.words, a.words))

  /** The NUMBERS the listing's title and the film's carry, where the two name one series
   *  ([[sameSeries]]): `same` when a reading of the listing (a title shape) and a title of the
   *  film number themselves alike — "The Texas Chainsaw Massacre 2" and "… Part 2", "Mortal
   *  Kombat 2" and "… II"; else the instalment one side numbers and the other does not
   *  (`listing_only`: "Toy Story 2" beside "Toy Story"; `film_only`), or numbers otherwise
   *  (`different`). Missing when neither numbers itself (`none`: nothing to compare — and a
   *  decoration's number, "Cineworld 30: The Matrix", never counts while a reading without it
   *  names the film) or when no reading names the film's series (`unrelated`: the title relation
   *  weighs that). A remake, or a title whose number IS its name ("1917", "Se7en", "Ocean's
   *  Eleven"), has nothing to compare. */
  def numeralRelation(l: Listing, f: Film): Measure = {
    val related = for (a <- l.numberedShapes; b <- f.numberedTitles if sameSeries(a, b)) yield (a.numbers, b.numbers)
    if (related.isEmpty) Missing("unrelated")
    else if (related.exists { case (a, b) => a.nonEmpty && a == b }) Category("same")
    else if (related.exists { case (a, b) => a.isEmpty && b.isEmpty }) Missing("none")
    else related.head match {
      case (_, b) if b.isEmpty => Category("listing_only")
      case (a, _) if a.isEmpty => Category("film_only")
      case _                   => Category("different")
    }
  }

  /** The numeral relations under which the listing numbers ANOTHER instalment than the film. */
  val OtherInstalment: Set[String] = Set("listing_only", "film_only", "different")

  /** Does the listing's title NAME the film: a naming title relation ([[NamingRelations]]), and not
   *  another instalment of its series ([[numeralRelation]]) — "The Texas Chainsaw Massacre 2"
   *  carries the whole of "The Texas Chainsaw Massacre" and names its sequel. */
  def namesFilm(l: Listing, f: Film, houses: Houses = Houses.Unknown): Boolean =
    names(titleRelation(l, f, houses).value, l, f)
  /** [[namesFilm]] on a title relation already measured. */
  def names(relation: String, l: Listing, f: Film): Boolean =
    NamingRelations(relation) && !numbersAnotherInstalment(l, f)
  def numbersAnotherInstalment(l: Listing, f: Film): Boolean = numeralRelation(l, f) match {
    case Category(c) => OtherInstalment(c)
    case _           => false
  }

  /** ISO 3166-1 alpha-2 code of a country as a venue spells it, read from the JDK's own
   *  country names in the deployment languages and its alpha-2/alpha-3 codes. No curated table:
   *  a spelling the JDK does not know is unmapped. */
  private lazy val isoByName: Map[String, String] = {
    val languages = Seq("pl", "en", "de", "es", "fr", "it").map(Locale.forLanguageTag)
    Locale.getISOCountries.iterator.flatMap { iso =>
      val l = Locale.of("", iso)
      // A country with no alpha-3 code throws; it simply has no such spelling.
      (languages.map(l.getDisplayCountry) ++ Seq(iso) ++ scala.util.Try(l.getISO3Country).toOption)
        .map(key).filter(_.nonEmpty).map(_ -> iso)
    }.toMap
  }

  def countryCode(name: String): Option[String] = isoByName.get(key(name))

  def countryRelation(listing: Seq[String], film: Option[Seq[String]]): Measure = {
    val own = listing.flatMap(countryCode).toSet
    if (listing.isEmpty) MissingListing
    else if (own.isEmpty) Missing("unmapped")
    else film.map(_.toSet) match {
      case None                => MissingFilm
      case Some(fc) if fc.isEmpty => MissingFilm
      case Some(fc)            => Category(if ((own intersect fc).nonEmpty) "match" else "mismatch")
    }
  }

  private def delta(a: Option[Int], b: Option[Int]): Measure = (a, b) match {
    case (None, _)          => MissingListing
    case (_, None)          => MissingFilm
    case (Some(x), Some(y)) => Number((x - y).toDouble)
  }

  /** The film's year minus a year the listing's title carries. */
  private def filmMinus(film: Option[Int], listing: Option[Int]): Measure = (listing, film) match {
    case (None, _)          => MissingListing
    case (_, None)          => MissingFilm
    case (Some(l), Some(f)) => Number((f - l).toDouble)
  }

  /** The listing's `runtime.delta` against the film: how many minutes its stated runtime is off the CLOSEST of the runtimes
   *  TMDB states for the film in any language it was fetched in ([[Film.runtimes]]) — a listing running as any translation's
   *  cut runs as the film. A listing billing an EDITION ([[DecorationSegments.billsAnEdition]]: "… (Extended Edition)",
   *  "Director's Cut", "Redux") that runs LONGER than every one of them states nothing for or against it: TMDB keeps no
   *  record of an extended cut apart from its film's ("The Return of the King" is 201 minutes in every language, its
   *  extended edition 263), so the longer cut is neutral. A shorter one still counts: an edition never makes a film shorter. */
  private def runtimeDelta(l: Listing, f: Film, country: Option[String]): Measure = l.statedRuntime match {
    case None => MissingListing
    case Some(stated) =>
      // walked in place, not as a collection: every listing is measured against every candidate
      var off = Int.MaxValue
      var longest = 0
      def against(runtime: Int): Unit = if (runtime > 0) { off = math.min(off, math.abs(stated - runtime)); longest = math.max(longest, runtime) }
      f.runtime.foreach(against)
      if (f.alternativeRuntimes.nonEmpty) f.alternativeRuntimes.foreach(against)
      FilmCuts.of(f.imdbNumber).foreach(cut => against(cut.runtime))  // a billed cut runs as the film ([[FilmCuts]])
      if (longest == 0) MissingFilm
      else if (stated > longest && billsAnEdition(l, f, country)) MissingListing
      else Number(off.toDouble)
  }

  /** Does the listing bill an edition of the film: by its title ([[DecorationSegments.billsAnEdition]]), or by its year, one
   *  TMDB dates a release of the film's edition in ([[Film.editionYear]]: Prince Charles's "Apocalypse Now", 2019, is
   *  its 2019 Final Cut)? */
  private def billsAnEdition(l: Listing, f: Film, country: Option[String]): Boolean =
    DecorationSegments.billsAnEdition(Seq(l.title) ++ l.rawTitle, f.titles) || l.year.exists(f.editionYear(_, country))

  private def absDelta(a: Option[Int], b: Option[Int]): Measure = delta(a, b) match {
    case Number(d) => Number(math.abs(d))
    case other     => other
  }

  /** The title searches a listing's evidence issues: every title shape and its original title,
   *  each asked WITHOUT a year (TMDB dates a film by first release, a venue by production or
   *  re-release). ONE definition: the calibration's candidate pools, the resolver's queries and
   *  the recording sweep all read it. */
  def searchQueries(l: Listing): Seq[String] = {
    val asked = titleShapes(l) ++ l.originalTitle ++ seasonProductionQueries(l) ++ stageWorkQueries(l) ++ billedWorks(l) ++ uncredited(l)
    (asked ++ asked.flatMap(beforeItsYear)).map(_.trim).filter(_.nonEmpty).distinct
  }

  /** A title up to the year it dates itself by in a bracket ("Człowiek z żelaza (1981) 4K" →
   *  "Człowiek z żelaza"): TMDB's search finds nothing for the bracketed spelling, and a search is
   *  asked without a year. */
  private def beforeItsYear(title: String): Option[String] =
    BracketedYear.findFirstMatchIn(title).map(m => title.take(m.start).trim).filter(_.exists(_.isLetter))

  /** The title without an anniversary it dates ("… 20th Anniversary") and a director's possessive
   *  credit before it ("Guillermo del Toro's …"): the work a re-release bills under both, which no
   *  piece of the title is. A credit is a name of two words or more, so "Schindler's List" keeps its own. */
  private def uncredited(l: Listing): Seq[String] = (Seq(l.title) ++ l.originalTitle).flatMap { title =>
    // the original title too: ES "Drácula. 30 Aniversario" is "Bram Stoker's Dracula 30th Anniversary" in it
    val undated = AnniversarySuffix.replaceFirstIn(title.trim, "").trim
    Seq(undated, PossessiveCredit.replaceFirstIn(undated, "").trim).filter(q => q.nonEmpty && q != title.trim)
  }
  private val AnniversarySuffix = """(?i)\s*[-–—:]?\s*\(?\d{1,3}(?:st|nd|rd|th)\s+anniversary\)?\s*$""".r
  private val PossessiveCredit  = """^\p{Lu}[\p{L}.-]*(?:\s+[\p{L}.-]+){1,3}['’]s\s+(?=\S)""".r
  /** An author's credit closing a title: a lower-case "by" and a name of two to four capitalised words ("Fallen Angels by
   *  Noël Coward", "Swan Lake by Matthew Bourne"). One word after it, or a word not capitalised, is the title's own:
   *  "Stand by Me", "Death by Chocolate", Polish "Żyć by tańczyć". */
  private val AuthorCredit = """\s+by\s+\p{Lu}[\p{L}.'’-]*(?:\s+\p{Lu}[\p{L}.'’-]*){1,3}\s*$""".r
  /** `title` without the author's credit closing it, when it has one and a title is left. */
  private[identity] def creditedWork(title: String): Option[String] =
    AuthorCredit.findFirstMatchIn(title).map(m => title.take(m.start).trim).filter(_.exists(_.isLetter))

  private val BillJoin = """\s\+\s""".r
  /** May the piece of `l`'s title that names `f` stand for the whole, the rest a banner? Two words at least —
   *  a one-word piece is many films' title ("Bhutan – Trails of Happiness" is not the 1928 "Bhutan") — and no
   *  Roman numeral in the rest: a numbered set ("Bolek i Lolek – zestaw IV") is an instalment, not the film;
   *  and a year the rest states agrees with the record's. */
  def standsForTheWhole(l: Listing, f: Film): Boolean = {
    val pieces = namingPieces(l, f).filter(_.sizeIs >= 2)
    pieces.nonEmpty && {
      val rest  = l.ownForms.flatMap(_.words).toSet -- pieces.flatten
      // a year the rest states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      val years = rest.filter(_.matches("(?:19|20)\\d\\d")).map(_.toInt)
      !rest.exists(w => w.length > 1 && RomanNumeral.pattern.matcher(w).matches()) &&
        (years.isEmpty || f.year.forall(y => years.exists(FactRelations.yearsNear(_, y))))
    }
  }

  /** What a double bill joins LAST with a spaced "+" ("… + The Tiger Who Came to Tea") — a film, or a talk. */
  def billedSecondTitle(l: Listing): Option[String] = BillJoin.split(l.decorations.withoutTail(l.title)).lastOption.filter(_ => billsTwoWorks(l)).map(_.trim)

  /** Does the listing bill two works with a spaced "+" — a double bill, whose facts are one of its films'? Not when
   *  what it bills last is a learned event tail ([[TitleDecorations.withoutTail]]): "Punku + spotkanie z reżyserem". */
  def billsTwoWorks(l: Listing): Boolean = (Seq(l.title) ++ l.rawTitle).exists(t => BillJoin.findFirstIn(l.decorations.withoutTail(t)).isDefined)
  /** Does the listing bill two WHOLE WORKS — two of the pieces its spaced "+" joins naming no event, whatever a database
   *  holds of either? A talk, a screening note or a format joined to one film ("+ prelekcja", "+ spotkanie z reżyserem",
   *  "+ napisy EN", a bracketed "(AD + CC + PJM)") is no second work. PL Kino Pałacowe "… | Historia kina w Popielawach +
   *  Pruska kultura" bills a 1908 short TMDB does not hold: no candidate stood for it, yet the bill is neither film. */
  def billsTwoWholeWorks(l: Listing): Boolean =
    (Seq(l.title) ++ l.rawTitle).exists { t =>
      val pieces = BillJoin.split(l.decorations.withoutTail(Bracketed.replaceAllIn(t, " ").trim)).toSeq
      // the programme the last work runs into after a spaced dash is the bill's ("… + Kocia Szajka - Festiwal TAURON
      // Młode Horyzonty"): the work is what precedes it
      (pieces.init :+ ProgrammeDash.pattern.split(pieces.last, 2).head).count(namesAWork) >= 2
    }
  private val Bracketed     = """\([^()]*\)|\[[^\[\]]*\]""".r
  private val ProgrammeDash = """\s[-–—]\s""".r
  private def namesAWork(piece: String): Boolean = {
    // a one-letter word names nothing: "Q&A" is no work, and the "a" of "We're Going on a Bear Hunt" no event; a title
    // of numbers is one ("2 x 2 = 4", billed after "Zakazane piosenki")
    val all   = TitleContainment.tokens(piece)
    val words = all.filter(_.length > 1)
    (words.exists(_.exists(_.isLetter)) || all.sizeIs >= 3) &&
      !words.exists(w => EventTailWords(w) || services.movies.FormatTags.FormatToken.contains(w))
  }
  /** What a piece a bill joins after a film names when it names an event, not a work: a talk, a lecture, a workshop, a
   *  quiz, a gala, an exhibition or its opening, a concert, a (poster) tour, a signing — PL, EN, DE, ES — beside the
   *  segment model's event words. "OPĘTANIE + TRASA PLAKATOWA" (Światowid) is one film and a poster tour. */
  private val EventTailWords: Set[String] = DecorationSegments.EventWords ++ Set(
    "wstep", "wyklad", "warsztaty", "quiz", "gala", "wystawa", "wernisaz", "trasa", "debata", "podpisywanie", "autografy", "dyskusji",
    "lecture", "workshop", "exhibition", "vernissage", "concert", "tour", "signing", "discussion", "debate",
    "vortrag", "werkstatt", "ausstellung", "konzert", "tournee", "diskussion", "lesung",
    "charla", "coloquio", "taller", "exposicion", "concierto", "gira", "encuentro", "presentacion")
  /** The works a DOUBLE BILL joins with a spaced "+" ("Basia. Humor w paski mam + Kocia Szajka"),
   *  each searched on its own: the database has no record of the bill, so without them the only
   *  candidates are what a credited director's filmography walks to. Searched, not shapes: a bill
   *  is neither of its works, which is why its family keys leave them out. */
  private def billedWorks(l: Listing): Seq[String] =
    (Seq(l.title) ++ l.rawTitle).map(t => BillJoin.split(l.decorations.withoutTail(t)).toSeq.map(_.trim).filter(_.nonEmpty)).filter(_.sizeIs > 1).flatten

  /** A year titles put in as a delimited annotation ("(2026)"), outside any season. */
  def titleYearOf(titles: Seq[String]): Option[Int] = EmbeddedYear.ofAll(titles.map(withoutSeasons), Int.MaxValue)

  /** Do `titles` (a listing's, `rawTitle` the venue's own) bill a stage work — the work named whole, or within a piece:
   *  run into a house's word ("OPERA-COSI FAN TUTTE"), after its composer? */
  def billsStageWork(titles: Seq[String], rawTitle: String): Boolean =
    stageWorksBilled(titles, seasonYear(titles).isDefined).nonEmpty ||
      titles.map(title => Listing(title, Some(rawTitle).filter(_ != title))).exists(stageWorks(_).nonEmpty)

  /** The stage works ([[StageWorks]]) the titles BILL: a piece naming one whole, or ending in one's name — after its
   *  composer (DE "Met Opera 2026/27: Camille Saint-Saëns SAMSON ET DALILA") or a house's word run on ("OPERA-MAKBET") —
   *  or, in a title naming its season (`seasonNamed`), opening with one before a venue's tag ("SAMSON I DALILA-
   *  RETRANSMISJA"). Never a work a title only opens with otherwise: "Manon des sources" is no broadcast of "Manon". */
  def stageWorksBilled(titles: Seq[String], seasonNamed: Boolean): Set[String] =
    titles.flatMap(t => pieces(t) :+ t).flatMap(billedIn(_, seasonNamed)).toSet

  /** A title's delimited pieces, its seasons and bracketed years out. */
  private[identity] def pieces(title: String): Seq[String] = PieceBreak.split(withoutYears(title)).toSeq.map(_.trim).filter(_.nonEmpty)

  /** The stage works one title piece bills ([[stageWorksBilled]]). */
  private[identity] def billedIn(piece: String, seasonNamed: Boolean): Set[String] = {
    val tokens = services.movies.TitleContainment.tokens(piece).toIndexedSeq
    val runs   = tokens.indices.map(tokens.drop) ++ (if (seasonNamed) (1 until tokens.size).map(tokens.take) else Nil)
    runs.flatMap(run => StageWorks.resolver.named(run.mkString)).toSet
  }

  /** The stage works ([[StageWorks]]) a listing's title pieces name, in whatever language. */
  def stageWorks(l: Listing): Set[String] =
    (Seq(l.title) ++ l.rawTitle).flatMap(t => PieceBreak.split(withoutYears(t)).toSeq :+ t).map(key).filter(_.nonEmpty)
      .flatMap(StageWorks.resolver.named).toSet
  /** The stage works a film's own titles name, as [[stageWorks]] reads a listing's. */
  def stageWorks(f: Film): Set[String] =
    f.titles.flatMap(t => PieceBreak.split(withoutYears(t)).toSeq).map(key).filter(_.nonEmpty).flatMap(StageWorks.resolver.named).toSet
  /** A stage broadcast a venue bills with no season but with its year — DE "Royal Ballet & Opera im Kino: Manon"
   *  [2026] — searched as its work's name and that year ("Manon 2026"): the house's season record carries both, while
   *  the work alone ranks it below every namesake film. */
  private def stageWorkQueries(l: Listing): Seq[String] =
    if (l.seasonYear.isDefined) Nil
    else l.year.orElse(l.broadcastSeason).toSeq.flatMap(year =>
      stageWorksBilled(l.rawTitle.toSeq :+ l.title, seasonNamed = false).toSeq.sorted.flatMap(StageWorks.resolver.searchNames).map(name => s"$name $year"))

  /** Do the two titles' banners — their pieces naming no work — share a word ([[bannersMeet]])? */
  def bannersMeetOf(l: Listing, f: Film): Boolean = bannersMeet(l.title +: l.rawTitle.toSeq, f.titles)

  /** A season production searched as its WORK AND ITS SEASON ("Manon 2026"): a house's record of
   *  it ("Royal Ballet & Opera 2026/27: Manon") carries both, however the venue spells the house,
   *  while the work alone ranks it below every namesake. Only the pieces of a title naming the
   *  season ask it, without a bracketed year. */
  private def seasonProductionQueries(l: Listing): Seq[String] =
    l.seasonYear.toSeq.flatMap { season =>
      val seasonTitles = (Seq(l.title) ++ l.rawTitle).filter(t => seasonYear(Seq(t)).isDefined)
      val works = shapes(seasonTitles).filter(t => seasonYear(Seq(t)).isEmpty).map(withoutYears(_).trim).filter(_.nonEmpty).distinct
      // a work named in another language than the record's is asked for by its search name too ("The Nutcracker 2026")
      // — including one named before a translated subtitle ("Cosi fan tutte. Tak czynią wszystkie"), as the match reads it
      // — and by every other name of the work it is filed under, a house's own ("Samson et Dalila 2026"), the work billed
      // within the piece too ("Camille Saint-Saëns SAMSON ET DALILA")
      val translated = works.flatMap(work => (StageWorks.resolver.named(key(work)) ++ StageWorks.resolver.named(key(SentenceStop.split(work, 2).head)) ++
        stageWorksBilled(Seq(work), seasonNamed = true).toSeq).toSeq.sorted.distinct.flatMap(StageWorks.resolver.searchNames))
      (works ++ translated).distinct.map(work => s"$work $season")
    }

  /** The title relations under which another film RIVALS a listing's film: the listing's title
   *  names it as closely (`rivals`). */
  val Rivalling: Set[String] = Set("exact", "original", "alternative")
  /** An IMDb id's title number ("tt0064570" → 64570), 0 for anything else. */
  def imdbNumber(imdbId: String): Int =
    Option.when(imdbId.startsWith("tt"))(imdbId.drop(2)).flatMap(_.toIntOption).filter(_ > 0).getOrElse(0)

  /** The relations under which a film carries the listing's title as one of its own: whole, or as a delimited piece. */
  val TitlesItsOwn: Set[String] = Rivalling + "segment"
  /** The ONE film the listing's search titles name on IMDb ([[CandidateQuery.ImdbTitled]]), with the titles that name
   *  it: a title IMDb lists under several films names none of them ("Santiago", the original Helios publishes beside
   *  "Camino dla opornych", is a dozen films), and the films the rest name must be one. `titled`: each film with the
   *  titles IMDb lists it under. */
  def soleImdbTitled(titled: Map[Int, Set[String]]): Option[(Int, Set[String])] = {
    val sole = titled.toSeq.flatMap { case (film, titles) => titles.filter(_.exists(_.isLetter)).map(_ -> film) }
      .groupMap(_._1)(_._2).collect { case (title, Seq(film)) => film -> title }.toSeq.groupMap(_._1)(_._2)
    Option.when(sole.sizeIs == 1)(sole.head).map { case (film, titles) => film -> titles.toSet }
  }

  /** May the listing take a title IMDb lists `f` under (an AKA TMDB does not carry) as `f`'s own: no year the title
   *  writes, or the venue states, against the film's ("Miłość 2024" is not Haneke's 2012 film), no director it credits
   *  against the film's, no numbered set ("Bolek i Lolek – zestaw
   *  IV"), and no stage work broadcast as a non-season film (the Met's "Così fan tutte" is not Tinto Brass's). Which
   *  films stand beside it is the caller's to read: the ONE IMDb lists so, and none carrying the title already. */
  def takesImdbTitle(l: Listing, f: Film): Boolean = {
    def near(year: Int) = f.year.forall(FactRelations.yearsNear(_, year))
    // nor a year or director the venue publishes against it: "Afrykanska Przygoda 3D IMAX" [2007] {Ben Stassen} is not
    // the 1954 film IMDb also calls "Afrykańska przygoda"
    yearsWritten(l).forall(near) && l.statedYear.forall(near) &&
      !f.directorCredits.exists(credits => creditRelation(l.directorCredits, credits) == Category("different")) &&
      !numbersASet(l) && (stageWorks(l).isEmpty || filmSeason(f).isDefined)
  }

  /** The years the listing's title writes anywhere, bracketed or bare ("Miłość 2024"). */
  def yearsWritten(l: Listing): Set[Int] = l.ownForms.flatMap(_.words).filter(_.matches("(?:19|20)\\d\\d")).map(_.toInt).toSet
  /** Does the listing's title number a set with a Roman numeral ("Bolek i Lolek – zestaw IV")? */
  def numbersASet(l: Listing): Boolean =
    l.ownForms.flatMap(_.words).exists(w => w.length > 1 && RomanNumeral.pattern.matcher(w).matches())

  /** How many films of `pool` other than `film` the listing's title names as closely as a
   *  rival does — the `rivals` measure, over whatever pool the caller searched. */
  def rivals(l: Listing, pool: Map[Int, Film], film: Int): Int =
    pool.count { case (id, f) => id != film && Rivalling(titleRelation(l, f).value) }

  /** The films a listing's title names EXACTLY that one of its own title searches returned FIRST,
   *  from `pool` (each candidate with its best 1-based rank over the listing's searches, `None` when
   *  none returned it). A banner segment's first hit is not one: the listing's whole title must be
   *  the film's. The resolver's top-hit acceptance and the evidence class it is measured as
   *  (`scripts.IdentityEvidenceClasses`) read this one definition. */
  def exactTopHits(l: Listing, pool: Seq[(Int, Film, Option[Int])]): Seq[Int] =
    pool.collect { case (id, f, Some(1)) if titleRelation(l, f) == Category("exact") => id }.distinct.sorted

  /** The venues among `group` (the listings sharing the listing's title key, with their venue)
   *  whose OWN facts back `f` — a title naming it, and its exact year or a credit of its director —
   *  other than `ownVenue`: the `venues.corroborating` count. */
  def corroboratingVenues(f: Film, group: Seq[(String, Listing)], ownVenue: String): Int =
    (backingVenues(f, group) - ownVenue).size

  /** Every venue of `group` whose own facts back `f` ([[corroboratingVenues]] before the asking
   *  venue is taken out): one answer per title group and film ([[VenueBacking]]). */
  def backingVenues(f: Film, group: Seq[(String, Listing)]): Set[String] = {
    // A node's venues share one listing: its answer is found once, not once per venue.
    val backs = new java.util.IdentityHashMap[Listing, java.lang.Boolean]()
    group.iterator.filter { case (_, l) => backs.computeIfAbsent(l, listing => Boolean.box(backsFilm(listing, f))) }.map(_._1).toSet
  }
  /** Do the listing's own facts back `f`? The venue's title must NAME the film: a year or a director
   *  alone backs every film of that year or that director, and the walk of a director's filmography
   *  turns up all of them. */
  private def backsFilm(l: Listing, f: Film): Boolean =
    namesFilm(l, f) && (
      l.statedYear.exists(y => f.year.contains(y)) ||
        f.directorCredits.exists(creditRelation(l.directorCredits, _) == Category("same_person")))

  /** [[corroboratingVenues]] for every asker of the same title groups, each group's backing venues
   *  of a film found once: a wide release lists one title at thousands of venues over one candidate
   *  pool, and asking per member re-reads the whole group per member. A group no venue lists (an
   *  undecorated title nobody lists bare) backs nothing. One per resolve or calibration pass; not
   *  thread-safe. */
  final class VenueBacking(groups: Map[String, Seq[(String, Listing)]]) {
    // By the record INSTANCE, not its value: hashing a whole record (every title, credit and
    // country) per lookup cost more than the backing it saved. An equal record held twice is only
    // computed twice, to the same venues.
    private val memo = scala.collection.mutable.HashMap.empty[String, java.util.IdentityHashMap[Film, Set[String]]]
    /** The venues of `titleGroups` (a listing's [[titleGroups]]) other than `ownVenue` backing `f`. */
    def corroborating(titleGroups: Seq[String], f: Film, ownVenue: String): Int =
      (titleGroups.iterator.flatMap(group => memo.getOrElseUpdate(group, new java.util.IdentityHashMap[Film, Set[String]]())
        .computeIfAbsent(f, film => backingVenues(film, groups.getOrElse(group, Nil)))).toSet - ownVenue).size
  }

  /** The title groups (by [[key]]) whose venues' listings corroborate `l`: its own title's, and the
   *  title a learned venue decoration wraps (`TitleDecorations`) — the other venues list "Mistyczka
   *  2D PL" as "Mistyczka". */
  def titleGroups(l: Listing): Seq[String] =
    (key(l.title) +: l.decoratedOnlyShapes.map(key)).filter(_.nonEmpty).distinct

  /** The title groups (by [[key]]) of the titles `l`'s search asks for — banners and screening notes off
   *  (`searchTitles`) — that are `f`'s own title, original or alternative: "Kino Konesera: Róża" is the
   *  "Róża" other venues list with Schleinzer and 2026 (as the old pipeline folded rows by their search
   *  key), but "ANDRÉ RIEU - NIECH ŻYJE MAASTRICHT!", searched as "André Rieu", is no concert's title. */
  def searchGroups(l: Listing, f: Film): Seq[String] = {
    val titles = f.titleKeys
    l.searchKeys.filter(titles).distinct
  }
  /** Every group [[searchGroups]] can name for `l`, whatever the film — what a family's resolve may read. */
  def searchGroupsAny(l: Listing): Seq[String] = l.searchTitles.map(key).filter(_.nonEmpty).distinct

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
   *  apart from one billing less. The hand-written rules that read a runtime gap read its size. */
  def runtimeGap(m: Map[String, Measure]): Option[Double] =
    m.get("runtime.delta").collect { case Number(d) => math.abs(d) }
  def sameDirector(m: Map[String, Measure]): Boolean = m.get("director").contains(SamePersonMeasure)
  private val SamePersonMeasure = Category("same_person")
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
    m.get("director").foreach { case Category(c) => if (c == "same_person") agree += "director" else if (c == "different") deny += "director"; case _ => }
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
   * @param houses     the houses the listings' banners were learned to be ([[Houses.learn]])
   * @param qualifiers the title pieces learned to be qualifiers, not works ([[Qualifiers.learn]])
   */
  def listingFilm(l: Listing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int,
                  houses: Houses = Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown, country: Option[String] = None): Map[String, Measure] =
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
                    creditRelation(if (persons.size == l.directors.size) l.directorCredits else new Credits(persons), _))
                }
    slots(8)  = runtimeDelta(l, f, country)
    slots(9)  = countryRelation(l.countries, f.countries)
    slots(10) = searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble))
    slots(11) = f.popularity.fold[Measure](MissingFilm)(p => Number(PopularityBucket.of(p).toDouble))
    slots(12) = Number(rivals.toDouble)
    slots(13) = Number(corroboratingVenues.toDouble)
    anniversaryYear(l, f, screeningYearAbsent(l, f, title, new ListingFilmMeasures(slots)))
  }

  /** `m` with the published year absent when it is the year of an ANNIVERSARY the title bills: ES "Drácula. 30
   *  Aniversario" (original title "Bram Stoker's Dracula 30th Anniversary"), dated 2022, is Coppola's 1992 film — its
   *  year that many years before the stated one, within a year. Any other year stays the listing's fact. */
  private def anniversaryYear(l: Listing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (l.anniversary.isEmpty || l.year.isEmpty || f.year.isEmpty || !FactRelations.yearsNear(l.year.get - l.anniversary.get, f.year.get)) m
    else m ++ PublishedYear.map(_ -> MissingListing)
  /** "30th Anniversary", "30 Aniversario": how many years a re-release celebrates. */
  private[identity] val AnniversaryYears = """(?i)\b(\d{1,3})(?:st|nd|rd|th)?\.?\s+(?:anniversary|aniversario)\b""".r

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
        f.directorCredits.exists(creditRelation(l.creditsBesideYear, _) == Category("same_person")))
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
      "year.delta"    -> absDelta(a.year, b.year),
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
    "season.delta", "director", "runtime.delta", "country", "search.rank", "popularity.log2", "rivals", "venues.corroborating")
  private val Slots = { val m = new java.util.HashMap[String, Integer](Keys.length * 2); Keys.zipWithIndex.foreach { case (k, i) => m.put(k, i) }; m }
  private def slotOf(key: String): Int = { val i = Slots.get(key); if (i == null) -1 else i.intValue }
}
