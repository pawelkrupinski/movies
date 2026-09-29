package services.identity

import java.util.Locale

import services.movies.{EmbeddedYear, TitleContainment}
import services.resolution.SearchTitles

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
                           decorations: TitleDecorations = TitleDecorations.None) {
    private def titles: Seq[String] = rawTitle.toSeq :+ title
    /** The directors credited beside the published `year` — one listing's own, unless a pooled
     *  read took its year and its credits from different listings (`yearCredits`). */
    def creditedBesideYear: Seq[String] = yearCredits.getOrElse(directors)
    /** The season the title names ("2026/27"), by its first year. */
    lazy val seasonYear: Option[Int] = IdentityMeasures.seasonYear(titles)
    /** A year the venue put in its title as a delimited annotation ("(2026)"), outside any season. */
    lazy val titleYear: Option[Int] = EmbeddedYear.ofAll(titles.map(IdentityMeasures.withoutSeasons), Int.MaxValue)
    /** The venue's own year: its field, else the one its title brackets. */
    def statedYear: Option[Int] = year.orElse(titleYear)
    /** A running time the venue put in its title as a bracketed annotation ("(97’)", "[97 min]"). */
    lazy val titleRuntime: Option[Int] = IdentityMeasures.bracketedRuntime(titles)
    /** The venue's own runtime: its field, else the one its title brackets. */
    def statedRuntime: Option[Int] = runtime.filter(_ > 0).orElse(titleRuntime)
    /** `titleShapes`, once per listing: every title relation and billing reads them. */
    private[identity] lazy val shapes: Seq[String] = IdentityMeasures.shapesOf(this)
    /** The title and raw title as comparison forms, and the shapes' keys, once per listing: the
     *  resolver relates every listing to every film of its family's pool (`titleRelation`). */
    private[identity] lazy val ownForms: Seq[IdentityMeasures.TitleForm] = (Seq(title) ++ rawTitle).map(IdentityMeasures.TitleForm(_))
    private[identity] lazy val shapeKeys: Seq[String] = shapes.map(IdentityMeasures.key)
    private[identity] lazy val shapeWords: Seq[Seq[String]] = shapes.map(IdentityMeasures.words)
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
  def seasonYear(titles: Seq[String]): Option[Int] =
    titles.iterator.flatMap(Season.findAllMatchIn).flatMap(seasonStart).toSeq.distinct match {
      case Seq(one) => Some(one)
      case _        => None
    }
  /** The season a film's own titles name ("The Metropolitan Opera 2026/27: Macbeth"). */
  def filmSeason(f: Film): Option[Int] = seasonYear(filmTitles(f))

  private def filmTitles(f: Film): Seq[String] = Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles

  /** Does the film's own title name the listing's SEASON PRODUCTION: both name the same season,
   *  and they share a whole title segment outside it — the work. "Met Opera 2026-27: Samson et
   *  Dalila" and the film database's "The Metropolitan Opera 2026/27: Samson et Dalila" are one
   *  season's production of one work, however each spells the house. Segments are the listing's
   *  own delimiters (`SearchTitles.candidates`) on both sides; a segment carrying the season is
   *  the banner, never the work. */
  def namesSeasonProduction(l: Listing, f: Film): Boolean =
    l.seasonYear.exists(s => filmSeason(f).contains(s)) && seasonWork(l, f).isDefined

  /** The work a listing and a film's titles share outside the season (`namesSeasonProduction`). */
  private def seasonWork(l: Listing, f: Film): Option[String] = {
    def works(titles: Seq[String]) = titles.filter(t => seasonYear(Seq(t)).isEmpty).map(key).filter(_.nonEmpty).toSet
    (works(titleShapes(l)) intersect works(filmTitles(f).flatMap(SearchTitles.candidates(_, None)))).toSeq.sorted.headOption
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
    def banner(title: Seq[String], work: Seq[String]): Option[Seq[String]] =
      Option.when(title.lengthIs > work.length)(
        if (title.endsWith(work)) Some(title.dropRight(work.length)) else if (title.startsWith(work)) Some(title.drop(work.length)) else None
      ).flatten
    val works = (listing.billedWorks intersect film.billedWorks).toSeq.sortBy(work => (-work.length, work.mkString(" ")))
    works.iterator.map { work =>
      (for {
        listingHouse <- listing.billedTitles.flatMap(banner(_, work))
        filmHouse    <- film.billedTitles.flatMap(banner(_, work))
      } yield Billing(listingHouse, filmHouse, work.mkString))
        .sortBy(billed => (billed.listingHouse, billed.filmHouse, billed.listingWords.mkString(" "), billed.filmWords.mkString(" ")))
    }.find(_.nonEmpty).getOrElse(Nil)
  }

  /** Does the film's record bill the listing's work under the listing's OWN house — the banner the
   *  listing puts on the work, spelt as the record spells it or learned to be it (`Houses.same`)?
   *  "NT Live: The Misanthrope" and "National Theatre Live: The Misanthrope" do — by any of the
   *  record's titles, its "National Theatre at Home" alternative aside ([[billings]]); a record titled
   *  the work alone ("The Misanthrope") bills no house at all. Never for a listing naming a season:
   *  a season names its production (`namesSeasonProduction`), and a season-free record is not a
   *  "MetOpera 2025-26" listing's. A season's record is a season-free listing's only when the
   *  listing's banner spells the house ([[spellsItsHouse]]): TMDB filing the Paris Opera's works only
   *  under the Met's 2026/27 records teaches the Paris banner to be the Met. And not a banner numbering its edition otherwise than the record's: a
   *  house's name carries no number ("League of Legends Worlds 26" is not "… Worlds25"). */
  def billsUnderItsHouse(listing: Listing, film: Film, houses: Houses): Boolean =
    listing.seasonYear.isEmpty &&
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
     *  and how many of the banner's DISTINCT works it bills. */
    final case class Contender(house: String, spelt: Int, works: Int) {
      def render: String = s"$house (words $spelt, works $works)"
    }

    /** Each banner's contending houses, best first — what [[learn]] chooses among. */
    def ranking(billings: Iterable[Billing]): Map[String, Seq[Contender]] =
      billings.toSeq.distinct.groupBy(_.listingHouse).map { case (banner, bannerBillings) =>
        val words = bannerBillings.flatMap(_.listingWords).toSet
        banner -> bannerBillings.groupBy(_.filmHouse).toSeq.map { case (house, houseBillings) =>
          Contender(house, (houseBillings.flatMap(_.filmWords).toSet intersect words).size, houseBillings.map(_.work).distinct.size)
        }.sortBy(contender => (-contender.spelt, -contender.works, contender.house))
      }

    /** The house [[learn]] takes from a banner's ranked contenders, if any. */
    def chosen(ranked: Seq[Contender]): Option[Contender] = {
      val best = ranked.head
      val next = ranked.lift(1)
      val spelt  = next.fold(best.spelt > 0)(_.spelt < best.spelt)
      val billed = best.works >= 2 && next.forall(runnerUp => runnerUp.spelt < best.spelt || runnerUp.works < best.works)
      Option.when(spelt || billed)(best)
    }

    /** What a listing's candidates say about its banner: how each record of its work bills it — of
     *  its season, when the listing names one (another season's record says nothing about which
     *  house this season's broadcast is). */
    def evidence(l: Listing, films: Iterable[Film]): Iterable[Billing] =
      films.filter(f => l.seasonYear.isEmpty || namesSeasonProduction(l, f)).flatMap(billing(l, _))
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
  final case class Qualifiers(companions: Map[(String, Boolean), Int]) {
    private def count(piece: Seq[String], trails: Boolean): Int = companions.getOrElse((piece.mkString, trails), 0)
    /** The listing's qualifier pieces, by key: each piece its whole title adds another to along one
     *  edge, which records bill on the same side beside at least two works — one is a coincidence,
     *  as a house's is ([[Houses.learn]]) — and beside more than they bill the rest of the title on
     *  its side. A tie is no qualifier, and neither is a piece the listing publishes as its original
     *  title: the venue names it as the film ("Cineworld 30: The Dark Knight", originally "The Dark
     *  Knight", though TMDB also bills "Enter the World of Hans Zimmer: The Dark Knight"). */
    def of(l: Listing): Set[String] = memo.getOrElseUpdate(l, {
      val named = l.originalTitle.map(yearlessTokens(_).mkString)
      (Seq(l.title) ++ l.rawTitle).flatMap(Qualifiers.split).collect {
        case (piece, rest, trails) if count(piece, trails) >= 2 && count(piece, trails) > count(rest, !trails) && !named.contains(piece.mkString) =>
          piece.mkString
      }.toSet
    })
    private val memo = scala.collection.concurrent.TrieMap.empty[Listing, Set[String]]
  }
  object Qualifiers {
    val Unknown: Qualifiers = Qualifiers(Map.empty)

    /** A title's pieces, each with the rest of the title and whether it TRAILS the rest: every
     *  delimited piece of it ([[shapes]]) that is a token run along one of its edges, and the rest,
     *  both ways round, as yearless tokens ("Dark City: Director's Cut" → `director s cut` trailing
     *  `dark city`, and `dark city` leading `director s cut`). */
    def split(title: String): Seq[(Seq[String], Seq[String], Boolean)] = {
      val whole = yearlessTokens(title)
      shapes(Seq(title)).map(yearlessTokens).filter(p => TitleContainment.isTokenRun(p, whole)).flatMap { p =>
        val trails = !whole.startsWith(p)
        val rest   = if (trails) whole.dropRight(p.length) else whole.drop(p.length)
        Seq((p, rest, trails), (rest, p, !trails))
      }.distinct
    }

    /** Learn from the candidate records one family's listings searched up, by their titles and
     *  original titles. */
    def learn(records: Seq[Film]): Qualifiers =
      Qualifiers(records.flatMap(f => Seq(f.title) ++ f.originalTitle).distinct.flatMap(split)
        .map { case (p, r, trails) => ((p.mkString, trails), r.mkString) }.distinct.groupMapReduce(_._1)(_ => 1)(_ + _))
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
  def withoutYears(t: String): String = BracketedYear.replaceAllIn(withoutSeasons(t), " ")
  private[identity] def yearlessTokens(t: String): Seq[String] = TitleContainment.tokens(withoutYears(t))

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def withoutSeasons(t: String): String =
    Season.replaceAllIn(t, m => if (seasonStart(m).isDefined) " " else scala.util.matching.Regex.quoteReplacement(m.matched))

  /** What TMDB says about a candidate film. `directors`/`countries` are `None` when the film's
   *  details were not fetched, which is not the same as TMDB crediting nobody. `countries` are
   *  ISO 3166-1 alpha-2 codes. */
  final case class Film(title: String, originalTitle: Option[String] = None, alternativeTitles: Seq[String] = Nil,
                        year: Option[Int] = None, runtime: Option[Int] = None, directors: Option[Seq[String]] = None,
                        countries: Option[Seq[String]] = None, popularity: Option[Double] = None) {
    /** The film's titles and their delimited pieces as yearless tokens, once per record (`billing`). */
    private[identity] lazy val billedTitles: Seq[Seq[String]] =
      (Seq(title) ++ originalTitle ++ alternativeTitles).map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty).distinct
    private[identity] lazy val billedWorks: Set[Seq[String]] =
      (Seq(title) ++ originalTitle ++ alternativeTitles).flatMap(SearchTitles.candidates(_, None)).map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
    /** The title, original title and alternative titles, in that order, as comparison forms once
     *  per record (`titleRelation`). */
    private[identity] lazy val forms: Seq[IdentityMeasures.TitleForm] =
      (Seq(title) ++ originalTitle ++ alternativeTitles).map(IdentityMeasures.TitleForm(_))
    /** The film's titles as series and numbers (`numeralRelation`). */
    private[identity] lazy val numberedTitles: Seq[IdentityMeasures.Numbered] =
      (Seq(title) ++ originalTitle ++ alternativeTitles).map(_.trim).filter(_.nonEmpty).distinct.map(IdentityMeasures.numbered)
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
    NonWord.matcher(tools.TextNormalization.deburr(s).toLowerCase(Locale.ROOT)).replaceAll("")
  private val NonWord = java.util.regex.Pattern.compile("[^\\p{L}\\p{N}]+")

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
    lazy val yearless: String       = yearlessTokens(text).mkString
    lazy val latinKey: String       = IdentityMeasures.latinKey(text)
  }

  private def words(s: String): Seq[String] = TitleContainment.tokens(s)

  private def credits(names: Iterable[String]): Seq[String] =
    names.iterator.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).toSeq

  private def latin(names: Iterable[String]): Boolean =
    names.exists(_.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** A name in Latin letters: ICU's general Any-Latin transliteration, then to ASCII. Pinyin for
   *  Han, ISO-style for Cyrillic, Greek, Georgian, …: no table of names. One instance per thread
   *  (an ICU transliterator is not documented as safe to share). */
  private val toLatin = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII"))
  private def latinized(name: String): String = toLatin.get.transliterate(name)

  /** Credit lists as the director relation compares them, each written form found once. */
  final class Credits(raw: Iterable[String]) {
    val names: Seq[String] = credits(raw)
    lazy val keys: Set[String]              = names.map(services.movies.PersonKey.of).filter(_.nonEmpty).toSet
    lazy val words: Seq[Seq[String]]        = names.map(TitleContainment.tokens).filter(_.nonEmpty)
    lazy val isLatin: Boolean               = latin(names)
    lazy val inLatin: Credits               = new Credits(names.map(latinized))
    def isEmpty: Boolean = keys.isEmpty

    /** Some credit names the same person as some credit of `other`: the same words in any order
     *  ([[services.movies.PersonKey]]) or the same letters split otherwise ([[sameLetters]]). */
    def samePerson(other: Credits): Boolean =
      (keys intersect other.keys).nonEmpty || words.exists(a => other.words.exists(sameLetters(a, _)))
  }

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
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = {
    val (ca, cb) = (new Credits(a), new Credits(b))
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

  private def shapesOf(l: Listing): Seq[String] = {
    shapes(Seq(l.title) ++ l.rawTitle ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None)), l.decorations)
  }

  /** `titles` and every part a split leaves, each de-decorated in turn ("Throwback: Donnie Darko
   *  (25th Anniversary)" → "Donnie Darko (25th Anniversary)" → "Donnie Darko"; "(4DX Rewind) Shrek"
   *  → "Shrek" by a learned `decorations` run), to a fixpoint: every shape still a whole delimited
   *  or decorated piece of one of the titles. */
  private def shapes(titles: Seq[String], decorations: TitleDecorations = TitleDecorations.None): Seq[String] =
    Iterator.iterate(titles.map(_.trim).filter(_.nonEmpty).distinct)(s =>
        (s ++ s.flatMap(SearchTitles.candidates(_, None)) ++ s.flatMap(decorations.strip)).map(_.trim).filter(_.nonEmpty).distinct)
      .sliding(2).collectFirst { case Seq(a, b) if a == b => a }.get

  private def jaccard(a: Set[String], b: Set[String]): Double =
    if (a.isEmpty || b.isEmpty) 0.0 else (a intersect b).size.toDouble / (a union b).size

  /** Two titles that are the same words but for ONE, a letter apart — a venue's typo ("The Beast of
   *  Mossy Botton", "Pradhama Drishtiya Kuttakkar") — where both spellings of that word run to five
   *  letters and neither is a number, in a title of two words or more: a sequel's numeral ("Scary Movie 3", "Mission: Impossible II")
   *  or a short word ("Hunt"/"Hurt") is a different title, not a typo. */
  private[identity] def oneTypoApart(a: Seq[String], b: Seq[String]): Boolean =
    // Two words at least: a one-word title a letter from another is another film ("Lalka"/"Lalkar").
    a.size >= 2 && a.size == b.size && a != b && {
      val differing = a.indices.filter(i => a(i) != b(i))
      differing.sizeIs == 1 && {
        val (x, y) = (a(differing.head), b(differing.head))
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
  def titleIsWorkOf(l: Listing, f: Film): Option[Int] = {
    val own = (Seq(l.title) ++ l.rawTitle).map(key).filter(_.nonEmpty).toSet
    (Seq(f.title) ++ f.originalTitle).flatMap(t => """\s*[,:]\s+|\s+[-–—]\s+""".r.findFirstMatchIn(t).map(m => t.substring(0, m.start).trim))
      .find(w => own(key(w))).map(w => TitleContainment.tokens(w).size)
  }

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
    val own = ls.map(_.key).filter(_.nonEmpty).toSet
    lazy val qualifying = qualifiers.of(l)
    val fs  = if (qualifiers.companions.isEmpty) all else all.filterNot(t => qualifying(t.yearless))
    val (titleForm, rest) = (all.head, all.tail)
    val (originalForms, alternativeForms) = rest.splitAt(f.originalTitle.size)
    if (own.contains(titleForm.key) || ls.exists(a => oneTypoApart(a.words, titleForm.words)) ||
        (titleForm.latinKey.nonEmpty && ls.exists(_.latinKey == titleForm.latinKey))) Category("exact")
    else if (originalForms.map(_.key).exists(own)) Category("original")
    else if (alternativeForms.map(_.key).exists(own)) Category("alternative")
    else (if (fs.isEmpty) None
          else containment(ls, l.shapeKeys, fs, houses.exists(h => namesSeasonProduction(l, f) || billing(l, f).exists(h.same)), l.shapeWords)).getOrElse(
      if (ls.exists(a => all.exists(b => jaccard(a.wordSet, b.wordSet) > 0))) Category("overlap") else Category("none"))
  }

  /** How one side's titles name the other's once no whole title matches: a whole delimited piece
   *  of them (a banner segment, the title without a trailing bracket — `segment`, from `shapes`),
   *  the other's title as a token run along one edge of one of them (`decorated`: "Ken Russell's
   *  The Devils"), or one of them as a run along one edge of the other's (`fragment`: "It" beside
   *  "It Ends with Us"). `alsoSegment` is another reason to read a segment (the title relation's
   *  season production), asked only when no shape matches. ONE definition for the title and the
   *  original-title relations. */
  private def containment(own: Seq[TitleForm], shapeKeys: Seq[String], others: Seq[TitleForm], alsoSegment: => Boolean = false,
                          shapeWords: Seq[Seq[String]] = Nil): Option[Category] = {
    val otherKeys = others.map(_.key).filter(_.nonEmpty).toSet
    val ow = own.map(_.words).filter(_.nonEmpty)
    val fw = others.map(_.words).filter(_.nonEmpty)
    // A shape one venue typo from the film's title is a segment too ("Pradhama Drishtiya Kuttakkar
    // (Malayalam)" of "Pradhama Drishtya Kuttakkar", `oneTypoApart`).
    if (shapeKeys.exists(otherKeys) || shapeWords.exists(sw => fw.exists(oneTypoApart(sw, _))) || alsoSegment) Some(Category("segment"))
    else if (ow.exists(a => fw.exists(b => TitleContainment.isTokenRun(b, a)))) Some(Category("decorated"))
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
    unanimous.toSeq.flatMap { case (t, o) =>
      filmsByKey.getOrElse(o, Nil).distinctBy(_._1) match {
        case Seq((id, f)) if !NamingRelations(titleRelation(Listing(spelling(t)), f).value) => Some(id -> spelling(t))
        case _                                                                               => None
      }
    }.groupMap(_._1)(_._2).map { case (id, ts) => id -> ts.distinct.sorted }
  }

  /** `f` with the titles venues publish for it ([[venueTitles]]) among its alternative titles. */
  def withVenueTitles(f: Film, titles: Seq[String]): Film =
    if (titles.isEmpty) f else f.copy(alternativeTitles = f.alternativeTitles ++ titles)

  /** The listing's own ORIGINAL title against every title of the other side: the same title, a
   *  decorated or delimited spelling of one ([[containment]], as the title relation reads it: "Your
   *  Name (re-release)" is `segment` of "Your Name."), a shared long word, or nothing. */
  /** Does a year the original title writes ("… (2024)") agree with the film's — within one — when the
   *  yearless comparison drops it? A title that writes no year drops nothing; one that does, against a
   *  film whose year is unknown, is not read as agreeing: nothing else measures that year. */
  private val WrittenYear = """(?<!\d)(?:18|19|20)\d{2}(?!\d)""".r
  private def yearAgrees(title: String, filmYear: Option[Int]): Boolean = {
    val written = WrittenYear.findAllIn(title).map(_.toInt).toSeq
    written.isEmpty || filmYear.exists(y => written.exists(w => math.abs(w - y) <= 1))
  }

  def originalTitleRelation(original: Option[String], otherTitles: Seq[String], filmYear: Option[Int] = None): Measure =
    original.map(_.trim).filter(_.nonEmpty) match {
      case None => MissingListing
      case Some(o) =>
        val others = otherTitles.map(_.trim).filter(_.nonEmpty)
        if (others.isEmpty) MissingFilm
        // The same title once years and seasons are dropped: "The Metropolitan Opera: Così fan tutte
        // (2026)" is "The Metropolitan Opera 2026/27: Così fan tutte" (the year is measured apart).
        else if (others.map(key).contains(key(o)) || others.exists(t => oneTypoApart(words(o), words(t))) ||
                 others.map(latinKey).contains(latinKey(o)) ||
                 (yearAgrees(o, filmYear) && others.exists(t => yearlessTokens(t).nonEmpty && yearlessTokens(t) == yearlessTokens(o))))
          Category("match")
        else containment(Seq(TitleForm(o)), shapes(Seq(o)).map(key), others.map(TitleForm(_))).getOrElse {
          val ow = words(o).filter(_.length >= 4).toSet
          if (others.exists(t => (words(t).filter(_.length >= 4).toSet intersect ow).nonEmpty)) Category("overlap")
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
  private[identity] final case class Numbered(words: Seq[String], numbers: Set[Int])
  private[identity] def numbered(title: String): Numbered = {
    val pieces = withoutYears(title).split("""[:|/()\[\]–—,.;!?]|\s-\s""").map(TitleContainment.tokens).filter(_.nonEmpty).toSeq
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
      (a.words.mkString == b.words.mkString || TitleContainment.isTokenRun(a.words, b.words) || TitleContainment.isTokenRun(b.words, a.words))

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

  private def absDelta(a: Option[Int], b: Option[Int]): Measure = delta(a, b) match {
    case Number(d) => Number(math.abs(d))
    case other     => other
  }

  /** The title searches a listing's evidence issues: every title shape and its original title,
   *  each asked WITHOUT a year (TMDB dates a film by first release, a venue by production or
   *  re-release). ONE definition: the calibration's candidate pools, the resolver's queries and
   *  the recording sweep all read it. */
  def searchQueries(l: Listing): Seq[String] = (titleShapes(l) ++ l.originalTitle ++ seasonProductionQueries(l) ++ billedWorks(l))
    .map(_.trim).filter(_.nonEmpty).distinct

  private val BillJoin = """\s\+\s""".r
  /** The works a DOUBLE BILL joins with a spaced "+" ("Basia. Humor w paski mam + Kocia Szajka"),
   *  each searched on its own: the database has no record of the bill, so without them the only
   *  candidates are what a credited director's filmography walks to. Searched, not shapes: a bill
   *  is neither of its works, which is why its family keys leave them out. */
  private def billedWorks(l: Listing): Seq[String] =
    (Seq(l.title) ++ l.rawTitle).map(BillJoin.split(_).toSeq.map(_.trim).filter(_.nonEmpty)).filter(_.sizeIs > 1).flatten

  /** A season production searched as its WORK AND ITS SEASON ("Manon 2026"): a house's record of
   *  it ("Royal Ballet & Opera 2026/27: Manon") carries both, however the venue spells the house,
   *  while the work alone ranks it below every namesake. Only the pieces of a title naming the
   *  season ask it, without a bracketed year. */
  private def seasonProductionQueries(l: Listing): Seq[String] =
    l.seasonYear.toSeq.flatMap { season =>
      val seasonTitles = (Seq(l.title) ++ l.rawTitle).filter(t => seasonYear(Seq(t)).isDefined)
      shapes(seasonTitles).filter(t => seasonYear(Seq(t)).isEmpty).map(withoutYears(_).trim).filter(_.nonEmpty).distinct.map(work => s"$work $season")
    }

  /** The title relations under which another film RIVALS a listing's film: the listing's title
   *  names it as closely (`rivals`). */
  val Rivalling: Set[String] = Set("exact", "original", "alternative")

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
        f.directors.exists(ds => directorRelation(l.directors, ds) == Category("same_person")))

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
    (key(l.title) +: undecorated(l).map(key)).filter(_.nonEmpty).distinct

  /** The title shapes only `l`'s learned decorations leave. */
  private def undecorated(l: Listing): Seq[String] =
    if (l.decorations == TitleDecorations.None) Nil
    else (l.shapes.toSet -- l.copy(decorations = TitleDecorations.None).shapes).toSeq.sorted

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
  def sameDirector(m: Map[String, Measure]): Boolean = m.get("director").contains(Category("same_person"))
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
   *  venues) cannot weigh more evidence below less. Signed measures (a year's difference peaks at 0)
   *  and ones with no direction of their own are left to the data. */
  enum EvidenceDirection { case Rising, Falling }
  val NumericDirection: Map[String, EvidenceDirection] = Map(
    "venues.corroborating" -> EvidenceDirection.Rising,
    "search.rank"          -> EvidenceDirection.Falling,
    "runtime.delta"        -> EvidenceDirection.Falling)

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
    m.get("year.distance").foreach { case Number(d) => if (d <= 1) agree += "year" else deny += "year"; case _ => }
    m.get("titleYear.delta").foreach { case Number(d) if math.abs(d) <= 1 => agree += "year"; case _ => }
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
                  houses: Houses = Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown): Map[String, Measure] = {
    val title = titleRelation(l, f, houses, qualifiers)
    screeningYearAbsent(l, f, title, Map(
      "title"          -> title,
      "numeral"        -> numeralRelation(l, f),
      "originalTitle"  -> ownOriginalTitle(l, f, title),
      "year.delta"     -> delta(l.year, f.year),
      "year.distance"  -> absDelta(l.year, f.year),
      "titleYear.delta" -> filmMinus(f.year, l.titleYear),
      "season.delta"   -> filmMinus(f.year, l.seasonYear),
      "director"       -> f.directors.fold[Measure](if (l.directors.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
                            directorRelation(l.directors, _)),
      "runtime.delta"  -> absDelta(l.statedRuntime, f.runtime.filter(_ > 0)),
      "country"        -> countryRelation(l.countries, f.countries),
      "search.rank"    -> searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble)),
      "popularity.log2" -> f.popularity.fold[Measure](MissingFilm)(p => Number(PopularityBucket.of(p).toDouble)),
      "rivals"         -> Number(rivals.toDouble),
      "venues.corroborating" -> Number(corroboratingVenues.toDouble)
    ))
  }

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
   *  borrowed from a sibling's credit. */
  private def screeningYearAbsent(l: Listing, f: Film, title: Category, m: Map[String, Measure]): Map[String, Measure] =
    if (TitledRelations(title.value) && ownAgreement(m)._2("year") &&
        f.directors.exists(directorRelation(l.creditedBesideYear, _) == Category("same_person")))
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
    originalTitleRelation(l.originalTitle, Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles, f.year) match {
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
