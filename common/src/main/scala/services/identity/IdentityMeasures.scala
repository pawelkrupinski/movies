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

  /** What one venue published about one film. */
  final case class Listing(title: String, rawTitle: Option[String] = None, originalTitle: Option[String] = None,
                           year: Option[Int] = None, runtime: Option[Int] = None, directors: Seq[String] = Nil,
                           countries: Seq[String] = Nil, yearCredits: Option[Seq[String]] = None) {
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
    /** The title and raw title, and the shapes, as yearless tokens (`billing`). */
    private[identity] lazy val billedTitles: Seq[Seq[String]] = (Seq(title) ++ rawTitle).map(IdentityMeasures.yearlessTokens).distinct
    private[identity] lazy val billedWorks: Set[Seq[String]] = shapes.map(IdentityMeasures.yearlessTokens).toSet.filter(_.nonEmpty)
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
  def billing(l: Listing, f: Film): Option[Billing] = {
    def banner(title: Seq[String], work: Seq[String]): Option[Seq[String]] =
      Option.when(title.lengthIs > work.length)(
        if (title.endsWith(work)) Some(title.dropRight(work.length)) else if (title.startsWith(work)) Some(title.drop(work.length)) else None
      ).flatten
    val works = (l.billedWorks intersect f.billedWorks).toSeq.sortBy(w => (-w.length, w.mkString(" ")))
    works.iterator.flatMap { w =>
      (for {
        lh <- l.billedTitles.flatMap(banner(_, w))
        fh <- f.billedTitles.flatMap(banner(_, w))
      } yield Billing(lh, fh, w.mkString)).sortBy(b => (b.listingHouse, b.filmHouse, b.listingWords.mkString(" "), b.filmWords.mkString(" "))).headOption
    }.nextOption()
  }

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
      Houses(billings.toSeq.distinct.groupBy(_.listingHouse).flatMap { case (banner, bs) =>
        val words  = bs.flatMap(_.listingWords).toSet
        val ranked = bs.groupBy(_.filmHouse).toSeq.map { case (h, hb) =>
          (h, (hb.flatMap(_.filmWords).toSet intersect words).size, hb.map(_.work).distinct.size)
        }.sortBy { case (h, spelt, works) => (-spelt, -works, h) }
        val best = ranked.head
        val next = ranked.lift(1)
        val spelt = next.fold(best._2 > 0)(_._2 < best._2)
        val billed = best._3 >= 2 && next.forall(n => n._2 < best._2 || n._3 < best._3)
        Option.when(spelt || billed)(banner -> best._1)
      })

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

  /** The listing-film measures that compare a FACT the listing published beside its title — a
   *  year (field, bracket or season), a director, a runtime, a country, an original title: every
   *  measure but the title relation, the ranking priors and the pooled count. Derived from
   *  [[listingFilm]] itself, so a new measure is a fact unless it is classified otherwise. */
  lazy val FactMeasures: Set[String] =
    listingFilm(Listing(""), Film(""), None, 0, 0).keySet -- RankingPriors -- PooledMeasures - "title"

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
    tools.TextNormalization.deburr(s).toLowerCase(Locale.ROOT).replaceAll("[^\\p{L}\\p{N}]+", "")

  private def words(s: String): Seq[String] = TitleContainment.tokens(s)

  private def people(names: Iterable[String]): Set[String] =
    names.iterator.flatMap(_.split(",")).map(services.movies.PersonKey.of).filter(_.nonEmpty).toSet

  private def latin(names: Iterable[String]): Boolean =
    names.exists(_.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** A name in Latin letters: ICU's general Any-Latin transliteration, then to ASCII. Pinyin for
   *  Han, ISO-style for Cyrillic, Greek, Georgian, …: no table of names. One instance per thread
   *  (an ICU transliterator is not documented as safe to share). */
  private val toLatin = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII"))
  private def latinized(name: String): String = toLatin.get.transliterate(name)

  private def byName(pa: Set[String], pb: Set[String], disagree: String): Category =
    if ((pa intersect pb).nonEmpty) Category("same_person")
    else {
      val wa = pa.flatMap(_.split(" ")).filter(_.length >= 3)
      val wb = pb.flatMap(_.split(" ")).filter(_.length >= 3)
      Category(if ((wa intersect wb).nonEmpty) "shared_name" else disagree)
    }

  /** How two credit lists relate: one person in common, a shared name word only (a surname, a
   *  transliteration), or nobody in common. Credits in different scripts (a venue's "Bi Gan",
   *  TMDB's "毕赣") are compared in Latin letters ([[latinized]]); nobody in common THERE is
   *  `different_script`, its own category, since a transliteration can miss a person a Latin
   *  spelling names (a Japanese reading of Kanji is not its pinyin). `incomparable` only when a
   *  side has no name left to compare in Latin letters. */
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = {
    val (pa, pb) = (people(a), people(b))
    if (pa.isEmpty) MissingListing
    else if (pb.isEmpty) MissingFilm
    else if (latin(a) == latin(b)) byName(pa, pb, "different")
    else {
      val (la, lb) = (people(a.map(latinized)), people(b.map(latinized)))
      if (la.isEmpty || lb.isEmpty) Category("incomparable") else byName(la, lb, "different_script")
    }
  }

  /** The shapes a listing's title can name a film by: the whole title, its raw form, and each
   *  programme-banner segment (`SearchTitles.candidates`: `|`, ` - `, a first `: `, …). */
  def titleShapes(l: Listing): Seq[String] = l.shapes

  private def shapesOf(l: Listing): Seq[String] = {
    shapes(Seq(l.title) ++ l.rawTitle ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None)))
  }

  /** `titles` and every part a split leaves, each de-decorated in turn ("Throwback: Donnie Darko
   *  (25th Anniversary)" → "Donnie Darko (25th Anniversary)" → "Donnie Darko"), to a fixpoint:
   *  every shape still a whole delimited piece of one of the titles. */
  private def shapes(titles: Seq[String]): Seq[String] =
    Iterator.iterate(titles.map(_.trim).filter(_.nonEmpty).distinct)(s =>
        (s ++ s.flatMap(SearchTitles.candidates(_, None))).map(_.trim).filter(_.nonEmpty).distinct)
      .sliding(2).collectFirst { case Seq(a, b) if a == b => a }.get

  private def jaccard(a: Set[String], b: Set[String]): Double =
    if (a.isEmpty || b.isEmpty) 0.0 else (a intersect b).size.toDouble / (a union b).size

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
    val ls  = Seq(l.title) ++ l.rawTitle
    val all = Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles
    val own = ls.map(key).filter(_.nonEmpty).toSet
    lazy val qualifying = qualifiers.of(l)
    val fs  = if (qualifiers.companions.isEmpty) all else all.filterNot(t => qualifying(yearlessTokens(t).mkString))
    if (own.contains(key(f.title))) Category("exact")
    else if (f.originalTitle.map(key).exists(own)) Category("original")
    else if (f.alternativeTitles.map(key).exists(own)) Category("alternative")
    else (if (fs.isEmpty) None
          else containment(ls, titleShapes(l), fs, houses.exists(h => namesSeasonProduction(l, f) || billing(l, f).exists(h.same)))).getOrElse(
      if (ls.map(words).exists(a => all.map(words).exists(b => jaccard(a.toSet, b.toSet) > 0))) Category("overlap") else Category("none"))
  }

  /** How one side's titles name the other's once no whole title matches: a whole delimited piece
   *  of them (a banner segment, the title without a trailing bracket — `segment`, from `shapes`),
   *  the other's title as a token run along one edge of one of them (`decorated`: "Ken Russell's
   *  The Devils"), or one of them as a run along one edge of the other's (`fragment`: "It" beside
   *  "It Ends with Us"). `alsoSegment` is another reason to read a segment (the title relation's
   *  season production), asked only when no shape matches. ONE definition for the title and the
   *  original-title relations. */
  private def containment(own: Seq[String], shapes: Seq[String], others: Seq[String], alsoSegment: => Boolean = false): Option[Category] = {
    val otherKeys = others.map(key).filter(_.nonEmpty).toSet
    val ow = own.map(words).filter(_.nonEmpty)
    val fw = others.map(words).filter(_.nonEmpty)
    if (shapes.map(key).exists(otherKeys) || alsoSegment) Some(Category("segment"))
    else if (ow.exists(a => fw.exists(b => TitleContainment.isTokenRun(b, a)))) Some(Category("decorated"))
    else if (ow.exists(a => fw.exists(b => TitleContainment.isTokenRun(a, b)))) Some(Category("fragment"))
    else None
  }

  /** The pieces of the listing's title that NAME the film, as words: a title shape equal to one of
   *  the film's titles (the whole title, a banner segment), or one of the film's titles as a run
   *  along one edge of the listing's (a decoration). Empty when no piece names it. */
  def namingPieces(l: Listing, f: Film): Set[Seq[String]] = {
    val filmTitles = (Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles).filter(t => key(t).nonEmpty)
    val keys       = filmTitles.map(key).toSet
    val segments   = titleShapes(l).filter(s => keys(key(s))).map(words)
    val edgeRuns   = for {
      own  <- (Seq(l.title) ++ l.rawTitle).map(words)
      film <- filmTitles.map(words) if TitleContainment.isTokenRun(film, own)
    } yield film
    (segments ++ edgeRuns).filter(_.nonEmpty).toSet
  }

  /** Does the listing's title name the two films by DISJOINT pieces — "Lalka (Dolly)" names
   *  Kawalski's "Lalka" by one word and Blackhurst's "Dolly" by the other, and neither by the
   *  whole? Then its title names both alike, and only what else it publishes can tell them apart.
   *  Nested pieces are no such tie: "Joker: Folie à deux" names its own film by the whole title
   *  and "Joker" only by a piece of it. */
  def namedApart(l: Listing, a: Film, b: Film): Boolean = {
    val (pa, pb) = (namingPieces(l, a), namingPieces(l, b))
    pa.nonEmpty && pb.nonEmpty && pa.forall(x => pb.forall(y => (x.toSet intersect y.toSet).isEmpty))
  }

  /** The listing's own ORIGINAL title against every title of the other side: the same title, a
   *  decorated or delimited spelling of one ([[containment]], as the title relation reads it: "Your
   *  Name (re-release)" is `segment` of "Your Name."), a shared long word, or nothing. */
  def originalTitleRelation(original: Option[String], otherTitles: Seq[String]): Measure =
    original.map(_.trim).filter(_.nonEmpty) match {
      case None => MissingListing
      case Some(o) =>
        val others = otherTitles.map(_.trim).filter(_.nonEmpty)
        if (others.isEmpty) MissingFilm
        else if (others.map(key).contains(key(o))) Category("match")
        else containment(Seq(o), shapes(Seq(o)), others).getOrElse {
          val ow = words(o).filter(_.length >= 4).toSet
          if (others.exists(t => (words(t).filter(_.length >= 4).toSet intersect ow).nonEmpty)) Category("overlap")
          else Category("disjoint")
        }
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
  def searchQueries(l: Listing): Seq[String] = (titleShapes(l) ++ l.originalTitle ++ seasonProductionQueries(l))
    .map(_.trim).filter(_.nonEmpty).distinct

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
  def backingVenues(f: Film, group: Seq[(String, Listing)]): Set[String] =
    group.iterator.filter { case (_, l) =>
      // The venue's title must NAME the film: a year or a director alone backs every film of that
      // year or that director, and the walk of a director's filmography turns up all of them.
      NamingRelations(titleRelation(l, f).value) && (
        l.statedYear.exists(y => f.year.contains(y)) ||
          f.directors.exists(ds => directorRelation(l.directors, ds) == Category("same_person")))
    }.map(_._1).toSet

  /** [[corroboratingVenues]] for every asker of the same title groups, each group's backing venues
   *  of a film found once: a wide release lists one title at thousands of venues over one candidate
   *  pool, and asking per member re-reads the whole group per member. One per resolve or
   *  calibration pass; not thread-safe. */
  final class VenueBacking(groups: String => Seq[(String, Listing)]) {
    private val memo = scala.collection.mutable.HashMap.empty[(String, Film), Set[String]]
    def corroborating(group: String, f: Film, ownVenue: String): Int =
      (memo.getOrElseUpdate((group, f), backingVenues(f, groups(group))) - ownVenue).size
  }

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
   *  Judgment Day"), a delimited piece of it, or a run cut off one edge of it ("BTS World Tour
   *  'ARIRANG' In Buenos Aires: Live" beside "…: LIVE VIEWING")? Then it carries nothing the title
   *  does not: it is the title again, not a second fact, and — as a title relation alone — never
   *  vetoes. Read by [[originalTitleRelation]] against the listing's own titles, so one definition
   *  of "carries" serves both. An original title LONGER than the listing's (a `decorated` one)
   *  says more than the title, and stays a fact. */
  def repeatsItsTitle(l: Listing): Boolean =
    originalTitleRelation(l.originalTitle, l.rawTitle.toSeq :+ l.title) match {
      case Category(c) => CopiesOfTheTitle(c)
      case _           => false
    }
  private val CopiesOfTheTitle: Set[String] = Set("match", "segment", "fragment")

  /** Categories whose evidence cannot weaken as the listing carries more of the other side, per
   *  measure, strongest first: a decoration carries the film's whole title, an overlap some of its
   *  words, `none` nothing. The calibration fits their weights under this order
   *  (`IdentityCalibrate.inOrder`) — an ORDER, never a weight; categories it leaves out are placed by
   *  the data alone. */
  val EvidenceOrder: Map[String, Seq[String]] = Map("title" -> Seq("decorated", "overlap", "none"))

  /** Title relations that name a film: the listing's title is (a spelling of) the film's. */
  val NamingRelations: Set[String] = Set("exact", "original", "alternative", "segment", "decorated")

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
                  houses: Houses = Houses.Unknown, qualifiers: Qualifiers = Qualifiers.Unknown): Map[String, Measure] =
    screeningYearAbsent(l, f, Map(
      "title"          -> titleRelation(l, f, houses, qualifiers),
      "originalTitle"  -> ownOriginalTitle(l, f),
      "year.delta"     -> delta(l.year, f.year),
      "year.distance"  -> absDelta(l.year, f.year),
      "titleYear.delta" -> filmMinus(f.year, l.titleYear),
      "season.delta"   -> filmMinus(f.year, l.seasonYear),
      "director"       -> f.directors.fold[Measure](if (l.directors.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
                            directorRelation(l.directors, _)),
      "runtime.delta"  -> absDelta(l.statedRuntime, f.runtime.filter(_ > 0)),
      "country"        -> countryRelation(l.countries, f.countries),
      "search.rank"    -> searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble)),
      "popularity.log2" -> f.popularity.fold[Measure](MissingFilm)(p => Number(math.floor(math.log(math.max(p, 1e-3)) / math.log(2)))),
      "rivals"         -> Number(rivals.toDouble),
      "venues.corroborating" -> Number(corroboratingVenues.toDouble)
    ))

  /** `m` with the published year absent when it dates a screening ([[PublishedYear]]): beside the
   *  same director, a year that denies the film is the year the venue shows it (Kinoteka's 2026 on
   *  Ken Russell's 1971 "Diabły") or releases it, not another film's — in the score as in a veto.
   *  "Beside" is one listing's: the director must be credited by the listing that published the
   *  year ([[Listing.creditedBesideYear]]), never borrowed from a sibling's credit. */
  private def screeningYearAbsent(l: Listing, f: Film, m: Map[String, Measure]): Map[String, Measure] =
    if (ownAgreement(m)._2("year") && f.directors.exists(directorRelation(l.creditedBesideYear, _) == Category("same_person")))
      m ++ PublishedYear.map(_ -> MissingListing)
    else m

  /** The listing's original title against the film's titles — unless it only repeats the
   *  listing's own title ([[repeatsItsTitle]]): then it is the title again, which the title relation
   *  already weighs, so it counts only where it agrees (the film carries it whole, `match`) and is
   *  otherwise absent. It never weighs against a film, in the score or a veto: counted, a
   *  truncated copy ("…: Live") read as a fragment of the very record the title names exactly. */
  private def ownOriginalTitle(l: Listing, f: Film): Measure =
    originalTitleRelation(l.originalTitle, Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles) match {
      case m if repeatsItsTitle(l) && m != Category("match") => MissingListing
      case m                                                 => m
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
