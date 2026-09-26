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
                           countries: Seq[String] = Nil) {
    private def titles: Seq[String] = rawTitle.toSeq :+ title
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

  /** The BANNER of a season title — the title without its season and without the works its own
   *  delimiters split off ("RBO Cinema Season 2026-27: Manon" → `rbocinemaseason`, "The
   *  Metropolitan Opera 2026/27: Manon" → `themetropolitanopera`): how the title spells its house. */
  def listingBanner(l: Listing): Option[String] = banner(Seq(l.title) ++ l.rawTitle, titleShapes(l))
  def filmBanner(f: Film): Option[String] = banner(filmTitles(f), filmTitles(f).flatMap(SearchTitles.candidates(_, None)))
  private def banner(titles: Seq[String], shapes: Seq[String]): Option[String] = {
    val works = shapes.filter(t => seasonYear(Seq(t)).isEmpty).map(key).filter(_.nonEmpty).distinct.sortBy(w => (-w.length, w))
    titles.filter(t => seasonYear(Seq(t)).isDefined).map(t => works.foldLeft(key(withoutSeasons(t)))((k, w) => k.replace(w, "")))
      .filter(_.nonEmpty).distinct.minByOption(k => (k.length, k))
  }

  /** The house each listing banner spells, read from the season productions its works' titles
   *  name: `(listing banner, film banner, work)` triples, one per season-production candidate. A
   *  banner's house is the film banner naming the most of its DISTINCT works, when strictly more
   *  than any other — "RBO Cinema Season" is Royal Ballet & Opera because its Swan Lake and Alice
   *  are, though TMDB files its Manon only under the Met. No banner is known in advance. */
  def housesOf(named: Iterable[(String, String, String)]): Map[String, String] =
    named.toSeq.distinct.groupBy(_._1).flatMap { case (listingBanner, xs) =>
      xs.groupMapReduce(_._2)(_ => 1)(_ + _).toSeq.sortBy { case (b, n) => (-n, b) } match {
        case Seq((house, _))                          => Some(listingBanner -> house)
        case (house, a) +: (_, b) +: _ if a > b       => Some(listingBanner -> house)
        case _                                        => None
      }
    }

  /** The work a listing and a film's titles share outside the season (`namesSeasonProduction`). */
  def seasonWork(l: Listing, f: Film): Option[String] = {
    def works(titles: Seq[String]) = titles.filter(t => seasonYear(Seq(t)).isEmpty).map(key).filter(_.nonEmpty).toSet
    (works(titleShapes(l)) intersect works(filmTitles(f).flatMap(SearchTitles.candidates(_, None)))).toSeq.sorted.headOption
  }

  /** `t` with every season removed, so a season's end year is never read as a bracketed year. */
  def withoutSeasons(t: String): String =
    Season.replaceAllIn(t, m => if (seasonStart(m).isDefined) " " else scala.util.matching.Regex.quoteReplacement(m.matched))

  /** What TMDB says about a candidate film. `directors`/`countries` are `None` when the film's
   *  details were not fetched, which is not the same as TMDB crediting nobody. `countries` are
   *  ISO 3166-1 alpha-2 codes. */
  final case class Film(title: String, originalTitle: Option[String] = None, alternativeTitles: Seq[String] = Nil,
                        year: Option[Int] = None, runtime: Option[Int] = None, directors: Option[Seq[String]] = None,
                        countries: Option[Seq[String]] = None, popularity: Option[Double] = None)

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
  def titleShapes(l: Listing): Seq[String] = {
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
   *  Royal Opera's "Carmen" of one season), so `listingListing` does not read it. */
  def titleRelation(l: Listing, f: Film): Category = titleRelation(l, f, seasonProductions = true)

  private def titleRelation(l: Listing, f: Film, seasonProductions: Boolean): Category = {
    val ls  = Seq(l.title) ++ l.rawTitle
    val fs  = Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles
    val own = ls.map(key).filter(_.nonEmpty).toSet
    if (own.contains(key(f.title))) Category("exact")
    else if (f.originalTitle.map(key).exists(own)) Category("original")
    else if (f.alternativeTitles.map(key).exists(own)) Category("alternative")
    else containment(ls, titleShapes(l), fs, seasonProductions && namesSeasonProduction(l, f)).getOrElse(
      if (ls.map(words).exists(a => fs.map(words).exists(b => jaccard(a.toSet, b.toSet) > 0))) Category("overlap") else Category("none"))
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
  def searchQueries(l: Listing): Seq[String] = (titleShapes(l) ++ l.originalTitle).map(_.trim).filter(_.nonEmpty).distinct

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

  /** What a listing's own yearless title searches ([[searchQueries]]) said about `film`: where it
   *  ranked (1-based, best over the queries; `None` when no query returned it) and how many other
   *  films they returned that the listing's title names as closely. `None` when no query was
   *  answered. `search` answers ONE query with its ranked results, `None` when it could not. */
  def titleSearch(l: Listing, film: Int, search: String => Option[Seq[Hit]]): Option[models.TitleSearch] = {
    val answers = searchQueries(l).flatMap(q => search(q).toSeq)
    Option.when(answers.nonEmpty) {
      val ranked = answers.flatMap(_.zipWithIndex).groupMapReduce(_._1.tmdbId)(h => h)((a, b) => if (a._2 <= b._2) a else b)
      val pool = ranked.map { case (id, (h, _)) => id -> Film(h.title, h.originalTitle, Nil, h.year, popularity = Some(h.popularity)) }
      models.TitleSearch(key(l.title), ranked.get(film).map(_._2 + 1), rivals(l, pool, film))
    }
  }

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
   */
  def listingFilm(l: Listing, f: Film, searchRank: Option[Int], rivals: Int, corroboratingVenues: Int): Map[String, Measure] =
    Map(
      "title"          -> titleRelation(l, f),
      "originalTitle"  -> originalTitleRelation(l.originalTitle, Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles),
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
    )

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
    val title = (titleRelation(a, asFilm(b), seasonProductions = false), titleRelation(b, asFilm(a), seasonProductions = false)) match {
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
