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
    /** The venue's own year: its field, else the one its title brackets. */
    def statedYear: Option[Int] = year.orElse(EmbeddedYear.ofAll(rawTitle.toSeq :+ title, Int.MaxValue))
  }

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

  // ── keys ─────────────────────────────────────────────────────────────────────────────

  /** A title or name as a comparison key: accents folded, lowercased, every non-letter and
   *  non-digit dropped. Script-preserving, rule-free: no title-specific canonicalisation. */
  def key(s: String): String =
    tools.TextNormalization.deburr(s).toLowerCase(Locale.ROOT).replaceAll("[^\\p{L}\\p{N}]+", "")

  private def words(s: String): Seq[String] = TitleContainment.tokens(s)

  /** A person as an order-insensitive whole-name key: "Makoto Shinkai" = "Shinkai Makoto". */
  private def personKey(name: String): String = words(name).sorted.mkString(" ")

  private def people(names: Iterable[String]): Set[String] =
    names.iterator.flatMap(_.split(",")).map(personKey).filter(_.nonEmpty).toSet

  private def latin(names: Iterable[String]): Boolean =
    names.exists(_.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN))

  /** How two credit lists relate: one person in common, a shared name word only (a surname, a
   *  transliteration), nobody in common, or incomparable (one side not in Latin script). */
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = {
    val (pa, pb) = (people(a), people(b))
    if (pa.isEmpty) MissingListing
    else if (pb.isEmpty) MissingFilm
    else if ((pa intersect pb).nonEmpty) Category("same_person")
    else if (latin(a) != latin(b)) Category("incomparable")
    else {
      val wa = pa.flatMap(_.split(" ")).filter(_.length >= 3)
      val wb = pb.flatMap(_.split(" ")).filter(_.length >= 3)
      if ((wa intersect wb).nonEmpty) Category("shared_name") else Category("different")
    }
  }

  /** The shapes a listing's title can name a film by: the whole title, its raw form, and each
   *  programme-banner segment (`SearchTitles.candidates`: `|`, ` - `, a first `: `, …). */
  def titleShapes(l: Listing): Seq[String] =
    (Seq(l.title) ++ l.rawTitle ++ SearchTitles.candidates(l.title, l.originalTitle) ++
      l.rawTitle.toSeq.flatMap(SearchTitles.candidates(_, None))).map(_.trim).filter(_.nonEmpty).distinct

  private def jaccard(a: Set[String], b: Set[String]): Double =
    if (a.isEmpty || b.isEmpty) 0.0 else (a intersect b).size.toDouble / (a union b).size

  /** How the listing's title names the film: its localised title exactly, its original title,
   *  an alternative title, one whole banner segment, a token run along one edge (a decoration),
   *  some shared words, or nothing. */
  def titleRelation(l: Listing, f: Film): Category = {
    val own    = (Seq(l.title) ++ l.rawTitle).map(key).filter(_.nonEmpty).toSet
    val shapes = titleShapes(l).map(key).filter(_.nonEmpty).toSet
    val primary  = key(f.title)
    val original = f.originalTitle.map(key).filter(_.nonEmpty).toSet
    val alts     = f.alternativeTitles.map(key).filter(_.nonEmpty).toSet
    val all      = original ++ alts + primary
    if (own.contains(primary)) Category("exact")
    else if (own.exists(original.contains)) Category("original")
    else if (own.exists(alts.contains)) Category("alternative")
    else if (shapes.exists(all.contains)) Category("segment")
    else {
      val ls = (Seq(l.title) ++ l.rawTitle).map(words).filter(_.nonEmpty)
      val fs = (Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles).map(words).filter(_.nonEmpty)
      if (ls.exists(a => fs.exists(b => TitleContainment.isTokenRun(b, a) || TitleContainment.isTokenRun(a, b))))
        Category("contains")
      else if (ls.exists(a => fs.exists(b => jaccard(a.toSet, b.toSet) > 0))) Category("overlap")
      else Category("none")
    }
  }

  /** The listing's own ORIGINAL title against every title of the other side. */
  def originalTitleRelation(original: Option[String], otherTitles: Seq[String]): Measure =
    original.map(_.trim).filter(_.nonEmpty) match {
      case None => MissingListing
      case Some(o) =>
        val others = otherTitles.map(_.trim).filter(_.nonEmpty)
        if (others.isEmpty) MissingFilm
        else if (others.map(key).contains(key(o))) Category("match")
        else {
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

  private def absDelta(a: Option[Int], b: Option[Int]): Measure = delta(a, b) match {
    case Number(d) => Number(math.abs(d))
    case other     => other
  }

  /** The title searches a listing's evidence issues: every title shape and its original title,
   *  each asked WITHOUT a year (TMDB dates a film by first release, a venue by production or
   *  re-release). ONE definition: the calibration's candidate pools, the resolver's queries and
   *  the recording sweep all read it. */
  def searchQueries(l: Listing): Seq[String] = (titleShapes(l) ++ l.originalTitle).map(_.trim).filter(_.nonEmpty).distinct

  /** The venues among `group` (the listings sharing the listing's title key, with their venue)
   *  whose OWN facts back `f` — its exact year, or a credit of its director — other than
   *  `ownVenue`: the `venues.corroborating` count. */
  def corroboratingVenues(f: Film, group: Seq[(String, Listing)], ownVenue: String): Int =
    group.iterator.filter { case (_, l) =>
      l.statedYear.exists(y => f.year.contains(y)) ||
        f.directors.exists(ds => directorRelation(l.directors, ds) == Category("same_person"))
    }.map(_._1).toSet.-(ownVenue).size

  /** What a listing's own measurements say about a film by themselves: the corroborators that
   *  agree (a year within one, the same director, the original title) and those that deny it. */
  def ownAgreement(m: Map[String, Measure]): (Set[String], Set[String]) = {
    val agree = Set.newBuilder[String]; val deny = Set.newBuilder[String]
    m.get("year.distance").foreach { case Number(d) => if (d <= 1) agree += "year" else deny += "year"; case _ => }
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
      "year.delta"     -> delta(l.statedYear, f.year),
      "year.distance"  -> absDelta(l.statedYear, f.year),
      "director"       -> f.directors.fold[Measure](if (l.directors.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
                            directorRelation(l.directors, _)),
      "runtime.delta"  -> absDelta(l.runtime.filter(_ > 0), f.runtime.filter(_ > 0)),
      "country"        -> countryRelation(l.countries, f.countries),
      "search.rank"    -> searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble)),
      "popularity.log2" -> f.popularity.fold[Measure](MissingFilm)(p => Number(math.floor(math.log(math.max(p, 1e-3)) / math.log(2)))),
      "rivals"         -> Number(rivals.toDouble),
      "venues.corroborating" -> Number(corroboratingVenues.toDouble)
    )

  /** Titles a listing names itself by, as the other side of a listing-listing comparison. */
  private def asFilm(l: Listing): Film =
    Film(l.title, l.originalTitle, l.rawTitle.toSeq, l.statedYear, l.runtime, Some(l.directors).filter(_.exists(_.trim.nonEmpty)),
      None, None)

  /**
   * Two listings: are they one film? `sharedChainId` is whether the two venues' chains published
   * an id in a common namespace, and whether it was the same id (`None` when there is no common
   * namespace).
   */
  def listingListing(a: Listing, b: Listing, sameVenue: Boolean, sharedChainId: Option[Boolean]): Map[String, Measure] = {
    val fb = asFilm(b)
    val title = titleRelation(a, fb) match {
      case Category("original") | Category("alternative") => Category("exact")
      case other                                          => other
    }
    Map(
      "title"         -> title,
      "originalTitle" -> ((a.originalTitle.filter(_.trim.nonEmpty), b.originalTitle.filter(_.trim.nonEmpty)) match {
                           case (Some(_), Some(ob)) => originalTitleRelation(a.originalTitle, Seq(ob))
                           case (None, _)           => MissingListing
                           case (_, None)           => MissingFilm
                         }),
      "year.delta"    -> absDelta(a.statedYear, b.statedYear),
      "director"      -> directorRelation(a.directors, b.directors),
      "runtime.delta" -> absDelta(a.runtime.filter(_ > 0), b.runtime.filter(_ > 0)),
      "venue"         -> Category(if (sameVenue) "same" else "different"),
      "chainId"       -> sharedChainId.fold[Measure](Missing("no-shared-namespace"))(s => Category(if (s) "same" else "different"))
    )
  }
}
