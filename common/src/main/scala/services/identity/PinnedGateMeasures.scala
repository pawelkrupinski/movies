package services.identity

import java.util.Locale

import services.identity.IdentityMeasures.{Category, Film, Listing, Measure, Missing, MissingFilm, MissingListing, Number}
import services.movies.{EmbeddedYear, TitleContainment}
import services.resolution.SearchTitles

/**
 * The LIVE rating gate's measurements, frozen: [[IdentityMeasures]] as it was when the gate's
 * artefact (`identity-weights-gate.json`) was measured and pinned. Two consumers read them, and
 * both are served paths:
 *  - [[StoredIdentityConfidence]], which scores a stored film against the pinned artefact;
 *  - the pipeline's `MovieService.measureTitleSearches`, which stores each TMDB slot's
 *    `titleSearches` — the rank and rivals the gate reads.
 * The resolver's measures evolve in shadow; these may not, or a shadow-only resolver change would
 * move a served rating or a stored row. They change only together with the pinned artefact: a
 * re-pin copies [[IdentityMeasures]] and the refit artefact in one step, after
 * `scripts.IdentityGateImpact` measured the gate on both.
 */
object PinnedGateMeasures {

  /** The measures that are the film database's ranking of its search, as the pinned gate read them. */
  val RankingPriors: Set[String] = Set("search.rank", "popularity.log2", "rivals")

  /** The venue's own year, as the pinned gate read it: its field, else a year its title brackets. */
  private def statedYear(l: Listing): Option[Int] = l.year.orElse(EmbeddedYear.ofAll(l.rawTitle.toSeq :+ l.title, Int.MaxValue))


  /** A title or name as a comparison key: accents folded, lowercased, every non-letter and
   *  non-digit dropped. Script-preserving, rule-free: no title-specific canonicalisation. */
  def key(s: String): String =
    tools.TextNormalization.lettersAndDigitsOnly(tools.TextNormalization.deburr(s).toLowerCase(Locale.ROOT))

  private def words(s: String): Seq[String] = TitleContainment.tokens(s)

  private def people(names: Iterable[String]): Set[String] =
    names.iterator.flatMap(_.split(",")).map(services.movies.PersonKey.of).filter(_.nonEmpty).toSet

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

  /** The title relations under which another film RIVALS a listing's film: the listing's title
   *  names it as closely (`rivals`). */
  val Rivalling: Set[String] = Set("exact", "original", "alternative")

  /** How many films of `pool` other than `film` the listing's title names as closely as a
   *  rival does — the `rivals` measure, over whatever pool the caller searched. */
  def rivals(l: Listing, pool: Map[Int, Film], film: Int): Int =
    pool.count { case (id, f) => id != film && Rivalling(titleRelation(l, f).value) }

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
   *  whose OWN facts back `f` — its exact year, or a credit of its director — other than
   *  `ownVenue`: the `venues.corroborating` count. */
  def corroboratingVenues(f: Film, group: Seq[(String, Listing)], ownVenue: String): Int =
    group.iterator.filter { case (_, l) =>
      statedYear(l).exists(y => f.year.contains(y)) ||
        f.directors.exists(ds => directorRelation(l.directors, ds) == Category("same_person"))
    }.map(_._1).toSet.-(ownVenue).size

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
      "year.delta"     -> delta(statedYear(l), f.year),
      "year.distance"  -> absDelta(statedYear(l), f.year),
      "director"       -> f.directors.fold[Measure](if (l.directors.exists(_.trim.nonEmpty)) MissingFilm else MissingListing)(
                            directorRelation(l.directors, _)),
      "runtime.delta"  -> absDelta(l.runtime.filter(_ > 0), f.runtime.filter(_ > 0)),
      "country"        -> countryRelation(l.countries, f.countries),
      "search.rank"    -> searchRank.fold[Measure](Missing("not-returned"))(r => Number(r.toDouble)),
      "popularity.log2" -> f.popularity.fold[Measure](MissingFilm)(p => Number(PopularityBucket.of(p).toDouble)),
      "rivals"         -> Number(rivals.toDouble),
      "venues.corroborating" -> Number(corroboratingVenues.toDouble)
    )
}
