package services.identity

import play.api.libs.json.{Json, OFormat}
import services.cinemas.CountryNames
import services.movies.{ListingConstraints, MixedFilmDetector, TitleContainment, TitleNormalizer}
import services.resolution.{SearchTitles, YearWindow}

/**
 * What a listing's evidence says about ONE candidate film, as SIGNALS — the only inputs the score
 * and the learned cannot-links read. Every signal is a general agreement measure (title shape,
 * year, director, runtime, country, where the candidate came from, how many rivals it has, how
 * many other venues' own queries named it); none names a title, a venue, a chain or a franchise.
 * Missing evidence is a value of its own ("missing"), never a zero that reads as disagreement.
 *
 * How much each signal is worth is not decided here: [[IdentityWeights]] is DATA, fitted from
 * corroborated films (docs/design/identity-resolver.md §phase 2).
 */
object Signals {

  /** The categorical signals and their values. A weight is keyed `signal=value`. */
  val Categorical: Seq[(String, Seq[String])] = Seq(
    "title"    -> Seq("exact", "original", "segment", "none"),
    "year"     -> Seq("0", "1-2", "3+", "3+bracket", "missing"),
    "director" -> Seq("match", "mismatch", "missing"),
    "runtime"  -> Seq("0-5", "6-15", "16+", "missing"),
    "country"  -> Seq("match", "mismatch", "missing"),
    "rank"     -> Seq("1", "2-3", "4+", "none"),
    "imdb"     -> Seq("match", "mismatch", "missing"),
    "source"   -> Seq("search+walk", "search", "walk", "family")
  )

  /** The numeric signals the score reads (each scaled to 0..1). `year.delta` and `runtime.delta`
   *  are raw and exist for cannot-link rules; they are absent when either side is missing. */
  val Numeric: Seq[String] = Seq("title.containment", "rivals", "popularity.share", "venues.agreeing", "venues.count")

  /** Every model input, in weight-vector order: the bias, each `signal=value`, each numeric. */
  val ModelInputs: Seq[String] = "bias" +: (Categorical.flatMap { case (s, vs) => vs.map(v => s"$s=$v") } ++ Numeric)
  private val inputIndex: Map[String, Int] = ModelInputs.zipWithIndex.toMap

  final case class Values(categorical: Map[String, String], numeric: Map[String, Double]) {
    /** The model input vector (one-hot categoricals, then the numerics). */
    lazy val vector: Array[Double] = {
      val v = new Array[Double](ModelInputs.size)
      v(0) = 1.0
      categorical.foreach { case (s, value) => inputIndex.get(s"$s=$value").foreach(v(_) = 1.0) }
      Numeric.foreach(n => numeric.get(n).foreach(x => v(inputIndex(n)) = x))
      v
    }
    def category(signal: String): Option[String] = categorical.get(signal)
    def number(signal: String): Option[Double]    = numeric.get(signal)
    def render: String = (Categorical.map(_._1).flatMap(s => categorical.get(s).map(v => s"$s=$v")) ++
      numeric.toSeq.sortBy(_._1).map { case (k, x) => f"$k=$x%.2f" }).mkString(" ")
  }

  private val RivalScale    = math.log1p(20.0)
  private val AgreeingScale = math.log1p(1000.0)

  /** A listing's view for scoring: its title shapes and published facts, which candidates its own
   *  queries named (and at what rank). A family's POOLED view (several listings read as one) has
   *  the same shape. */
  final case class View(exact: Set[String], original: Set[String], segments: Set[String], tokens: Seq[Set[String]],
                        year: Option[Int], yearFromBracket: Boolean, directors: Seq[String], runtime: Option[Int],
                        countries: Set[String], ownSearch: Map[Int, Int], ownWalk: Set[Int])

  object View {
    /** `ownSearch` maps each candidate a title query named to the best (0-based) rank any of the
     *  listing's title queries gave it. */
    def of(e: Evidence, ownSearch: Map[Int, Int], ownWalk: Set[Int], normalizer: TitleNormalizer): View = {
      val exact    = Set(normalizer.sanitize(e.cleanTitle)).filter(_.nonEmpty)
      val original = e.originalTitle.map(normalizer.sanitize).filter(_.nonEmpty).toSet -- exact
      val shapes   = SearchTitles.candidates(e.cleanTitle, e.originalTitle)
      val segments = shapes.map(normalizer.sanitize).filter(_.nonEmpty).toSet -- exact -- original
      View(exact, original, segments,
        shapes.map(s => TitleContainment.tokens(normalizer.searchQuery(s)).toSet).filter(_.nonEmpty).distinct,
        e.statedYear, e.yearFromBracket, e.directors, e.runtime, e.countries.map(CountryNames.canonical).toSet,
        ownSearch, ownWalk)
    }

    /** Several listings read as ONE: every title shape any of them published, the year most of
     *  them state (the smaller on a tie; a bracket year only when no member publishes a field),
     *  every director and country, the median runtime, and every candidate any member's queries
     *  named. A function of the member SET, weighted by listings. */
    def pooled(members: Seq[(View, Int)]): View = {
      val weightOf = members.groupMapReduce(_._1)(_._2)(_ + _)
      val views    = weightOf.keys.toSeq
      def modal(years: Seq[(Int, Int)]): Option[Int] =
        years.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (y, w) => (-w, y) }.headOption.map(_._1)
      val fieldYears   = views.filter(v => v.year.isDefined && !v.yearFromBracket).map(v => v.year.get -> weightOf(v))
      val bracketYears = views.filter(_.yearFromBracket).map(v => v.year.get -> weightOf(v))
      val runtimes     = views.flatMap(v => v.runtime.toSeq.flatMap(r => Seq.fill(weightOf(v))(r))).sorted
      val exact        = views.flatMap(_.exact).toSet
      val original     = views.flatMap(_.original).toSet -- exact
      View(
        exact           = exact,
        original        = original,
        segments        = views.flatMap(_.segments).toSet -- exact -- original,
        tokens          = views.flatMap(_.tokens).distinct.sortBy(_.toSeq.sorted.mkString(" ")),
        year            = modal(fieldYears).orElse(modal(bracketYears)),
        yearFromBracket = fieldYears.isEmpty && bracketYears.nonEmpty,
        directors       = views.flatMap(_.directors).distinct.sorted,
        runtime         = runtimes.lift(runtimes.size / 2),
        countries       = views.flatMap(_.countries).toSet,
        ownSearch       = views.flatMap(_.ownSearch).groupMapReduce(_._1)(_._2)(math.min),
        ownWalk         = views.flatMap(_.ownWalk).toSet)
    }
  }

  private def jaccard(a: Set[String], b: Set[String]): Double =
    if (a.isEmpty || b.isEmpty) 0.0 else (a intersect b).size.toDouble / (a union b).size

  /** How the candidate's titles relate to the view's: exact, original title, a segment, or none. */
  def titleRelation(v: View, c: Candidate, normalizer: TitleNormalizer): String = {
    val titles = c.titles.map(normalizer.sanitize).filter(_.nonEmpty)
    if (titles.exists(v.exact.contains)) "exact"
    else if (titles.exists(v.original.contains)) "original"
    else if (titles.exists(v.segments.contains)) "segment"
    else "none"
  }

  private def yearCompatible(v: View, c: Candidate): Boolean =
    (v.year, c.year) match {
      case (Some(a), Some(b)) => math.abs(a - b) <= YearWindow.ProductionToRelease
      case _                  => true
    }

  /** The signals between two LISTINGS — what a learned "listing-listing" cannot-link reads: how
   *  their titles relate, their year, director, runtime and country agreement, and whether one
   *  venue lists both. The second listing is read as a candidate of the first's view. */
  def between(x: Evidence, y: Evidence, sameVenue: Boolean, normalizer: TitleNormalizer): Values = {
    val vx = View.of(x, Map.empty, Set.empty, normalizer)
    val asCandidate = Candidate(0, Seq(y.cleanTitle) ++ y.originalTitle, y.statedYear, y.directors, y.runtime, 0.0, y.countries)
    val s = new Scorer(vx.copy(yearFromBracket = x.yearFromBracket || y.yearFromBracket), Seq(asCandidate), NoAgreement, normalizer).of(asCandidate)
    Values(s.categorical.view.filterKeys(k => k != "rank" && k != "source" && k != "imdb").toMap +
      ("venue" -> (if (sameVenue) "same" else "different")),
      s.numeric.view.filterKeys(k => k == "year.delta" || k == "runtime.delta" || k == "title.containment").toMap)
  }

  /** Venue agreement on a candidate within a family: the SHARE of the family's listings whose
   *  title relates to it that matched it on their own evidence, and how many listings those are.
   *  The co-occurrence signal that lets a decorated or bare spelling inherit what its plain
   *  siblings' own evidence matched. */
  final case class Agreement(share: Double, listings: Int)
  val NoAgreement: Int => Agreement = _ => Agreement(0.0, 0)

  /** The signals of every candidate of a family's `pool` for the listing — or pooled cluster — `v`. */
  final class Scorer(v: View, pool: Seq[Candidate], agreeing: Int => Agreement, normalizer: TitleNormalizer) {
    private val relation = pool.map(c => c.tmdbId -> titleRelation(v, c, normalizer)).toMap
    private val matching = pool.filter(c => relation(c.tmdbId) != "none" && yearCompatible(v, c))
    private val matchingPopularity = matching.map(_.popularity).sum

    def of(c: Candidate): Values = {
      val cat = Map.newBuilder[String, String]
      val num = Map.newBuilder[String, Double]
      val title = relation.getOrElse(c.tmdbId, titleRelation(v, c, normalizer))
      cat += "title" -> title
      num += "title.containment" -> (if (title != "none") 1.0 else {
        val cTokens = c.titles.map(t => TitleContainment.tokens(normalizer.searchQuery(t)).toSet)
        (for (a <- v.tokens; b <- cTokens) yield jaccard(a, b)).maxOption.getOrElse(0.0)
      })
      (v.year, c.year) match {
        case (Some(a), Some(b)) =>
          val d = math.abs(a - b)
          num += "year.delta" -> d.toDouble
          cat += "year" -> (if (d == 0) "0" else if (d <= YearWindow.ProductionToRelease) "1-2"
                            else if (v.yearFromBracket) "3+bracket" else "3+")
        case _ => cat += "year" -> "missing"
      }
      cat += "director" -> (
        if (v.directors.isEmpty || c.directors.isEmpty) "missing"
        else if (MixedFilmDetector.creditSamePerson(v.directors, c.directors, normalizer)) "match"
        else "mismatch")
      (v.runtime, c.runtime) match {
        case (Some(a), Some(b)) =>
          val d = math.abs(a - b)
          num += "runtime.delta" -> d.toDouble
          cat += "runtime" -> (if (d <= 5) "0-5" else if (d <= 15) "6-15" else "16+")
        case _ => cat += "runtime" -> "missing"
      }
      val cc = c.countries.map(CountryNames.canonical).toSet
      cat += "country" -> (if (v.countries.isEmpty || cc.isEmpty) "missing" else if ((v.countries intersect cc).nonEmpty) "match" else "mismatch")
      cat += "rank" -> v.ownSearch.get(c.tmdbId).fold("none")(r => if (r == 0) "1" else if (r <= 2) "2-3" else "4+")
      cat += "imdb" -> "missing"
      cat += "source" -> ((v.ownSearch.contains(c.tmdbId), v.ownWalk(c.tmdbId)) match {
        case (true, true)   => "search+walk"
        case (true, false)  => "search"
        case (false, true)  => "walk"
        case (false, false) => "family"
      })
      val rivals = matching.count(_.tmdbId != c.tmdbId)
      num += "rivals" -> math.min(1.0, math.log1p(rivals.toDouble) / RivalScale)
      num += "popularity.share" -> (
        if (!matching.exists(_.tmdbId == c.tmdbId)) 0.0
        else if (matchingPopularity > 0) c.popularity / matchingPopularity else 1.0 / matching.size)
      val agreement = agreeing(c.tmdbId)
      num += "venues.agreeing" -> agreement.share
      num += "venues.count" -> math.min(1.0, math.log1p(agreement.listings.toDouble) / AgreeingScale)
      Values(cat.result(), num.result())
    }
  }
}

/**
 * The resolver's DATA: the score's weights and acceptance threshold, and the cannot-link rules
 * learned from corroborated films, each with its measured false-veto rate. Nothing in it is
 * written by hand — the artefact is produced by the calibration (branch `identity-calibration`,
 * `identity-weights.json`) and checked in with its provenance.
 *
 * Weights are keyed by model input (`bias`, `signal=value`, a numeric signal); an input the
 * artefact does not name weighs 0, so the artefact and [[Signals]] can grow independently.
 */
final case class IdentityWeights(version: String, weights: Map[String, Double], threshold: Double,
                                 cannotLinks: Seq[ListingConstraints.LearnedCannotLink] = Nil,
                                 provenance: Map[String, String] = Map.empty) {
  private val w: Array[Double] = Signals.ModelInputs.map(weights.getOrElse(_, 0.0)).toArray

  def logOdds(s: Signals.Values): Double = {
    val x = s.vector; var sum = 0.0; var i = 0
    while (i < w.length) { sum += w(i) * x(i); i += 1 }
    sum
  }
  def probability(s: Signals.Values): Double = IdentityWeights.sigmoid(logOdds(s))

  /** The inputs that moved this score most, for an explanation. */
  def why(s: Signals.Values, top: Int = 4): String = {
    val x = s.vector
    Signals.ModelInputs.indices.filter(i => i > 0 && x(i) != 0 && w(i) != 0).map(i => i -> w(i) * x(i))
      .sortBy(p => -math.abs(p._2)).take(top)
      .map { case (i, c) => f"${Signals.ModelInputs(i)}${if (c >= 0) "+" else ""}$c%.1f" }.mkString(" ")
  }
}

object IdentityWeights {
  implicit val conditionFormat: OFormat[ListingConstraints.LearnedCondition] = Json.format[ListingConstraints.LearnedCondition]
  implicit val ruleFormat: OFormat[ListingConstraints.LearnedCannotLink]     = Json.format[ListingConstraints.LearnedCannotLink]
  implicit val format: OFormat[IdentityWeights]                              = Json.using[Json.WithDefaultValues].format[IdentityWeights]

  def sigmoid(x: Double): Double = 1.0 / (1.0 + math.exp(-x))

  /** The calibration's artefact, and the interim one this branch fitted until it lands. */
  val ArtefactPath = "identity-weights.json"
  val InterimPath  = "services/identity/interim-weights.json"

  def fromResource(path: String): Option[IdentityWeights] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[IdentityWeights] finally in.close()
    }

  /** The calibration's artefact when it is on the classpath, else the interim one. */
  lazy val default: IdentityWeights = fromResource(ArtefactPath).orElse(fromResource(InterimPath))
    .getOrElse(throw new IllegalStateException(s"neither $ArtefactPath nor $InterimPath is on the classpath"))
}
