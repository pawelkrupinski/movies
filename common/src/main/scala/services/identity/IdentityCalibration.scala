package services.identity

import play.api.libs.json.{Json, OFormat}

import services.identity.IdentityMeasures.{Category, Measure, Missing, Number}

/**
 * The identity resolver's calibrated evidence model, as DATA: `identity-weights.json`, produced
 * by `scripts.IdentityCalibrate` from corroborated films (docs/design/identity-resolver.md
 * §14). Nothing in the artefact is written by hand; this class only evaluates it.
 *
 * Per scope ("listing-film": a listing against a TMDB candidate; "listing-listing": two listings)
 * the artefact holds:
 *  - `prior` and one log-likelihood-ratio weight per signal VALUE (naive Bayes). A categorical
 *    signal weighs by its category, a numeric one by the data-derived bin its number falls in,
 *    and a missing one by which side is missing. A signal the artefact does not name weighs 0.
 *  - `calibration`: an isotonic map from the summed log-odds to a probability, fitted on a
 *    held-out split.
 *  - `thresholds`: the probability above which ratings are shown, and the one below which a pair
 *    is a cannot-link, each with the held-out error it was chosen at.
 * Plus `cannotLinks`: conjunctions of signal conditions whose measured false-veto rate is under
 * the stated bound — the learned vetoes, evaluated generically by [[cannotLink]]. And
 * `evidenceClasses`: conjunctions of signal conditions whose measured wrong share, as a class, is
 * within the wrong rate ratings are shown at — each with the probability it measured, which a
 * listing in the class may be credited with where the naive-Bayes sum undersells it
 * ([[classProbability]]; derived by `scripts.IdentityEvidenceClasses`).
 */
final case class IdentityCalibration(version: String,
                                     scopes: Map[String, IdentityCalibration.ScopeModel],
                                     cannotLinks: Seq[IdentityCalibration.CannotLinkRule] = Nil,
                                     evidenceClasses: Seq[IdentityCalibration.EvidenceClass] = Nil,
                                     provenance: Map[String, String] = Map.empty) {
  import IdentityCalibration.*

  private def model(scope: String): ScopeModel = {
    val m = scopes.getOrElse(scope, null)   // no closure over `scope` per call: every score asks
    if (m == null) throw new NoSuchElementException(s"identity-weights.json has no scope '$scope'")
    m
  }

  /** Each signal's contribution (its log-likelihood ratio) to the log-odds. */
  def contributions(scope: String, measures: Map[String, Measure]): Seq[(String, Double)] = {
    val m = model(scope)
    m.bySignalName.map { case (name, w) => name -> w.weightOf(measures.getOrElse(name, null)) }
  }

  /** The naive-Bayes log-odds that the pair is one film: the prior plus every signal's weight. */
  def logOdds(scope: String, measures: Map[String, Measure]): Double =
    model(scope).prior + contributionSum(scope, measures)

  /** The sum of [[contributions]]' weights, added in the same order as `.map(_._2).sum` adds them (the first, then each
   *  next) — with no tuple, option or box per signal: the resolver scores every candidate pair through it. */
  def contributionSum(scope: String, measures: Map[String, Measure]): Double = {
    val m = model(scope)
    val names   = m.signalNames
    val weights = m.signalWeights
    if (names.isEmpty) 0.0
    else {
      var sum = weights(0).weightOf(measures.getOrElse(names(0), null))
      var i   = 1
      while (i < names.length) { sum += weights(i).weightOf(measures.getOrElse(names(i), null)); i += 1 }
      sum
    }
  }

  /** The calibrated probability that the pair is one film. */
  def probability(scope: String, measures: Map[String, Measure]): Double =
    model(scope).calibration.apply(logOdds(scope, measures))

  /** The signals that moved the score most, for a stored explanation. */
  def explain(scope: String, measures: Map[String, Measure], top: Int = 5): String =
    contributions(scope, measures).filter(_._2 != 0).sortBy(c => (-math.abs(c._2), c._1)).take(top)
      .map { case (s, w) => f"$s=${render(measures.get(s))}%s(${if (w >= 0) "+" else ""}$w%.2f)" }.mkString(" ")

  /** Every measure that moved `measures`' probability, with its weight, largest first: `director=same_person +4.22`.
   *  A number is named by the bin its weight comes from (`venues.corroborating=300..* +5.06`), never its raw
   *  value: a trace keeps these lines and is rewritten when they move, and a count drifting inside one bin
   *  (571 → 570 venues) moved every such trace each tick for a weight that had not changed. */
  def evidence(scope: String, measures: Map[String, Measure]): Seq[String] = {
    val signals = model(scope).signals
    contributions(scope, measures).filter(_._2 != 0).sortBy(c => (-math.abs(c._2), c._1))
      .map { case (s, w) => f"$s=${binned(signals.get(s), measures.get(s))}%s ${if (w >= 0) "+" else ""}$w%.2f" }
  }

  /** A number as the bin of `weights` it falls in (`a..b`, `*` for an open end); anything else as [[render]]. */
  private def binned(weights: Option[SignalWeights], m: Option[Measure]): String = m match {
    case Some(Number(x)) =>
      def end(bound: Option[Double]) = bound.fold("*")(b => if (b == math.rint(b)) b.toLong.toString else f"$b%.2f")
      weights.flatMap(_.bins.find(_.contains(x))).fold(render(m))(bin => s"${end(bin.atLeast)}..${end(bin.atMost)}")
    case _ => render(m)
  }

  /** This calibration with the search priors' spread — [[PriorSignals]] in the listing-film scope — scaled by `scale`
   *  about each one's positives-weighted mean weight: ×1.5 trusts where a source's search ranks the film more, ×0.5
   *  less, while the score's scale stays put. Another film database's search is not TMDB's, and the signal-combination
   *  experiment (2026-10-04, REPORT §10) chose each source's scale by cross-validation ([[agreement.VoterFamily]]). */
  def withPriorSpread(scale: Double): IdentityCalibration = if (scale == 1.0) this else {
    val listingFilm = model(IdentityMeasures.ListingFilm)
    val spread = listingFilm.signals.map { case (name, weights) =>
      name -> (if (!PriorSignals(name) || weights.bins.isEmpty) weights else {
        val positives = weights.bins.map(_.positives).sum.max(1)
        val centre    = weights.bins.map(bin => bin.positives * bin.weight).sum / positives
        def scaled(w: Double) = centre + scale * (w - centre)
        weights.copy(bins = weights.bins.map(bin => bin.copy(weight = scaled(bin.weight))), missing = weights.missing.map { case (k, w) => k -> scaled(w) })
      })
    }
    copy(version = s"$version-spread$scale", scopes = scopes.updated(IdentityMeasures.ListingFilm, listingFilm.copy(signals = spread)))
  }

  /** Is this probability high enough to show the film's ratings? */
  def showsRatings(probability: Double): Boolean = probability >= ratingCut
  /** The probability a listing's film must reach to show its ratings — the cut an own match is accepted at. */
  def ratingCut: Double = model(IdentityMeasures.ListingFilm).thresholds("showRatings").probability

  /** Is this probability low enough that the pair must never be one film? */
  def forbidsLink(scope: String, probability: Double): Boolean =
    model(scope).thresholds.get("cannotLink").exists(t => probability < t.probability)

  /** The first learned cannot-link rule of `scope` whose every condition holds. A condition on a
   *  signal that is missing never holds: missing evidence never vetoes. */
  def cannotLink(scope: String, measures: Map[String, Measure]): Option[CannotLinkRule] =
    cannotLinks.find(r => r.scope == scope && r.all.nonEmpty && r.all.forall(_.holds(measures)))

  /** The measured probability of the best evidence class of `scope` whose every condition holds.
   *  As with a cannot-link, a condition on missing evidence never holds. */
  def classProbability(scope: String, measures: Map[String, Measure]): Option[Double] =
    evidenceClasses.filter(c => c.scope == scope && c.all.nonEmpty && c.all.forall(_.holds(measures))).map(_.probability).maxOption
}

object IdentityCalibration {

  /** The listing-film signals that weigh where and among what a search returned a film, not the film's facts. */
  val PriorSignals: Set[String] = Set("search.rank", "rivals", "popularity.log2")

  /** A number's bin, inclusive at both ends; an open end is `None`. */
  final case class Bin(atLeast: Option[Double], atMost: Option[Double], weight: Double, positives: Int, negatives: Int) {
    // Read without a closure over `x` per bound: every numeric measure of every scored pair asks.
    def contains(x: Double): Boolean = (atLeast.isEmpty || x >= atLeast.get) && (atMost.isEmpty || x <= atMost.get)
  }

  /** One signal's weights. `neutral` names missing sides deliberately weighted 0 (with why). */
  final case class SignalWeights(kind: String,
                                 categories: Map[String, Double] = Map.empty,
                                 bins: Seq[Bin] = Nil,
                                 missing: Map[String, Double] = Map.empty,
                                 counts: Map[String, Seq[Int]] = Map.empty,
                                 neutral: Map[String, String] = Map.empty) {
    def weight(m: Option[Measure]): Double = weightOf(m.orNull)

    /** [[weight]] of a measure or none (null), reading the weights without an option or box per call. */
    def weightOf(m: Measure): Double = m match {
      case Category(v) => SignalWeights.weightIn(byCategory, v)
      case Number(x)   =>
        var i = 0
        while (i < binArray.length && !binArray(i).contains(x)) i += 1
        if (i < binArray.length) binArray(i).weight else 0.0
      case Missing(s)  => SignalWeights.weightIn(byMissing, s)
      case null        => 0.0
    }
    private lazy val byCategory = SignalWeights.javaMap(categories)
    private lazy val byMissing  = SignalWeights.javaMap(missing)
    private lazy val binArray   = bins.toArray
  }

  object SignalWeights {
    private[IdentityCalibration] def javaMap(weights: Map[String, Double]): java.util.HashMap[String, java.lang.Double] = {
      val map = new java.util.HashMap[String, java.lang.Double](weights.size * 2)
      weights.foreach { case (k, v) => map.put(k, v) }
      map
    }
    private[IdentityCalibration] def weightIn(weights: java.util.HashMap[String, java.lang.Double], key: String): Double = {
      val w = weights.get(key)
      if (w == null) 0.0 else w.doubleValue
    }
  }

  /** Piecewise-linear isotonic map from log-odds to probability, clamped at both ends. */
  final case class Calibration(method: String, logOdds: Seq[Double], probabilities: Seq[Double]) {
    private lazy val xs = logOdds.toArray
    private lazy val ps = probabilities.toArray

    def apply(x: Double): Double =
      if (xs.isEmpty) 1.0 / (1.0 + math.exp(-x))
      else if (x <= xs.head) ps.head
      else if (x >= xs.last) ps.last
      else {
        val found = java.util.Arrays.binarySearch(xs, x)
        val i = if (found >= 0) found else -found - 1 // the first knot >= x
        val (x0, x1, p0, p1) = (xs(i - 1), xs(i), ps(i - 1), ps(i))
        if (x1 == x0) p1 else p0 + (p1 - p0) * (x - x0) / (x1 - x0)
      }
  }

  /** A probability cut and what it measured on held-out data. */
  final case class Threshold(probability: Double, measured: Map[String, Double] = Map.empty, basis: String = "")

  final case class ScopeModel(prior: Double, signals: Map[String, SignalWeights], calibration: Calibration,
                              thresholds: Map[String, Threshold] = Map.empty) {
    /** The signals in name order, sorted once: every scoring sums its contributions in this order
     *  (`logOdds`), and sorting them per call was ~2–5% of a re-resolve (JFR, 2026-09-29). */
    lazy val bySignalName: Seq[(String, SignalWeights)] = signals.toSeq.sortBy(_._1)
    /** [[bySignalName]] as two arrays, read by index where a score is summed. */
    lazy val signalNames: Array[String]          = bySignalName.map(_._1).toArray
    lazy val signalWeights: Array[SignalWeights] = bySignalName.map(_._2).toArray
  }

  /** One condition of a learned cannot-link: the signal's category is one of `in`, or its number
   *  lies in [`atLeast`, `atMost`]. Same shape as `ListingConstraints.LearnedCondition`. */
  final case class Condition(signal: String, in: Seq[String] = Nil, atLeast: Option[Double] = None,
                             atMost: Option[Double] = None) {
    def holds(measures: Map[String, Measure]): Boolean = measures.get(signal) match {
      case Some(Category(v)) => in.nonEmpty && in.contains(v)
      case Some(Number(x))   => in.isEmpty && (atLeast.nonEmpty || atMost.nonEmpty) && atLeast.forall(x >= _) && atMost.forall(x <= _)
      case _                 => false
    }
  }

  /** A learned cannot-link. `falseVetoBound` is the Bonferroni-corrected upper bound of the share
   *  of corroborated same-film units it fired on in the FITTING splits (the bound it was selected
   *  under); `falseVetoRate`, `trueVetoRate` and `support` (different-film units vetoed) are what
   *  it measured on the HELD-OUT split, over `positives` same-film and `negatives` different-film
   *  units. `origin` is "derived" for a rule the
   *  calibration searched for, or the name of today's hand-written veto it re-expresses. */
  final case class CannotLinkRule(name: String, scope: String, all: Seq[Condition], falseVetoRate: Double,
                                  support: Int, falseVetoBound: Double = 0.0, trueVetoRate: Double = 0.0,
                                  positives: Int = 0, negatives: Int = 0, origin: String = "derived")

  /** A class of evidence measured as a whole on the labels: every listing-film pair whose measures
   *  meet `all` is the listing's film with `probability` (a conservative bound on its measured
   *  precision, `measured` saying on which units), and the class qualified because its wrong share
   *  is within the wrong rate ratings are shown at (`basis`). */
  final case class EvidenceClass(name: String, scope: String, all: Seq[Condition], probability: Double,
                                 measured: Map[String, Double] = Map.empty, basis: String = "")

  private def render(m: Option[Measure]): String = m match {
    case Some(Category(v)) => v
    case Some(Number(x))   => if (x == math.rint(x)) x.toLong.toString else f"$x%.2f"
    case Some(Missing(s))  => s"missing:$s"
    case None              => "absent"
  }

  implicit val binFormat: OFormat[Bin]                   = Json.format[Bin]
  implicit val signalFormat: OFormat[SignalWeights]      = Json.using[Json.WithDefaultValues].format[SignalWeights]
  implicit val calibrationFormat: OFormat[Calibration]   = Json.format[Calibration]
  implicit val thresholdFormat: OFormat[Threshold]       = Json.using[Json.WithDefaultValues].format[Threshold]
  implicit val scopeFormat: OFormat[ScopeModel]          = Json.using[Json.WithDefaultValues].format[ScopeModel]
  implicit val conditionFormat: OFormat[Condition]       = Json.using[Json.WithDefaultValues].format[Condition]
  implicit val ruleFormat: OFormat[CannotLinkRule]       = Json.using[Json.WithDefaultValues].format[CannotLinkRule]
  implicit val classFormat: OFormat[EvidenceClass]       = Json.using[Json.WithDefaultValues].format[EvidenceClass]
  implicit val format: OFormat[IdentityCalibration]      = Json.using[Json.WithDefaultValues].format[IdentityCalibration]

  /** The RESOLVER's artefact on the classpath (common/src/main/resources): what
   *  `scripts/identity-calibrate.sh` writes, refitted as the resolver improves. */
  val ResolverResourcePath = "identity-weights.json"
  /** The LIVE RATING GATE's artefact (`RatingGate.fromEvidence`): a PINNED copy of the artefact
   *  the gate was measured and switched on with. No refit writes it — a resolver refit moved the
   *  gate's false hides in DE from 0 to 9 — so it changes only by a deliberate copy, with the
   *  gate's impact measured first (`scripts.IdentityGateImpact`). */
  val RatingGateResourcePath = "identity-weights-gate.json"

  def fromResource(path: String): Option[IdentityCalibration] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[IdentityCalibration] finally in.close()
    }

  private def required(path: String): IdentityCalibration =
    fromResource(path).getOrElse(throw new IllegalStateException(s"$path is not on the classpath"))

  lazy val resolver: IdentityCalibration   = required(ResolverResourcePath)
  lazy val ratingGate: IdentityCalibration = required(RatingGateResourcePath)
}
