package services.identity

import play.api.libs.json.{Json, OFormat}

import services.identity.IdentityMeasures.{Category, Measure, Missing, Number}

/**
 * The identity resolver's calibrated evidence model, as DATA: `identity-weights.json`, produced
 * by `scripts.IdentityCalibrate` from corroborated films (docs/design/identity-resolver.md
 * §calibration). Nothing in the artefact is written by hand; this class only evaluates it.
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
 * the stated bound — the learned vetoes, evaluated generically by [[cannotLink]].
 */
final case class IdentityCalibration(version: String,
                                     scopes: Map[String, IdentityCalibration.ScopeModel],
                                     cannotLinks: Seq[IdentityCalibration.CannotLinkRule] = Nil,
                                     provenance: Map[String, String] = Map.empty) {
  import IdentityCalibration.*

  private def model(scope: String): ScopeModel =
    scopes.getOrElse(scope, throw new NoSuchElementException(s"identity-weights.json has no scope '$scope'"))

  /** Each signal's contribution (its log-likelihood ratio) to the log-odds. */
  def contributions(scope: String, measures: Map[String, Measure]): Seq[(String, Double)] = {
    val m = model(scope)
    m.signals.toSeq.sortBy(_._1).map { case (name, w) => name -> w.weight(measures.get(name)) }
  }

  /** The naive-Bayes log-odds that the pair is one film: the prior plus every signal's weight. */
  def logOdds(scope: String, measures: Map[String, Measure]): Double =
    model(scope).prior + contributions(scope, measures).map(_._2).sum

  /** The calibrated probability that the pair is one film. */
  def probability(scope: String, measures: Map[String, Measure]): Double =
    model(scope).calibration.apply(logOdds(scope, measures))

  /** The signals that moved the score most, for a stored explanation. */
  def explain(scope: String, measures: Map[String, Measure], top: Int = 5): String =
    contributions(scope, measures).filter(_._2 != 0).sortBy(c => (-math.abs(c._2), c._1)).take(top)
      .map { case (s, w) => f"$s=${render(measures.get(s))}%s(${if (w >= 0) "+" else ""}$w%.2f)" }.mkString(" ")

  /** Is this probability high enough to show the film's ratings? */
  def showsRatings(probability: Double): Boolean =
    probability >= model(IdentityMeasures.ListingFilm).thresholds("showRatings").probability

  /** Is this probability low enough that the pair must never be one film? */
  def forbidsLink(scope: String, probability: Double): Boolean =
    model(scope).thresholds.get("cannotLink").exists(t => probability < t.probability)

  /** The first learned cannot-link rule of `scope` whose every condition holds. A condition on a
   *  signal that is missing never holds: missing evidence never vetoes. */
  def cannotLink(scope: String, measures: Map[String, Measure]): Option[CannotLinkRule] =
    cannotLinks.find(r => r.scope == scope && r.all.nonEmpty && r.all.forall(_.holds(measures)))
}

object IdentityCalibration {

  /** A number's bin, inclusive at both ends; an open end is `None`. */
  final case class Bin(atLeast: Option[Double], atMost: Option[Double], weight: Double, positives: Int, negatives: Int) {
    def contains(x: Double): Boolean = atLeast.forall(x >= _) && atMost.forall(x <= _)
  }

  /** One signal's weights. `neutral` names missing sides deliberately weighted 0 (with why). */
  final case class SignalWeights(kind: String,
                                 categories: Map[String, Double] = Map.empty,
                                 bins: Seq[Bin] = Nil,
                                 missing: Map[String, Double] = Map.empty,
                                 counts: Map[String, Seq[Int]] = Map.empty,
                                 neutral: Map[String, String] = Map.empty) {
    def weight(m: Option[Measure]): Double = m match {
      case Some(Category(v)) => categories.getOrElse(v, 0.0)
      case Some(Number(x))   => bins.find(_.contains(x)).map(_.weight).getOrElse(0.0)
      case Some(Missing(s))  => missing.getOrElse(s, 0.0)
      case None              => 0.0
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
                              thresholds: Map[String, Threshold] = Map.empty)

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
  implicit val format: OFormat[IdentityCalibration]      = Json.using[Json.WithDefaultValues].format[IdentityCalibration]

  /** Where the artefact sits on the classpath (common/src/main/resources). */
  val ResourcePath = "identity-weights.json"

  def fromResource(path: String = ResourcePath): Option[IdentityCalibration] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[IdentityCalibration] finally in.close()
    }

  lazy val default: IdentityCalibration =
    fromResource().getOrElse(throw new IllegalStateException(s"$ResourcePath is not on the classpath"))
}
