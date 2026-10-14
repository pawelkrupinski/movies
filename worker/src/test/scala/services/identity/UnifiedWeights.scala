package services.identity

import play.api.libs.json.{Json, OFormat}

/**
 * The unified evidence model's weights ([[UnifiedEvidence]]), as DATA: `identity-unified-weights.json`, written by
 * `scripts.IdentityUnifiedFit` from the rows `integration.IdentityUnifiedDataset` emits — a sign-constrained (monotone),
 * L2-regularised logistic regression ([[LogisticFit.fitSigned]]) — and `cut`, the probability a contender is taken at:
 * the lowest above every held-out take a hand label calls wrong. `heldOut` and `ablation` are what the fit measured
 * with whole venues held out.
 */
final case class UnifiedWeights(version: String, signals: Seq[String], weights: Seq[Double], cut: Double, l2: Double, folds: Int,
                                rows: Int, heldOut: Map[String, Double] = Map.empty, ablation: Seq[UnifiedWeights.Ablation] = Nil,
                                provenance: Map[String, String] = Map.empty, guards: Seq[String] = Nil) {
  private lazy val bySignal: Map[String, Double] = signals.zip(weights.drop(1)).toMap
  /** Each signal's contribution to the log-odds, largest first: what a decision lists as its evidence. */
  def contributions(features: Map[String, Double]): Seq[(String, Double)] =
    features.toSeq.flatMap { case (name, x) => bySignal.get(name).map(w => name -> w * x) }.filter(_._2 != 0.0).sortBy(c => (-math.abs(c._2), c._1))
  def logOdds(features: Map[String, Double]): Double = weights.head + contributions(features).map(_._2).sum
  def probability(features: Map[String, Double]): Double = LogisticFit.sigmoid(logOdds(features))
  /** `model.logit=2.25 +3.10 family.imdb.took=1 +1.42 …` */
  def explain(features: Map[String, Double]): String =
    contributions(features).map { case (name, c) => f"$name=${features(name)}%.2f ${if (c >= 0) "+" else ""}$c%.2f" }.mkString(" ")

  /** The guards `features` trips ([[UnifiedEvidence.vetoes]], of this model's `guards`): a contender tripping one is
   *  never taken, whatever it scores. */
  def vetoes(features: Map[String, Double]): Seq[String] =
    UnifiedEvidence.vetoes(name => features.getOrElse(name, 0.0)).filter(guards.contains)

  /** A decision as it explains itself: the guards it passed (or tripped), its probability against the cut, and every
   *  signal's weighted contribution — `taken 97.3% ≥ 95.0%; guards passed: bill.several, …; family.imdb.took=1.00 +2.74 …`. */
  def decision(features: Map[String, Double]): String = {
    val tripped = vetoes(features)
    val p       = probability(features)
    val verdict = if (tripped.nonEmpty) s"vetoed by ${tripped.mkString(", ")}" else if (p >= cut) f"taken ${p * 100}%.1f%% ≥ ${cut * 100}%.1f%%"
                  else f"not taken ${p * 100}%.1f%% < ${cut * 100}%.1f%%"
    s"$verdict; guards passed: ${guards.filterNot(tripped.contains).mkString(", ")}; ${explain(features)}"
  }
}

object UnifiedWeights {
  /** A signal group dropped: what the held-out log-loss and the right and wrong takes came to without it. */
  final case class Ablation(group: String, logLoss: Double, handLogLoss: Double, right: Int, wrong: Int)

  implicit val ablationFormat: OFormat[Ablation]      = Json.format[Ablation]
  implicit val format: OFormat[UnifiedWeights]        = Json.using[Json.WithDefaultValues].format[UnifiedWeights]

  val ResourcePath = "identity-unified-weights.json"
  /** The HYBRID: the guards hard, the fitted score in place of the thresholds among the contenders passing them. */
  val HybridResourcePath = "identity-unified-hybrid-weights.json"

  def fromResource(path: String = ResourcePath): Option[UnifiedWeights] =
    ClasspathResource.json[UnifiedWeights](path)
}
