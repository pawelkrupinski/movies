package services.identity

/**
 * Where the rating gate draws its line — derived from data, never a hand-picked constant
 * (docs/design/identity-resolver.md, "Confidence-gated ratings").
 *
 * The input is the labelled shadow diff: every decision whose correctness is known (the
 * pipeline agrees, or a reviewed known-issues verdict says right or wrong), with its confidence.
 * The threshold is the cut that misclassifies the fewest of them — a decision below it has its
 * ratings withheld, so the ideal cut withholds exactly the wrong ones and shows exactly the right
 * ones. Where the classes overlap, the cut minimises (wrong shown + right withheld); on a tie it
 * takes the LOWER cut, withholding less: with no evidence either way, the film keeps its ratings.
 *
 * Only the ORDER of confidences matters, so the resolver's score need not be a probability, and
 * a change to how the resolver scores re-calibrates on the next labelled run by itself.
 */
object ConfidenceCalibration {

  /** One labelled decision: its confidence, and whether the resolver got it right. */
  final case class Sample(confidence: Double, correct: Boolean)

  /** The chosen cut and how it splits the labelled set — the numbers the admin view shows. */
  final case class Calibration(threshold: Double, gatedWrong: Int, gatedCorrect: Int, shownWrong: Int, shownCorrect: Int) {
    def gates(confidence: Double): Boolean = confidence < threshold
  }

  /** None without labelled data: no threshold can be derived, and the gate withholds nothing. */
  def calibrate(samples: Seq[Sample]): Option[Calibration] =
    Option.when(samples.nonEmpty) {
      // Candidate cuts: every observed confidence (gate what is strictly below it), plus one
      // above them all (gate everything). Lowest first, so `minBy` keeps the lowest on a tie.
      val cuts = samples.map(_.confidence).distinct.sorted :+ Double.PositiveInfinity
      cuts.map { t =>
        val (gated, shown) = samples.partition(_.confidence < t)
        Calibration(t, gated.count(!_.correct), gated.count(_.correct), shown.count(!_.correct), shown.count(_.correct))
      }.minBy(c => c.shownWrong + c.gatedCorrect)
    }
}
