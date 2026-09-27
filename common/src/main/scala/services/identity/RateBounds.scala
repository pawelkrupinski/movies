package services.identity

/**
 * Confidence bounds of a rate measured on a count — the ONE definition the calibration fitter
 * (`scripts.IdentityCalibrate`: its thresholds and learned rules) and the resolver (a title
 * family's majority film) read.
 */
object RateBounds {

  /** The standard normal quantile the calibration certifies its thresholds at: one-sided 95%. */
  val OneSided95: Double = 1.6448536269514722

  /** One-sided Wilson upper bound of the rate `x / n` at the normal quantile `z`. */
  def upperBound(x: Int, n: Int, z: Double): Double =
    if (n == 0) 1.0 else {
      val p = x.toDouble / n; val z2 = z * z
      math.min(1.0, (p + z2 / (2 * n) + z * math.sqrt(p * (1 - p) / n + z2 / (4.0 * n * n))) / (1 + z2 / n))
    }

  /** One-sided Wilson lower bound of the rate `x / n` at `z`: one minus the upper bound of the rest. */
  def lowerBound(x: Int, n: Int, z: Double): Double = if (n == 0) 0.0 else 1.0 - upperBound(n - x, n, z)

  /** One-sided 95% Wilson upper bound of a rate. */
  def upper95(x: Int, n: Int): Double = upperBound(x, n, OneSided95)

  /** One-sided 95% Wilson lower bound of a rate. */
  def lower95(x: Int, n: Int): Double = lowerBound(x, n, OneSided95)
}
