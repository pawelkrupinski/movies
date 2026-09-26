package scripts

import services.identity.IdentityCalibration.{Calibration, SignalWeights}
import services.identity.IdentityMeasures.{Category, Measure, Missing, Number}

/**
 * The JOINT candidate beside naive Bayes: a logistic regression over the same cells (each
 * category, numeric bin and non-neutral missing side of a signal's table), so a weight measures
 * what its cell adds GIVEN the others — search rank and same-titled rivals no longer re-count what
 * an agreeing director already said.
 *
 * Only the signals no label reads are refitted (`free`): a corroborator's table (year, director,
 * original title, venues, runtime) stays its leave-one-out naive-Bayes weight and enters as a fixed
 * offset, because a joint fit on labels built from that very signal would learn the label rule, not
 * the evidence. The tables keep naive Bayes' cells and counts; only free cells' weights and the
 * prior change. An evidence order (`IdentityMeasures.EvidenceOrder`) is kept by tying adjacent
 * cells that break it into one weight and refitting, the joint twin of `IdentityCalibrate.inOrder`.
 * Test code, run by [[IdentityCalibrate]], which keeps whichever model the held-out split favours.
 */
object IdentityJointFit {

  /** A Gaussian prior on every free weight (not the intercept): a cell seen a handful of times
   *  cannot run to ±∞, as `IdentityCalibrate`'s additive smoothing keeps its ratios finite. */
  val Ridge = 1.0

  /** One free cell: a signal and which part of its table. */
  final case class Cell(signal: String, part: String)

  def cellOf(signal: String, w: SignalWeights, m: Option[Measure]): Option[Cell] = m match {
    case Some(Category(v)) if w.categories.contains(v) => Some(Cell(signal, v))
    case Some(Number(x))   => w.bins.indexWhere(_.contains(x)) match { case -1 => None; case i => Some(Cell(signal, s"bin:$i")) }
    case Some(Missing(s)) if w.missing.contains(s) && !w.neutral.contains(s) => Some(Cell(signal, s"missing:$s"))
    case _ => None
  }

  /** One labelled unit: its fixed offset, its free cells, its label. */
  final case class Unit(offset: Double, cells: Seq[Cell], same: Boolean)

  /** The maximum-penalised-likelihood intercept and free weights, with the cells of each group in
   *  `ties` sharing one weight. Newton's method: the problem is convex and small (a few dozen cells). */
  def solve(units: Seq[Unit], ties: Seq[Seq[Cell]]): (Double, Map[Cell, Double]) = {
    val groupOf: Map[Cell, Int] = ties.zipWithIndex.flatMap { case (cs, g) => cs.map(_ -> (g + 1)) }.toMap
    val k = ties.size + 1 // index 0: the intercept
    val xs = units.map(u => (u.offset, u.cells.flatMap(groupOf.get).distinct.prepended(0).toArray, if (u.same) 1.0 else 0.0)).toArray
    val theta = new Array[Double](k)
    var step = Double.MaxValue; var iteration = 0
    while (step > 1e-9 && iteration < 100) {
      val g = new Array[Double](k); val h = Array.ofDim[Double](k, k)
      xs.foreach { case (o, active, y) =>
        var z = o; active.foreach(j => z += theta(j))
        val p = 1.0 / (1.0 + math.exp(-z)); val r = p * (1 - p)
        active.foreach { a => g(a) += p - y; active.foreach(b => h(a)(b) += r) }
      }
      (1 until k).foreach { j => g(j) += Ridge * theta(j); h(j)(j) += Ridge }
      (0 until k).foreach(j => h(j)(j) += 1e-9)
      val delta = gauss(h, g)
      (0 until k).foreach(j => theta(j) -= delta(j))
      step = delta.map(math.abs).max; iteration += 1
    }
    (theta(0), ties.zipWithIndex.flatMap { case (cs, g) => cs.map(_ -> theta(g + 1)) }.toMap)
  }

  /** `a x = b` by Gaussian elimination with partial pivoting (a is small and positive definite). */
  private def gauss(a0: Array[Array[Double]], b0: Array[Double]): Array[Double] = {
    val n = b0.length; val a = a0.map(_.clone); val b = b0.clone
    (0 until n).foreach { c =>
      val p = (c until n).maxBy(r => math.abs(a(r)(c)))
      val tr = a(c); a(c) = a(p); a(p) = tr; val tb = b(c); b(c) = b(p); b(p) = tb
      (c + 1 until n).foreach { r =>
        val f = a(r)(c) / a(c)(c)
        if (f != 0) { (c until n).foreach(j => a(r)(j) -= f * a(c)(j)); b(r) -= f * b(c) }
      }
    }
    val x = new Array[Double](n)
    (n - 1 to 0 by -1).foreach { r => x(r) = (b(r) - (r + 1 until n).map(j => a(r)(j) * x(j)).sum) / a(r)(r) }
    x
  }

  /** The joint fit under the evidence orders: fit, tie the first adjacent pair of groups an order
   *  finds reversed, refit, until none is. */
  def fitOrdered(units: Seq[Unit], cells: Seq[Cell], order: Map[String, Seq[String]]): (Double, Map[Cell, Double]) = {
    def ordered(ties: Seq[Seq[Cell]]): Seq[Seq[Seq[Cell]]] = order.toSeq.sortBy(_._1).map { case (signal, parts) =>
      parts.flatMap(p => ties.find(_.contains(Cell(signal, p)))).distinct }
    @annotation.tailrec
    def loop(ties: Seq[Seq[Cell]]): (Double, Map[Cell, Double]) = {
      val (b, w) = solve(units, ties)
      val reversed = ordered(ties).iterator.flatMap(_.sliding(2).collect { case Seq(strong, weak) if w(weak.head) > w(strong.head) => (strong, weak) })
      if (!reversed.hasNext) (b, w)
      else { val (s, x) = reversed.next(); loop(ties.filterNot(t => (t eq s) || (t eq x)) :+ (s ++ x)) }
    }
    loop(cells.map(Seq(_)))
  }

  /** `nb` with its free signals' weights and its prior refitted jointly on `rows`' training split,
   *  and a fresh isotonic map from the calibration split. */
  def fit(nb: IdentityCalibrate.Fitted, rows: Seq[IdentityCalibrate.Row], free: Set[String],
          order: Map[String, Seq[String]]): IdentityCalibrate.Fitted = {
    val tables = nb.tables.filter(t => free(t.signal))
    val fixed  = nb.tables.filterNot(t => free(t.signal))
    val units = rows.iterator.filter(_.split == "train").flatMap(r => r.label(Set.empty).map(y => (r.unit, r.measures, y)))
      .toSeq.distinct.map { case (_, m, y) =>
        Unit(fixed.map(t => t.weights.weight(m.get(t.signal))).sum, tables.flatMap(t => cellOf(t.signal, t.weights, m.get(t.signal))), y)
      }
    val cells = units.flatMap(_.cells).distinct.sortBy(c => (c.signal, c.part))
    val (prior, w) = fitOrdered(units, cells, order.filter { case (s, _) => free(s) })
    val refitted = nb.tables.map { t =>
      if (!free(t.signal)) t
      else {
        def weightOf(part: String, old: Double) = w.getOrElse(Cell(t.signal, part), old)
        t.copy(weights = t.weights.copy(
          categories = t.weights.categories.map { case (v, old) => v -> weightOf(v, old) },
          bins = t.weights.bins.zipWithIndex.map { case (b, i) => b.copy(weight = weightOf(s"bin:$i", b.weight)) },
          missing = t.weights.missing.map { case (s, old) => s -> (if (t.weights.neutral.contains(s)) old else weightOf(s"missing:$s", old)) }))
      }
    }
    val raw = IdentityCalibrate.Fitted(nb.scope, refitted, prior, Calibration("isotonic", Nil, Nil))
    val (xs, ps) = IdentityCalibrate.isotonic(IdentityCalibrate.units(rows, "calibration", raw.logOdds))
    raw.copy(calibration = Calibration("isotonic", xs, ps))
  }
}
