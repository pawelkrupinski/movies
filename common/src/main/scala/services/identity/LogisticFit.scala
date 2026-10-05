package services.identity

/** L2-penalised logistic regression by Newton's method, a fixed number of steps: a function of its rows alone (no
 *  random start, no clock), so a refit from the same rows gives the same weights. Column 0 is the intercept, left
 *  unpenalised. Weights are rounded to 1e-6 so a stored artefact compares equal to a refit. */
object LogisticFit {

  def fit(xs: Array[Array[Double]], ys: Array[Double], l2: Double, iterations: Int): Seq[Double] = {
    val n = xs.headOption.fold(0)(_.length)
    var w = Array.fill(n)(0.0)
    (1 to iterations).foreach { _ =>
      val grad = Array.tabulate(n)(j => if (j == 0) 0.0 else l2 * w(j))
      val hess = Array.tabulate(n, n)((i, j) => if (i == j && i > 0) l2 else 0.0)
      xs.indices.foreach { r =>
        val x = xs(r)
        val p = sigmoid(dot(w, x))
        val s = p * (1 - p)
        val e = p - ys(r)
        var i = 0
        while (i < n) {
          grad(i) += e * x(i)
          val sx = s * x(i)
          val row = hess(i)
          var j = 0
          while (j < n) { row(j) += sx * x(j); j += 1 }
          i += 1
        }
      }
      (0 until n).foreach(i => hess(i)(i) += 1e-9)
      val step = solve(hess, grad)
      w = Array.tabulate(n)(i => w(i) - step(i))
    }
    w.toSeq.map(x => math.rint(x * 1e6) / 1e6)
  }

  def sigmoid(z: Double): Double = 1.0 / (1.0 + math.exp(-z))
  def dot(w: collection.IndexedSeq[Double], x: collection.IndexedSeq[Double]): Double = {
    var s = 0.0; var i = 0
    while (i < w.length) { s += w(i) * x(i); i += 1 }
    s
  }

  /** `a x = b` by Gaussian elimination with partial pivoting. */
  private def solve(a0: Array[Array[Double]], b0: Array[Double]): Array[Double] = {
    val n = b0.length
    val a = a0.map(_.clone); val b = b0.clone
    (0 until n).foreach { c =>
      val p = (c until n).maxBy(r => math.abs(a(r)(c)))
      val (ra, rb) = (a(c), b(c)); a(c) = a(p); b(c) = b(p); a(p) = ra; b(p) = rb
      (c + 1 until n).foreach { r =>
        val f = a(r)(c) / a(c)(c)
        (c until n).foreach(k => a(r)(k) -= f * a(c)(k))
        b(r) -= f * b(c)
      }
    }
    val x = Array.fill(n)(0.0)
    (n - 1 to 0 by -1).foreach(r => x(r) = (b(r) - (r + 1 until n).map(k => a(r)(k) * x(k)).sum) / a(r)(r))
    x
  }
}
