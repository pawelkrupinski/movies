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

  /** [[fit]] with each weight held to the sign `signs` gives it — +1 never below 0, −1 never above, 0 free; the
   *  intercept always free — the MONOTONE fit a signal with a known direction gets: a signal that can only speak for a
   *  film never weighs against it. An active set: weights breaking their sign are held at 0 and the rest refitted, and
   *  a held weight is freed again when the loss's slope at 0 pulls it the way its sign allows — until neither happens.
   *  Each row counts `counts(row)` times (identical rows folded into one). Newton steps until no weight moves by 1e-9, at
   *  most `iterations`; a function of its rows alone, rounded as [[fit]]. */
  def fitSigned(xs: Array[Array[Double]], ys: Array[Double], counts: Array[Double], signs: Seq[Int], l2: Double,
                iterations: Int = 50): Seq[Double] = {
    val n = xs.headOption.fold(0)(_.length)
    require(signs.size == n, s"${signs.size} signs for $n columns")
    var free  = (0 until n).toSet
    var w     = Array.fill(n)(0.0)
    var done  = false
    var round = 0
    while (!done && round < 4 * n + 4) {
      round += 1
      w = newton(xs, ys, counts, l2, iterations, free.toSeq.sorted, w)
      val broken = (1 until n).filter(j => free(j) && signs(j) * w(j) < 0)
      if (broken.nonEmpty) { free --= broken; broken.foreach(w(_) = 0.0) }
      else {
        val slope = gradient(xs, ys, counts, w)
        (1 until n).filter(j => !free(j) && signs(j) != 0 && -slope(j) * signs(j) > 1e-9).sortBy(j => (-math.abs(slope(j)), j)).headOption match {
          case Some(j) => free += j
          case None    => done = true
        }
      }
    }
    w.toSeq.map(x => math.rint(x * 1e6) / 1e6)
  }

  /** The log-loss's gradient at `w` (unpenalised: a held weight sits at 0, where the penalty has none). */
  private def gradient(xs: Array[Array[Double]], ys: Array[Double], counts: Array[Double], w: Array[Double]): Array[Double] = {
    val g = Array.fill(w.length)(0.0)
    xs.indices.foreach { r =>
      val e = counts(r) * (sigmoid(dot(w, xs(r))) - ys(r))
      var i = 0
      while (i < w.length) { g(i) += e * xs(r)(i); i += 1 }
    }
    g
  }

  /** Newton's method over the `cols` columns alone (the rest held where `start` has them), from `start`. */
  private def newton(xs: Array[Array[Double]], ys: Array[Double], counts: Array[Double], l2: Double, iterations: Int, cols: Seq[Int],
                     start: Array[Double]): Array[Double] = {
    val m = cols.size
    val w = start.clone
    var it = 0
    var moving = true
    while (moving && it < iterations) {
      it += 1
      val grad = Array.tabulate(m)(a => if (cols(a) == 0) 0.0 else l2 * w(cols(a)))
      val hess = Array.tabulate(m, m)((a, b) => if (a == b && cols(a) > 0) l2 else 0.0)
      xs.indices.foreach { r =>
        val x = xs(r)
        val p = sigmoid(dot(w, x))
        val s = counts(r) * p * (1 - p)
        val e = counts(r) * (p - ys(r))
        var a = 0
        while (a < m) {
          val xa = x(cols(a))
          if (xa != 0.0) {
            grad(a) += e * xa
            val row = hess(a)
            var b = 0
            while (b < m) { row(b) += s * xa * x(cols(b)); b += 1 }
          }
          a += 1
        }
      }
      (0 until m).foreach(a => hess(a)(a) += 1e-9)
      val step = solve(hess, grad)
      cols.indices.foreach(a => w(cols(a)) -= step(a))
      moving = step.exists(d => math.abs(d) > 1e-9)
    }
    w
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
