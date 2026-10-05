package services.identity

import play.api.libs.json.{Json, OFormat}

/**
 * How likely a CANDIDATE decoration ([[TitleDecorations.candidates]], [[TitleDecorations.aligned]]) is one whose
 * stripping takes listings right and none wrong — a logistic regression over what the corpus says of it
 * ([[DecorationScore.Features]]), FITTED (`scripts.IdentityDecorationScoreFit`) to the outcomes the detector measured
 * (`integration.IdentityDecorationCandidates`), never hand-weighted. The detector measures the candidates in the order
 * this scores them and keeps one only when its measure is clean AND it scores at least [[cut]] — the cut the fit chose
 * where no held-out candidate with a wrong take or a moved match scored above it.
 */
final case class DecorationScore(version: String, features: Seq[String], weights: Seq[Double], cut: Double, folds: Int,
                                 trainingRows: Int, heldOut: DecorationScore.HeldOut) {
  def probability(f: DecorationScore.Features): Double = DecorationScore.sigmoid(DecorationScore.dot(weights, f.vector))
  def accepts(f: DecorationScore.Features): Boolean = probability(f) >= cut
}

object DecorationScore {

  /** What the corpus says of one candidate run:
   *  @param aligned       inner titles it decorates beside a plain sibling of the same film ([[TitleDecorations.aligned]])
   *  @param recurring     different inner titles it recurs around ([[TitleDecorations.candidates]])
   *  @param venues        venues billing it
   *  @param prefix        whether it leads the title (else it trails)
   *  @param tokens        its words
   *  @param formatShare   the share of its words that are format or version words ([[services.movies.FormatTags.FormatToken]])
   *  @param innerResolved the share of its inner titles some listing billing that title alone is matched as
   *  @param matchedShare  the share of the listings carrying it the model already matches */
  final case class Features(aligned: Int, recurring: Int, venues: Int, prefix: Boolean, tokens: Int, formatShare: Double,
                            innerResolved: Double, matchedShare: Double) {
    def vector: Seq[Double] = Seq(1.0, math.log1p(aligned.toDouble), math.log1p(recurring.toDouble), math.log1p(venues.toDouble),
      if (prefix) 1.0 else 0.0, tokens.toDouble, formatShare, innerResolved, matchedShare)
  }
  val Names: Seq[String] = Seq("intercept", "log1p(aligned)", "log1p(recurring)", "log1p(venues)", "prefix", "tokens", "formatShare",
    "innerResolved", "matchedShare")

  /** Held-out performance at [[DecorationScore.cut]]: candidates scored by a model fitted without their fold. */
  final case class HeldOut(accepted: Int, acceptedGood: Int, good: Int, acceptedBad: Int)

  /** One measured candidate: its features and outcome — `good` (a right take, nothing wrong or moved), `bad` (a wrong
   *  take or a moved match), or neither (nothing changed). `key` names it, and decides its cross-validation fold. */
  final case class Row(key: String, features: Features, good: Boolean, bad: Boolean)

  val L2         = 1.0
  val Iterations = 50
  val Folds      = 5

  /** The model `rows` fit: weights by Newton's method (L2-penalised logistic regression, a fixed number of steps —
   *  a function of the rows alone), and the cut: the lowest probability above every held-out BAD candidate's, so no
   *  held-out candidate with a wrong take or a moved match is accepted. */
  def fit(rows: Seq[Row], version: String): DecorationScore = {
    val sorted  = rows.sortBy(_.key)
    val weights = newton(sorted)
    val heldOut = sorted.groupBy(row => foldOf(row.key)).toSeq.sortBy(_._1).flatMap { case (fold, test) =>
      val model = newton(sorted.filterNot(row => foldOf(row.key) == fold))
      test.map(row => row -> sigmoid(dot(model, row.features.vector)))
    }
    val worstBad = heldOut.filter(_._1.bad).map(_._2).maxOption.getOrElse(0.0)
    val cut      = heldOut.map(_._2).filter(_ > worstBad).minOption.getOrElse(1.0)
    val accepted = heldOut.filter(_._2 >= cut).map(_._1)
    DecorationScore(version, Names, weights, cut, Folds, sorted.size,
      HeldOut(accepted.size, accepted.count(_.good), sorted.count(_.good), accepted.count(_.bad)))
  }

  private def foldOf(key: String): Int = Math.floorMod(scala.util.hashing.MurmurHash3.stringHash(key), Folds)

  private def newton(rows: Seq[Row]): Seq[Double] = {
    val n = Names.size
    val xs = rows.map(_.features.vector.toArray).toArray
    val ys = rows.map(row => if (row.good) 1.0 else 0.0).toArray
    var w  = Array.fill(n)(0.0)
    (1 to Iterations).foreach { _ =>
      val grad = Array.tabulate(n)(j => if (j == 0) 0.0 else L2 * w(j))
      val hess = Array.tabulate(n, n)((i, j) => if (i == j && i > 0) L2 else 0.0)
      xs.indices.foreach { r =>
        val p = sigmoid(dot(w.toSeq, xs(r).toSeq))
        val s = p * (1 - p)
        (0 until n).foreach { i =>
          grad(i) += (p - ys(r)) * xs(r)(i)
          (0 until n).foreach(j => hess(i)(j) += s * xs(r)(i) * xs(r)(j))
        }
      }
      (0 until n).foreach(i => hess(i)(i) += 1e-9)
      val step = solve(hess, grad)
      w = Array.tabulate(n)(i => w(i) - step(i))
    }
    w.toSeq.map(x => math.rint(x * 1e6) / 1e6)
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

  private[identity] def sigmoid(z: Double): Double = 1.0 / (1.0 + math.exp(-z))
  private[identity] def dot(w: Seq[Double], x: Seq[Double]): Double = w.lazyZip(x).map(_ * _).sum

  implicit val heldOutFormat: OFormat[HeldOut]       = Json.format[HeldOut]
  implicit val scoreFormat: OFormat[DecorationScore] = Json.format[DecorationScore]

  val ResourcePath = "identity-decoration-score.json"

  def fromResource(path: String = ResourcePath): Option[DecorationScore] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[DecorationScore] finally in.close()
    }
}
