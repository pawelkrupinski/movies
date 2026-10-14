package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogisticFitSpec extends AnyFlatSpec with Matchers {

  // y follows feature 1; feature 2 is noise that, by construction, leans NEGATIVE on these rows (it fires on a positive
  // row less often than on a negative one), so an unconstrained fit gives it a negative weight
  private val rows: Seq[(Array[Double], Double)] = {
    val base = for {
      i <- 0 until 200
      f1 = if (i % 2 == 0) 1.0 else 0.0
      y  = if ((i % 2 == 0 && i % 10 != 0) || (i % 2 == 1 && i % 9 == 0)) 1.0 else 0.0
      f2 = if ((y == 1.0 && i % 7 == 0) || (y == 0.0 && i % 3 == 0)) 1.0 else 0.0
    } yield (Array(1.0, f1, f2), y)
    base
  }
  private val xs = rows.map(_._1).toArray
  private val ys = rows.map(_._2).toArray

  "a sign-constrained fit" should "hold a weight the unconstrained fit gives the wrong sign at zero" in {
    val free = LogisticFit.fit(xs, ys, 1.0, 50)
    free(2) should be < 0.0
    val signed = LogisticFit.fitSigned(xs, ys, Array.fill(xs.length)(1.0), signs = Seq(0, 1, 1), l2 = 1.0)
    signed(2) shouldBe 0.0
    signed(1) should be > 0.0
  }

  it should "equal the unconstrained fit where no sign is broken" in {
    val signed = LogisticFit.fitSigned(xs, ys, Array.fill(xs.length)(1.0), signs = Seq(0, 1, 0), l2 = 1.0)
    signed.zip(LogisticFit.fit(xs, ys, 1.0, 50)).foreach { case (a, b) => a shouldBe b +- 1e-5 }
  }

  it should "weigh a row by its count as that many copies of it" in {
    val copies  = LogisticFit.fitSigned(xs ++ xs, ys ++ ys, Array.fill(2 * xs.length)(1.0), signs = Seq(0, 1, 1), l2 = 1.0)
    val counted = LogisticFit.fitSigned(xs, ys, Array.fill(xs.length)(2.0), signs = Seq(0, 1, 1), l2 = 1.0)
    copies.zip(counted).foreach { case (a, b) => a shouldBe b +- 1e-5 }
  }

  it should "be a function of its rows alone" in {
    val order = xs.indices.reverse
    LogisticFit.fitSigned(order.map(xs).toArray, order.map(ys).toArray, Array.fill(xs.length)(1.0), Seq(0, 1, 1), 1.0) shouldBe
      LogisticFit.fitSigned(xs, ys, Array.fill(xs.length)(1.0), Seq(0, 1, 1), 1.0)
  }
}
