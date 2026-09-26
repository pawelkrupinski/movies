package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Signals

import scala.util.Random

class IdentityInterimFitSpec extends AnyFlatSpec with Matchers {

  private val d = Signals.ModelInputs.size
  private def input(name: String) = Signals.ModelInputs.indexOf(name)

  /** Nodes of three candidates; the right one has `title=exact` and `director=match` more often. */
  private def rows(seed: Long, nodes: Int): Seq[IdentityInterimFit.Row] = {
    val rnd = new Random(seed)
    (1 to nodes).flatMap { n =>
      (0 until 3).map { c =>
        val right = c == 0
        val x = new Array[Double](d)
        x(0) = 1.0
        if (rnd.nextDouble() < (if (right) 0.9 else 0.3)) x(input("title=exact")) = 1.0
        if (rnd.nextDouble() < (if (right) 0.6 else 0.05)) x(input("director=match")) = 1.0
        IdentityInterimFit.Row(s"n$n", if (n % 5 == 0) "test" else "train", c, Some(right), x)
      }
    }
  }

  "The interim fit" should "learn positive weights for the signals that mark the right candidate, deterministically" in {
    val data = rows(1, 400)
    val w = IdentityInterimFit.fit(data)
    w(input("title=exact")) should be > 0.5
    w(input("director=match")) should be > 1.0
    IdentityInterimFit.fit(data).toSeq shouldBe w.toSeq
    IdentityInterimFit.fit(new Random(3).shuffle(data)).zip(w).foreach { case (a, b) => a shouldBe b +- 1e-9 }
  }

  it should "agree with most held-out nodes at a threshold chosen on the train nodes" in {
    val data = rows(2, 400)
    val (train, test) = data.partition(_.split == "train")
    val w = IdentityInterimFit.fit(train)
    val t = (5 to 95 by 5).map(_ / 100.0).maxBy(t => (IdentityInterimFit.agreement(train, w, t)._1, -t))
    val (ok, n) = IdentityInterimFit.agreement(test, w, t)
    ok.toDouble / n should be > 0.7
  }
}
