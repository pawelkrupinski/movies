package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures
import services.identity.IdentityMeasures.{Category, Measure}

/** The joint (logistic) candidate: it recovers the likelihood the data implies, keeps a
 *  corroborator's naive-Bayes table as a fixed offset, and keeps an evidence order. */
class IdentityJointFitSpec extends AnyFlatSpec with Matchers {

  import IdentityJointFit.{Cell, Unit}

  private def units(offset: Double, cells: Seq[Cell], same: Int, different: Int): Seq[Unit] =
    Seq.fill(same)(Unit(offset, cells, same = true)) ++ Seq.fill(different)(Unit(offset, cells, same = false))

  private val top = Cell("search.rank", "bin:0")

  "the joint fit" should "recover the log-odds the data implies, beside a fixed offset" in {
    // Top-ranked: 800 same, 200 different; the rest 500 / 500 — log-odds 0 and log 4 = 1.386.
    val (b, w) = IdentityJointFit.solve(units(0, Seq(top), 800, 200) ++ units(0, Nil, 500, 500), Seq(Seq(top)))
    b shouldBe 0.0 +- 0.02
    w(top) shouldBe math.log(4) +- 0.02
    // An offset every unit carries is the corroborators' say, not the free cell's.
    val (b1, w1) = IdentityJointFit.solve(units(1, Seq(top), 800, 200) ++ units(1, Nil, 500, 500), Seq(Seq(top)))
    b1 shouldBe -1.0 +- 0.02
    w1(top) shouldBe math.log(4) +- 0.02
  }

  it should "weigh a free signal for what it adds beyond the fixed ones, where naive Bayes counts it twice" in {
    // Credited listings: the director tells same from different outright, and both sit low in the
    // search. Bare listings: the rank is all there is, and low means 1 in 4 same.
    val shapes: Seq[(Measure, Double, Boolean, Int)] = Seq((Category("same_person"), 3.0, true, 300), (Category("different"), 3.0, false, 300),
      (IdentityMeasures.Missing("listing"), 1.0, true, 300), (IdentityMeasures.Missing("listing"), 1.0, false, 100),
      (IdentityMeasures.Missing("listing"), 3.0, true, 100), (IdentityMeasures.Missing("listing"), 3.0, false, 300))
    val rows = (for {
      split <- Seq("train", "calibration")
      (director, rank, same, n) <- shapes
      _ <- 0 until n
    } yield (split, director, rank, same)).zipWithIndex.map { case ((split, director, rank, same), i) =>
      IdentityCalibrate.Row(split, s"u$i", s"u$i", "us", Map("director" -> director, "search.rank" -> IdentityMeasures.Number(rank)), _ => Some(same))
    }
    val nb    = IdentityCalibrate.fit(IdentityMeasures.ListingFilm, Seq("director", "search.rank"), rows)
    val joint = IdentityJointFit.fit(nb, rows, free = Set("search.rank"), order = Map.empty)
    def rankWeights(f: IdentityCalibrate.Fitted) = f.tables.find(_.signal == "search.rank").get.weights.bins.map(_.weight)
    def gap(f: IdentityCalibrate.Fitted) = rankWeights(f).head - rankWeights(f).last
    // Among bare listings, top against low is log 3 − log(1/3) = 2.2; naive Bayes, reading the
    // credited listings' low ranks as evidence too, puts it elsewhere.
    gap(joint) shouldBe (2 * math.log(3)) +- 0.1
    math.abs(gap(nb) - 2 * math.log(3)) should be > 0.3
    // The corroborator's table is naive Bayes' own.
    joint.tables.find(_.signal == "director") shouldBe nb.tables.find(_.signal == "director")
  }

  it should "keep an evidence order by tying the categories that break it" in {
    val (decorated, overlap) = (Cell("title", "decorated"), Cell("title", "overlap"))
    // The data put overlap (3:1) above decorated (1:1); the order says decorated ≥ overlap.
    val data = units(0, Seq(decorated), 100, 100) ++ units(0, Seq(overlap), 300, 100) ++ units(0, Nil, 200, 200)
    val (_, free) = IdentityJointFit.fitOrdered(data, Seq(decorated, overlap), Map.empty)
    free(overlap) should be > free(decorated)
    val (_, tied) = IdentityJointFit.fitOrdered(data, Seq(decorated, overlap), Map("title" -> Seq("decorated", "overlap")))
    tied(decorated) shouldBe tied(overlap)
    tied(decorated) shouldBe math.log(2) +- 0.05
  }
}
