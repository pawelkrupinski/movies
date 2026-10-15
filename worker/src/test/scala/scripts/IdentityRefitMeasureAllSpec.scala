package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityCalibration

/** Measuring the refit's candidate changes over the captures, side by side.
 *  A suite of its own, apart from [[IdentityRefitSpec]]: each of these re-resolves recorded captures, the two
 *  slowest tests of the worker's unit run, and ScalaTest runs one suite's tests one after another but suites side
 *  by side. */
class IdentityRefitMeasureAllSpec extends AnyFlatSpec with Matchers {
  import IdentityRefit._

  private val shipped = IdentityCalibration.resolver

  "Measuring the changes" should "give each change the measure it has alone, its captures in order, whatever task finished first" in {
    // Poland's and Spain's captures (2.5 MB and 0.2, against Germany's 5.7): the claim is about order and pairing across
    // two captures, and Poland's takes move with the rating cut, so the strict change still takes fewer films.
    val replays = Seq(models.Country.Poland, models.Country.Spain).map(c => new CaptureReplay(tools.UnmatchedClusters.read(tools.UnmatchedClusters.fixturePath(c)), None))
    val pool    = java.util.concurrent.Executors.newFixedThreadPool(4)
    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(pool)
    try {
      val control = baseline(replays, shipped)
      control.captures.map(_.decisions) shouldBe replays.map(_.capture.decisions)
      val changes = Seq(CutChange("listing-film", "showRatings", shipped.ratingCut, 0.999999), CutChange("listing-film", "showRatings", shipped.ratingCut, shipped.ratingCut))
      val all     = measureAll(replays, control, changes)
      all.map(_.size) shouldBe Seq(2, 2)
      all shouldBe changes.map(c => measureOn(replays, control, c.applyTo(control.calibration)))
      all.head.flatMap(_.takes).size should be < control.takes.size
      measureAll(replays, control, Nil) shouldBe empty
    } finally pool.shutdown()
  }
}
