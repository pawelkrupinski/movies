package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityCalibration
import tools.UnmatchedClusters.Label

/** The refit's re-resolve of the captures: what a calibration takes on them.
 *  A suite of its own, apart from [[IdentityRefitSpec]]: each of these re-resolves recorded captures, the two
 *  slowest tests of the worker's unit run, and ScalaTest runs one suite's tests one after another but suites side
 *  by side. */
class IdentityRefitCaptureReplaySpec extends AnyFlatSpec with Matchers {
  import IdentityRefit._

  private val shipped = IdentityCalibration.resolver

  "A calibration" should "reach the captures' re-resolve: one accepting nothing takes fewer films than the shipped one" in {
    val replay  = new CaptureReplay(tools.UnmatchedClusters.read(tools.UnmatchedClusters.fixturePath(models.Country.Germany)), None)
    val nothing = CutChange("listing-film", "showRatings", shipped.ratingCut, 0.999999).applyTo(
      shipped.copy(evidenceClasses = Nil))
    val control = baseline(Seq(replay), shipped)
    val strict  = measureOn(Seq(replay), control, nothing).flatMap(_.takes)
    strict.size should be < control.takes.size
    // the control is the ratchet's own replay of the captured decisions, untouched by a change that moves nothing
    control.captures.head.decisions shouldBe replay.capture.decisions
    measureOn(Seq(replay), control, shipped).flatMap(_.takes) shouldBe control.takes
    // a take labelled right is a unit the refit measures — one whose film the model's own evidence reaches (a film
    // only a family or the fill takes is no candidate of a node)
    control.takes.filter(_.tmdb.isDefined).exists(taken =>
      labelledUnits(replay.capture, Seq(Label("de", "*", taken.rawTitle, taken.film, right = true, "")), shipped).nonEmpty) shouldBe true
  }
}
