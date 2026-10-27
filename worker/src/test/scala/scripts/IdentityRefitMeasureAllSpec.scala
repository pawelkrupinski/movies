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

  "A refit whose round 1 shards measured on other runners" should "refit and report byte for byte as the refit measured whole" in {
    // Poland's and Spain's captures, as above: Poland's takes move with the cut, so the reports compare moves (asserted
    // below), at under half the resolve of Germany's 5.7 MB capture, which made this the worker's slowest test.
    val replays  = Seq(models.Country.Poland, models.Country.Spain).map(c => new CaptureReplay(tools.UnmatchedClusters.read(tools.UnmatchedClusters.fixturePath(c)), None))
    val labels   = tools.IdentityJudgements.labels()
    val expected = tools.UnmatchedClusters.readTakeLines(tools.UnmatchedClusters.Directory.resolve("expected-matches.tsv"))
    // three proposals, not the ~58 a real round makes: a cut far up (it loses takes), a cut a step down, a bound
    val ground = new Ground(replays, labels, expected, shipped, Set.empty, _ => ()) {
      override def propose(c: IdentityCalibration): Seq[Change] =
        Seq(CutChange("listing-film", "showRatings", c.ratingCut, 0.999999), CutChange("listing-film", "showRatings", c.ratingCut, c.ratingCut - 0.05)) ++
          boundChanges(c).take(1)
    }
    val whole  = search(ground, maxChanges = 2, threads = 4)
    // each shard's file as it crosses runners: written, read back
    val shards  = (0 until 2).map(i => play.api.libs.json.Json.parse(play.api.libs.json.Json.stringify(play.api.libs.json.Json.toJson(measureShard(ground, Shard(i, 2), 4)))).as[ShardFile])
    shards.map(_.measured.map(_.index)) shouldBe Seq(Seq(0, 2), Seq(1))
    val sharded = search(ground, maxChanges = 2, threads = 4, firstRound = Some(merged(_, shards)))
    whole._3.head.exists(_.moved.nonEmpty) shouldBe true // something moved: the reports compare more than empty tables
    report(sharded._2, sharded._3, "units") shouldBe report(whole._2, whole._3, "units")
    sharded._1 shouldBe whole._1
    val proposed = ground.propose(shipped)
    the[IllegalStateException] thrownBy merged(proposed, shards.take(1)) should have message
      "the shards' measures do not merge: shards 0/2, not one of each"
    the[IllegalStateException] thrownBy merged(proposed.reverse, shards) should have message
      s"the shards' measures do not merge: proposal 0 is ${proposed(2).parameter} ${proposed(2).from} → ${proposed(2).to} here, " +
        s"${proposed(0).parameter} ${proposed(0).from} → ${proposed(0).to} in a shard"
  }
}
