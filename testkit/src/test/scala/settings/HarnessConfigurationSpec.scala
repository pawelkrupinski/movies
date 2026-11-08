package settings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import java.nio.file.Path

/** The typed values a test, replay or measuring harness reads, resolved from an `Env` — driven
 *  here over maps, so nothing in this spec reads (or writes) the real process. */
class HarnessConfigurationSpec extends AnyFlatSpec with Matchers {

  private def resolvedFrom(vars: (String, String)*) = new HarnessConfiguration(Env.of(vars*))

  "HarnessConfiguration" should "resolve the fixture root, the repository's own tree when unset" in {
    resolvedFrom().fixtureRoot shouldBe FixtureRoot.RepositoryRelative
    resolvedFrom("KINOWO_FIXTURE_ROOT" -> "/tmp/fixtures").fixtureRoot shouldBe FixtureRoot(Path.of("/tmp/fixtures"))
  }

  it should "resolve a convergence leg's replay: hermetic, or filling only the gaps, each off unless asked" in {
    resolvedFrom().hermeticReplay shouldBe HermeticReplay(false)
    resolvedFrom().gapFill shouldBe GapFill(false)
    resolvedFrom("KINOWO_CONVERGENCE_HERMETIC" -> "true").hermeticReplay shouldBe HermeticReplay(true)
    resolvedFrom("KINOWO_CONVERGENCE_FILL_ONLY" -> "true").gapFill shouldBe GapFill(true)
    resolvedFrom("KINOWO_CONVERGENCE_FILL_ONLY" -> "no").gapFill shouldBe GapFill(false)
  }

  it should "resolve which order-independence passes a split row replays, every pass when unset" in {
    resolvedFrom().orderPasses shouldBe None
    resolvedFrom("KINOWO_CONVERGENCE_ORDER_PASSES" -> "0,1").orderPasses shouldBe Some(OrderPasses(Seq(0, 1)))
    resolvedFrom("KINOWO_CONVERGENCE_ORDER_PASSES" -> " 1, 2 ").orderPasses shouldBe Some(OrderPasses(Seq(1, 2)))
  }

  it should "resolve where a one-pass order row writes its corpus and where a comparison reads them" in {
    resolvedFrom().orderPassOut shouldBe None
    resolvedFrom().orderPassIn shouldBe None
    resolvedFrom("KINOWO_CONVERGENCE_ORDER_PASS_OUT" -> "/tmp/out").orderPassOut shouldBe Some(OrderPassOut(Path.of("/tmp/out")))
    resolvedFrom("KINOWO_CONVERGENCE_ORDER_PASS_IN" -> "/tmp/in").orderPassIn shouldBe Some(OrderPassIn(Path.of("/tmp/in")))
  }

  it should "refuse an order-pass list it cannot read, rather than replay passes nobody asked for" in {
    an[IllegalArgumentException] should be thrownBy resolvedFrom("KINOWO_CONVERGENCE_ORDER_PASSES" -> "0,x").orderPasses
  }

  // Every boolean harness switch reads through the one `Env.flag` rule (`1` or `true`), not a
  // spelling of its own: they took "true" alone, one of them any value at all.
  it should "read every boolean harness switch as one flag, `1` and `true` alike" in {
    val on = resolvedFrom("KINOWO_CONVERGENCE_HERMETIC" -> "1", "KINOWO_CONVERGENCE_FILL_ONLY" -> "1", "KINOWO_IDENTITY_LOOKUPS" -> "1",
      "KINOWO_HARD_CLUSTERS_DUMP" -> "true")
    (on.hermeticReplay, on.gapFill, on.identityLookupSweep, on.hardClusterDump) shouldBe
      (HermeticReplay(true), GapFill(true), IdentityLookupSweepEnabled(true), HardClusterDump(true))
    resolvedFrom("KINOWO_HARD_CLUSTERS_DUMP" -> "no").hardClusterDump shouldBe HardClusterDump(false)
  }

  it should "give a capture's live reads four at a time per host, unless the parallel runs split it" in {
    resolvedFrom().identityLivePerHost shouldBe IdentityLivePerHost(4)
    resolvedFrom("KINOWO_IDENTITY_LIVE_PER_HOST" -> "2").identityLivePerHost shouldBe IdentityLivePerHost(2)
  }

}
