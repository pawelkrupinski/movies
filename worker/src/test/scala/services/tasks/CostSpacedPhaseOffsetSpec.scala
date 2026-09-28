package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class CostSpacedPhaseOffsetSpec extends AnyFlatSpec with Matchers {

  private val period = 60.minutes.toMillis

  // A chunked country's shape: a few venues fanning out dozens of tasks among many
  // that cost a couple.
  private val heavy  = (0 until 30).map(i => s"scrape|Heavy $i")
  private val light  = (0 until 270).map(i => s"scrape|Light $i")
  private val roster = heavy ++ light
  private val cost   = heavy.map(_ -> 60.0).toMap ++ light.map(_ -> 2.0)

  private def spaced(meanCost: Map[String, Double], periodOf: String => Long = _ => period): CostSpacedPhaseOffset = {
    val phases = new CostSpacedPhaseOffset
    phases.plan(CostSpacedPhaseOffset.fractions(roster, meanCost, periodOf))
    phases
  }

  /** The most cost falling due in any 5-minute stretch of the period — what lands in
   *  the queue together, since a chunked venue's fan-out is released over 5 minutes. */
  private def peakFiveMinuteCost(phases: PhaseOffset): Double = {
    val perMinute = Array.fill(60)(0.0)
    roster.foreach(key => perMinute((phases.millis(key, period) / 60000).toInt) += cost(key))
    (0 until 60).map(m => (0 until 5).map(d => perMinute((m + d) % 60)).sum).max
  }

  private val meanFiveMinuteCost = cost.values.sum / 12

  // The whole point. Hashed phases spread venues evenly by COUNT, so wherever heavy
  // venues happen to hash close together their fan-outs land in the queue together.
  "CostSpacedPhaseOffset" should "spread cost evenly across the period where hashed phases let heavy venues bunch" in {
    // Spaced by cost, a stretch holds its share of the load plus at most the one venue
    // straddling its edge — the tightest any schedule of whole venues can do.
    peakFiveMinuteCost(HashedPhaseOffset) should be > meanFiveMinuteCost * 1.4
    peakFiveMinuteCost(spaced(cost))      should be <= meanFiveMinuteCost + cost.values.max
  }

  it should "place a cinema with no measured cost as if it cost the median" in {
    val unmeasured = light.head
    val median     = 2.0 // 270 light at 2, 30 heavy at 60
    spaced(cost - unmeasured).millis(heavy.last, period) shouldBe spaced(cost.updated(unmeasured, median)).millis(heavy.last, period)
  }

  // A thin venue re-scraped on half the period runs twice as often, so it loads the
  // queue as much as a venue of twice its cost on the full period.
  it should "weigh a venue by how often it runs, not only by what one run costs" in {
    val thin = light.head
    val halfPeriod = spaced(cost, key => if (key == thin) period / 2 else period)
    val doubleCost = spaced(cost.updated(thin, cost(thin) * 2))
    roster.foreach(key => halfPeriod.millis(key, period) shouldBe doubleCost.millis(key, period))
  }

  it should "place a key at the same fraction of whatever period it is asked about" in {
    val phases = spaced(cost)
    val key    = heavy(3)
    phases.millis(key, 2 * period) shouldBe (2 * phases.millis(key, period) +- 1L)
  }

  it should "fall back to the hashed phase for a key the plan doesn't know" in {
    spaced(cost).millis("scrape|Opened Today", period) shouldBe HashedPhaseOffset.millis("scrape|Opened Today", period)
    new CostSpacedPhaseOffset().millis(heavy.head, period) shouldBe HashedPhaseOffset.millis(heavy.head, period)
  }
}

class ScrapePhasePlannerSpec extends AnyFlatSpec with Matchers {

  private val period = 60.minutes
  private val roster = Seq("scrape|A", "scrape|B", "scrape|C")

  "ScrapePhasePlanner" should "plan from the mean of each cinema's recent runs" in {
    val store = new InMemoryScrapeCostStore
    Seq(10, 30).foreach(n => store.record("scrape|A", ScrapeCost(n)))
    store.record("scrape|B", ScrapeCost(1))
    store.record("scrape|C", ScrapeCost(5))
    val phases = new CostSpacedPhaseOffset
    new ScrapePhasePlanner(roster, store, _ => period, phases).replan()

    val expected = CostSpacedPhaseOffset.fractions(roster, Map("scrape|A" -> 20.0, "scrape|B" -> 1.0, "scrape|C" -> 5.0), _ => period.toMillis)
    roster.foreach(key => phases.millis(key, period.toMillis) shouldBe (expected(key) * period.toMillis).toLong)
  }

  it should "keep the plan in place when the costs can't be read" in {
    val store = new InMemoryScrapeCostStore
    store.record("scrape|A", ScrapeCost(40))
    val phases  = new CostSpacedPhaseOffset
    new ScrapePhasePlanner(roster, store, _ => period, phases).replan()
    val planned = roster.map(phases.millis(_, period.toMillis))

    val unreadable = new ScrapeCostStore {
      def record(dedupKey: String, cost: ScrapeCost): Unit = ()
      def recent(): Map[String, Seq[ScrapeCost]] = throw new IllegalStateException("Mongo down")
    }
    new ScrapePhasePlanner(roster, unreadable, _ => period, phases).replan()
    roster.map(phases.millis(_, period.toMillis)) shouldBe planned
  }
}

class InMemoryScrapeCostStoreSpec extends AnyFlatSpec with Matchers {
  "InMemoryScrapeCostStore" should "keep each key's latest RecentRuns costs, oldest first" in {
    val store = new InMemoryScrapeCostStore
    (1 to 7).foreach(n => store.record("scrape|A", ScrapeCost(n)))
    store.record("scrape|B", ScrapeCost(9))
    store.recent() shouldBe Map("scrape|A" -> (3 to 7).map(ScrapeCost(_)), "scrape|B" -> Seq(ScrapeCost(9)))
  }
}
