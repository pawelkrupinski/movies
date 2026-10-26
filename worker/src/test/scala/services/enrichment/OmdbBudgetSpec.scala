package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/** The fleet's daily OMDb budget, by ask: a new film may use the whole day and the last 10% is its alone, a user's
 *  answer and a routine refresh stop short of it, and a bulk sweep takes only its paced share of what is left. */
class OmdbBudgetSpec extends AnyFlatSpec with Matchers {
  // OMDb's day starts at 05:00 UTC (midnight US Central, daylight time in October).
  private val dayStart = Instant.parse("2026-10-26T05:00:00Z")
  private val noon     = Instant.parse("2026-10-26T17:00:00Z") // half the day gone

  private def spend(budget: OmdbBudget, ask: OmdbAsk, now: Instant): Int =
    Iterator.continually(budget.take(ask, now)).takeWhile(_ == OmdbBudget.Granted).size

  "OMDb's day" should "turn at midnight US Central, which was 05:00 UTC on 2026-10-26" in {
    OmdbBudget.dayOf(Instant.parse("2026-10-26T04:59:59Z")).toString shouldBe "2026-10-25"
    OmdbBudget.dayOf(dayStart).toString shouldBe "2026-10-26"
    OmdbBudget.nextReset(noon) shouldBe Instant.parse("2026-10-27T05:00:00Z")
  }

  "the budget" should "keep the last tenth of the day for new films" in {
    val budget = new InMemoryOmdbBudget(limit = 1000)
    spend(budget, OmdbAsk.UserWaiting, noon) shouldBe 900
    spend(budget, OmdbAsk.Routine, noon) shouldBe 0 // routine stopped at 800, already passed
    spend(budget, OmdbAsk.NewFilm, noon) shouldBe 100
    budget.take(OmdbAsk.NewFilm, noon) shouldBe OmdbBudget.Spent(Instant.parse("2026-10-27T05:00:00Z"))
  }

  it should "let routine refreshes use up to 80%, and user answers the 10% above them" in {
    val budget = new InMemoryOmdbBudget(limit = 1000)
    spend(budget, OmdbAsk.Routine, noon) shouldBe 800
    spend(budget, OmdbAsk.UserWaiting, noon) shouldBe 100
    spend(budget, OmdbAsk.NewFilm, noon) shouldBe 100
  }

  it should "pace a bulk sweep across the day, and give it 60% at most" in {
    val budget = new InMemoryOmdbBudget(limit = 1000)
    spend(budget, OmdbAsk.Bulk, dayStart) shouldBe 0
    spend(budget, OmdbAsk.Bulk, noon) shouldBe 300
    spend(budget, OmdbAsk.Bulk, Instant.parse("2026-10-27T04:59:59Z")) shouldBe 299
    budget.usedToday(Instant.parse("2026-10-27T04:59:59Z")) shouldBe 599
    // What the sweeps left is still there for the asks above them.
    spend(budget, OmdbAsk.NewFilm, Instant.parse("2026-10-27T04:59:59Z")) shouldBe 401
  }

  it should "send a refused bulk sweep back in a share's time, and everything else to the next day" in {
    val budget = new InMemoryOmdbBudget(limit = 1000)
    spend(budget, OmdbAsk.Bulk, noon)
    budget.take(OmdbAsk.Bulk, noon) match {
      case OmdbBudget.Spent(at) => (at.toEpochMilli - noon.toEpochMilli) shouldBe (86400000L / 600)
      case other                => fail(s"bulk past its share was $other")
    }
    spend(budget, OmdbAsk.Routine, noon)
    budget.take(OmdbAsk.Routine, noon) shouldBe OmdbBudget.Spent(Instant.parse("2026-10-27T05:00:00Z"))
  }

  it should "start every OMDb day afresh" in {
    val budget = new InMemoryOmdbBudget(limit = 10)
    spend(budget, OmdbAsk.NewFilm, noon) shouldBe 10
    spend(budget, OmdbAsk.NewFilm, Instant.parse("2026-10-27T05:00:00Z")) shouldBe 10
  }

  it should "stop everyone, new films too, once OMDb itself says the quota is spent" in {
    val budget = new InMemoryOmdbBudget(limit = 1000)
    budget.markSpent(noon)
    budget.take(OmdbAsk.NewFilm, noon) shouldBe OmdbBudget.Spent(Instant.parse("2026-10-27T05:00:00Z"))
    budget.take(OmdbAsk.NewFilm, Instant.parse("2026-10-27T05:00:00Z")) shouldBe OmdbBudget.Granted
  }
}
