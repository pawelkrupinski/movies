package services.schedule

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class OccurrenceKeySpec extends AnyFlatSpec with Matchers {

  private val hourMs = 3600_000L

  "OccurrenceKey.at" should "give the same id for two instants in the same window" in {
    // A 1h job: 09:00:03 and 09:59:59 are the same window → same claim id, so
    // two machines firing anywhere in that hour contend for one claim.
    val early = OccurrenceKey.at("job", 9 * hourMs + 3_000L, 1.hour, 0.seconds)
    val late  = OccurrenceKey.at("job", 10 * hourMs - 1L, 1.hour, 0.seconds)
    early shouldBe late
  }

  it should "give different ids for adjacent windows" in {
    val w0 = OccurrenceKey.at("job", 9 * hourMs + 1L, 1.hour, 0.seconds)
    val w1 = OccurrenceKey.at("job", 10 * hourMs + 1L, 1.hour, 0.seconds)
    w0 should not be w1
  }

  it should "namespace by job so two jobs in the same window don't collide" in {
    val a = OccurrenceKey.at("scrape", 9 * hourMs, 1.hour, 0.seconds)
    val b = OccurrenceKey.at("detail", 9 * hourMs, 1.hour, 0.seconds)
    a should not be b
  }

  it should "shift the boundary by the offset" in {
    // 4h period, offset 1h → boundaries at 01:00, 05:00, 09:00, …
    // 08:30 falls in the [05:00, 09:00) window; 09:30 in [09:00, 13:00).
    val before = OccurrenceKey.at("enrich", 8 * hourMs + hourMs / 2, 4.hours, 1.hour)
    val after  = OccurrenceKey.at("enrich", 9 * hourMs + hourMs / 2, 4.hours, 1.hour)
    before should not be after
    // The boundary is offset by the hour: the id ends in the 05:00 / 09:00 instant.
    before should include ("T05:00:00Z")
    after  should include ("T09:00:00Z")
  }
}
