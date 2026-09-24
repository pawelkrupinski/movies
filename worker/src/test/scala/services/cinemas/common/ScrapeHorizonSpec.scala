package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, YearMonth}

/** The horizon walk tolerates a probe failing — a blip reads as a blank step — but a
 *  walk in which EVERY probe failed is a dead source, and must fail rather than hand the
 *  scraper an empty day list that reads as a dormant venue. */
class ScrapeHorizonSpec extends AnyFlatSpec with Matchers {

  private val from  = LocalDate.of(2026, 9, 24)
  private val boom  = new RuntimeException("down")

  "ScrapeHorizon.liveDays" should "keep the live days across a probe that threw" in {
    ScrapeHorizon.liveDays(from, maxEmptyDays = 3) { day =>
      if (day == from) throw boom else day == from.plusDays(1)
    } shouldBe Seq(from.plusDays(1))
  }

  it should "answer an empty list for a source that answered every probe with nothing" in {
    ScrapeHorizon.liveDays(from, maxEmptyDays = 3)(_ => false) shouldBe empty
  }

  it should "throw the first failure when every probe threw" in {
    the[RuntimeException] thrownBy ScrapeHorizon.liveDays(from, maxEmptyDays = 3)(_ => throw boom) shouldBe boom
  }

  "ScrapeHorizon.liveMonths" should "throw the first failure when every probe threw" in {
    the[RuntimeException] thrownBy ScrapeHorizon.liveMonths(YearMonth.from(from))(_ => throw boom) shouldBe boom
  }
}
