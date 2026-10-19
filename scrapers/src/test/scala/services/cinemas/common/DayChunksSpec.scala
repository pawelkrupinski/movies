package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** Every per-day chunked scraper plans with [[DayChunks.keys]] and fetches with
 *  [[DayChunks.days]], so the two must round-trip and group by [[DayChunks.PerChunk]]. */
class DayChunksSpec extends AnyFlatSpec with Matchers {

  private val days = (0 until 16).map(LocalDate.of(2026, 12, 25).plusDays(_))

  "DayChunks.keys" should "group days into runs of PerChunk, the last one short" in {
    DayChunks.keys(days).map(DayChunks.days(_).size) shouldBe Seq(7, 7, 2)
  }

  it should "round-trip every day in order, across a year boundary" in {
    DayChunks.keys(days).flatMap(DayChunks.days) shouldBe days
  }

  it should "plan no chunk for no days" in {
    DayChunks.keys(Nil) shouldBe empty
  }
}
