package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/** Pins the exact strings the shared formatters emit — these replaced inline
 *  duplicates in the controllers and Twirl partials, so the assertions capture
 *  the pre-refactor output byte-for-byte (the snapshot-stability guard). */
class CardFormatSpec extends AnyFlatSpec with Matchers {

  "time" should "render a showtime clock as HH:mm" in {
    CardFormat.time(LocalDateTime.of(2026, 6, 4, 17, 30)) shouldBe "17:30"
    CardFormat.time(LocalDateTime.of(2026, 6, 4, 9, 5))   shouldBe "09:05"
    CardFormat.time(LocalDateTime.of(2026, 6, 4, 0, 0))   shouldBe "00:00"
  }

  "date" should "match DateFormatter's long Polish label" in {
    val d = LocalDate.of(2026, 6, 4)
    CardFormat.date(d) shouldBe DateFormatter.format(d)
  }

  "runtimePill" should "render the pill form, dropping a whole-hour minutes part" in {
    CardFormat.runtimePill(148) shouldBe "2h 28min"
    CardFormat.runtimePill(110) shouldBe "1h 50min"
    CardFormat.runtimePill(120) shouldBe "2h"
    CardFormat.runtimePill(55)  shouldBe "0h 55min"
    CardFormat.runtimePill(60)  shouldBe "1h"
  }

  "runtimeText" should "render the spaced OG-card form, always showing minutes" in {
    CardFormat.runtimeText(55)  shouldBe "0h 55min"
    CardFormat.runtimeText(102) shouldBe "1h 42min"
    CardFormat.runtimeText(120) shouldBe "2h 0min"
  }
}
