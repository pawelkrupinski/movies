package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class RelativeTimeSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-19T12:00:00Z")

  "RelativeTime" should "phrase a past instant as elapsed" in {
    RelativeTime.of(now.minusSeconds(90),      now) shouldBe "1m ago"
    RelativeTime.of(now.minusSeconds(3 * 3600), now) shouldBe "3h ago"
    RelativeTime.of(now.minusSeconds(5 * 86400), now) shouldBe "5d ago"
  }

  // The bug this exists for: a "next due" timestamp is in the FUTURE, and
  // `Duration.between(future, now)` is negative — so it rendered as "-225m ago",
  // which reads as both wrong and in the past.
  it should "phrase a future instant as remaining, never as negative elapsed" in {
    RelativeTime.of(now.plusSeconds(225 * 60), now) shouldBe "in 3h"
    RelativeTime.of(now.plusSeconds(90),       now) shouldBe "in 1m"
    RelativeTime.of(now.plusSeconds(3 * 86400), now) shouldBe "in 3d"
    RelativeTime.of(now.plusSeconds(225 * 60), now) should not include "-"
    RelativeTime.of(now.plusSeconds(225 * 60), now) should not include "ago"
  }

  it should "call the present moment now, in either direction" in {
    RelativeTime.of(now, now)                  shouldBe "now"
    RelativeTime.of(now.plusSeconds(20), now)  shouldBe "now"
    RelativeTime.of(now.minusSeconds(20), now) shouldBe "now"
  }
}
