package services.cadence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

class RatingCadenceSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-27T10:00:00Z")
  private def later(d: FiniteDuration): Instant = t0.plusMillis(d.toMillis)

  "intervalFor" should "start at the base for a never-seen film" in {
    RatingCadence.intervalFor(None) shouldBe RatingCadence.BaseInterval
  }

  it should "double per consecutive no-change refresh and clamp at the max" in {
    RatingCadence.intervalForStreak(0) shouldBe 2.hours
    RatingCadence.intervalForStreak(1) shouldBe 4.hours
    RatingCadence.intervalForStreak(2) shouldBe 8.hours
    RatingCadence.intervalForStreak(3) shouldBe 16.hours
    // 2h × 2^6 = 128h would exceed the 4-day (96h) cap → clamped.
    RatingCadence.intervalForStreak(6) shouldBe RatingCadence.MaxInterval
    RatingCadence.intervalForStreak(99) shouldBe RatingCadence.MaxInterval
  }

  "record" should "grow the streak on no-change and reset it on a change" in {
    val s1 = RatingCadence.record(None, changed = false, t0)
    s1.unchangedStreak shouldBe 1
    val s2 = RatingCadence.record(Some(s1), changed = false, later(2.hours))
    s2.unchangedStreak shouldBe 2
    val s3 = RatingCadence.record(Some(s2), changed = true, later(6.hours))
    s3.unchangedStreak shouldBe 0   // a visible change snaps cadence back to the base
  }

  it should "count checks and changes within the window" in {
    val s1 = RatingCadence.record(None, changed = true, t0)
    val s2 = RatingCadence.record(Some(s1), changed = false, later(2.hours))
    val s3 = RatingCadence.record(Some(s2), changed = true, later(4.hours))
    s3.windowChecks  shouldBe 3
    s3.windowChanges shouldBe 2
  }

  it should "roll the window over once it is older than a week (drop stale history)" in {
    val s1 = RatingCadence.record(None, changed = true, t0)
    val s1b = RatingCadence.record(Some(s1), changed = true, later(1.hour))
    s1b.windowChecks shouldBe 2
    // A refresh more than a week after the window opened starts a fresh window.
    val stale = RatingCadence.record(Some(s1b), changed = false, later(8.days))
    stale.windowChecks  shouldBe 1
    stale.windowChanges shouldBe 0
    JDuration.between(stale.windowStartedAt, later(8.days)).toMillis shouldBe 0
  }

  it should "drive the interval end-to-end: stable film stretches toward the cap" in {
    val stable = (1 to 6).foldLeft(Option.empty[RatingChangeStats]) { (acc, i) =>
      Some(RatingCadence.record(acc, changed = false, later(i * 2.hours)))
    }
    RatingCadence.intervalFor(stable) shouldBe RatingCadence.MaxInterval
  }
}
