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
    val s1 = RatingCadence.record(None, reportedValue = None, t0)
    s1.unchangedStreak shouldBe 1
    val s2 = RatingCadence.record(Some(s1), reportedValue = None, later(2.hours))
    s2.unchangedStreak shouldBe 2
    val s3 = RatingCadence.record(Some(s2), reportedValue = Some("7.5"), later(6.hours))
    s3.unchangedStreak shouldBe 0   // a visible change snaps cadence back to the base
  }

  it should "count checks and changes within the window" in {
    val s1 = RatingCadence.record(None, reportedValue = Some("7.5"), t0)
    val s2 = RatingCadence.record(Some(s1), reportedValue = None, later(2.hours))
    // A genuine move to a DIFFERENT displayed value is the second change.
    val s3 = RatingCadence.record(Some(s2), reportedValue = Some("7.6"), later(4.hours))
    s3.windowChecks  shouldBe 3
    s3.windowChanges shouldBe 2
  }

  it should "not count a re-reported identical displayed value as a change (re-key / re-resolution)" in {
    // A re-keyed row re-resolves None → 7.3 and re-reports the SAME 7.3 the user
    // already saw. That isn't a visible change — it must not log a phantom change
    // nor reset the streak, or the film stays pinned to the base interval forever.
    val first  = RatingCadence.record(None, reportedValue = Some("7.3"), t0)
    first.windowChanges  shouldBe 1
    first.unchangedStreak shouldBe 0
    val repeat = RatingCadence.record(Some(first), reportedValue = Some("7.3"), later(2.hours))
    repeat.unchangedStreak shouldBe 1                          // backs off, not reset
    repeat.windowChecks    shouldBe 2                          // the refresh still counts as a check
    repeat.windowChanges   shouldBe 1                          // but NOT as a change
    repeat.lastChange      shouldBe Some(RatingChange(t0, "7.3"))  // history untouched
    repeat.prevChange      shouldBe None                       // no phantom "7.3 → 7.3" shift
  }

  it should "still count a move to a new value after an identical re-report" in {
    val first  = RatingCadence.record(None, reportedValue = Some("7.3"), t0)
    val repeat = RatingCadence.record(Some(first), reportedValue = Some("7.3"), later(2.hours))
    val moved  = RatingCadence.record(Some(repeat), reportedValue = Some("7.4"), later(4.hours))
    moved.unchangedStreak shouldBe 0
    moved.windowChanges   shouldBe 2
    moved.lastChange      shouldBe Some(RatingChange(later(4.hours), "7.4"))
    moved.prevChange      shouldBe Some(RatingChange(t0, "7.3"))
  }

  it should "roll the window over once it is older than a week (drop stale history)" in {
    val s1 = RatingCadence.record(None, reportedValue = Some("7.5"), t0)
    val s1b = RatingCadence.record(Some(s1), reportedValue = Some("7.5"), later(1.hour))
    s1b.windowChecks shouldBe 2
    // A refresh more than a week after the window opened starts a fresh window.
    val stale = RatingCadence.record(Some(s1b), reportedValue = None, later(8.days))
    stale.windowChecks  shouldBe 1
    stale.windowChanges shouldBe 0
    JDuration.between(stale.windowStartedAt, later(8.days)).toMillis shouldBe 0
  }

  it should "keep the last two changes (with value), shifting on each new change and surviving no-change refreshes" in {
    val c1 = RatingCadence.record(None, reportedValue = Some("7.0"), t0)
    c1.lastChange shouldBe Some(RatingChange(t0, "7.0"))
    c1.prevChange shouldBe None
    // A no-change refresh leaves the change history untouched.
    val c1b = RatingCadence.record(Some(c1), reportedValue = None, later(2.hours))
    c1b.lastChange shouldBe Some(RatingChange(t0, "7.0"))
    // A second change shifts the first into `prevChange`.
    val c2 = RatingCadence.record(Some(c1b), reportedValue = Some("7.2"), later(5.hours))
    c2.lastChange shouldBe Some(RatingChange(later(5.hours), "7.2"))
    c2.prevChange shouldBe Some(RatingChange(t0, "7.0"))
  }

  it should "keep change history across a window roll-over (it's the last 2 ever, not last-week)" in {
    val c1   = RatingCadence.record(None, reportedValue = Some("80"), t0)
    val roll = RatingCadence.record(Some(c1), reportedValue = None, later(8.days))   // window resets
    roll.windowChecks shouldBe 1                                              // counters rolled
    roll.lastChange   shouldBe Some(RatingChange(t0, "80"))                   // change history survived
  }

  it should "drive the interval end-to-end: stable film stretches toward the cap" in {
    val stable = (1 to 6).foldLeft(Option.empty[RatingChangeStats]) { (acc, i) =>
      Some(RatingCadence.record(acc, reportedValue = None, later(i * 2.hours)))
    }
    RatingCadence.intervalFor(stable) shouldBe RatingCadence.MaxInterval
  }
}
