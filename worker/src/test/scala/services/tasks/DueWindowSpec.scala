package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.cadence.DueBoundary

import java.time.Instant
import scala.concurrent.duration._

class DueWindowSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-19T00:00:00Z")

  "DueWindow.isDue" should "treat a never-stamped key as due" in {
    new DueWindow(30.minutes).isDue("scrape|Foo", None, t0) shouldBe true
  }

  // The cadence dev page shows "next refresh" via CadenceReport.nextRefreshAt — it
  // must agree with the boundary at which THIS window actually becomes due, or the
  // page lies about the reaper.
  it should "agree with CadenceReport.nextRefreshAt on the next-due boundary" in {
    val key      = "mc|tmdb:1"
    val interval = 8.hours
    val next     = services.cadence.CadenceReport.nextRefreshAt(key, t0, interval)
    val dw       = new DueWindow(interval)
    dw.isDue(key, Some(t0), next.minusMillis(1)) shouldBe false   // not due just before the boundary
    dw.isDue(key, Some(t0), next)                shouldBe true    // due exactly at it
  }

  it should "resolve the period PER KEY so an adaptive schedule backs some keys off" in {
    // A key on a long (4-day) period and one on the short base, both stamped at t0.
    val dw = new DueWindow(key => if (key.contains("stable")) 4.days else 2.hours, 2.hours)
    val day = t0.plusSeconds(24 * 3600)
    dw.isDue("mc|stable", Some(t0), day) shouldBe false   // long period → not due a day later
    dw.isDue("mc|fresh",  Some(t0), day) shouldBe true    // base period → long overdue
  }

  it should "not be due again inside the same window, but is once the key's boundary passes" in {
    val dw  = new DueWindow(30.minutes)
    val key = "scrape|Foo"
    dw.isDue(key, Some(t0), t0.plusSeconds(60)) shouldBe false               // same window, 1 min later
    dw.isDue(key, Some(t0), t0.plusSeconds(2 * 30 * 60)) shouldBe true       // two periods later → boundary crossed
  }

  // The point of the phase offset: a synchronized corpus (every key stamped at the
  // same instant) must NOT all come due at the same boundary one period later — the
  // lockstep wave. Each key's boundary sits at its own hashed phase, so the re-scrapes
  // spread across the whole window.
  it should "spread a synchronized corpus of keys across the period rather than at one boundary" in {
    val period = 30.minutes
    val dw     = new DueWindow(period)
    val keys   = (0 until 300).map(i => s"scrape|Cinema$i")
    // For each key, the first whole-minute after t0 at which it becomes due again.
    val firstDueMinute = keys.map { k =>
      (1 to period.toMinutes.toInt).find(m => dw.isDue(k, Some(t0), t0.plusSeconds(m * 60L))).getOrElse(0)
    }
    val byMinute = firstDueMinute.groupBy(identity).view.mapValues(_.size).toMap
    byMinute.values.max should be < 40            // 300 keys / 30 min ≈ 10 avg; no minute holds the whole wave
    firstDueMinute.distinct.size should be >= 20  // genuinely spread across most minutes of the window
  }

  // Under nearest-boundary counting (the scrape schedule's), a refresh that ran late —
  // past half its window, behind a backlog — counts toward the next boundary, instead of
  // leaving the key due again there, a second refresh moments after the first.
  it should "count a refresh that ran late in its window toward the next boundary, when counting to the nearest" in {
    val period = 60.minutes
    val phase  = FixedPhase(10.minutes)
    val dw     = new DueWindow(_ => period, period, phase, DueBoundary.NearestBoundary)
    val key    = "scrape|Foo"
    val lateRun = t0.plusSeconds(10 * 60 + 45 * 60)                    // 45 min past the t0+10min boundary
    dw.isDue(key, Some(lateRun), t0.plusSeconds(70 * 60 + 1)) shouldBe false   // the next boundary, 15 min later
    dw.isDue(key, Some(lateRun), t0.plusSeconds(130 * 60))    shouldBe true    // the one after it
  }

  // Moving a key's phase — a cost-spaced plan rebuilt — must not re-run it at once or
  // leave it for up to two periods. Under preceding-boundary counting a phase change
  // made about half the corpus due the moment it landed.
  it should "keep the gap after a phase change within half to one-and-a-half periods, when counting to the nearest" in {
    val period  = 60.minutes
    val rng     = new scala.util.Random(7)
    val gaps = (0 until 500).map { i =>
      val key      = s"scrape|Cinema$i"
      val oldPhase = FixedPhase(rng.nextInt(60).minutes)
      val newPhase = FixedPhase(rng.nextInt(60).minutes)
      val lastRun  = t0.plusSeconds(oldPhase.offset.toSeconds + rng.nextInt(120))   // ran just after its old boundary
      val moved    = new DueWindow(_ => period, period, newPhase, DueBoundary.NearestBoundary)
      (1 to 180).find(m => moved.isDue(key, Some(lastRun), lastRun.plusSeconds(m * 60L))).get
    }
    gaps.min should be >= 30
    gaps.max should be <= 91
  }

  private final case class FixedPhase(offset: FiniteDuration) extends PhaseOffset {
    def millis(dedupKey: String, periodMillis: Long): Long = offset.toMillis % periodMillis
  }
}
