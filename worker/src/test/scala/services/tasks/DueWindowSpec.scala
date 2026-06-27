package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
}
