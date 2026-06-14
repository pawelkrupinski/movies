package services.alerts

import models.{Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepo

import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/**
 * StagingStuckAlerter: a staging row that has been TMDB-unresolved for longer
 * than the threshold fires exactly one Telegram alert naming the film + cinemas;
 * a concluded (resolved or definitive no-match) row never alerts; a row that
 * resolves or disappears before crossing the threshold never alerts and is freed
 * to alert again on a future re-occurrence.
 */
class StagingStuckAlerterSpec extends AnyFlatSpec with Matchers {

  private val Start = Instant.parse("2026-06-14T18:00:00Z")

  /** A clock whose `instant` the test advances between scan passes, so one alerter
   *  instance (carrying its in-memory first-seen map) ages its rows over time. */
  private class MutableClock(var now: Instant) extends Clock {
    override def getZone: ZoneId               = ZoneOffset.UTC
    override def withZone(z: ZoneId): Clock     = this
    override def instant(): Instant            = now
    def advance(d: FiniteDuration): Unit       = now = now.plusMillis(d.toMillis)
  }

  /** A staging seed tuple for a still-unresolved newcomer (default record), or a
   *  resolved/no-match one. The SourceData title is what `findAll` derives the
   *  row's display title from. */
  private def staged(cinema: Source, title: String, year: Option[Int],
                     tmdbId: Option[Int] = None, noMatch: Boolean = false)
  : (Source, String, Option[Int], MovieRecord) =
    (cinema, title, year, MovieRecord(tmdbId = tmdbId, tmdbNoMatch = noMatch,
      data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), releaseYear = year))))

  private def newAlerter(repo: InMemoryStagingRepo, clock: MutableClock)
  : (StagingStuckAlerter, ListBuffer[String]) = {
    val sent = ListBuffer.empty[String]
    (new StagingStuckAlerter(repo, s => { sent += s; () }, stuckThreshold = 1.hour, clock = clock), sent)
  }

  "StagingStuckAlerter" should "alert once after a row stays unresolved past the threshold" in {
    val clock = new MutableClock(Start)
    val repo  = new InMemoryStagingRepo(Seq(staged(Helios, "Brand New Film", Some(2026))))
    val (a, sent) = newAlerter(repo, clock)

    a.runOnce() shouldBe None              // just observed — not stuck yet
    sent shouldBe empty

    clock.advance(59.minutes)
    a.runOnce() shouldBe None              // still under 1h
    sent shouldBe empty

    clock.advance(2.minutes)               // now 61m unresolved
    val msg = a.runOnce()
    msg should not be empty
    sent.size shouldBe 1
    sent.head should include ("Brand New Film")
    sent.head should include ("(2026)")
    sent.head should include ("Helios")

    clock.advance(1.hour)                  // still stuck → no repeat
    a.runOnce() shouldBe None
    sent.size shouldBe 1
  }

  it should "never alert on a concluded row (TMDB hit or definitive no-match)" in {
    val clock = new MutableClock(Start)
    val repo  = new InMemoryStagingRepo(Seq(
      staged(Helios,    "Resolved Film", Some(2026), tmdbId = Some(123)),
      staged(Multikino, "No Match Film", Some(2026), noMatch = true)))
    val (a, sent) = newAlerter(repo, clock)

    a.runOnce()
    clock.advance(3.hours)
    a.runOnce() shouldBe None
    sent shouldBe empty
  }

  it should "not alert a row that resolves before crossing the threshold" in {
    val clock = new MutableClock(Start)
    val repo  = new InMemoryStagingRepo(Seq(staged(Helios, "Quick Film", Some(2026))))
    val (a, sent) = newAlerter(repo, clock)

    a.runOnce()
    clock.advance(30.minutes)
    // Resolves (folds out / gets a tmdbId) before the hour is up.
    repo.upsert(Helios, "Quick Film", Some(2026),
      MovieRecord(tmdbId = Some(999),
        data = Map[Source, SourceData](Helios -> SourceData(title = Some("Quick Film"), releaseYear = Some(2026)))))
    clock.advance(40.minutes)              // 70m elapsed, but now concluded
    a.runOnce() shouldBe None
    sent shouldBe empty
  }

  it should "batch several newly-stuck films into one message, one bullet per film" in {
    val clock = new MutableClock(Start)
    val repo  = new InMemoryStagingRepo(Seq(
      staged(Helios,    "Alpha", Some(2026)),
      staged(Multikino, "Alpha", Some(2026)),   // same film, two cinemas → one bullet
      staged(Helios,    "Bravo", Some(2025))))
    val (a, sent) = newAlerter(repo, clock)

    a.runOnce()
    clock.advance(61.minutes)
    a.runOnce()

    sent.size shouldBe 1
    val msg = sent.head
    msg.linesIterator.count(_.startsWith("•")) shouldBe 2   // Alpha + Bravo, not 3 rows
    msg should include (s"Alpha (2026) — ${Helios.displayName}, ${Multikino.displayName}")
    msg should include (s"Bravo (2025) — ${Helios.displayName}")
  }

  it should "re-arm a film that vanishes then reappears" in {
    val clock = new MutableClock(Start)
    val repo  = new InMemoryStagingRepo(Seq(staged(Helios, "Comeback Film", Some(2026))))
    val (a, sent) = newAlerter(repo, clock)

    a.runOnce()
    clock.advance(61.minutes)
    a.runOnce()
    sent.size shouldBe 1

    // Cinema stops listing it → pruned from staging; tracking clears.
    repo.delete(Helios, "Comeback Film", Some(2026))
    a.runOnce() shouldBe None

    // It comes back as a fresh newcomer; the clock starts over.
    repo.upsert(Helios, "Comeback Film", Some(2026),
      MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some("Comeback Film"), releaseYear = Some(2026)))))
    a.runOnce() shouldBe None              // freshly re-seen → not stuck yet
    sent.size shouldBe 1
    clock.advance(61.minutes)
    a.runOnce() should not be empty        // stuck again → alerts again
    sent.size shouldBe 2
  }
}
