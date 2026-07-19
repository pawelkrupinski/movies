package services.attempts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cadence.{RatingCadenceReader, RatingChangeStats}
import services.freshness.FreshnessKind

import java.time.Instant
import scala.concurrent.ExecutionContext

class FilmAttemptReportSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-19T06:00:00Z")

  private def stats(backoffLevel: Int, checkedAt: Instant = now) =
    RatingChangeStats(unchangedStreak = backoffLevel, windowChecks = 10, windowChanges = 0,
                      windowStartedAt = now, lastCheckedAt = checkedAt, backoffLevel = backoffLevel)

  "FilmAttemptReport" should "join each source's last attempt with its cadence, on the tmdbId key" in {
    val attempts = Map(
      "imdb|tmdb:7" -> EnrichmentAttempt(now, 120, AttemptOutcome.Changed("8.3")),
      "rt|tmdb:7"   -> EnrichmentAttempt(now, 90,  AttemptOutcome.Failed("IOException: connect timed out"))
    )
    val report = FilmAttemptReport.build(Some(7), attempts, Map("rt|tmdb:7" -> stats(3)))

    report.map(_.source) shouldBe FilmAttemptReport.Sources
    report.find(_.source == FreshnessKind.ImdbRating).flatMap(_.attempt).map(_.outcome) shouldBe
      Some(AttemptOutcome.Changed("8.3"))
    report.find(_.source == FreshnessKind.RtRating).map(_.failing)  shouldBe Some(true)
    report.find(_.source == FreshnessKind.McRating).map(_.neverRan) shouldBe Some(true)
  }

  // The distinction the whole section exists for: "never attempted" is a different
  // problem from "attempted and errored", and they used to look identical.
  it should "distinguish a source that never ran from one whose last run failed" in {
    val report = FilmAttemptReport.build(
      Some(7),
      Map("imdb|tmdb:7" -> EnrichmentAttempt(now, 5, AttemptOutcome.Failed("HTTP 503"))),
      Map.empty)

    val imdb = report.find(_.source == FreshnessKind.ImdbRating).get
    imdb.failing  shouldBe true
    imdb.neverRan shouldBe false

    val mc = report.find(_.source == FreshnessKind.McRating).get
    mc.failing  shouldBe false
    mc.neverRan shouldBe true
  }

  it should "report a never-checked source as due now, and a backed-off one as due later" in {
    val report = FilmAttemptReport.build(Some(7), Map.empty, Map("mc|tmdb:7" -> stats(5)))

    report.find(_.source == FreshnessKind.ImdbRating).flatMap(_.nextDueAt) shouldBe None  // due now
    val mcDue = report.find(_.source == FreshnessKind.McRating).flatMap(_.nextDueAt)
    mcDue.map(_.isAfter(now)) shouldBe Some(true)
  }

  // Both stores are tmdbId-keyed, so there is no correct key for an unresolved row —
  // a title-keyed fallback would show a DIFFERENT film's history after a fold.
  it should "report nothing for a film with no tmdbId rather than guess a title key" in {
    FilmAttemptReport.build(None, Map("imdb|tmdb:7" -> EnrichmentAttempt(now, 1, AttemptOutcome.Unchanged)), Map.empty) shouldBe empty
    FilmAttemptReport.keysFor(7) should contain theSameElementsAs
      Seq("imdb|tmdb:7", "fw|tmdb:7", "rt|tmdb:7", "mc|tmdb:7")
  }

  // The row-expand's latency is round-trips, not query time: against a remote Mongo
  // each store read costs ~110ms, so reading them in turn doubles the wait for no
  // reason — neither read's keys depend on the other's result. The gate below makes
  // that structural rather than a timing assertion: each reader parks until the
  // OTHER has entered, so a serial implementation can never get past the first.
  it should "read the attempt log and the cadence history concurrently, not one after the other" in {
    val bothEntered = new java.util.concurrent.CountDownLatch(2)
    def gate(): Unit = {
      bothEntered.countDown()
      if (!bothEntered.await(5, java.util.concurrent.TimeUnit.SECONDS))
        fail("the two store reads ran serially — the second never started while the first was in flight")
    }
    val attemptReader = new EnrichmentAttemptReader {
      override def all() = Seq.empty
      override def forKeys(keys: Seq[String]) = {
        gate(); Map("imdb|tmdb:7" -> EnrichmentAttempt(now, 1, AttemptOutcome.Unchanged))
      }
    }
    val cadenceReader = new RatingCadenceReader {
      override def all() = Seq.empty
      override def forKeys(keys: Seq[String]) = { gate(); Map("mc|tmdb:7" -> stats(5)) }
    }

    val report = FilmAttemptReport.buildFrom(Some(7), attemptReader, cadenceReader)(using ExecutionContext.global)

    // Both reads' results still land on the right sources — concurrency didn't lose a half.
    report.find(_.source == FreshnessKind.ImdbRating).flatMap(_.attempt).map(_.outcome) shouldBe
      Some(AttemptOutcome.Unchanged)
    report.find(_.source == FreshnessKind.McRating).flatMap(_.stats).map(_.backoffLevel) shouldBe Some(5)
  }

  // An unresolved row has no keys to look up, so it must not pay a round-trip at all.
  it should "not touch either store for a film with no tmdbId" in {
    val exploding = new EnrichmentAttemptReader {
      override def all() = Seq.empty
      override def forKeys(keys: Seq[String]) = fail("should not read the attempt log without a tmdbId")
    }
    val explodingCadence = new RatingCadenceReader {
      override def all() = Seq.empty
      override def forKeys(keys: Seq[String]) = fail("should not read the cadence history without a tmdbId")
    }
    FilmAttemptReport.buildFrom(None, exploding, explodingCadence)(using ExecutionContext.global) shouldBe empty
  }
}
