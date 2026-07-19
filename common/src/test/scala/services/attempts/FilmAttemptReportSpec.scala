package services.attempts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cadence.RatingChangeStats
import services.freshness.FreshnessKind

import java.time.Instant

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
}
