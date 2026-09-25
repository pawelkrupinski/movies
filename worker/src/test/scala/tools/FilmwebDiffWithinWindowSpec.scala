package tools

import models.{CinemaMovie, KinoMuza, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * `withinWindow` restricts each side's showtimes to the comparison window
 * before the diff keys them by film. Regression for the 2026-09-13
 * FilmwebDiff investigation: a bare `today`-at-midnight lower bound let a
 * showtime that already elapsed earlier TODAY count as "missing" the moment
 * our own live scrape stopped listing it, before Filmweb's listing caught up
 * — a pure artifact of what time of day the diff happens to run, not a real
 * gap. `withinWindow` now applies `Showtime.isUpcoming(now)` — the SAME
 * grace-period rule production uses everywhere else to decide "is this still
 * showing" — to BOTH sides identically.
 */
class FilmwebDiffWithinWindowSpec extends AnyFlatSpec with Matchers {
  private val titles = services.movies.TitleNormalizer.forCountry(models.Country.Poland)


  private val now       = LocalDateTime.of(2026, 9, 13, 23, 33)
  private val windowEnd = now.toLocalDate.plusDays(3)

  private def movieWith(title: String, times: LocalDateTime*): CinemaMovie =
    CinemaMovie(
      movie = Movie(title), cinema = KinoMuza, posterUrl = None, filmUrl = None,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = times.map(t => Showtime(t, None)))

  "withinWindow" should "drop a showtime that elapsed more than the grace period ago" in {
    val elapsed = now.minusMinutes(40) // outside Showtime.Grace (30 min)
    FilmwebDiff.withinWindow(Seq(movieWith("Film", elapsed)), now, windowEnd, titles) shouldBe empty
  }

  it should "keep a showtime that started within the grace period" in {
    val justStarted = now.minusMinutes(10)
    FilmwebDiff.withinWindow(Seq(movieWith("Film", justStarted)), now, windowEnd, titles)("film") shouldBe
      Seq(justStarted)
  }

  it should "keep an upcoming showtime inside the window and drop one past windowEnd" in {
    val soon = now.plusHours(1)
    val late = windowEnd.plusDays(1).atStartOfDay() // the day AFTER windowEnd — excluded
    val result = FilmwebDiff.withinWindow(Seq(movieWith("Film", soon, late)), now, windowEnd, titles)
    result("film") shouldBe Seq(soon)
  }

  // The actual regression: apply the SAME pruning to a side standing in for
  // Filmweb's listing, proving neither side gets special treatment.
  it should "prune an elapsed showtime identically regardless of which side it came from" in {
    val elapsed = now.minusMinutes(45)
    val ours = FilmwebDiff.withinWindow(Seq(movieWith("Film", elapsed)), now, windowEnd, titles)
    val fw   = FilmwebDiff.withinWindow(Seq(movieWith("Film", elapsed)), now, windowEnd, titles)
    ours shouldBe empty
    fw   shouldBe empty
  }
}
