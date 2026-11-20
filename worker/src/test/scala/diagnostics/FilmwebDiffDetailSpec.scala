package diagnostics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** The text report's per-cinema DETAIL: each film both sides list (`=` identical, `~` with each side's extra slots),
 *  then the films only ours list, then the films only Filmweb lists — and how the weekly run paces Filmweb. */
class FilmwebDiffDetailSpec extends AnyFlatSpec with Matchers {

  private def at(day: Int, hour: Int, minute: Int = 0) = LocalDateTime.of(2026, 11, day, hour, minute)

  "perFilmDetail" should "list matched films first, each side's extra slots under a differing one, then one-sided films" in {
    val ours = Map(
      "lalka"   -> Seq(at(20, 18), at(20, 21, 5)),
      "same"    -> Seq(at(21, 12)),
      "ours"    -> Seq(at(20, 10), at(21, 10)),
      "doubled" -> Seq(at(22, 9), at(22, 9)))
    val fw = Map(
      "lalka"   -> Seq(at(20, 18), at(20, 20, 30)),
      "same"    -> Seq(at(21, 12)),
      "fw"      -> Seq(at(23, 16)),
      "doubled" -> Seq(at(22, 9)))
    FilmwebDiff.perFilmDetail(ours, fw) shouldBe
      """  ~ doubled  ours=2 fw=1
        |      ours-only: 11-22 09:00
        |  ~ lalka  ours=2 fw=2
        |      ours-only: 11-20 21:05
        |      fw-only:   11-20 20:30
        |  = same  (1 slots, identical)
        |  - [ours only] ours  (2 slots)
        |  + [fw only]   fw  (1 slots)""".stripMargin
  }

  it should "say when neither side has a screening in the window" in {
    FilmwebDiff.perFilmDetail(Map.empty, Map.empty) shouldBe "  (no screenings in window on either side)"
  }

  "filmwebPauseMillis" should "not wait before the first cinema, and wait longer once 3 Filmweb fetches in a row failed" in {
    FilmwebDiff.filmwebPauseMillis(0, 5) shouldBe 0L
    FilmwebDiff.filmwebPauseMillis(1, 2) shouldBe 1500L
    FilmwebDiff.filmwebPauseMillis(4, 3) shouldBe 8000L
  }
}
