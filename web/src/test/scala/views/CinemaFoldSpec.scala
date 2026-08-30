package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Cinema, Movie, MovieRecord, Poznan, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/**
 * A film in a big city plays 60+ venues a day (London's `one-night-only` ran
 * 62 on every one of its first five dates), and the /film page listed every
 * one of them under every date — a page you scroll past rather than read.
 * Each date now opens with its first ten cinemas; the rest render `.folded`
 * behind a button that unfolds them.
 *
 * The row of cinema-link pills under the title folds on the same rule and the
 * same button, for the same reason: 63 pills between the title and the synopsis
 * push the rest of the page off the screen.
 *
 * The fold is a DETAIL-page decision: the listing's cards do their own,
 * line-budget fold in JS (`truncateShowings`, class `.truncated`), so
 * `_filmShowings` leaves `collapseCinemasBeyond` at 0 for `_filmCards`.
 */
class CinemaFoldSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val date     = LocalDate.of(2026, 5, 13)
  private val baseTime = LocalDateTime.of(2026, 5, 13, 18, 0)

  /** A schedule whose dates carry `counts.head`, `counts(1)`, … cinemas —
   *  each a distinct real `Cinema` so display names never collide.
   *  `linkCount` drives the separate cinema-link row under the title. */
  private def schedule(counts: Int*)(linkCount: Int = 0): FilmSchedule = {
    val cinemas = Cinema.all.distinct
    FilmSchedule(
      movie          = Movie("Test movie", Some(120)),
      posterUrl      = None,
      synopsis       = None,
      cast           = Seq.empty,
      director       = Seq.empty,
      cinemaFilmUrls = cinemas.take(linkCount).map(c => c -> s"https://example.test/${c.pillName}"),
      showings       = counts.zipWithIndex.map { case (count, day) =>
        date.plusDays(day) -> cinemas.take(count).map(c =>
          CinemaShowtimes(c, Seq(Showtime(baseTime.plusDays(day), None, Some("Sala 1"), List("2D")))))
      },
      resolved       = TestReadModel.resolved("Test movie", None, MovieRecord())
    )
  }

  private def unfolded(html: String): Int = """class="cinema-group"""".r.findAllIn(html).size
  private def folded(html: String): Int   = """class="cinema-group folded"""".r.findAllIn(html).size
  private def buttons(html: String): Int  = """class="cinemas-more"""".r.findAllIn(html).size

  "the film page" should "fold every cinema past the tenth on a date behind one button" in {
    val html = views.html._filmDetailContent(schedule(12)()).body

    unfolded(html) shouldBe 10
    folded(html)   shouldBe 2
    buttons(html)  shouldBe 1
    html should include ("Pokaż pozostałe kina (2)")
    // The button unfolds only its OWN date's cinemas.
    html should include ("""onclick="unfoldCinemas(this)"""")
    html should include ("function unfoldCinemas(btn)")
  }

  it should "keep the fold at the tail — the first ten cinemas stay in source order" in {
    val expected = Cinema.all.distinct.take(12).map(_.displayName)
    val html     = views.html._filmDetailContent(schedule(12)()).body

    val rendered = """data-cinema="([^"]+)"""".r.findAllMatchIn(html).map(_.group(1)).toList
    rendered shouldBe expected

    val foldedNames = """class="cinema-group folded" data-cinema="([^"]+)"""".r
      .findAllMatchIn(html).map(_.group(1)).toList
    foldedNames shouldBe expected.drop(10)
  }

  it should "leave a date at exactly ten cinemas whole, with no button" in {
    val html = views.html._filmDetailContent(schedule(10)()).body

    unfolded(html) shouldBe 10
    folded(html)   shouldBe 0
    buttons(html)  shouldBe 0
  }

  it should "fold each date on its own count" in {
    // Day one runs 13 venues, day two runs 4 — only day one folds, and only
    // its own three extras.
    val html = views.html._filmDetailContent(schedule(13, 4)()).body

    unfolded(html) shouldBe 14
    folded(html)   shouldBe 3
    buttons(html)  shouldBe 1
    html should include ("Pokaż pozostałe kina (3)")
  }

  private def linkPills(html: String): Int   = """class="cinema-link"""".r.findAllIn(html).size
  private def foldedPills(html: String): Int = """class="cinema-link folded"""".r.findAllIn(html).size

  "the cinema-link row under the title" should "show ten pills and fold the rest behind a button" in {
    val html = views.html._filmDetailContent(schedule(3)(63)).body

    linkPills(html)   shouldBe 10
    foldedPills(html) shouldBe 53
    html should include ("Pokaż pozostałe kina (53)")
    // Its button and the showings tree's share one handler, hosted here.
    html should include ("function unfoldCinemas(btn)")
    html should include ("btn.parentElement.querySelectorAll('.folded')")
  }

  it should "leave a row of exactly ten pills whole, with no button" in {
    val html = views.html._filmDetailContent(schedule(3)(10)).body

    linkPills(html)   shouldBe 10
    foldedPills(html) shouldBe 0
    buttons(html)     shouldBe 0
  }

  it should "fold on its own count, not the showings tree's" in {
    // 12 cinemas on the date but 63 link pills. The two folds are separate
    // decisions off separate counts: the tree hides 2, the pill row hides 53,
    // and each gets its own button.
    val html = views.html._filmDetailContent(schedule(12)(63)).body

    unfolded(html)    shouldBe 10
    folded(html)      shouldBe 2
    linkPills(html)   shouldBe 10
    foldedPills(html) shouldBe 53
    buttons(html)     shouldBe 2
  }

  "the listing" should "fold nothing — its cards cap themselves by line budget in JS" in {
    val html = views.html._filmCards(Seq(schedule(12)())).body

    unfolded(html) shouldBe 12
    folded(html)   shouldBe 0
    buttons(html)  shouldBe 0
    html should not include "unfoldCinemas"
  }
}
