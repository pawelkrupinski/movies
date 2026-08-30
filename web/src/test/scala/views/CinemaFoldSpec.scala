package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Cinema, Movie, MovieRecord, Poznan, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/**
 * The row of cinema-link pills under the title folds at ten: a film in a big
 * city plays 60+ venues (London's `one-night-only` runs 63), and that many
 * pills between the title and the synopsis push the rest of the page off the
 * screen. The rest render `.folded` behind a button that unfolds them.
 *
 * The SHOWINGS tree below is deliberately NOT folded — it renders every
 * cinema, and what narrows it is the visitor's own Filtry selection, applied
 * client-side by `applyFilters` in `film.scala.html`. Asserted over a real
 * browser in `PageJsBehaviourSpec`, since it is a runtime decision the server
 * render knows nothing about; what this spec pins server-side is that the
 * markup that decision needs is present — one `data-cinema` per cinema, and
 * the `#showings-empty` block for a selection that leaves nothing.
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

  private def cinemaGroups(html: String): Int = """class="cinema-group"""".r.findAllIn(html).size
  private def buttons(html: String): Int      = """class="cinemas-more"""".r.findAllIn(html).size

  "the showings tree" should "render every cinema on a date, with no fold and no button" in {
    val html = views.html._filmDetailContent(schedule(12)()).body

    cinemaGroups(html) shouldBe 12
    buttons(html)      shouldBe 0
    html should not include "cinema-group folded"
  }

  it should "tag every cinema with the display name the filter matches on" in {
    val expected = Cinema.all.distinct.take(12).map(_.displayName)
    val html     = views.html._filmDetailContent(schedule(12)()).body

    // `applyFilters` keys on `data-cinema`, and `disabledCinemas` stores
    // DISPLAY names — a pill name here would silently match nothing.
    val rendered = """<div class="cinema-group" data-cinema="([^"]+)"""".r
      .findAllMatchIn(html).map(_.group(1)).toList
    rendered shouldBe expected
  }

  it should "carry the empty-state block the filter reveals when nothing is left" in {
    val html = views.html._filmDetailContent(schedule(3)()).body

    html should include ("""<div id="showings-empty" class="showings-empty" style="display:none">""")
    html should include ("Brak repertuaru.")
  }

  private def linkPills(html: String): Int   = """class="cinema-link"""".r.findAllIn(html).size
  private def foldedPills(html: String): Int = """class="cinema-link folded"""".r.findAllIn(html).size

  "the cinema-link row under the title" should "show ten pills and fold the rest behind a button" in {
    val html = views.html._filmDetailContent(schedule(3)(63)).body

    linkPills(html)   shouldBe 10
    foldedPills(html) shouldBe 53
    html should include ("Pokaż pozostałe kina (53)")
    html should include ("function unfoldCinemas(btn)")
  }

  it should "tag every pill with the display name the filter matches on" in {
    val expected = Cinema.all.distinct.take(12).map(_.displayName)
    val html     = views.html._filmDetailContent(schedule(3)(12)).body

    // `applyCinemaLinkFold` keys on `data-cinema`, and `disabledCinemas`
    // stores DISPLAY names — the pill's link text carries a trailing ↗, so
    // reading the label instead would match nothing.
    val rendered = """<a [^>]*data-cinema="([^"]+)"[^>]*class="cinema-link""".r
      .findAllMatchIn(html).map(_.group(1)).toList
    rendered shouldBe expected
  }

  it should "carry the button's copy as a template the filtered count fills in" in {
    val html = views.html._filmDetailContent(schedule(3)(63)).body

    // The rendered count is the no-JS answer; `data-label` is what
    // `applyCinemaLinkFold` rewrites once it knows how many survived.
    html should include ("""data-label="Pokaż pozostałe kina ({0})"""")
  }

  it should "leave a row of exactly ten pills whole, with no button" in {
    val html = views.html._filmDetailContent(schedule(3)(10)).body

    linkPills(html)   shouldBe 10
    foldedPills(html) shouldBe 0
    buttons(html)     shouldBe 0
  }

  it should "fold without touching the showings tree" in {
    // 12 cinemas on the date and 63 link pills. Only the pill row folds — the
    // tree renders whole, and there is exactly ONE button on the page.
    val html = views.html._filmDetailContent(schedule(12)(63)).body

    linkPills(html)    shouldBe 10
    foldedPills(html)  shouldBe 53
    cinemaGroups(html) shouldBe 12
    buttons(html)      shouldBe 1
    html should not include "cinema-group folded"
  }

  "the listing" should "share the same unfolded showings markup" in {
    // `_filmShowings` renders the whole tree for both hosts; a card is capped
    // by line budget in JS (`truncateShowings`, class `.truncated`), which is
    // a channel of its own.
    val html = views.html._filmCards(Seq(schedule(12)())).body

    cinemaGroups(html) shouldBe 12
    buttons(html)      shouldBe 0
    html should not include "folded"
  }
}
