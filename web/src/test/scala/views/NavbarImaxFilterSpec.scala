package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Cinema, Movie, MovieRecord, Poznan, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/**
 * The Filtry panel's "Tylko IMAX" checkbox filters on a literal `IMAX` format
 * token. A city with no IMAX screen anywhere in the listing got the checkbox
 * anyway -- a filter that could only blank the page. It renders only when some
 * showtime on ANY day of the listing carries the token, not just today's.
 */
class NavbarImaxFilterSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val today = LocalDate.of(2026, 6, 8)

  /** One film playing `formats.head` today, `formats(1)` tomorrow, … */
  private def schedule(formats: List[String]*): FilmSchedule = FilmSchedule(
    movie          = Movie("Test movie", Some(120)),
    posterUrl      = None,
    synopsis       = None,
    cast           = Seq.empty,
    director       = Seq.empty,
    cinemaFilmUrls = Seq.empty,
    showings       = formats.zipWithIndex.map { case (format, day) =>
      today.plusDays(day) -> Seq(CinemaShowtimes(Cinema.all.head,
        Seq(Showtime(LocalDateTime.of(today.plusDays(day), java.time.LocalTime.of(18, 0)), None, None, format))))
    },
    resolved       = TestReadModel.resolved("Test movie", None, MovieRecord()),
    slug           = controllers.FilmHref.slugOf("Test movie"),
    asOf           = today
  )

  private def render(films: FilmSchedule*): String =
    views.html.repertoire(
      films = films, allCinemas = Nil, cinemaPills = Map.empty,
      devMode = false, minifier = tools.Minify, oauthProviders = Set.empty,
      renderedAt = today.atStartOfDay,
    ).body

  "the IMAX checkbox" should "not render for a city with no IMAX showtime" in {
    render(schedule(List("2D", "NAP"), List("3D"))) should not include "format-imax"
  }

  it should "not render for an empty listing" in {
    render() should not include "format-imax"
  }

  it should "render when any later day carries an IMAX showtime" in {
    render(schedule(List("2D"), List("2D"), List("IMAX", "3D"))) should include ("""id="format-imax"""")
  }
}
