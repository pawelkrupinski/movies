package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Helios, Movie, MovieRecord, Showtime, Source, SourceData, Tmdb}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

import models.Poznan

// Confirms that Showtime.format (a List[String]) is rendered as a single
// space-separated badge — never with a slash. The previous renderer used
// .mkString("/") which leaked "2D/NAP/ATMOS" into the DOM.
class FormatBadgeSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private def schedule(showtimes: Seq[Showtime], ageRating: Option[String] = None): FilmSchedule = {
    val record = MovieRecord(
      data = ageRating.fold(Map.empty[Source, SourceData])(r => Map(Tmdb -> SourceData(ageRating = Some(r)))))
    FilmSchedule(
      movie          = Movie("Test movie", Some(120)),
      posterUrl      = None,
      synopsis       = None,
      cast           = Seq.empty,
      director       = Seq.empty,
      cinemaFilmUrls = Nil,
      showings       = Seq(LocalDate.of(2026, 5, 13) -> Seq(CinemaShowtimes(Helios, showtimes))),
      resolved       = TestReadModel.resolved("Test movie", None, record),
      slug           = controllers.FilmHref.slugOf("Test movie")
    )
  }

  private val baseTime = LocalDateTime.of(2026, 5, 13, 18, 0)

  "_filmCards" should "render multi-token format with a space separator (not slash)" in {
    val showtimes = Seq(
      Showtime(baseTime,             Some("https://example.com/a"), Some("Sala 1"), List("2D", "NAP", "ATMOS")),
      // A second showtime with a different format prevents the renderer from
      // dropping every token as "common to all showtimes for this cinema".
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("3D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should     include ("""<span class="badge-fmt">2D NAP ATMOS</span>""")
    html should not include "2D/NAP/ATMOS"
    html should not include "<span class=\"badge-fmt\">2D/"
  }

  it should "render an IMAX 3D badge with a space, not 'IMAX/3D'" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("IMAX"),  List("IMAX", "3D")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 1"), List("2D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should     include ("""<span class="badge-fmt">IMAX 3D</span>""")
    html should not include "IMAX/3D"
  }

  it should "render no badge when every showtime shares the same single token" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("2D")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should not include "badge-fmt"
  }

  // ── a token the whole cinema shares is dropped, version or not ────────────
  //
  // Stripping tokens common to a cinema keeps a pill narrow, and a token on
  // every pill of the group separates no slot from any other — a LANGUAGE
  // version included. The version earns its place on a pill exactly when the
  // cinema screens the film more than one way.

  it should "drop a version token the whole cinema shares, like any other common token" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("2D", "DUB")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D", "DUB")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    // Nothing separates the two slots, so neither carries a badge at all.
    html should not include "badge-fmt"
    // The full list still reaches the format filter through data-format.
    html should include ("""data-format="2D DUB"""")
  }

  it should "drop the country's own voice-over token too when it is shared" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("2D", "LEK")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D", "LEK")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should not include "badge-fmt"
  }

  it should "keep a version that differs between slots on each pill" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("2D", "NAP")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D", "DUB")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should include ("""<span class="badge-fmt">NAP</span>""")
    html should include ("""<span class="badge-fmt">DUB</span>""")
  }

  it should "still drop a screen format the whole cinema shares" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("IMAX", "NAP")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("IMAX", "3D", "NAP")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    // IMAX and NAP are on both slots; only the 3D that separates them is said.
    html should include ("""<span class="badge-fmt">3D</span>""")
    html should include ("""data-format="IMAX NAP"""")
    html should not include ("""<span class="badge-fmt">IMAX NAP</span>""")
  }

  // ── data-format attribute (drives the navbar format filter JS) ────────────

  it should "carry the full unstripped format list on each badge's data-format attribute" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), List("IMAX", "2D", "NAP")),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("3D", "DUB")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should include ("""data-format="IMAX 2D NAP"""")
    html should include ("""data-format="3D DUB"""")
  }

  it should "OMIT data-format on showtimes that have no format tokens, rather than emit it empty" in {
    // Most showtimes carry no format token, so an unconditional attribute is
    // mostly empty ones: 12,200 of them on one London listing, 179 KB of a 17 MB
    // page. The reader cannot tell the difference -- `_repertoireView` reads
    // `(badge.dataset.format || '')` and the navbar filter selects on `.badge-time`,
    // not on `[data-format]` -- so the empty spelling bought nothing. This mirrors
    // `data-room` on the same element, which has always been omitted when absent.
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), Nil),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should not include ("""data-format=""""")
    html should include ("""data-format="2D"""")
  }

  // ── age-rating pill + data-age-rating attribute ───────────────────────────

  private val oneShowtime = Seq(Showtime(baseTime, Some("https://example.com/a"), Some("Sala 1"), List("2D")))

  "the age-rating pill" should "render on the card and expose data-age-rating when the film carries a certificate" in {
    val html = views.html._filmCards(Seq(schedule(oneShowtime, ageRating = Some("15")))).body
    html should include ("""<span class="pill age-rating">15</span>""")
    html should include (""" data-age-rating="15"""")
  }

  it should "render neither the pill nor the attribute when no certificate is present" in {
    val html = views.html._filmCards(Seq(schedule(oneShowtime, ageRating = None))).body
    html should not include "age-rating"
    html should not include "data-age-rating"
  }

}
