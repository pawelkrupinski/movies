package views

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

// Confirms that Showtime.format (a List[String]) is rendered as a single
// space-separated badge — never with a slash. The previous renderer used
// .mkString("/") which leaked "2D/NAP/ATMOS" into the DOM.
class FormatBadgeSpec extends AnyFlatSpec with Matchers {

  private def schedule(showtimes: Seq[Showtime]): FilmSchedule =
    FilmSchedule(
      movie          = Movie("Test movie", Some(120)),
      posterUrl      = None,
      synopsis       = None,
      cast           = None,
      director       = None,
      cinemaFilmUrls = Nil,
      showings       = Seq(LocalDate.of(2026, 5, 13) -> Seq(CinemaShowtimes(Helios, showtimes)))
    )

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

  it should "emit an empty data-format on showtimes that have no format tokens" in {
    val showtimes = Seq(
      Showtime(baseTime,              Some("https://example.com/a"), Some("Sala 1"), Nil),
      Showtime(baseTime.plusHours(2), Some("https://example.com/b"), Some("Sala 2"), List("2D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    html should include ("""data-format=""""")
    html should include ("""data-format="2D"""")
  }

  // ── Per-screening favourite star + data-screening-id ──────────────────────

  it should "render a minimal fav-star with no per-element handler attributes" in {
    val showtimes = Seq(
      Showtime(baseTime, Some("https://example.com/a"), Some("Sala 1"), List("2D")),
    )
    val html = views.html._filmCards(Seq(schedule(showtimes))).body
    // Per-pill star — minimal markup, no per-element onclick (a single
    // document-level delegated click handler in `_sharedJs` routes
    // `.fav-star` clicks to `toggleFavScreening`).
    html should include ("""<span class="fav-star">★</span>""")
    html should not include "onclick=\"toggleFavScreening"
    // No inline `data-screening-id` either — the JS `badgeScreeningId`
    // helper reconstructs the id (`title|cinema|datetime`) from the
    // badge's ancestor chain at click time. The format stays
    // byte-identical to the server's, so existing localStorage entries
    // continue to round-trip after the markup change.
    html should not include "data-screening-id="
    // `data-time` is the JS indexer's read site for the from-hour
    // filter AND the time-half of the reconstructed screening id.
    html should include ("""data-time="18:00"""")
    // Ancestors carry the other two id components: `.col[data-title]`
    // wraps the showings list and `.cinema-group[data-cinema]` the
    // per-cinema block. The reconstruction reads both via `closest`.
    html should include (s"""data-title="Test movie"""")
    html should include (s"""data-cinema="${Helios.displayName}"""")
  }
}
