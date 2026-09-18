package clients.cineworld

import clients.tools.FakeHttpFetch
import models.CineworldBarnsley
import org.scalatest.{LoneElement, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.uk.{CineworldClient, CineworldParser}

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays Cineworld Barnsley (`G01HN`) through [[CineworldClient]] entirely
 * from disk — the real responses recorded 2026-09-18, the day after
 * Cineworld's site relaunch onto the Webedia "box office" Gatsby platform
 * (see the client's own doc for the discovery):
 *
 *   - `page-data/sq/d/3836549025.json` — the chain-wide film catalogue
 *     (trimmed to the fields [[services.cinemas.common.GatsbyBoxOfficeParser]]
 *     reads; the live payload also carries a huge per-venue `theaters[]`/
 *     `events[]` block nothing here parses)
 *   - `api/gatsby-source-boxofficeapi/schedule.ec33ebc2.json` — the ONE call
 *     covering the client's whole 730-day horizon
 *   - four `api/gatsby-source-boxofficeapi/movies.*.json` — single-film
 *     detail responses for the `fetchFilmDetail` cases below
 *
 * The listing half of this (catalogue + schedule join, tag → format tokens,
 * expired-session dropping) is [[services.cinemas.common.GatsbyBoxOfficeClient]]'s
 * own contract, already covered by `GatsbyBoxOfficeClientSpec` against
 * Showcase — this spec's listing tests exist to confirm CineworldClient wires
 * that shared client correctly (right theaterId derived from the slug, right
 * base URL, right venue path), not to re-prove the shared parser. The DETAIL
 * tests below are what's actually new to Cineworld: nothing else on this
 * platform has a `fetchFilmDetail`.
 */
class CineworldClientSpec extends AnyFlatSpec with Matchers with OptionValues with LoneElement {

  private val Today   = LocalDate.of(2026, 9, 18)
  private val Barnsley = "g01hn-cineworld-cinema-barnsley"
  private val fake    = new FakeHttpFetch("cineworld")

  private def client(http: tools.HttpFetch = fake) =
    new CineworldClient(http, Barnsley, CineworldBarnsley, today = Today)

  private lazy val films = client().fetch()
  private def film(title: String) = films.find(_.movie.title == title).value

  // ── the listing: CineworldClient composes GatsbyBoxOfficeClient correctly ──

  "fetch" should "derive the platform theaterId from the slug and join the venue's schedule against the catalogue" in {
    films.size shouldBe 61
    films.map(_.cinema).toSet shouldBe Set(CineworldBarnsley)
    films.map(_.movie.title) should contain("How to Train Your Dragon")
  }

  it should "expose the venue's public cinemas page as its source URL" in {
    client().sourceUrl.value shouldBe
      "https://www.cineworld.co.uk/cinemas/g01hn-cineworld-cinema-barnsley/"
  }

  it should "carry the catalogue's poster, film page and genres, with no synopsis/cast/certificate off the listing" in {
    val dragon = film("How to Train Your Dragon")
    dragon.posterUrl.value shouldBe
      "https://all.web.img.acsta.net/img/cb/c1/cbc10fab7a11505fc74ff8546e3d458e.jpg"
    dragon.filmUrl.value shouldBe
      "https://www.cineworld.co.uk/films/313481-how-to-train-your-dragon"
    dragon.movie.genres shouldBe Seq("Adventure", "Fantasy", "Action")
    dragon.externalIds shouldBe Map("boxoffice" -> "313481")
    // The listing/catalogue never carries these on ANY brand this platform
    // serves (see class doc) — Cineworld's detail fetch fills them in later.
    dragon.synopsis shouldBe None
    dragon.cast shouldBe empty
    dragon.director shouldBe empty
    dragon.ageRating shouldBe None
  }

  it should "translate the new premium-format tags this platform's Cineworld deployment adds (4DX, ScreenX, Infinity Vision, 4K)" in {
    // Barnsley's own 4DX+Laser 3D screening.
    film("How to Train Your Dragon").showtimes.loneElement.format shouldBe List("3D", "4DX", "LASER")
    film("Resident Evil").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 19, 17, 40)).value
      .format shouldBe List("2D", "SCREENX", "LASER")
    film("Avengers: Endgame (Re-Release)").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 25, 11, 20)).value
      .format shouldBe List("3D", "4DX", "INFINITY", "LASER")
    film("The Secret World of Arrietty 4K").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 10, 14, 17, 0)).value
      .format shouldBe List("2D", "IMAX", "LASER", "4K", "SUB")
  }

  it should "badge a foreign-language screening's spoken language alongside its subtitle marker" in {
    film("Hanuman Ansh").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 20, 19, 20)).value
      .format shouldBe List("2D", "LASER", "SUB", "HINDI")
  }

  // ── the detail fetch: the ONE thing genuinely new to Cineworld here ───────

  "fetchFilmDetail" should "read synopsis, cast, director, runtime and certificate off the movies endpoint" in {
    val detail = client().fetchFilmDetail("https://www.cineworld.co.uk/films/313481-how-to-train-your-dragon").value
    detail.cast shouldBe Seq(
      "Mason Thames", "Gerard Butler", "Nico Parker", "Nick Frost", "Gabriel Howell",
      "Julian Dennison", "Bronwyn James", "Harry Trevaldwyn", "Ruth Codd", "Peter Serafinowicz")
    detail.director shouldBe Seq("Dean DeBlois")
    detail.runtimeMinutes.value shouldBe 125   // 7500 seconds on the wire
    detail.ageRating.value shouldBe "PG"
    detail.synopsis.value should include("rugged isle of Berk")
  }

  it should "read a different film's certificate off the same endpoint shape" in {
    client().fetchFilmDetail("https://www.cineworld.co.uk/films/34193-donnie-darko").value
      .ageRating.value shouldBe "15"
  }

  it should "leave ageRating/runtime unset for a film the platform hasn't rated or timed yet" in {
    val detail = client()
      .fetchFilmDetail("https://www.cineworld.co.uk/films/1000050581-dune-part-three-the-imax-experience").value
    detail.ageRating shouldBe None
    detail.runtimeMinutes shouldBe None
  }

  it should "return None for an id the platform doesn't recognise, rather than throw" in {
    // The endpoint answers an unknown id with 200 + `[]`, not a 404 — a real
    // recorded response, not a synthesised one (see class doc: there is no
    // durable-vs-transient HTTP signal on this endpoint the way the old
    // detail PAGE had, so this becomes a plain retry-later None/Failed, not Gone).
    client().fetchFilmDetail("https://www.cineworld.co.uk/films/999999999-does-not-exist") shouldBe None
  }

  "movieIdOf" should "read the numeric id off a listing's filmUrl" in {
    CineworldClient.movieIdOf("https://www.cineworld.co.uk/films/313481-how-to-train-your-dragon") shouldBe "313481"
  }

  it should "fall back to SOME id for a ref that doesn't match the expected shape, rather than short-circuit" in {
    // See the doc on `movieIdOf`: a fetch is still attempted (and so a real
    // HTTP failure still surfaces) rather than every unexpected ref silently
    // becoming None before ever reaching the network.
    CineworldClient.movieIdOf("https://example.test/film") shouldBe "film"
  }

  "movieDetailUrl" should "build the endpoint URL with a fixed casting limit" in {
    CineworldClient.movieDetailUrl(CineworldClient.BaseUrl, "313481") shouldBe
      "https://www.cineworld.co.uk/api/gatsby-source-boxofficeapi/movies?basic=false&castingLimit=10&ids=313481"
  }

  // ── the pure detail parser, against the same recorded shape ───────────────

  "CineworldParser.parseMovieDetail" should "drop an unrecognised certificate rather than leak it onto a card" in {
    CineworldParser.parseMovieDetail(
      """[{"id":"1","title":"x","synopsis":null,"casting":[],"direction":[],"coDirection":[],
        |"runtime":null,"certificate":"TBC"}]""".stripMargin
    ).value.ageRating shouldBe None
  }

  it should "return None for an empty array" in {
    CineworldParser.parseMovieDetail("[]") shouldBe None
  }

  it should "return None for an unparseable body rather than throw" in {
    CineworldParser.parseMovieDetail("<!doctype html><html></html>") shouldBe None
  }
}
