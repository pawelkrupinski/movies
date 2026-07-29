package tools

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{ArchivedScrape, SuccessfulScrape}

import java.time.{Instant, LocalDateTime}

/**
 * The corpus fixture is only worth having if what comes back is what went in —
 * a lossy capture would quietly change the corpus a convergence run replays, and
 * the suite asserts byte-identical records across passes.
 */
class CorpusFixtureSpec extends AnyFlatSpec with Matchers {

  private val cinema = Cinema.all.head

  private def film(title: String, year: Option[Int], poster: Option[String]) = CinemaMovie(
    movie     = Movie(title, Some(104), year, Seq("Polska"), Seq("Dramat"), Some("Original"), None),
    cinema    = cinema,
    posterUrl = poster,
    filmUrl   = Some("https://cinema.test/film"),
    synopsis  = Some("A blurb."),
    cast      = Seq("A. Actor"),
    director  = Seq("D. Director"),
    showtimes = Seq(Showtime(LocalDateTime.parse("2026-08-01T18:30"), bookingUrl = Some("https://book.test/1"),
                             room = Some("5"), format = List("2D", "NAP"))),
    externalIds = Map("imdb" -> "tt123"),
    trailerUrl  = Some("https://youtube.test/x"),
    ageRating   = Some("15")
  )

  private val rows = Seq(ArchivedScrape(
    cinema      = cinema,
    city        = Some("poznan"),
    lastSuccess = Some(SuccessfulScrape(Instant.parse("2026-07-28T06:00:00Z"), listingComplete = true,
                       films = Seq(film("Cicha noc", Some(2017), Some("https://p.test/a.jpg")),
                                   film("Zimna wojna", None, None)))),
    lastBarren  = None))

  "a captured corpus" should "round-trip every field through the compressed fixture" in {
    val parsed = CorpusFixture.parse(CorpusFixture.render(rows))

    parsed should have size 1
    parsed.head.cinema shouldBe cinema
    parsed.head.city shouldBe Some("poznan")
    parsed.head.lastSuccess.get.at shouldBe Instant.parse("2026-07-28T06:00:00Z")
    parsed.head.films.map(_.movie.title) shouldBe Seq("Cicha noc", "Zimna wojna")
    // The fields the pipeline actually keys and merges on must survive verbatim.
    parsed.head.films shouldBe rows.head.films
  }

  it should "sort by cinema id, so the file is a pure function of the corpus" in {
    val a = rows.head
    val b = a.copy(cinema = Cinema.all(1))
    CorpusFixture.capture(Seq(b, a)).map(_._id) shouldBe
      CorpusFixture.capture(Seq(a, b)).map(_._id)
  }
}
