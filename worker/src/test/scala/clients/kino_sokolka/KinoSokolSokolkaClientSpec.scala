package clients.kino_sokolka

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoSokolSokolka
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoSokolSokolkaClient

import java.time.{LocalDateTime, LocalTime}

/** Replays the recorded `kinosokolka.pl/repertuar/` MEC (Modern Events
 *  Calendar) grid plus its per-film detail pages for Kino Sokół (Sokółka)
 *  through the client.
 *
 *  The grid carries dates but no times; the times are read off each film's
 *  detail page. Previously scraped from biletyna.pl, which no longer carries
 *  the venue's film repertoire. */
class KinoSokolSokolkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoSokolSokolkaClient(new FakeHttpFetch("kino-sokolka")).fetch()

  "KinoSokolSokolkaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSokolSokolka)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "carry real (non-midnight) start times read off the detail pages" in {
    val everyTime = movies.flatMap(_.showtimes.map(_.dateTime.toLocalTime))
    everyTime should not contain LocalTime.MIDNIGHT
  }

  it should "pin a concrete screening at its real grid date and detail-page time" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("sprawiedliwość owiec")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 23, 18, 0))
  }

  it should "surface the screening version labels as format tokens" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("kurozając")).value
    val showing = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 23, 16, 0)).value
    showing.format should contain allOf ("2D", "DUB")
  }
}
