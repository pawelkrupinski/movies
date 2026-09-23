package clients.filmweb_showtimes

import models._
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.FilmwebShowtimesClient

import java.time.{LocalDate, LocalDateTime}

/**
 * Kleczew's Kino Zachęta is the one Konin-catchment independent still scraped off Filmweb by
 * internal cinema id — the others moved onto their own bilety24 / biletyna listings. What is
 * fallible per venue is the id (a transcription slip wires the wrong cinema, or one Filmweb
 * lists but serves empty), so this replays the recorded 2026-06-07 seances + title/info capture for that id and pins a real film; that the catalog
 * wires this id is held in `CinemaScraperCatalogSpec`.
 */
class KoninFilmwebShowtimesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http    = new FakeHttpFetch("filmweb-konin")
  private val day     = LocalDate.of(2026, 6, 7)
  private lazy val movies = new FilmwebShowtimesClient(http, 2405, KinoZacheta, daysAhead = 0, today = day).fetch()

  "Kino Zachęta (Filmweb id 2405)" should "resolve to its own non-empty programme with a concrete, real title" in {
    movies.map(_.cinema).toSet shouldBe Set(KinoZacheta)
    all(movies.map(_.filmUrl.value)) should startWith("https://www.filmweb.pl/film/")
    val masters = movies.find(_.movie.title == "Władcy Wszechświata").value
    masters.externalIds("filmweb") shouldBe "176583"
    masters.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 19, 0))
  }
}
