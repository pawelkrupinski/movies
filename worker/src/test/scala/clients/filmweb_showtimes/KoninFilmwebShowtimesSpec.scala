package clients.filmweb_showtimes

import models._
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.FilmwebShowtimesClient

import java.time.{LocalDate, LocalDateTime}

/**
 * Each Konin-catchment independent we added is just a new `FilmwebShowtimesClient`
 * instance pointed at a Filmweb internal cinema id (the client itself is parsing-
 * tested in [[FilmwebShowtimesClientSpec]]). What's fallible per venue is the id:
 * a transcription slip wires the wrong cinema, or a venue Filmweb lists but serves
 * empty (like Poznań's Kino Apollo, suppressed in `FilmwebCinemaIdResolver`).
 *
 * So this replays a recorded seances + title/info capture for every Filmweb id
 * wired in `CinemaScraperCatalog.koninScrapers` and asserts each yields real,
 * this-venue films — fixtures recorded 2026-06 from `/api/v1/cinema/<id>/seances`.
 * The capture date is per-venue: most published on 2026-06-07, Koło's Kino nad
 * Wartą only screens midweek (2026-06-11 in-window). Helios (chain client) and
 * Oskard (Bilety24) are covered elsewhere; Września's Kino Trójka is excluded.
 */
class KoninFilmwebShowtimesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("filmweb-konin")

  // (Filmweb cinema id, modelled cinema, capture date) — mirrors the Filmweb
  // entries in koninScrapers.
  private val venues: Seq[(Int, Cinema, LocalDate)] = Seq(
    (2405, KinoZacheta,  LocalDate.of(2026, 6, 7)),  // Kleczew
    (1526, KinoNadWarta, LocalDate.of(2026, 6, 11)), // Koło — midweek only
    (2417, KinoHel,      LocalDate.of(2026, 6, 7)),  // Pleszew
    (1694, KinoSokolnia, LocalDate.of(2026, 6, 7)),  // Słupca
    (1523, KinoTur,      LocalDate.of(2026, 6, 7)),  // Turek
    (1851, KinoMok,      LocalDate.of(2026, 6, 7)),  // Zagórów
  )

  venues.foreach { case (id, cinema, date) =>
    s"${cinema.displayName} (Filmweb id $id)" should "resolve to its own non-empty programme" in {
      val client = new FilmwebShowtimesClient(http, id, cinema, daysAhead = 0, today = date)
      val movies = client.fetch()

      withClue(s"${cinema.displayName} returned no films for $date — wrong id or empty venue: ") {
        movies should not be empty
      }
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.movie.title.trim)) should not be empty
      all(movies.map(_.filmUrl.value)) should startWith("https://www.filmweb.pl/film/")
      all(movies.map(_.externalIds.keySet)) should contain("filmweb")
      movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet shouldBe Set(date)
    }
  }

  "Kino Hel (Pleszew)" should "carry a concrete, real title from the capture" in {
    val client = new FilmwebShowtimesClient(http, 2417, KinoHel, daysAhead = 0, today = LocalDate.of(2026, 6, 7))
    val movies = client.fetch()

    // Filmweb id 10046150 → "Gwiezdne wojny: Mandalorian i Grogu", screened 15:30.
    val mando = movies.find(_.movie.title == "Gwiezdne wojny: Mandalorian i Grogu").value
    mando.externalIds("filmweb") shouldBe "10046150"
    mando.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 15, 30))
  }
}
