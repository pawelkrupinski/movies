package clients.biletyna

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import services.cinemas.BiletynaClient

import java.time.LocalDateTime

/** One spec for every cinema on the shared biletyna.pl platform. Each row
 *  replays that venue's recorded place page — a single schema.org `Place`
 *  JSON-LD block whose `events` array is the full programme — through
 *  `BiletynaClient` and pins a concrete screening with its exact booking link,
 *  proving the one generic client serves all of them off nothing but a page URL.
 *
 *  Kino Pegaz / WCK (Wodzisław Śląski) was previously scraped from Filmweb,
 *  whose API had silently gone empty for it (every poll returned `[]`); this
 *  fixture is the proof its programme is real and reachable on biletyna. */
class BiletynaClientSpec
    extends AnyFlatSpec
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  // (label, fixtureDir, pageUrl, cinema, pinned title, pinned showtime, exact booking URL)
  private val venues = Table(
    ("label", "dir", "pageUrl", "cinema", "title", "when", "booking"),
    ("ADA Kino Studyjne", "ada-kino-studyjne", "https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne",
      AdaKinoStudyjne: Cinema, "Posłani", LocalDateTime.of(2026, 6, 7, 15, 0),
      "https://biletyna.pl/film/Poslani?eid=665826#opis"),
    ("Kino Kameralne Cafe", "kino-kameralne", "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe",
      KinoKameralne, "Mikey i Nicky (1976)", LocalDateTime.of(2026, 6, 6, 18, 0),
      "https://biletyna.pl/film/Mikey-i-Nicky-1976?eid=667728#opis"),
    ("Kino Pegaz Wodzisław", "kino-pegaz", "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury",
      KinoPegaz, "Piękność dnia", LocalDateTime.of(2026, 6, 9, 20, 0),
      "https://biletyna.pl/film/Pieknosc-dnia?eid=666810#opis")
  )

  forAll(venues) { (label, dir, pageUrl, cinema, title, when, booking) =>
    lazy val movies = new BiletynaClient(new FakeHttpFetch(dir), pageUrl, cinema).fetch()

    it should s"return a non-empty, single-cinema film list — $label" in {
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.showtimes)) should not be empty
    }

    it should s"pin a concrete screening with its booking link — $label" in {
      val film = movies.find(_.movie.title == title).value
      val slot = film.showtimes.find(_.dateTime == when).value
      slot.bookingUrl.value shouldBe booking
    }
  }
}
