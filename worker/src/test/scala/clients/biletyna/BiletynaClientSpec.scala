package clients.biletyna

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.Json
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

  // (label, fixtureDirectory, pageUrl, cinema, pinned title, pinned showtime, exact booking URL)
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
      "https://biletyna.pl/film/Pieknosc-dnia?eid=666810#opis"),
    // Kinoteatr Rondo publishes the descriptive title form
    // `„Title" | reżyseria: Director | Country Year`; the pinned title is the
    // clean form the client splits out of it (metadata lifted out in the test
    // below). Replays the recorded 08-06-2026 snapshot page.
    ("Kinoteatr Rondo Chełmno", "08-06-2026", "https://biletyna.pl/Chelmno/Kinoteatr-Rondo",
      KinoRondo, "Dyrygent", LocalDateTime.of(2026, 6, 26, 20, 0),
      "https://biletyna.pl/film/Dyrygent-rezyseria-Ondej-Provaznk-Czechy-2025?eid=668818#opis")
  )

  forAll(venues) { (label, directory, pageUrl, cinema, title, when, booking) =>
    lazy val movies = new BiletynaClient(new FakeHttpFetch(directory), pageUrl, cinema).fetch()

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

  // Some venues (Kinoteatr Rondo here) publish the descriptive title form
  // `„Title" | reżyseria: Director | Country Year` in the JSON-LD `name`. The
  // client must split off the clean title and lift the director / countries /
  // year — which are all present in the string, no detail fetch — into their
  // own fields, keeping the raw string as `rawTitle`.
  it should "lift director, countries and year out of the descriptive title — Kinoteatr Rondo" in {
    val movies = new BiletynaClient(
      new FakeHttpFetch("08-06-2026"), "https://biletyna.pl/Chelmno/Kinoteatr-Rondo", KinoRondo
    ).fetch()
    val film = movies.find(_.movie.title == "Dyrygent").value
    film.movie.rawTitle.value shouldBe "„Dyrygent\" | reżyseria: Ondřej Provazník | Czechy 2025"
    film.director shouldBe Seq("Ondřej Provazník")
    film.movie.countries shouldBe Seq("Czechy")
    film.movie.releaseYear.value shouldBe 2025
  }

  // A bare film title with no `reżyseria:` marker is left untouched — no
  // false-positive splitting on a pipe that isn't a metadata separator.
  it should "leave a plain title (no reżyseria marker) untouched — Kino Kameralne" in {
    val movies = new BiletynaClient(
      new FakeHttpFetch("kino-kameralne"), "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe", KinoKameralne
    ).fetch()
    val film = movies.find(_.movie.title == "Mikey i Nicky (1976)").value
    film.movie.rawTitle shouldBe None
    film.director shouldBe empty
  }

  // Small municipal venues (e.g. Pyrzycki Dom Kultury / Kino PDK) sell their own
  // theatre plays, kabaret/stand-up nights, concerts and quiz nights through the
  // same biletyna place page their film repertoire comes from. biletyna stamps a
  // schema.org `@type` on each event — films are `ScreeningEvent`, live events
  // carry `TheaterEvent` / `ComedyEvent` / `MusicEvent` / generic `Event` — so
  // the client drops the live ones on the structured type. The pinned regression
  // is „Być Kobietą" — Czyli Szaleństwa Dojrzałej Młodości, a TheaterEvent that
  // carries NO event vocabulary, so the title-based classifier alone can't catch
  // it. A real children's film is tagged `ChildrensEvent`, so that type is kept.
  private val theaterPlay = "\"Być Kobietą\" - Czyli Szaleństwa Dojrzałej Młodości"

  private def event(eventType: String, name: String, hour: Int): String =
    s"""{"@type":"$eventType","name":${Json.toJson(name)},
       |"startDate":"2026-06-20T$hour:00:00+02:00",
       |"url":"https://biletyna.pl/film/x?eid=$hour#opis"}""".stripMargin

  private def jsonLdPage(events: String): String =
    s"""<html><head><script type="application/ld+json">
       |{"@type":"Place","name":"Kino PDK","events":[$events]}
       |</script></head><body></body></html>""".stripMargin

  it should "drop live stage/music events by schema.org @type, keeping films — Kino PDK" in {
    val movies = BiletynaClient.parse(
      jsonLdPage(Seq(
        event("ScreeningEvent", "28 lat później", 20),
        event("ChildrensEvent", "„Willow i tajemniczy las\" | reżyseria: Mike Marzuk | Niemcy 2025", 12),
        event("TheaterEvent",   theaterPlay, 18),
        event("ComedyEvent",    "Kabaret Trzecia Strona Medalu", 19),
        event("MusicEvent",     "Tenorzy przy świecach", 17),
        event("Event",          "FilmQuiz w PDK", 21)
      ).mkString(",")),
      KinoPDK
    )
    val titles = movies.map(_.movie.title).toSet
    titles should contain("28 lat później")
    titles should contain("Willow i tajemniczy las")  // a real kids' film — ChildrensEvent is kept
    titles should not contain theaterPlay
    titles should not contain "Kabaret Trzecia Strona Medalu"
    titles should not contain "Tenorzy przy świecach"
    titles should not contain "FilmQuiz w PDK"
  }
}
