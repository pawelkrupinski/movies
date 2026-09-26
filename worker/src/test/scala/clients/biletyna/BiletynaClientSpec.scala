package clients.biletyna

import models._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.OptionValues
import play.api.libs.json.Json
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.BiletynaClient

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
      "https://biletyna.pl/film/Dyrygent-rezyseria-Ondej-Provaznk-Czechy-2025?eid=668818#opis"),
    // Strzegom's programme page (sck.strzegom.pl/bilety/) is itself just a
    // "KUP BILET" button embedding a biletyna.pl widget; the venue's real
    // programme lives only on its biletyna place page.
    ("Kino SCK Strzegom", "kino-sck-strzegom", "https://biletyna.pl/Strzegom/Kino-SCK",
      KinoSCKStrzegom, "Posłani", LocalDateTime.of(2026, 9, 27, 17, 0),
      "https://biletyna.pl/film/Poslani?eid=702533#opis"),
    // Chrzanów's own site (mckis.chrzanow.pl/repertuar-2/) is likewise just a
    // biletyna.pl iframe embed with no static programme of its own.
    ("Kino Sztuka Chrzanów", "kino-sztuka-chrzanow",
      "https://biletyna.pl/Chrzanow/Miejski-Osrodek-Kultury-Sportu-i-Rekreacji",
      KinoSztuka, "100 dni: Misja Zeus", LocalDateTime.of(2026, 9, 23, 16, 0),
      "https://biletyna.pl/film/100-dni-Misja-Zeus?eid=693269#opis"),
    // Gryfiński Dom Kultury (Kino Gryf) — 2026-09-23 nearby-towns sweep.
    ("Gryfiński Dom Kultury", "kino-gryfinski-dom-kultury",
      "https://biletyna.pl/Gryfino/Sala-widowiskowa-Gryfinskiego-Domu-Kultury",
      KinoGryfinskiDomKultury, "OBCY", LocalDateTime.of(2026, 9, 26, 18, 30),
      "https://biletyna.pl/film/OBCY?eid=706192#opis"),
    // Tucholski Ośrodek Kultury (Kino Sokół) — same sweep.
    ("Tucholski Ośrodek Kultury", "kino-tucholski-osrodek-kultury",
      "https://biletyna.pl/Tuchola/Tucholski-Osrodek-Kultury",
      KinoTucholskiOsrodekKultury, "100 dni: Misja Zeus", LocalDateTime.of(2026, 9, 25, 18, 15),
      "https://biletyna.pl/film/100-dni-Misja-Zeus?eid=696509#opis")
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

  // "Event cinema" broadcasts (an André Rieu / NT Live / opera retransmisja
  // shown ON the screen) are legitimate cinema content the app keeps — but
  // biletyna tags them MusicEvent/TheaterEvent exactly like a live concert. The
  // @type filter must honour the same broadcast veto the title classifier uses,
  // or it regresses these screened transmissions. (Real case: Kino Świt's
  // "André Rieu … Retransmisja letniego koncertu z Maastricht", a MusicEvent.)
  it should "keep screened broadcasts even when @type is a non-film type — Kino PDK" in {
    val movies = BiletynaClient.parse(
      jsonLdPage(Seq(
        event("MusicEvent",  "André Rieu. Niech żyje Maastricht! Retransmisja letniego koncertu", 19),
        event("TheaterEvent","National Theatre Live: Hamlet", 20),
        event("MusicEvent",  "Edyta Geppert - recital", 18)  // a genuine live concert — still dropped
      ).mkString(",")),
      KinoPDK
    )
    val titles = movies.map(_.movie.title).toSet
    titles should contain("André Rieu. Niech żyje Maastricht! Retransmisja letniego koncertu")
    titles should contain("National Theatre Live: Hamlet")
    titles should not contain "Edyta Geppert - recital"
  }

  // Chrzanów's real captured page mixes 48 film screenings with a MusicEvent
  // concert and two TheaterEvent plays on the same programme — the live
  // regression for the @type filter proven synthetically above.
  it should "drop the real concert and plays off Chrzanów's programme, keeping every film" in {
    val movies = new BiletynaClient(
      new FakeHttpFetch("kino-sztuka-chrzanow"),
      "https://biletyna.pl/Chrzanow/Miejski-Osrodek-Kultury-Sportu-i-Rekreacji",
      KinoSztuka
    ).fetch()
    val titles = movies.map(_.movie.title).toSet
    titles should contain("Terminator 2. Dzień sądu 35. rocznica")
    titles should not contain "Gdy kino zaczyna śpiewać - koncert polskiej muzyki filmowej"
    titles should not contain "O mało co... - Anna Mucha i Michał Sitarski w kultowej komedii"
    titles should not contain "Klimakterium 2 czyli Menopauzy Szał"
  }

  // Tucholski Ośrodek Kultury's real captured page mixes its film programme with
  // a MusicEvent concert on the same feed — the live regression for the @type
  // filter, off a real recorded page rather than the synthetic Kino PDK fixture.
  it should "drop the real concert off Tucholski Ośrodek Kultury's programme, keeping every film" in {
    val movies = new BiletynaClient(
      new FakeHttpFetch("kino-tucholski-osrodek-kultury"),
      "https://biletyna.pl/Tuchola/Tucholski-Osrodek-Kultury",
      KinoTucholskiOsrodekKultury
    ).fetch()
    val titles = movies.map(_.movie.title).toSet
    titles should contain("Marsupilami")
    titles should not contain "Cztery Pory Miłowania"
  }

  // ── Past the place page's 50-event cap ────────────────────────────────────
  // The place page lists a venue's first 50 events only. Kino Len (Żyrardów)
  // sells 58; the page stops at 2026-10-20, and the rest come from the
  // `/ajax/events?params[h]=5126` feed. Page and feed captured live 2026-09-26.
  private lazy val len =
    new BiletynaClient(new FakeHttpFetch("biletyna-filmweb-desynced"), "https://biletyna.pl/Zyrardow/Kino-Len", KinoLen).fetch()

  "BiletynaClient (full page)" should "read the screenings past the page's 50-event cap from the hall's event feed" in {
    val lalka = len.find(_.movie.title == "Lalka (2026)").value
    val slot  = lalka.showtimes.find(_.dateTime == LocalDateTime.of(2026, 10, 22, 17, 0)).value
    slot.bookingUrl.value shouldBe "https://biletyna.pl/film/Lalka-2026?eid=701564#opis"
    len.flatMap(_.showtimes).size shouldBe 58   // every event once: page and feed merged on the booking link
  }

  it should "page the feed until a page comes back short — Kino Kameralne's 132 events over two" in {
    val movies = new BiletynaClient(new FakeHttpFetch("kino-kameralne"), "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe",
      KinoKameralne).fetch()
    movies.flatMap(_.showtimes).map(_.dateTime).max should be > LocalDateTime.of(2026, 11, 1, 0, 0)
  }

  it should "not ask the feed for a venue whose page isn't full" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    val http = new tools.GetOnlyHttpFetch {
      private val fixtures = new FakeHttpFetch("biletyna-filmweb-desynced")
      def get(url: String): String = { asked += url; fixtures.get(url) }
    }
    new BiletynaClient(http, "https://biletyna.pl/Dzialdowo/Miejski-Dom-Kultury", KinoApolloDzialdowo).fetch() should not be empty
    asked.toSeq shouldBe Seq("https://biletyna.pl/Dzialdowo/Miejski-Dom-Kultury")
  }

  it should "fail loudly when a full page names no hall to page the rest from" in {
    val page = new FakeHttpFetch("biletyna-filmweb-desynced").get("https://biletyna.pl/Zyrardow/Kino-Len")
      .replaceAll("""get_filter\s*=\s*\{.*?\};""", "")
    val http = new tools.GetOnlyHttpFetch { def get(url: String): String = page }
    an[IllegalStateException] should be thrownBy
      new BiletynaClient(http, "https://biletyna.pl/Zyrardow/Kino-Len", KinoLen).fetch()
  }
}
