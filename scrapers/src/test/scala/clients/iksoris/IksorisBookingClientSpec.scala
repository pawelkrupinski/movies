package clients.iksoris

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{KinoBCK, KinoKulturaBelchatow, KinoRCKDrzewica}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.{IksorisBookingClient, IksorisBookingPage, IksorisOrigin}

import java.time.LocalDateTime

/** Replays three venues' recorded iKsoris `rezerwacja/termin.html?idg=1` pages —
 *  one per page theme the platform ships — through the one client:
 *
 *  - RCK Drzewica (`bilety.rck.drzewica.pl`, captured 2026-09-23), the
 *    "programme" theme. The structured, complete source; the venue's own
 *    news-post schedule is unstructured text and misses two of these four showings.
 *  - MCK Bełchatów's Kino Kultura (`bilety.mckbelchatow.pl`, captured
 *    2026-09-27), the older Bootstrap "table" theme — one row per showing. Its
 *    Filmweb page (the venue's only source before) is thin next to it.
 *  - Biłgorajskie Centrum Kultury's Kino BCK (`bilet.bck.lbl.pl`, captured
 *    2026-09-27), the "terms list" theme — day headers over one card per
 *    showing. Its Filmweb page listed 2 films / 14 showings to 1 October; this
 *    page lists 4 films / 34 showings to 22 October. */
class IksorisBookingClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val drzewica = new IksorisBookingClient(new FakeHttpFetch("kino-rck-drzewica"),
    IksorisBookingPage(IksorisOrigin("https://bilety.rck.drzewica.pl")), KinoRCKDrzewica).fetch()

  private val belchatow = new IksorisBookingClient(new FakeHttpFetch("kino-kultura-belchatow"),
    IksorisBookingPage(IksorisOrigin("https://bilety.mckbelchatow.pl")), KinoKulturaBelchatow).fetch()

  private val bck = new IksorisBookingClient(new FakeHttpFetch("kino-bck-bilgoraj"),
    IksorisBookingPage(IksorisOrigin("https://bilet.bck.lbl.pl")), KinoBCK).fetch()

  "IksorisBookingClient on the programme theme (Drzewica)" should "return a non-empty, single-cinema film list" in {
    drzewica should not be empty
    drzewica.map(_.cinema).toSet shouldBe Set(KinoRCKDrzewica)
    all(drzewica.map(_.showtimes)) should not be empty
  }

  it should "merge a film's showtimes across the two days it screened" in {
    val film = drzewica.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 26, 15, 0), LocalDateTime.of(2026, 9, 27, 15, 0)
    )
  }

  it should "read the runtime and countries off the show-description text" in {
    val film = drzewica.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.movie.runtimeMinutes.value shouldBe 89
    film.movie.countries should contain allOf ("Kanada", "USA")
  }

  it should "recognise the dub/subtitle badge among the header labels" in {
    drzewica.find(_.movie.title == "Psi Patrol i Dinozaury").value.showtimes.head.format shouldBe List("DUB")
    drzewica.find(_.movie.title == "Niebo nad Normandią").value.showtimes.head.format shouldBe List("NAP")
  }

  it should "carry the iKsoris booking link" in {
    val film = drzewica.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.showtimes.flatMap(_.bookingUrl).head should include("bilety.rck.drzewica.pl/rezerwacja/numerowane.html")
  }

  it should "list every film screening in the verified window, including the single Wednesday-morning showing" in {
    drzewica.map(_.movie.title) should contain allOf ("Mistyczka", "VAIANA")
    val vaiana = drzewica.find(_.movie.title == "VAIANA").value
    vaiana.showtimes.map(_.dateTime) should contain (LocalDateTime.of(2026, 9, 30, 9, 0))
  }

  "IksorisBookingClient on the table theme (Bełchatów)" should "read every bookable film off the one-row-per-showing table" in {
    belchatow.map(_.cinema).toSet shouldBe Set(KinoKulturaBelchatow)
    belchatow.map(_.movie.title) should contain theSameElementsAs Seq(
      "André Rieu. Niech żyje Maastricht!", "Dzień dziecka księdza Jana Kaczkowskiego", "Kręciołek",
      "Lalka", "Nowa fala", "Totalna magia 2")
    belchatow.map(_.showtimes.size).sum shouldBe 18
  }

  it should "merge a film's showings across days, each at its own date-time and booking link" in {
    val lalka = belchatow.find(_.movie.title == "Lalka").value
    lalka.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 30, 16, 10), LocalDateTime.of(2026, 9, 30, 19, 20),
      LocalDateTime.of(2026, 10, 1, 16, 10), LocalDateTime.of(2026, 10, 1, 19, 20))
    lalka.showtimes.head.bookingUrl.value shouldBe
      "https://bilety.mckbelchatow.pl/rezerwacja/numerowane.html?ter_id=121321&ter_idt=db84e4910858c66a25b7b27e2d236647"
    lalka.showtimes.flatMap(_.bookingUrl).distinct should have size 4
  }

  it should "carry the row's poster thumbnail and no outbound distributor link as the film page" in {
    val magia = belchatow.find(_.movie.title == "Totalna magia 2").value
    magia.posterUrl.value shouldBe "https://bilety.mckbelchatow.pl/images/wydarzenia/mini/totalnamagia2.jpg"
    magia.filmUrl shouldBe None
  }

  "IksorisBookingClient on the terms-list theme (Biłgoraj)" should "read every bookable film and showing off the day-grouped cards" in {
    bck.map(_.cinema).toSet shouldBe Set(KinoBCK)
    bck.map(_.movie.title) should contain theSameElementsAs Seq(
      "André Rieu. „Niech żyje Maastricht!”", "Buntownik", "Lalka", "Mistyczka")
    bck.map(_.showtimes.size).sum shouldBe 34
    bck.find(_.movie.title == "Lalka").value.showtimes should have size 19
  }

  it should "date each showing by its day header and link its own booking page" in {
    val mistyczka = bck.find(_.movie.title == "Mistyczka").value
    mistyczka.showtimes.map(_.dateTime).take(3) shouldBe Seq(
      LocalDateTime.of(2026, 9, 27, 15, 0), LocalDateTime.of(2026, 9, 27, 17, 0), LocalDateTime.of(2026, 9, 28, 15, 0))
    mistyczka.showtimes.head.bookingUrl.value shouldBe
      "https://bilet.bck.lbl.pl/miejsca.html?id=12683&idt=ac4eccf0bf3f94b04313601166709986&idg=1"
    mistyczka.showtimes.flatMap(_.bookingUrl).distinct should have size mistyczka.showtimes.size
  }

  it should "read countries, runtime and genres off the description, skipping the age rating" in {
    val buntownik = bck.find(_.movie.title == "Buntownik").value.movie
    buntownik.countries shouldBe Seq("Wielka Brytania", "USA")
    buntownik.runtimeMinutes.value shouldBe 96
    buntownik.genres shouldBe Seq("Thriller", "Akcja")
    bck.find(_.movie.title == "Lalka").value.movie.genres shouldBe Seq("Dramat", "Romans")
  }

  it should "take the production year from the title's Filmweb link slug, and none from a non-Filmweb link" in {
    bck.find(_.movie.title == "Lalka").value.movie.releaseYear.value shouldBe 2026
    bck.find(_.movie.title == "Buntownik").value.movie.releaseYear.value shouldBe 2026
    val rieu = bck.find(_.movie.title.startsWith("André Rieu")).value
    rieu.movie.releaseYear shouldBe None
    rieu.movie.countries shouldBe empty
    rieu.movie.genres shouldBe empty
    rieu.movie.runtimeMinutes.value shouldBe 170
  }

  it should "recognise the subtitle badge among the tags, and leave the outbound title link off filmUrl" in {
    val buntownik = bck.find(_.movie.title == "Buntownik").value
    buntownik.showtimes.head.format should contain ("NAP")
    buntownik.filmUrl shouldBe None
  }
}
