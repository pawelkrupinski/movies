package clients.iksoris

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{KinoKulturaBelchatow, KinoRCKDrzewica}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.{IksorisClient, IksorisSite}

import java.time.LocalDateTime

/** Replays two venues' recorded iKsoris `rezerwacja/termin.html?idg=1` pages —
 *  one per page theme the platform ships — through the one client:
 *
 *  - RCK Drzewica (`bilety.rck.drzewica.pl`, captured 2026-09-23), the
 *    "programme" theme. The structured, complete source; the venue's own
 *    news-post schedule is unstructured text and misses two of these four showings.
 *  - MCK Bełchatów's Kino Kultura (`bilety.mckbelchatow.pl`, captured
 *    2026-09-27), the older Bootstrap "table" theme — one row per showing. Its
 *    Filmweb page (the venue's only source before) is thin next to it. */
class IksorisClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val drzewica = new IksorisClient(new FakeHttpFetch("kino-rck-drzewica"),
    IksorisSite("https://bilety.rck.drzewica.pl"), KinoRCKDrzewica).fetch()

  private val belchatow = new IksorisClient(new FakeHttpFetch("kino-kultura-belchatow"),
    IksorisSite("https://bilety.mckbelchatow.pl"), KinoKulturaBelchatow).fetch()

  "IksorisClient on the programme theme (Drzewica)" should "return a non-empty, single-cinema film list" in {
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

  "IksorisClient on the table theme (Bełchatów)" should "read every bookable film off the one-row-per-showing table" in {
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
}
