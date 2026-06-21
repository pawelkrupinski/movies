package clients.kino_tatry

import clients.tools.FakeHttpFetch
import models.KinoTatry
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoTatryClient

import java.time.{LocalDate, LocalDateTime}

/** Replays Kino Tatry's own WordPress HOMEPAGE ("START | REPERTUAR KINA",
 *  `kinotatrylodz.pl.html`) through the client.
 *
 *  Kino Tatry was previously scraped from Filmweb, which had gone empty for it;
 *  then from the `/repertuar/` archive — but the venue re-publishes old film
 *  posts when it re-screens them, so the archive's newest entries are NOT the
 *  current slate and every film aged out → an empty scrape. The current slate
 *  lives inline on the homepage, which is what we read. The recorded slate has
 *  three upcoming films, all on 21/06/2026. */
class KinoTatryClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fetchAsOf(today: LocalDate) =
    new KinoTatryClient(new FakeHttpFetch("08-06-2026"), KinoTatry, today = today).fetch()

  "KinoTatryClient" should "read the current slate from the homepage carousel" in {
    val movies = fetchAsOf(LocalDate.of(2026, 6, 8))
    movies.map(_.cinema).toSet shouldBe Set(KinoTatry)
    all(movies.map(_.showtimes)) should not be empty
    movies.map(_.movie.title) shouldBe Seq("NAGI INSTYNKT", "OPĘTANIE", "PIANISTA")
  }

  it should "pin each film's concrete screening parsed from its harmonogram-list" in {
    val movies  = fetchAsOf(LocalDate.of(2026, 6, 8))
    val pianista = movies.find(_.movie.title == "PIANISTA").value
    pianista.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 6, 21, 15, 0))
    pianista.filmUrl.value shouldBe "https://kinotatrylodz.pl/repertuar/pianista/"
    pianista.posterUrl.value should startWith("https://kinotatrylodz.pl/wp-content/uploads/")
    // Year is mined from the detail page's `Premiera</strong>YYYY` so TMDB
    // matches the 2002 film, not a same-titled remake.
    pianista.movie.releaseYear.value shouldBe 2002
  }

  it should "mine each classic's release year from its detail page" in {
    val byTitle = fetchAsOf(LocalDate.of(2026, 6, 8)).map(m => m.movie.title -> m.movie.releaseYear).toMap
    byTitle shouldBe Map(
      "NAGI INSTYNKT" -> Some(1992),
      "OPĘTANIE"      -> Some(1981),
      "PIANISTA"      -> Some(2002))
  }

  it should "drop a film whose only screening is in the past" in {
    // As of 21/06, the three homepage cards are all dated 21/06 → all kept.
    fetchAsOf(LocalDate.of(2026, 6, 21)).map(_.movie.title) shouldBe
      Seq("NAGI INSTYNKT", "OPĘTANIE", "PIANISTA")
    // One day later every card's only date is in the past → nothing remains.
    fetchAsOf(LocalDate.of(2026, 6, 22)) shouldBe empty
  }
}
