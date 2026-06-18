package clients.kino_tatry

import clients.tools.FakeHttpFetch
import models.KinoTatry
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoTatryClient

import java.time.{LocalDate, LocalDateTime}

/** Replays Kino Tatry's own WordPress repertoire (the `/repertuar/` archive page
 *  + its `/repertuar/<slug>/` detail pages, recorded into the 08-06-2026 corpus)
 *  through the client.
 *
 *  Kino Tatry was previously scraped from Filmweb, which had gone empty for it;
 *  these fixtures prove its (sparse) programme is real and reachable on its own
 *  site. The recorded slate has exactly one upcoming film (OMEN, 19/06) — the
 *  other nine archive entries carry no future date and must be dropped. */
class KinoTatryClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new KinoTatryClient(new FakeHttpFetch("08-06-2026"), KinoTatry, today = LocalDate.of(2026, 6, 8)).fetch()

  "KinoTatryClient" should "return only the films with an upcoming screening" in {
    movies.map(_.cinema).toSet shouldBe Set(KinoTatry)
    all(movies.map(_.showtimes)) should not be empty
    // 9 of the 10 archive entries have no future date → dropped; OMEN remains.
    movies.map(_.movie.title) shouldBe Seq("OMEN")
  }

  it should "pin OMEN's concrete screening parsed from its harmonogram-list" in {
    val omen = movies.find(_.movie.title == "OMEN").value
    omen.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 19, 20, 0))
    omen.movie.releaseYear.value shouldBe 1976
    omen.filmUrl.value shouldBe "https://kinotatrylodz.pl/repertuar/omen/"
  }

  it should "drop past-dated archive films" in {
    // "frantic", "tootsie", etc. are in the listing but carry no future date.
    movies.exists(_.movie.title.toLowerCase.contains("frantic")) shouldBe false
  }
}
