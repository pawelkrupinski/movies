package clients.falenica

import clients.tools.FakeHttpFetch
import models.{Showtime, StacjaFalenica}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FalenicaClient

import java.time.LocalDateTime

class FalenicaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new FalenicaClient(new FakeHttpFetch("kino-falenica"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "FalenicaClient.fetch" should "return 20 films and 44 showtimes" in {
    results.size shouldBe 20
    results.flatMap(_.showtimes).size shouldBe 44
  }

  it should "assign Stacja Falenica to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(StacjaFalenica)
  }

  // Regression for the `__trashed` over-filter: the venue trashes the WordPress
  // post but keeps live showtimes. These two films carry `/filmy/__trashed-9/`
  // and `/filmy/__trashed-10/` slugs yet have real "Dostępne terminy"; the old
  // slug filter dropped them entirely.
  it should "include films whose WordPress slug is __trashed but still have showtimes" in {
    val romeria = byTitle("Romeria")
    romeria.movie.runtimeMinutes shouldBe Some(114)
    romeria.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 14, 20, 0), Some("https://ksf.systembiletowy.pl/index.php/repertoire.html?id=33447"), None, Nil)
    byTitle("Znaki Pana Śliwki").showtimes.size shouldBe 3
  }

  it should "read runtime off the listing and showtimes off the detail page" in {
    val m = byTitle("Sny o słoniach")
    m.movie.runtimeMinutes shouldBe Some(98)
    m.showtimes should not be empty
  }

  it should "read the YouTube trailer off the detail page's WordPress video block" in {
    val detail = client.fetchFilmDetail(byTitle("Sny o słoniach").filmUrl.getOrElse(fail("no filmUrl for Sny o słoniach")))
      .getOrElse(fail("no detail for Sny o słoniach"))
    detail.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=_n9ODqV3A5Q")
  }

  // Regression: `div.section.tresc` wraps the synopsis prose AND the
  // "Dostępne terminy" showtime table AND the trailer's <video><a> (a YouTube
  // URL). Taking `.text` of the whole div leaked the showtime tail
  // ("Dostępne terminy … Kup bilet") and the trailer URL into the synopsis.
  it should "extract the synopsis prose without the showtime table or trailer URL" in {
    val detail = client.fetchFilmDetail(byTitle("Przepis na szczęście").filmUrl.getOrElse(fail("no filmUrl for Przepis na szczęście")))
      .getOrElse(fail("no detail for Przepis na szczęście"))
    val synopsis = detail.synopsis.getOrElse(fail("no synopsis for Przepis na szczęście"))
    synopsis should include("Cécile")
    synopsis should not include "Dostępne terminy"
    synopsis should not include "Kup bilet"
    synopsis should not include "youtube.com"
  }
}
