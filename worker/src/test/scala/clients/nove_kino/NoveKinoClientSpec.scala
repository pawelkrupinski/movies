package clients.nove_kino

import clients.tools.FakeHttpFetch
import models.{KinoAtlantic, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, NoveKinoClient}

import java.time.LocalDateTime

class NoveKinoClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new NoveKinoClient(new FakeHttpFetch("kino-atlantic"), "atlantic", KinoAtlantic)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "NoveKinoClient.fetch" should "walk the per-day pages and return 29 films / 142 showtimes" in {
    results.size shouldBe 29
    results.flatMap(_.showtimes).size shouldBe 142
  }

  it should "assign Kino Atlantic to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoAtlantic)
  }

  it should "strip the presentation suffix into the showtime format and merge by clean title" in {
    results.map(_.movie.title) should contain ("Diabeł ubiera się u Prady 2")
    results.map(_.movie.title).foreach(_ should not include "- napisy")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 17
    byTitle("Zawodowcy").showtimes.size shouldBe 16
    byTitle("Diabeł ubiera się u Prady 2").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 17, 30),
        Some("https://atlantic.novekino.pl/msi/default.aspx?event_id=45780&typetran=0&ReturnLink=https://www.novekino.pl/kina/atlantic/dziekujemy.php"),
        None, List("NAP")
      )
  }

  it should "read the cast list and the YouTube trailer off the film detail page" in {
    val d = detailFor("Diabeł ubiera się u Prady 2")
    d.cast       shouldBe Seq("Meryl Streep", "Emily Blunt", "Anne Hathaway", "Stanley Tucci")
    d.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=CdoTYdt4GQE")
  }

  it should "keep every paragraph of the synopsis, not just the first" in {
    // `section.text_panel` wraps an "Opis filmu" header `<div>` followed by the
    // prose paragraphs; the old `selectFirst("section.text_panel p")` kept only
    // the FIRST `<p>`, dropping the rest of a multi-paragraph plot.
    val synopsis = detailFor("Diabeł ubiera się u Prady 2").synopsis.getOrElse(fail("no synopsis"))
    synopsis should include ("Miranda Priestly powraca!")  // first paragraph
    synopsis should include ("David Frankel")              // second paragraph (was dropped)
    synopsis should include ("\n")                          // joined with a paragraph break
    synopsis should not include ("Opis filmu")             // the panel header is dropped
  }

  "NoveKinoClient.parseTitle" should "split format suffixes but leave dash-bearing titles intact" in {
    NoveKinoClient.parseTitle("Zawodowcy - napisy")     shouldBe ("Zawodowcy", List("NAP"))
    NoveKinoClient.parseTitle("Coco - dubbing 3D")      shouldBe ("Coco", List("DUB", "3D"))
    NoveKinoClient.parseTitle("Mission - Impossible")   shouldBe ("Mission - Impossible", Nil)
  }
}
