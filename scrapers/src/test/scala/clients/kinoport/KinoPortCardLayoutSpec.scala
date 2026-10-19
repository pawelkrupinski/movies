package clients.kinoport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.KinoPort
import services.cinemas.pl.KinoPortClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-10-07 capture of https://gcsw.pl/wp-json/wp/v2/posts?categories=49.
  * The 30.09 repertoire post ("1–9 PAŹDZIERNIKA") dropped the heading/paragraph
  * run the client walked for a styled card grid: one `div.gcsw-day` per day
  * (`gcsw-day__date` "1.10"), one `div.gcsw-screening` per screening with
  * `__time`, `__title` and a `·`-separated `__meta` ("102’ · 2026 · Morgan
  * Matthews · dramat"). No `h3`/`h4`/`p` carries a day or a time any more, so
  * every scrape from 30.09 read zero screenings and the Filmweb fallback had
  * covered the venue since 01.10. */
class KinoPortCardLayoutSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPortClient(
    new FakeHttpFetch("kinoport-card-layout-2026-10"), KinoPort, LocalDate.of(2026, 10, 7))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoPortClient.fetch" should "read every screening card of the post" in {
    results.flatMap(_.showtimes) should have size 16
  }

  it should "date a card from its day header and time" in {
    byTitle("Miłość, śmierć i dojrzewanie w Camp Miasma").showtimes.map(_.dateTime) should contain theSameElementsAs Seq(
      LocalDateTime.of(2026, 10, 1, 19, 30),
      LocalDateTime.of(2026, 10, 3, 20, 0))
  }

  it should "read runtime, year and director off the meta line" in {
    val m = byTitle("Człowiek z marmuru")
    m.movie.runtimeMinutes shouldBe Some(152)
    m.movie.releaseYear shouldBe Some(1977)
    m.director shouldBe Seq("Andrzej Wajda")
  }

  it should "read year and director when the meta line has no runtime" in {
    val m = byTitle("Frances Ha")
    m.movie.runtimeMinutes shouldBe None
    m.movie.releaseYear shouldBe Some(2012)
    m.director shouldBe Seq("Noah Baumbach")
    m.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 4, 20, 0))
  }

  it should "read a card with no meta line at all" in {
    byTitle("Yakari").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 3, 13, 0))
  }
}
