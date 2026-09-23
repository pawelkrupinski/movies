package clients.kino_grajfka

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoGrajfka
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoGrajfkaClient

import java.time.LocalDateTime

/** Replays the recorded `kino.chck.pl/repertuar/` "ctc_event" WordPress
 *  archive (2026-09-23 capture) through the client.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("kino-grajfka", real).get("https://kino.chck.pl/repertuar/")
 *  Fixture directory: test/resources/fixtures/kino-grajfka/
 *  Fetch URL:   https://kino.chck.pl/repertuar/ */
class KinoGrajfkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoGrajfkaClient(new FakeHttpFetch("kino-grajfka"), KinoGrajfka).fetch()

  "KinoGrajfkaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoGrajfka)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "resolve a three-letter Polish month abbreviation: Alcarras on 2026-09-25 at 18:00" in {
    val film = movies.find(_.movie.title == "Alcarras").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 18, 0))
  }

  it should "keep only the START time from an HH:MM – HH:MM range: Kafarnaum at 18:00, not 20:00" in {
    val film = movies.find(_.movie.title == "Kafarnaum").value
    film.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 9, 18, 0))
  }

  it should "carry the venue's own detail-page link as filmUrl" in {
    val film = movies.find(_.movie.title == "Alcarras").value
    film.filmUrl.value shouldBe "https://kino.chck.pl/repertuar/alcarras-3/"
  }

  it should "resolve a month rollover into October: Zwierzaki na zakręcie on 2026-10-06" in {
    val film = movies.find(_.movie.title == "Zwierzaki na zakręcie").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 6, 17, 0))
  }
}
