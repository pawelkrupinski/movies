package clients.kino_studio

import models.KinoStudio
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoStudioClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `mdk.opole.pl/kino-studio.html` page through the
 *  client — proving it finds titles, dates and times from the free-form CMS
 *  HTML. today is pinned to the fixture capture date (2026-06-21) so year
 *  inference is deterministic. */
class KinoStudioClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 6, 21)
  private val movies = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole"), KinoStudio, today).fetch()

  "KinoStudioClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoStudio)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "parse the title and both showtime times for the current film" in {
    val film = movies.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 18, 0))
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 20, 30))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty  // box-office only
  }

  it should "tag every film with KinoStudio cinema" in {
    all(movies.map(_.cinema)) shouldBe KinoStudio
  }

  it should "have non-empty titles" in {
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "capture the genre" in {
    val film = movies.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    film.movie.genres should contain("dramat")
  }

  // MDK parks the cinema on `kino-studio-przerwa.html` over a seasonal break and
  // leaves `kino-studio.html` serving a SOFT-404 — HTTP 200 carrying the site's
  // "Błąd 404" body, so the status code cannot tell the dead slug from the live
  // one. Found 2026-07-31: the client was reading the soft-404 and reporting
  // "no films", which is the white-bar-that-is-actually-our-bug shape.
  "a soft-404 on the in-season slug" should "fall through to the break slug and still find the films" in {
    val films = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-soft404"), KinoStudio, today).fetch()
    films should not be empty
    films.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę"))
      .value.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 18, 0))
  }

  it should "report a real break page as zero films, not as a failure" in {
    // The live break page renders normally, it just announces the hiatus
    // ("W czasie wakacji nasze kino jest nieczynne. Startujemy już 3 września").
    // That is a genuinely dormant venue — white is the CORRECT bar, so this must
    // return empty rather than throw.
    val films = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-break"), KinoStudio,
                                     LocalDate.of(2026, 7, 31)).fetch()
    films shouldBe empty
  }

  it should "fail loudly when NEITHER slug renders content, instead of reporting no films" in {
    // Both slugs soft-404 ⇒ the source is dead, not the venue. Reporting zero
    // films here would paint a white bar indistinguishable from a dormant venue;
    // the scrape must surface red. Same guard as MsiClient / KinoPatriaClient.
    val client = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-dead"), KinoStudio,
                                      LocalDate.of(2026, 7, 31))
    val error = intercept[IllegalStateException](client.fetch())
    error.getMessage should include ("ckeditor")
  }

  it should "capture the director and cast from the metadata block" in {
    // The same `gatunek/reżyseria/obsada` <br>-line carries director + cast;
    // `obsada` is `&nbsp;`-joined ("A,&nbsp;B,&nbsp;C") so the nbsp is normalised
    // before splitting.
    val film = movies.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    film.director shouldBe Seq("Eran Riklis")
    film.cast shouldBe Seq("Golshifteh Farahani", "Zar Amir Ebrahimi", "Mina Kavani")
  }
}
