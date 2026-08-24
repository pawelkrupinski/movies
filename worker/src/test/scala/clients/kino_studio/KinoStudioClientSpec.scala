package clients.kino_studio

import models.KinoStudio
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoStudioClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `mdk.opole.pl/kino-studio` page through the client —
 *  proving it finds titles, dates, times and the identity hints from the
 *  free-form CMS HTML. today is pinned to the fixture capture date (2026-08-24)
 *  so year inference is deterministic. */
class KinoStudioClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 8, 24)
  private val movies = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole"), KinoStudio, today).fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoStudioClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoStudio)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "parse the title and both showtime times for the season opener" in {
    val opener = film("Drugie życie")
    opener.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 3, 18, 0))
    opener.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 3, 20, 30))
    opener.showtimes.flatMap(_.bookingUrl) shouldBe empty  // box-office only
  }

  // The rebuilt (Drupal) CMS lists the WHOLE season on one page — 14 weekly
  // Thursday slots — where the old page carried a single film. Each `<h3>` date
  // block belongs to the film below it, so the dates must not pool: before the
  // block-boundary flush, film #1 collected every date on the page and every
  // later film ended up with none.
  it should "give each film on the season page its own date" in {
    movies should have size 14
    all(movies.map(_.showtimes.size)) shouldBe 2
    film("Drugie życie").showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 9, 3))
    film("O czym sobie nie mówimy").showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 9, 10))
    film("Takie jest życie").showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 9, 17))
    film("500 mil").showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 12, 3))
  }

  it should "tag every film with KinoStudio cinema" in {
    all(movies.map(_.cinema)) shouldBe KinoStudio
  }

  it should "have non-empty titles" in {
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "capture the genre" in {
    film("Drugie życie").movie.genres should contain("komediodramat")
  }

  it should "capture the director and cast from the metadata block" in {
    // The same `gatunek/reżyseria/obsada` <br>-line carries director + cast;
    // `obsada` is `&nbsp;`-joined ("A,&nbsp;B,&nbsp;C") so the nbsp is normalised
    // before splitting.
    val opener = film("Drugie życie")
    opener.director shouldBe Seq("Maryam Touzani")
    opener.cast shouldBe Seq("Carmen Maura", "Marta Etura", "Ahmed Boulane", "María Alfonsa Rosso")
  }

  // `produkcja: <countries> <year>` and `czas trwania: <n> min` are TMDB identity
  // hints — the year in particular is the dominant lever on match quality, and
  // this cinema's programme is full of titles ("Obcy", "Ojczyzna") a yearless
  // search resolves to the wrong film.
  it should "harvest the production year, countries and runtime" in {
    val opener = film("Drugie życie")
    opener.movie.releaseYear shouldBe Some(2025)
    opener.movie.countries shouldBe Seq("Hiszpania")
    opener.movie.runtimeMinutes shouldBe Some(116)
    film("O czym sobie nie mówimy").movie.releaseYear shouldBe Some(2026)
  }

  it should "carry the poster that heads each film's block" in {
    film("Drugie życie").posterUrl.value should include ("Drugie")
    film("Takie jest życie").posterUrl.value should include ("Takie")
  }

  it should "carry a synopsis" in {
    film("Drugie życie").synopsis.value should include ("Maryam Touzani")
  }

  // Until 2026-08 the page lived at `kino-studio.html` and used `<h1>` titles
  // with dot-separated times ("18.00 i 20.30"); the recorded scrape corpus still
  // carries that capture. The parser stays tolerant of both spellings.
  "the pre-rebuild markup" should "still parse" in {
    val legacy = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-legacy-markup"), KinoStudio,
                                      LocalDate.of(2026, 6, 21)).fetch()
    val lolita = legacy.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    lolita.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 18, 0))
    lolita.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 20, 30))
    lolita.movie.genres should contain("dramat")
    lolita.director shouldBe Seq("Eran Riklis")
    lolita.cast shouldBe Seq("Golshifteh Farahani", "Zar Amir Ebrahimi", "Mina Kavani")
    lolita.movie.releaseYear shouldBe Some(2024)
    lolita.movie.countries shouldBe Seq("Izrael", "Włochy")
    lolita.movie.runtimeMinutes shouldBe Some(108)
  }

  // MDK moves the cinema between two slugs and leaves the dead one 404ing, so
  // the status code can't pick between them; the CONTENT DIV can. Found
  // 2026-07-31: the client was reading the dead slug's body and reporting "no
  // films", which is the white-bar-that-is-actually-our-bug shape.
  "a dead in-season slug" should "fall through to the break slug and still find the films" in {
    val films = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-soft404"), KinoStudio, today).fetch()
    films should not be empty
    films.find(_.movie.title == "Drugie życie")
      .value.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 3, 18, 0))
  }

  it should "report a real break page as zero films, not as a failure" in {
    // The live break page renders normally, it just announces the hiatus
    // ("W czasie wakacji nasze kino jest nieczynne. Startujemy już 3 września").
    // That is a genuinely dormant venue — white is the CORRECT bar, so this must
    // return empty rather than throw. Its in-season slug is absent from the
    // fixture, standing in for the 404 the CMS serves for it.
    val films = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-break"), KinoStudio,
                                     LocalDate.of(2026, 7, 31)).fetch()
    films shouldBe empty
  }

  it should "fail loudly when NEITHER slug renders content, instead of reporting no films" in {
    // Both slugs serve the site's "Strona nie znaleziona" body ⇒ the source is
    // dead, not the venue. Reporting zero films here would paint a white bar
    // indistinguishable from a dormant venue; the scrape must surface red. Same
    // guard as MsiClient / KinoPatriaClient. The 404 body carries `ckeditor`
    // elements of its own (a `title-section`, a modal), so only the
    // `ckeditor clearfix` CONTENT div tells a real page from it.
    val client = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole-dead"), KinoStudio,
                                      LocalDate.of(2026, 7, 31))
    val error = intercept[IllegalStateException](client.fetch())
    error.getMessage should include ("ckeditor")
  }
}
