package clients.kino_roma

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoRoma
import services.cinemas.pl.KinoRomaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar` page (07-06-2026 capture) through the
 *  client. Each card on this page represents exactly one screening; multiple
 *  screenings of the same film appear as multiple cards. `today` is pinned
 *  so the "DD.MM" → year inference is stable.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("kino-roma", real).get("https://www.kinoroma.zabrze.pl/repertuar")
 *  Fixture directory: test/resources/fixtures/kino-roma/
 *  Fetch URL:   https://www.kinoroma.zabrze.pl/repertuar */
class KinoRomaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-roma")
  private val client = new KinoRomaClient(http, KinoRoma, LocalDate.of(2026, 6, 7))
  // Every case only reads the parsed result, so the fixture is replayed once per suite.
  private lazy val fetched = client.fetch()

  "KinoRomaClient" should "return a non-empty film list" in {
    val movies = fetched
    movies should not be empty
  }

  it should "tag every film with KinoRoma" in {
    val movies = fetched
    movies.map(_.cinema).toSet shouldBe Set(KinoRoma)
  }

  it should "give every film at least one showtime" in {
    val movies = fetched
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Tom i Jerry: Przygoda w muzeum on 2026-06-07 at 15:00" in {
    val movies = fetched
    val film   = movies.find(_.movie.title == "Tom i Jerry: Przygoda w muzeum").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 15, 0))
  }

  it should "read the production year from the card's div.year cell" in {
    // The card's `div.year` is 2025, distinct from the 2026 screening date the
    // DD.MM inference produces — so this is the production year, not a re-parse.
    val movies = fetched
    movies.find(_.movie.title == "Tom i Jerry: Przygoda w muzeum").value
      .movie.releaseYear shouldBe Some(2025)
  }

  // prod PL 2026-10-07: the card's `img src` is site-relative ("/app/assets/movie/…"), and the identity model's listing
  // carried it as it came — six poster questions filed under a link no fetch takes, the poster evidence gone. The
  // listing's poster is absolutised against its page, as the card's (CinemaSlotBuilder) is.
  it should "give the identity model's listing an absolute poster, as the card serves it" in {
    val film = fetched.find(_.movie.title == "Tom i Jerry: Przygoda w muzeum").value
    film.posterUrl.value should startWith ("/app/assets/movie/")
    services.identity.Listing.of(KinoRoma, film, services.movies.TitleNormalizer.forCountry(models.Country.Poland)).poster shouldBe
      Some("https://www.kinoroma.zabrze.pl/app/assets/movie/zieS_6XO_LtKn5uQL0krgB7Hlrd5.jpg")
  }
}
