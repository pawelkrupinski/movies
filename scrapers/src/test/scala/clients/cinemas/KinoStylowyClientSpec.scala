package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoStylowy
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.DetailEnricher
import services.cinemas.pl.KinoStylowyClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded CKF Stylowy (Zamość) day pages (2026-09-27 capture):
 *  `repertuar/repertuar.html?rep_date=YYYY-MM-DD` for each of the twelve days
 *  its date strip offers (27 Sep – 8 Oct), plus three `/film/<id>-<slug>.html`
 *  detail pages for the deferred-detail assertions.
 *
 *  Fixture directory: test/resources/fixtures/kino-stylowy/ */
class KinoStylowyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 27)
  private val client = new KinoStylowyClient(new FakeHttpFetch("kino-stylowy"), KinoStylowy, today)
  private lazy val movies = client.fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoStylowyClient" should "sweep every day of the date strip, not just today" in {
    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should contain allOf (LocalDate.of(2026, 9, 27), LocalDate.of(2026, 10, 3), LocalDate.of(2026, 10, 8))
    dates.min should be >= today
    movies.map(_.cinema).toSet shouldBe Set(KinoStylowy)
  }

  it should "date each screening by the day page it came from, with its own iKsoris booking link" in {
    val zeus = film("100 DNI: MISJA ZEUS")
    val byTime = zeus.showtimes.map(s => s.dateTime -> s.bookingUrl).toMap
    byTime(LocalDateTime.of(2026, 9, 27, 18, 45)).value should startWith(
      "https://bilety.stylowy.net/rezerwacja/rezerwacja/miejsca.html?id=3226&idt=7d3c0b82f6b99b8a7381fa3421061417")
    byTime(LocalDateTime.of(2026, 9, 28, 18, 30)).value should include("id=3235")
    byTime.keySet should contain(LocalDateTime.of(2026, 9, 29, 9, 0))
  }

  it should "carry the listing's film page, poster, trailer, genres and age rating" in {
    val zeus = film("100 DNI: MISJA ZEUS")
    zeus.filmUrl.value    shouldBe "https://www.stylowy.net/film/4155-100-dni-misja-zeus.html"
    zeus.posterUrl.value  shouldBe "https://www.stylowy.net/data/film/41/4155_500x720.jpg"
    zeus.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=3aFA2uiS_zQ"
    zeus.movie.genres     shouldBe Seq("komedia", "akcja")
    // "od lat " with no number is no rating; "od lat 10" is.
    zeus.ageRating        shouldBe None
    film("ANDRE RIEU. NIECH ŻYJE MAASTRICHT!").ageRating shouldBe Some("10+")
    // The listing's synopsis is cut off with "..." — the detail page carries it whole.
    zeus.synopsis shouldBe None
  }

  it should "fetch the detail page's identity signals: year, countries, runtime, director, cast" in {
    val d = client.fetchFilmDetail("https://www.stylowy.net/film/4155-100-dni-misja-zeus.html").value
    d.releaseYear    shouldBe Some(2026)
    d.countries      shouldBe Seq("Polska")
    d.genres         shouldBe Seq("komedia", "akcja")
    d.runtimeMinutes shouldBe Some(113)
    d.director       shouldBe Seq("Mikołaj Piszczan")
    d.cast           should contain allOf ("Bartek Laskowski", "Cezary Pazura", "Tomasz Kot")
    d.originalTitle  shouldBe None
    d.synopsis.value should (include("Kapsel i jego ekipa") and include("dream team"))
  }

  it should "read the original title and a multi-country production line" in {
    val tedi = client.fetchFilmDetail("https://www.stylowy.net/film/4164-tedi-i-magiczna-lampa.html").value
    tedi.originalTitle  shouldBe Some("Tad and the Magic Lamp")
    tedi.countries      shouldBe Seq("Hiszpania")
    tedi.runtimeMinutes shouldBe Some(94)

    val luna = client.fetchFilmDetail("https://www.stylowy.net/film/4166-luna-i-rozgadana-swinka.html").value
    luna.countries      shouldBe Seq("Francja", "Belgia")
    luna.releaseYear    shouldBe Some(2025)
    luna.runtimeMinutes shouldBe Some(88)
  }

  it should "defer TMDB resolution to the detail page, which alone carries year and director" in {
    client shouldBe a[DetailEnricher]
    client.defersTmdbResolution shouldBe true
    client.detailGroup shouldBe "kino-stylowy"
  }

  it should "fail the scrape, not report it empty, when every day page fails" in {
    val down = new KinoStylowyClient(new FailingHttpFetch(503), KinoStylowy, today)
    a[HttpStatusException] should be thrownBy down.fetch()
  }
}
