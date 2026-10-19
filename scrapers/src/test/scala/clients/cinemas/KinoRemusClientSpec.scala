package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoRemus
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.DetailEnricher
import services.cinemas.pl.KinoRemusClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-09-27 capture of Kino Remus (Kościerzyna): the Ticket
 *  Manager's `/Repertuar/Kalendarz1JsonDane` feed — 64 screenings, 27 Sep
 *  through 5 Nov — the KDK `/kino-remus/` film list that links each title to
 *  its page, and five of those film pages for the deferred-detail assertions.
 *
 *  Fixture directory: test/resources/fixtures/kino-remus/ */
class KinoRemusClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val client = new KinoRemusClient(new FakeHttpFetch("kino-remus"), KinoRemus)
  private lazy val movies = client.fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoRemusClient" should "read every screening of the Ticket Manager feed, weeks past what Filmweb lists" in {
    movies.flatMap(_.showtimes) should have size 64
    movies.map(_.movie.title) should contain theSameElementsAs Seq(
      "500 mil", "Czas, który nie nadszedł", "Dyskusyjny Klub Filmowy: Kumotry",
      "Dzień dziecka księdza Jana Kaczkowskiego", "Lalka", "Luna i rozgadana świnka", "Mistyczka",
      "Powiedz mi, co czujesz", "Tedi i magiczna lampa", "Totalna magia 2",
      "Verity. Coraz większy mrok", "Zapomniana wyspa")
    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should contain allOf (LocalDate.of(2026, 9, 27), LocalDate.of(2026, 10, 22), LocalDate.of(2026, 11, 5))
    movies.map(_.cinema).toSet shouldBe Set(KinoRemus)
  }

  it should "take the time from the event's own 'godz.' line, not the feed's 23:59 date stamp" in {
    val kumotry = film("Dyskusyjny Klub Filmowy: Kumotry")
    kumotry.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 11, 5, 19, 30))
    kumotry.showtimes.head.bookingUrl.value shouldBe "https://www.remus.hostingasp.pl/Bilety/Sala/13615"
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalTime.toString) should not contain "23:59"
  }

  it should "fold the straight- and curly-quoted billings of one film together, the version on the showtime" in {
    // 29 Sep 10:00 is billed `"Mistyczka"  2D`, every other day `„Mistyczka” 2D`.
    film("Mistyczka").showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 29, 10, 0), LocalDateTime.of(2026, 9, 29, 17, 30))
    all(film("Zapomniana wyspa").showtimes.map(_.format)) should contain("DUB")
    all(film("Verity. Coraz większy mrok").showtimes.map(_.format)) should contain("NAP")
    movies.map(_.movie.title).filter(t => t.contains("2D") || t.exists("„”\"".contains(_))) shouldBe empty
  }

  it should "link a film to its KDK page, matched by title across the two sites" in {
    film("Lalka").filmUrl.value shouldBe "https://kdkkoscierzyna.pl/lalka/"
    film("Czas, który nie nadszedł").filmUrl.value shouldBe "https://kdkkoscierzyna.pl/czas-ktory-nie-nadszedl/"
    film("Dyskusyjny Klub Filmowy: Kumotry").filmUrl.value shouldBe "https://kdkkoscierzyna.pl/dyskusyjny-klub-filmowy-kumotry-2/"
    // Billed on the Ticket Manager but not (yet) on the KDK list: no page, still screened.
    film("Dzień dziecka księdza Jana Kaczkowskiego").filmUrl shouldBe None
    // The feed's only poster is a generic "zapraszamy" placeholder.
    movies.flatMap(_.posterUrl) shouldBe empty
  }

  it should "fetch the film page's poster, genres, age rating, runtime and synopsis" in {
    val d = client.fetchFilmDetail("https://kdkkoscierzyna.pl/lalka/").value
    d.posterUrl.value shouldBe "https://kdkkoscierzyna.pl/wp-content/uploads/2026/09/Lalka-plakat.jpg"
    d.genres          shouldBe Seq("dramat", "romans")
    d.ageRating       shouldBe Some("13+")
    d.runtimeMinutes  shouldBe Some(160)
    d.synopsis.value  should startWith("Warszawa końca XIX wieku.")
    d.synopsis.value  should not include "Bilety do nabycia"
  }

  it should "drop the date/time/price header lines of an event-style page from the synopsis" in {
    val d = client.fetchFilmDetail("https://kdkkoscierzyna.pl/dyskusyjny-klub-filmowy-kumotry-2/").value
    d.genres         shouldBe Seq("dokumentalny")
    d.ageRating      shouldBe Some("15+")
    d.runtimeMinutes shouldBe Some(70)
    d.synopsis.value should include("Historia przyjaźni kobiet")
    d.synopsis.value should not include "listopada"
    d.synopsis.value should not include "cena biletu"
  }

  it should "resolve from the listing, since the film page carries no director, year or original title" in {
    client shouldBe a[DetailEnricher]
    client.defersTmdbResolution shouldBe false
    client.detailGroup shouldBe "kino-remus"
  }

  it should "propagate a fetch failure instead of reporting an empty scrape" in {
    a[HttpStatusException] should be thrownBy new KinoRemusClient(new FailingHttpFetch(503), KinoRemus).fetch()
  }

  // The KDK film list only links screenings to their pages; the Ticket Manager
  // feed is the programme. KDK being down must cost the links, not every screening.
  it should "keep every screening, unlinked, when only the KDK film list fails" in {
    val replay = new FakeHttpFetch("kino-remus")
    val kdkDown = new tools.HttpFetch {
      def get(url: String): String =
        if (url == KinoRemusClient.FilmListUrl) throw new HttpStatusException(503, "GET", url, None) else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val unlinked = new KinoRemusClient(kdkDown, KinoRemus).fetch()
    unlinked.flatMap(_.showtimes) should have size 64
    unlinked.flatMap(_.filmUrl) shouldBe empty
  }
}
