package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import org.scalatest.OptionValues
import models.KinoAwangarda2
import tools.HttpStatusException
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoAwangarda2Client

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded weekly repertoire article (21-06-2026 capture) through
 *  the client. The Joomla page is a flat run of sibling `<p>` blocks: seven day
 *  headers (`Piątek 19.06.` … `Czwartek 25.06.`) interleaved with screening lines
 *  carrying the film link + `godz. HH.MM` time. `today` is pinned so the
 *  year-less `DD.MM` headers resolve to a stable LocalDate regardless of when the
 *  test runs.
 *
 *  Fixture recorder: clients.tools.WriteKinoAwangarda2 (RecordingHttpFetch
 *    "kino-awangarda2" over RealHttpFetch).
 *  Fixture directory: test/resources/fixtures/kino-awangarda2/
 *  Fetch URL: KinoAwangarda2Client.RepertoireUrl */
class KinoAwangarda2ClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-awangarda2")
  private val client = new KinoAwangarda2Client(http, LocalDate.of(2026, 6, 21))

  "KinoAwangarda2Client" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoAwangarda2 and a non-empty title" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoAwangarda2)
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "give every film at least one showtime, all within the page's week" in {
    val movies    = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate)
    dates.min should be >= LocalDate.of(2026, 6, 19)
    dates.max should be <= LocalDate.of(2026, 6, 25)
  }

  it should "anchor a year-less DD.MM header to an absolute date + time" in {
    val movies = client.fetch()
    val film   = movies.find(_.movie.title == "Drugie życie").value
    // "Piątek 19.06." header → screening "godz. 17.45".
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 19, 17, 45))
    film.movie.countries should contain allOf ("Francja", "Belgia")
  }

  it should "recover the quoted film from a retrospective 'Series : \"Film\"' line" in {
    val movies = client.fetch()
    // `Federico Fellini : Ciao a tutti "Wałkonie"` → title is the quoted film.
    val film = movies.find(_.movie.title == "Wałkonie").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 19, 45))
  }

  // The venue is on cheap shared hosting (cyberfolks.pl) that intermittently
  // 503s ("Script execution exceeded allocated limits"). A swallowed 503 would
  // record a false "0 showtimes" scrape — white on /uptime, indistinguishable
  // from a genuinely film-dormant venue. The fetch failure must propagate so it
  // surfaces red instead.
  it should "propagate a fetch failure instead of swallowing it into an empty (white) scrape" in {
    val client = new KinoAwangarda2Client(new FailingHttpFetch(503), LocalDate.of(2026, 6, 21))
    a[HttpStatusException] should be thrownBy client.fetch()
  }
}
