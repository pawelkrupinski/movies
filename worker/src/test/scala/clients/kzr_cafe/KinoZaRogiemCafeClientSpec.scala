package clients.kzr_cafe

import models.KinoZaRogiemCafe
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZaRogiemCafeClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar/` page (07-06-2026 capture) through the
 *  client. The page lists 22 film cards spanning today through December 2026.
 *  Cards with "Dzisiaj" or "Jutro" labels resolve relative to `today = 2026-06-07`.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("kzr-cafe", real).get("https://kzrcafe.pl/repertuar/")
 *  Fixture directory: test/resources/fixtures/kzr-cafe/
 *  Fetch URL:   https://kzrcafe.pl/repertuar/ */
class KinoZaRogiemCafeClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kzr-cafe")
  private val client = new KinoZaRogiemCafeClient(http, KinoZaRogiemCafe, LocalDate.of(2026, 6, 7))

  "KinoZaRogiemCafeClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoZaRogiemCafe" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoZaRogiemCafe)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "resolve Dzisiaj label: Orzełek Iggy on 2026-06-07 at 11:00" in {
    val movies = client.fetch()
    val iggy   = movies.find(_.movie.title == "Orzełek Iggy").value
    iggy.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 11, 0))
  }

  it should "resolve an absolute-date label: Dzikość on 2026-06-08 at 17:30" in {
    // "Jutro, 17:30" resolves to 2026-06-08 when today = 2026-06-07.
    val movies = client.fetch()
    val dzik   = movies.find(_.movie.title == "Dzikość").value
    dzik.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 8, 17, 30))
  }
}
