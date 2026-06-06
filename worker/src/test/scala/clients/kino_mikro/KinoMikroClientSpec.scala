package clients.kino_mikro

import clients.tools.FakeHttpFetch
import models.{KinoMikro, MikroBronowice}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoMikroClient

import java.time.LocalDateTime

/** Replays a recorded `api.php/v1/repertoires?limit=1000` JSON payload
 *  (06.06.2026) through the client. The single feed carries both venues, so the
 *  spec pins that the `location_institution_name` filter cleanly partitions them
 *  and that one shared client tags each row with the venue it was constructed
 *  for. The `limit` param is load-bearing: without it the feed truncates to ~20
 *  records (one screening per film); with it the full 06.06–30.06 window comes
 *  back — 28 Kino Mikro screenings here, up from the 18 the un-paginated feed
 *  exposed. Mikro Bronowice carries only a single screening in this feed
 *  regardless of `limit`; the rest of its programme simply isn't published to
 *  this API. */
class KinoMikroClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-mikro")

  private def showtimeCount(movies: Seq[models.CinemaMovie]): Int = movies.map(_.showtimes.size).sum

  "KinoMikroClient" should "scope the shared feed to the Kino Mikro screen" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMikro)
    // The un-paginated feed returned 18 Kino Mikro screenings; `?limit=1000`
    // pulls the whole advance window, lifting this to 28.
    showtimeCount(movies) shouldBe 28
  }

  it should "scope the same feed to Mikro Bronowice, tagging rows with that venue" in {
    val movies = new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice).fetch()

    movies.map(_.cinema).toSet shouldBe Set(MikroBronowice)
    showtimeCount(movies) shouldBe 1

    // The single recorded Bronowice screening: Erupcja, 08.06.2026 18:00.
    val erupcja = movies.find(_.movie.title == "Erupcja").value
    erupcja.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 8, 18, 0))
    erupcja.showtimes.flatMap(_.bookingUrl).head should startWith("https://kinomikro.pl/repertoire/krakow-erupcja/")
  }
}
