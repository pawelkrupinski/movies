package clients.kino_mikro

import clients.tools.FakeHttpFetch
import models.{KinoMikro, MikroBronowice}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoMikroClient

import java.time.LocalDateTime

/** Replays a recorded `api.php/v1/repertoires` JSON payload (06.06.2026) through
 *  the client. The single feed carries both venues, so the spec pins that the
 *  `location_institution_name` filter cleanly partitions them — 18 Kino Mikro
 *  screenings, 2 at Mikro Bronowice — and that one shared client tags each row
 *  with the venue it was constructed for. */
class KinoMikroClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-mikro")

  private def showtimeCount(movies: Seq[models.CinemaMovie]): Int = movies.map(_.showtimes.size).sum

  "KinoMikroClient" should "scope the shared feed to the Kino Mikro screen" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMikro)
    showtimeCount(movies) shouldBe 18
  }

  it should "scope the same feed to Mikro Bronowice, tagging rows with that venue" in {
    val movies = new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice).fetch()

    movies.map(_.cinema).toSet shouldBe Set(MikroBronowice)
    showtimeCount(movies) shouldBe 2

    // The first recorded Bronowice screening: Erupcja, 06.06.2026 18:00.
    val erupcja = movies.find(_.movie.title == "Erupcja").value
    erupcja.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 18, 0))
    erupcja.showtimes.flatMap(_.bookingUrl).head should startWith("https://kinomikro.pl/repertoire/krakow-erupcja/")
  }
}
