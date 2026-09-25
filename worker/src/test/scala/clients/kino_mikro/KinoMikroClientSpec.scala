package clients.kino_mikro

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import play.api.libs.json.JsString
import org.scalatest.matchers.should.Matchers
import models.{KinoMikro, MikroBronowice}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.{KinoMikroClient, KinoMikroParser}

import java.time.LocalDateTime

/** Replays a live 2026-09-26 capture of the VisualSoft ticketing feed both Mikro
 *  screens sell through (`bilety.kinomikro.pl/service.php/repertoire/list.json`:
 *  95 screenings, 74 Kino Mikro + 21 Mikro Bronowice).
 *
 *  The venue's own `kinomikro.pl/api.php/v1/repertoires` feed, which the client
 *  read until then, went empty on 2026-09-23 and 404s since the site's WordPress
 *  rebuild — both screens sat on the Filmweb fallback. The old client never
 *  requests this URL, so against it this spec fails on a missing fixture. */
class KinoMikroClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-mikro")

  private def showtimeCount(movies: Seq[models.CinemaMovie]): Int = movies.map(_.showtimes.size).sum

  "KinoMikroClient" should "read Kino Mikro's whole programme off the ticketing feed" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro).fetch()

    movies.map(_.cinema).toSet shouldBe Set(KinoMikro)
    showtimeCount(movies) shouldBe 74

    val lalka = movies.find(_.movie.title == "Lalka").value
    lalka.showtimes.size shouldBe 15
    lalka.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 30, 16, 45))
    lalka.showtimes.flatMap(_.bookingUrl).head should startWith("https://bilety.kinomikro.pl/kup-bilet/lalka-")
    lalka.posterUrl.value should startWith("https://bilety.kinomikro.pl/uploads/event/")
  }

  it should "keep a screening's local wall-clock time across the CEST→CET switch" in {
    // "2026-10-29T21:00:00+01:00" — after the switch the offset changes, the
    // wall-clock time the venue advertises must not.
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro).fetch()
    movies.flatMap(_.showtimes).map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 29, 21, 0))
  }

  it should "scope the same feed to Mikro Bronowice and fold a dubbed title onto its film" in {
    val movies = new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice).fetch()

    movies.map(_.cinema).toSet shouldBe Set(MikroBronowice)
    showtimeCount(movies) shouldBe 21

    // "Marsupilami- dubbing" → title "Marsupilami", DUB carried on the showtime.
    val marsupilami = movies.find(_.movie.title == "Marsupilami").value
    marsupilami.showtimes.size shouldBe 5
    all(marsupilami.showtimes.map(_.format)) should contain("DUB")
    movies.map(_.movie.title).exists(_.toLowerCase.contains("dubbing")) shouldBe false
  }

  it should "parse the director out of the event description, stopping at the next label" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro).fetch()

    // `Reżyseria: François Ozon  Występują: Benjamin Voisin, …`
    movies.find(_.movie.title == "Obcy").value.director shouldBe Seq("François Ozon")
    // `Reżyseria: Louis Malle  Muzyka: Miles Davis  Scenariusz: …`
    movies.find(_.movie.title == "Windą na szafot").value.director shouldBe Seq("Louis Malle")
    // `Reżyseria: Sam Raimi | Produkcja: USA, 1987 | …`
    movies.find(_.movie.title == "Martwe zło 2").value.director shouldBe Seq("Sam Raimi")
  }

  // Director extraction is fixture-independent — exercise the no-colon layout
  // and co-director splitting through the public parser with a hand-built
  // record in the feed's shape, so coverage doesn't hinge on which films happen
  // to be showing in the recorded weeks.
  private def directorOf(description: String): Seq[String] = {
    val json =
      s"""{"repertoires":{"1":{"title":"Probe","date":"2026-10-15T18:00:00+02:00",
         |"location":{"institution_name":"Kino Mikro"},
         |"event":{"description":${JsString(description)}}}}}""".stripMargin
    KinoMikroParser.parse(json, "Kino Mikro", KinoMikro).head.director
  }

  "KinoMikroParser.parse" should "read a no-colon director terminated by the next label" in {
    directorOf("<div>Reżyseria George Sluizer</div><div>Obsada Bernard-Pierre Donnadieu</div>") shouldBe
      Seq("George Sluizer")
  }

  it should "split co-directors and return no director when the Reżyseria marker is absent" in {
    directorOf("<div>Reżyseria: Joel Coen, Ethan Coen</div><div>Gatunek dramat</div>") shouldBe
      Seq("Joel Coen", "Ethan Coen")
    directorOf("<div>Gatunek: dramat</div>") shouldBe empty
  }
}
