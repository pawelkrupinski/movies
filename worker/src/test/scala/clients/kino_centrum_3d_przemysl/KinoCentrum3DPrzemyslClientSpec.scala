package clients.kino_centrum_3d_przemysl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{KinoCentrum3DPrzemysl, Showtime}
import services.cinemas.pl.KinoCentrum3DPrzemyslClient

import java.time.LocalDateTime

/** Replays a recorded 2026-07-21 capture of
  * https://ck.przemysl.pl/kino-centrum/repertuar (IcAgenda event list) — the
  * venue's OWN site, which lists a full repertoire while its old Filmweb source
  * (cinemaId 1786) returns `[]`, painting the uptime bar white. */
class KinoCentrum3DPrzemyslClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoCentrum3DPrzemyslClient(new FakeHttpFetch("kino-centrum-3d-przemysl"), KinoCentrum3DPrzemysl)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoCentrum3DPrzemyslClient.fetch" should "fold the IcAgenda list into 8 films / 50 showtimes" in {
    results.size shouldBe 8
    results.flatMap(_.showtimes).size shouldBe 50
  }

  it should "assign Centrum 3D Przemyśl to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoCentrum3DPrzemysl)
  }

  it should "read the screening date + time off the event href and link to the event page" in {
    val m = byTitle("ROBIN HOOD: KONIEC LEGENDY")
    m.showtimes.size shouldBe 6
    m.showtimes.head shouldBe Showtime(
      LocalDateTime.of(2026, 7, 28, 15, 0),
      Some("https://ck.przemysl.pl/component/icagenda/1027-robin-hood-koniec-legendy/2026-07-28-15-00"),
      None, Nil
    )
    byTitle("MINIONKI I STRASZYDŁA").showtimes.size shouldBe 14
    byTitle("ZAPROSZENIE").showtimes.size            shouldBe 12
  }

  it should "leave the ALL-CAPS + format-word title verbatim (recased/stripped centrally on ingest)" in {
    // "DRZEWO MAGII dubbing" keeps its format word here; `FormatTags` strips it
    // at the ingest choke point, not in the client.
    byTitle.keySet should contain ("DRZEWO MAGII dubbing")
  }

  it should "carry the listing synopsis teaser" in {
    byTitle("ROBIN HOOD: KONIEC LEGENDY").synopsis.map(_.toLowerCase).getOrElse("") should
      include ("hugh jackman")
  }
}
