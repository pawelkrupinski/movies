package clients.nove_kino

import clients.tools.FakeHttpFetch
import models.{KinoAtlantic, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.NoveKinoClient

import java.time.LocalDateTime

class NoveKinoClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new NoveKinoClient(new FakeHttpFetch("kino-atlantic"), "atlantic", KinoAtlantic)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "NoveKinoClient.fetch" should "walk the per-day pages and return 29 films / 142 showtimes" in {
    results.size shouldBe 29
    results.flatMap(_.showtimes).size shouldBe 142
  }

  it should "assign Kino Atlantic to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoAtlantic)
  }

  it should "strip the presentation suffix into the showtime format and merge by clean title" in {
    results.map(_.movie.title) should contain ("Diabeł ubiera się u Prady 2")
    results.map(_.movie.title).foreach(_ should not include "- napisy")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 17
    byTitle("Zawodowcy").showtimes.size shouldBe 16
    byTitle("Diabeł ubiera się u Prady 2").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 17, 30),
        Some("https://atlantic.novekino.pl/msi/default.aspx?event_id=45780&typetran=0&ReturnLink=https://www.novekino.pl/kina/atlantic/dziekujemy.php"),
        None, List("NAP")
      )
  }

  "NoveKinoClient.parseTitle" should "split format suffixes but leave dash-bearing titles intact" in {
    NoveKinoClient.parseTitle("Zawodowcy - napisy")     shouldBe ("Zawodowcy", List("NAP"))
    NoveKinoClient.parseTitle("Coco - dubbing 3D")      shouldBe ("Coco", List("DUB", "3D"))
    NoveKinoClient.parseTitle("Mission - Impossible")   shouldBe ("Mission - Impossible", Nil)
  }
}
