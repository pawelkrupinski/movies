package clients.sdk

import clients.tools.FakeHttpFetch
import models.{Showtime, SluzewskiDomKultury}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.SdkClient

import java.time.LocalDateTime

class SdkClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new SdkClient(new FakeHttpFetch("kino-sdk"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "SdkClient.fetch" should "page through the Joomla listing and return 18 films / 18 showtimes" in {
    results.size shouldBe 18
    results.flatMap(_.showtimes).size shouldBe 18
  }

  it should "assign Służewski Dom Kultury to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(SluzewskiDomKultury)
  }

  it should "enrich runtime/year/countries from the event page and carry room + biletyna booking" in {
    val m = byTitle("Kopnęłabym cię, gdybym mogła")
    m.movie.runtimeMinutes shouldBe Some(93)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.countries      shouldBe Seq("USA")
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 28, 19, 0), Some("https://biletyna.pl/event/view/id/671618"), Some("sala widowiskowa"), Nil)
  }
}
