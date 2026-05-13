package clients.helios

import clients.HeliosClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeliosClientPosterRegressionsSpec extends AnyFlatSpec with Matchers {
  private val fakeHttp = new FakeHttpFetch("helios/posters")
  private val client   = new HeliosClient(fakeHttp)

  private def fetch() = client.fetch()

  // ── Smoke test ────────────────────────────────────────────────────────────

  "HeliosClient.fetch" should "return results from real fixture data" in {
    val result = fetch()
    result                    should not be empty
    result.size               should be >= 5
    result.flatMap(_.showtimes) should not be empty
  }

  it should "assign unique poster URLs — no movie steals another's poster" in {
    val posterUrls = fetch().flatMap(_.posterUrl)
    posterUrls.distinct.size shouldBe posterUrls.size
  }

  // ── Collision regression ───────────────────────────────────────────────────
  //
  // "O psie, który jeździł koleją" (movieId de2de832) has exactly ONE screening
  // that shares its timeslot with "Sprawiedliwość owiec" (movieId 0c138744,
  // 18 screenings).  The buggy Map-keyed-by-LocalDateTime code would hand "O psie"
  // the wrong poster because the later entry in the JSON array overwrites the earlier
  // one.  UUID-based matching (screening id from the booking URL) gives a guaranteed
  // 1:1 assignment regardless of ordering or collision count.

  it should "assign the correct poster to a movie whose only screening shares a timeslot with another movie" in {
    val oPsie = fetch().find(_.movie.title.startsWith("O psie"))
    oPsie                    shouldBe defined
    println(oPsie.get.showtimes)
    oPsie.get.showtimes.size shouldBe 1
    oPsie.get.posterUrl      shouldBe Some("https://movies.helios.pl/images/opsieplakat.jpg")
  }

  it should "not assign Sprawiedliwość owiec's poster to O psie" in {
    val wrongPoster = "https://movies.helios.pl/images/Sprawiedliwoscowiecplakat.jpg"
    fetch()
      .find(_.movie.title.startsWith("O psie"))
      .flatMap(_.posterUrl) should not be Some(wrongPoster)
  }
}
