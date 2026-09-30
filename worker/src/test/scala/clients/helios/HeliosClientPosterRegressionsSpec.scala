package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.HeliosClient
import services.movies.SingleCountryNormalizer.titleNormalizer

class HeliosClientPosterRegressionsSpec extends AnyFlatSpec with Matchers {
  // Every case reads the same immutable result, so the fixture is parsed once.
  private lazy val results = new HeliosClient(new FakeHttpFetch("helios/posters"), titles = titleNormalizer).fetch()

  // ── Smoke test ────────────────────────────────────────────────────────────

  "HeliosClient.fetch" should "return results from real fixture data" in {
    results                     should not be empty
    results.size                should be >= 5
    results.flatMap(_.showtimes) should not be empty
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
    val oPsie = results.find(_.movie.title.startsWith("O psie"))
    oPsie                    shouldBe defined
    oPsie.get.showtimes.size shouldBe 1
    oPsie.get.posterUrl      shouldBe Some("https://movies.helios.pl/images/opsieplakat.jpg")
  }
}
