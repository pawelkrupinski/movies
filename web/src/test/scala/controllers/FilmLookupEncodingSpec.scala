package controllers

import models.{MovieRecord, Poznan, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.LocalDateTime

/** The film deep-link carries the title in the query string as RFC-3986
 *  percent-encoding (`Drugie%20%C5%BCycie`). Telegram — and some other chat
 *  apps — re-encode a pasted URL that already contains `%XX` escapes, turning
 *  `%20` into `%2520` and `%C5%BC` into `%25C5%25BC`. Play decodes that once,
 *  so the controller is handed a title still carrying a literal `%20` / `%C5%BC`
 *  and the lookup used to miss with a 404. The lookup now decodes the residual
 *  escapes once more on a miss.
 */
class FilmLookupEncodingSpec extends AnyFlatSpec with Matchers {

  // Far in the future so the screening is always "upcoming" whenever the test runs.
  private val showAt = LocalDateTime.of(2099, 1, 1, 18, 0)

  private val service = new MovieControllerService(
    TestReadModel.fromRecords(Seq(("Drugie życie", Some(2026), MovieRecord(data = Map[Source, SourceData](
      Rialto -> SourceData(title = Some("Drugie życie"),
                           showtimes = Seq(Showtime(showAt, bookingUrl = None)))
    ))))))

  "film" should "resolve a plainly-decoded title" in {
    service.film(Poznan, "Drugie życie").map(_.movie.title) shouldBe Some("Drugie życie")
  }

  it should "resolve a title Telegram doubly-encoded (Play decoded only once)" in {
    // What the action receives after Play decodes Telegram's `%2520%25C5%25BC…`.
    service.film(Poznan, "Drugie%20%C5%BCycie").map(_.movie.title) shouldBe Some("Drugie życie")
  }

  it should "still return None for a genuinely unknown title" in {
    service.film(Poznan, "Nie ma takiego filmu") shouldBe None
  }
}
