package services.cinemas

import models.KinoDiana
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.ListingPages
import services.cinemas.pl.FilmwebShowtimesClient

import java.time.LocalDate
import scala.util.{Failure, Success}

/**
 * A scraper whose source is wholly down must fail its scrape — red on /uptime,
 * with the cause — rather than return an empty list, which reads as a
 * successful "0 showtimes" scrape (white, indistinguishable from a dormant
 * venue). [[ScraperOutageSpec]] holds every catalog scraper to that against a
 * failing fetch; this pins the two shapes it doesn't reach — a source that
 * HANGS, and the page-tolerance rule itself.
 */
class ListingOutageSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 9, 23)

  // A source that HANGS is as down as one that 503s: every day's page times out,
  // so no day reports anything at all — and "nothing reported" must not read as
  // "nothing failed".
  "FilmwebShowtimesClient" should "fail the scrape when every day's page timed out, not report it empty" in {
    val hanging = new tools.HttpFetch {
      def get(url: String): String = { Thread.sleep(10000); "[]" }
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val client = new FilmwebShowtimesClient(hanging, 2352, KinoDiana, daysAhead = 1, today = today,
      pageTimeout = scala.concurrent.duration.DurationInt(50).millis)
    a[java.util.concurrent.TimeoutException] should be thrownBy client.fetch()
  }

  "ListingPages.requireAnyReached" should "tolerate some pages failing, but throw the first failure when all did" in {
    val boom = new RuntimeException("boom")
    noException should be thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Success("page")))
    noException should be thrownBy ListingPages.requireAnyReached(Nil)
    the[RuntimeException] thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Failure(new RuntimeException("later")))) shouldBe boom
  }
}
