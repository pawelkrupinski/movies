package services.cinemas

import tools.SpecTimeouts

import models.KinoDiana
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.ListingPages
import services.cinemas.pl.FilmwebShowtimesClient

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

  // A source that HANGS is as down as one that 503s: the programme page times out,
  // so it reports nothing at all — and "nothing reported" must not read as
  // "nothing failed".
  "FilmwebShowtimesClient" should "fail the scrape when its programme page timed out, not report it empty" in {
    // Hangs until the test ends, so the abandoned page fetches don't outlive it.
    val release = new java.util.concurrent.CountDownLatch(1)
    val hanging = new clients.tools.AnyMethodByUrlHttpFetch(_ =>
      { release.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); "[]" })
    val client = new FilmwebShowtimesClient(hanging, 2352, KinoDiana,
      pageTimeout = scala.concurrent.duration.DurationInt(50).millis)
    try a[java.util.concurrent.TimeoutException] should be thrownBy client.fetch()
    finally release.countDown()
  }

  "ListingPages.requireAnyReached" should "tolerate some pages failing, but throw the first failure when all did" in {
    val boom = new RuntimeException("boom")
    noException should be thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Success("page")))
    noException should be thrownBy ListingPages.requireAnyReached(Nil)
    the[RuntimeException] thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Failure(new RuntimeException("later")))) shouldBe boom
  }
}
