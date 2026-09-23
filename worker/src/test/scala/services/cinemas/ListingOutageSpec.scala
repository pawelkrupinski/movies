package services.cinemas

import clients.tools.FailingHttpFetch
import models.{KinoAstra, KinoDiana, KinoKijow, KinoTatry, KinoZamekSzczecin, McswElektrowniaCinema}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScraper, ListingPages}
import services.cinemas.pl.{Bilety24SubdomainClient, FilmwebShowtimesClient, KinoDianaClient, KinoKijowClient, KinoTatryClient, KinoZamekClient, McswElektrowniaCinemaClient, MsiClient}
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.HttpStatusException

import java.time.LocalDate
import scala.util.{Failure, Success}

/**
 * A scraper whose source is wholly down must fail its scrape — red on /uptime,
 * with the cause — rather than return an empty list, which reads as a
 * successful "0 showtimes" scrape (white, indistinguishable from a dormant
 * venue). Every client here reads its listing off one page or a walk of
 * several, tolerating a single page failing; this pins that a TOTAL outage
 * still propagates.
 */
class ListingOutageSpec extends AnyFlatSpec with Matchers {

  private val down  = new FailingHttpFetch(503)
  private val today = LocalDate.of(2026, 9, 23)

  private val scrapers: Seq[CinemaScraper] = Seq(
    new KinoDianaClient(down),
    new KinoTatryClient(down, KinoTatry, today),
    new FilmwebShowtimesClient(down, 2352, KinoDiana, today = today),
    new Bilety24SubdomainClient(down, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = today, titles = titleNormalizer),
    new KinoKijowClient(down, KinoKijow, today, titles = titleNormalizer),
    new McswElektrowniaCinemaClient(down, McswElektrowniaCinema, today),
    new MsiClient(down, "https://bilety.example.pl", KinoDiana, today),
    new KinoZamekClient(down, KinoZamekSzczecin, today)
  )

  scrapers.foreach { scraper =>
    s"${scraper.getClass.getSimpleName}" should "propagate a total fetch outage instead of returning an empty (white) scrape" in {
      a[HttpStatusException] should be thrownBy scraper.fetch()
    }
  }

  "ListingPages.requireAnyReached" should "tolerate some pages failing, but throw the first failure when all did" in {
    val boom = new RuntimeException("boom")
    noException should be thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Success("page")))
    noException should be thrownBy ListingPages.requireAnyReached(Nil)
    the[RuntimeException] thrownBy ListingPages.requireAnyReached(Seq(Failure(boom), Failure(new RuntimeException("later")))) shouldBe boom
  }
}
