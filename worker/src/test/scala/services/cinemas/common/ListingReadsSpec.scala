package services.cinemas.common

import models.{CinemaMovie, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.StubCinemaScraper
import services.scrapes.InMemoryScrapeArchiveRepository

import java.io.IOException
import java.time.LocalDateTime
import java.util.concurrent.Executors
import tools.HostScrapeStats
import scala.util.{Failure, Success}

/**
 * A listing is complete only when every page its scrape read answered — decided from the read
 * outcomes in the shared walks, never declared by a client. It used to tolerate a failed page
 * and still hand the cache a COMPLETE listing, whose prune then deleted every film that lived
 * only on that page.
 */
class ListingReadsSpec extends AnyFlatSpec with Matchers {

  private val down = new IOException("day page 503")
  private def film(title: String) =
    CinemaMovie(Movie(title), Multikino, None, None, None, Nil, Nil, Seq(Showtime(LocalDateTime.of(2026, 8, 1, 18, 0), None)))

  "a walk's failed page" should "make the scope's listing incomplete, while a walk that read every page leaves it complete" in {
    val (_, failedOne) = ListingReads.during(ListingPages.requireAnyReached(Seq(Success(1), Failure(down))))
    failedOne.complete shouldBe false
    failedOne.failed shouldBe Seq(down)
    val (_, allRead) = ListingReads.during(ListingPages.requireAnyReached(Seq(Success(1), Success(2))))
    allRead.complete shouldBe true
  }

  it should "still fail the scrape when every page failed" in {
    an[IOException] should be thrownBy ListingReads.during(ListingPages.requireAnyReached(Seq(Failure(down))))
  }

  "ListingPages.readEach" should "report a page that failed" in {
    val (pages, reads) = ListingReads.during(
      ListingPages.readEach("spec", Seq("a", "b"), identity[String])(url => if (url == "b") throw down else url))
    pages shouldBe Seq("a" -> "a")
    reads.complete shouldBe false
  }

  "ScrapeHorizon's walk" should "report a day whose probe threw" in {
    val today = java.time.LocalDate.of(2026, 8, 1)
    val (_, reads) = ListingReads.during(
      ScrapeHorizon.liveDays(today, maxEmptyDays = 2)(day => if (day == today.plusDays(1)) throw down else day == today))
    reads.complete shouldBe false
  }

  "attempt" should "keep a failed attempt's page failures out of the retry that succeeded" in {
    val (_, reads) = ListingReads.during {
      scala.util.Try(ListingReads.attempt { ListingReads.pageFailed(down); throw down })
      ListingReads.attempt(ListingReads.pageFailed(new IOException("this one counts")))
    }
    reads.failed.map(_.getMessage) shouldBe Seq("this one counts")
  }

  "carry" should "report to the scrape's scope from another thread" in {
    val pool = Executors.newSingleThreadExecutor()
    try {
      val (_, reads) = ListingReads.during {
        val task = ListingReads.carry(ListingReads.pageFailed(down))
        pool.submit(new Runnable { def run(): Unit = task() }).get()
      }
      reads.complete shouldBe false
    } finally pool.shutdown()
  }

  "a page the upstream says is gone (404)" should "leave the listing complete — it is an answer" in {
    val (_, reads) = ListingReads.during(ListingReads.pageFailed(new tools.HttpStatusException(404, "GET", "https://x/day-9", None)))
    reads.complete shouldBe true
  }

  "a record outside any scrape" should "be a no-op" in {
    noException should be thrownBy ListingReads.pageFailed(down)
  }

  "CinemaScrapeRunner" should "land and archive a listing with a failed page as INCOMPLETE" in {
    val archive = new InMemoryScrapeArchiveRepository
    val runner = new CinemaScrapeRunner(DiscardingScrapeSink, tools.SpecClock.Pinned, scrapeArchive = archive)
    val dayTwoDown = new StubCinemaScraper(Multikino,
      ListingPages.readEach("spec", Seq("day-1", "day-2"), identity[String])(day =>
        if (day == "day-2") throw down else film("Dune")).map(_._2))
    runner.run(dayTwoDown)
    archive.find(Multikino).flatMap(_.lastSuccess).map(_.listingComplete) shouldBe Some(false)

    runner.run(new StubCinemaScraper(KinoMuza, Seq(film("Anora"))))
    archive.find(KinoMuza).flatMap(_.lastSuccess).map(_.listingComplete) shouldBe Some(true)
  }

  it should "tell its completeness recorder why each landing was (in)complete" in {
    val told = scala.collection.mutable.ListBuffer.empty[(models.Cinema, ListingCompleteness)]
    val runner = new CinemaScrapeRunner(DiscardingScrapeSink, tools.SpecClock.Pinned, completeness = (cinema, verdict) => { told += cinema -> verdict; () })
    runner.run(new StubCinemaScraper(Multikino, { ListingReads.pageFailed(down); Seq(film("Dune")) }))
    runner.run(new StubCinemaScraper(KinoMuza, Seq(film("Anora")), listingIsComplete = false))
    runner.run(new StubCinemaScraper(models.KinoApollo, Seq(film("Alien"))))
    told.toSeq shouldBe Seq(Multikino -> ListingCompleteness.PageFailed, KinoMuza -> ListingCompleteness.ChunkIncomplete,
      models.KinoApollo -> ListingCompleteness.Complete)
  }

  "RetryingCinemaScraper and AdaptiveTimeoutScraper" should "carry a failed page through to the scrape's completeness" in {
    val pool = Executors.newCachedThreadPool()
    try {
      val inner = new StubCinemaScraper(Multikino, { ListingReads.pageFailed(down); Seq(film("Dune")) })
      val wrapped = new RetryingCinemaScraper(new AdaptiveTimeoutScraper(inner, new HostScrapeStats, pool))
      wrapped.fetchWithSource().complete shouldBe false
    } finally pool.shutdown()
  }
}
