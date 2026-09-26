package services.alerts

import models.{Cinema, KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeAttempt}

import java.time.Instant
import scala.collection.mutable.ListBuffer

/** A venue with no fallback that keeps answering "page gone" across separate scrape
 *  runs is the one outcome nothing else pages: Mephisto Augsburg sat 404ing for 69
 *  days before a Filmweb wrapper that could never serve it happened to say so. */
class GoneVenueAlertingArchiveSpec extends AnyFlatSpec with Matchers {

  private val Start = Instant.parse("2026-09-20T08:00:00Z")
  private val Gone  = "HttpStatusException: HTTP 404 for GET https://www.filmstarts.de/kinoprogramm/kino/A1560/"

  private class Harness(withFallback: Set[Cinema] = Set.empty) {
    val pages   = ListBuffer.empty[String]
    val archive = new GoneVenueAlertingArchive(new InMemoryScrapeArchiveRepository,
      pagedElsewhere = withFallback.map(_.displayName), notify = message => { pages += message; () })
    private var run = 0
    def failRun(cinema: Cinema = KinoMuza, error: String = Gone): Unit = {
      archive.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), Start.plusSeconds(run * 36000L),
        listingComplete = true, films = Seq.empty, error = Some(error)))
      run += 1
    }
    /** A retry of the last run, a minute after it — the reaper's immediate retries. */
    def retry(cinema: Cinema = KinoMuza): Unit =
      archive.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), Start.plusSeconds((run - 1) * 36000L + 60),
        listingComplete = true, films = Seq.empty, error = Some(Gone)))
    def emptyRun(cinema: Cinema = KinoMuza): Unit = {
      archive.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), Start.plusSeconds(run * 36000L),
        listingComplete = true, films = Seq.empty))
      run += 1
    }
  }

  "GoneVenueAlertingArchive" should "page once the venue's page has been gone for three separate runs" in {
    val h = new Harness
    h.failRun(); h.failRun()
    h.pages shouldBe empty
    h.failRun()
    h.pages should have size 1
    h.pages.head should (include (KinoMuza.displayName) and include ("3 scrape runs") and include (Gone))
  }

  it should "page only once per failing spell" in {
    val h = new Harness
    (1 to 6).foreach(_ => h.failRun())
    h.pages should have size 1
  }

  // Retries of the third run keep the count AT three; paging on "is three" paged
  // once per retry — up to three times for one venue after a restart.
  it should "page once for the third run, not again for its retries" in {
    val h = new Harness
    (1 to 3).foreach(_ => h.failRun())
    h.retry(); h.retry()
    h.pages should have size 1
  }

  it should "page again for a new spell after the venue answered in between" in {
    val h = new Harness
    (1 to 3).foreach(_ => h.failRun())
    h.emptyRun()
    (1 to 3).foreach(_ => h.failRun())
    h.pages should have size 2
  }

  it should "stay quiet for a venue whose fallback already pages for it" in {
    val h = new Harness(withFallback = Set(KinoMuza))
    (1 to 3).foreach(_ => h.failRun())
    h.pages shouldBe empty
  }

  // An aggregator outage fails every venue it lists at once, and 1,518 German pages
  // would bury the one that means something. Only a page-gone answer says the venue
  // itself is the problem.
  it should "stay quiet for failures that do not say the page is gone" in {
    val h = new Harness
    (1 to 3).foreach(_ => h.failRun(error = "HttpStatusException: HTTP 503 for GET https://x/"))
    h.pages shouldBe empty
  }

  it should "count each venue's runs on their own" in {
    val h = new Harness
    h.failRun(KinoMuza); h.failRun(Multikino); h.failRun(KinoMuza); h.failRun(Multikino)
    h.pages shouldBe empty
    h.failRun(KinoMuza)
    h.pages should have size 1
  }
}
