package services.cinemas

import tools.SpecTimeouts

import clients.tools.{FakeHttpFetch, WriteCineamo}
import tools.{DaemonExecutors, HostScrapeStats}
import models.{CinemaMovie, GermanCinema, Multikino}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.ScriptedCinemaScraper.OneMovie
import services.cinemas.common.{AdaptiveTimeoutScraper, CineamoClient, CinemaScraper}

import java.time.LocalDate

import java.util.concurrent.{CountDownLatch, TimeoutException}
import scala.concurrent.duration._

class AdaptiveTimeoutScraperSpec extends AnyFlatSpec with Matchers {

  private def delegate(hosts: Set[String])(body: => Seq[CinemaMovie]): CinemaScraper =
    new StubCinemaScraper(Multikino, body, scrapeHosts = hosts)

  "hostKey" should "be the sorted host set so a chain's venues pool together" in {
    val s = new AdaptiveTimeoutScraper(delegate(Set("b.pl", "a.pl"))(OneMovie), new HostScrapeStats(), DaemonExecutors.directExecutor())
    s.hostKey shouldBe "a.pl,b.pl"
  }

  it should "fall back to the cinema name when the scraper declares no host" in {
    val s = new AdaptiveTimeoutScraper(delegate(Set.empty)(OneMovie), new HostScrapeStats(), DaemonExecutors.directExecutor())
    s.hostKey shouldBe Multikino.displayName
  }

  "fetch" should "pass the result through and record the scrape's duration in the stats" in {
    val stats = new HostScrapeStats(minSamples = 1, floor = 1.milli, ceiling = 5.seconds)
    val s     = new AdaptiveTimeoutScraper(delegate(Set("kino.pl"))(OneMovie), stats, DaemonExecutors.directExecutor())
    stats.deadlineFor("kino.pl", Multikino.displayName) shouldBe 5.seconds // warm-up ceiling, nothing recorded yet
    s.fetch() shouldBe OneMovie
    stats.deadlineFor("kino.pl", Multikino.displayName) should be < 5.seconds // a sample was recorded → budget tightened off the ceiling
  }

  it should "cut a scrape that overruns its budget and throw a TimeoutException naming the host" in {
    // ceiling 120ms is the warm-up budget; the delegate hangs far past it — until the
    // test releases it, so the abandoned scrape doesn't outlive the test.
    val stats    = new HostScrapeStats(floor = 1.milli, ceiling = 120.millis)
    val executor = DaemonExecutors.virtualThreadEC("test-adaptive-timeout")
    val release  = new CountDownLatch(1)
    try {
      val s = new AdaptiveTimeoutScraper(delegate(Set("slow.pl")) { release.await(SpecTimeouts.Io.toMillis, MILLISECONDS); OneMovie }, stats, executor)
      val t0 = System.nanoTime() / 1000000
      val thrown = intercept[TimeoutException](s.fetch())
      val elapsed = System.nanoTime() / 1000000 - t0
      thrown.getMessage should include ("slow.pl")
      elapsed should be < 2000L                       // returned at the budget, not after the 3s hang
      stats.deadlineFor("slow.pl", Multikino.displayName) shouldBe 120.millis // the cut scrape did NOT feed the baseline
    } finally { release.countDown(); executor.shutdown() }
  }

  it should "propagate the delegate's own exception unwrapped (so retry/uptime see the real cause)" in {
    val s = new AdaptiveTimeoutScraper(
      delegate(Set("kino.pl"))(throw new IllegalStateException("boom")),
      new HostScrapeStats(),
      DaemonExecutors.directExecutor())
    intercept[IllegalStateException](s.fetch()).getMessage shouldBe "boom"
  }

  // Cineamo, 2026-10-12: one host, ~800 German venues of ten showings a page. Its warm
  // baseline is a small venue's scrape, and every multiplex forty pages deep was cut against
  // it on every run — UCI Bochum and Kinopolis Bad Godesberg were never once scraped. Here the
  // real recorded Cineworld Dettelbach programme (fifteen pages) against a host whose usual
  // scrape is instant, so its budget is the 1ms floor no fifteen-page read finishes inside.
  "a multiplex on a host of small venues" should "finish its first scrape, and be timed against its own size after" in {
    val stats = new HostScrapeStats(minSamples = 5, multiplier = 2.0, floor = 1.milli, ceiling = 10.seconds)
    (1 to 20).foreach(i => stats.record("api.cineamo.com", s"village-$i", 0))
    val (name, id) = WriteCineamo.Venues.find(_._1.contains("Dettelbach")).get
    val executor   = DaemonExecutors.virtualThreadEC("test-adaptive-multiplex")
    try {
      val s = new AdaptiveTimeoutScraper(
        new CineamoClient(new FakeHttpFetch("cineamo"), id, new GermanCinema(name, name), today = LocalDate.of(2026, 10, 11)), stats, executor)
      s.hostKey shouldBe "api.cineamo.com"
      s.fetch().flatMap(_.showtimes) should not be empty // cut at the host's 1ms budget before
      stats.deadlineFor(s.hostKey, name) should be > stats.deadlineFor(s.hostKey, "village-1")
    } finally executor.shutdown()
  }
}
