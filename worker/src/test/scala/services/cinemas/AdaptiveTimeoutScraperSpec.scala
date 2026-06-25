package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ScriptedCinemaScraper.OneMovie
import tools.{DaemonExecutors, HostScrapeStats}

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

class AdaptiveTimeoutScraperSpec extends AnyFlatSpec with Matchers {

  private def delegate(
    hosts: Set[String],
    who:   Cinema = Multikino
  )(body: => Seq[CinemaMovie]): CinemaScraper = new CinemaScraper {
    val cinema: Cinema           = who
    def scrapeHosts: Set[String] = hosts
    def fetch(): Seq[CinemaMovie] = body
  }

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
    stats.deadlineFor("kino.pl") shouldBe 5.seconds // warm-up ceiling, nothing recorded yet
    s.fetch() shouldBe OneMovie
    stats.deadlineFor("kino.pl") should be < 5.seconds // a sample was recorded → budget tightened off the ceiling
  }

  it should "cut a scrape that overruns its budget and throw a TimeoutException naming the host" in {
    // ceiling 120ms is the warm-up budget; the delegate sleeps far past it.
    val stats    = new HostScrapeStats(floor = 1.milli, ceiling = 120.millis)
    val executor = DaemonExecutors.virtualThreadEC("test-adaptive-timeout")
    try {
      val s = new AdaptiveTimeoutScraper(delegate(Set("slow.pl")) { Thread.sleep(3000); OneMovie }, stats, executor)
      val t0 = System.currentTimeMillis()
      val thrown = intercept[TimeoutException](s.fetch())
      val elapsed = System.currentTimeMillis() - t0
      thrown.getMessage should include ("slow.pl")
      elapsed should be < 2000L                       // returned at the budget, not after the 3s sleep
      stats.deadlineFor("slow.pl") shouldBe 120.millis // the cut scrape did NOT feed the baseline
    } finally executor.shutdown()
  }

  it should "propagate the delegate's own exception unwrapped (so retry/uptime see the real cause)" in {
    val s = new AdaptiveTimeoutScraper(
      delegate(Set("kino.pl"))(throw new IllegalStateException("boom")),
      new HostScrapeStats(),
      DaemonExecutors.directExecutor())
    intercept[IllegalStateException](s.fetch()).getMessage shouldBe "boom"
  }
}
