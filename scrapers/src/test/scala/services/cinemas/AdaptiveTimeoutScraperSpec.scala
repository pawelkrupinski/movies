package services.cinemas

import tools.SpecTimeouts

import clients.tools.{FakeHttpFetch, WriteCineamo}
import tools.{DaemonExecutors, HostScrapeStats, LogCapture, RealHttpFetch}
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
      // cut at the budget: uncut, the hang ends only at the release below, after the fetch, and returns a film
      val thrown = intercept[TimeoutException](s.fetch())
      thrown.getMessage should include ("slow.pl")
      stats.deadlineFor("slow.pl", Multikino.displayName) shouldBe 120.millis // the cut scrape did NOT feed the baseline
    } finally { release.countDown(); executor.shutdown() }
  }

  // The cut interrupts the fetch in flight, which ends in InterruptedException inside RealHttpFetch. That was logged
  // as `HTTP GET … failed: InterruptedException: null`, a host failure that never happened (787 WARN lines in
  // worker-pl over 14 days), beside this decorator's own "scrape cut" line naming the real cause.
  it should "cut a scrape stalled in a real fetch without the fetch logging a host failure" in {
    // Accepts the TCP connect and never answers, so the read ends only by the cut's interrupt.
    val listener = new java.net.ServerSocket(0, 50, java.net.InetAddress.getLoopbackAddress)
    val held     = new java.util.concurrent.ConcurrentLinkedQueue[java.net.Socket]
    // Counted once the accepted socket is held: the cut returns on the test thread while this thread may not yet have
    // added it (Main #2407: `held.size` read 0 the instant the cut returned).
    val accepted = new CountDownLatch(1)
    val acceptor = new Thread(() => {
      scala.util.Try(while (true) { held.add(listener.accept()); accepted.countDown() }); ()
    })
    acceptor.setDaemon(true)
    acceptor.start()
    val executor = DaemonExecutors.virtualThreadEC("test-adaptive-timeout")
    try {
      val fetch  = new RealHttpFetch()
      // A new host's budget is the ceiling: long enough that the fetch is surely connected and reading when the cut
      // falls, however loaded the runner; the cut ends the wait, so this is the test's time, not a bound it asserts.
      val stats  = new HostScrapeStats(floor = 1.milli, ceiling = 2.seconds)
      val stalls = s"http://127.0.0.1:${listener.getLocalPort}/repertuar"
      val unwound = new CountDownLatch(1)
      val s      = new AdaptiveTimeoutScraper(
        delegate(Set("127.0.0.1")) { try fetch.get(stalls) finally unwound.countDown(); OneMovie }, stats, executor)
      val events = LogCapture.capture("tools.RealHttpFetch") {
        intercept[TimeoutException](s.fetch())
        accepted.await(SpecTimeouts.Io.toMillis, MILLISECONDS) shouldBe true // the read was on the wire when it was cut
        held.size shouldBe 1
        // The interrupted read unwinds on the executor's thread, after the cut returned: wait for it to log.
        unwound.await(SpecTimeouts.Io.toMillis, MILLISECONDS) shouldBe true
      }
      events.filter(_.getLevel.isGreaterOrEqual(ch.qos.logback.classic.Level.WARN)).map(_.getFormattedMessage) shouldBe empty
    } finally {
      executor.shutdown()
      listener.close()
      held.forEach(_.close())
    }
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
