package modules.wiring

import models.{Cinema, CinemaMovie, KinoMikro}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.fallback.{FallbackEvent, FallbackState, FallbackStore, InMemoryFallbackStore}
import tools.{FixtureTestWiring, MutableClock, TestWiring}

import scala.util.Try
import scala.concurrent.duration._
import tools.HostScrapeStats

/** Which fallback `recordingScraper` gives a venue. A wrapped venue shows up as a
 *  fallback-state row as soon as its primary fails, so that row is the probe. */
class ScrapeWiringFallbackSpec extends AnyFlatSpec with Matchers {

  private val failing: CinemaScraper = new CinemaScraper {
    val cinema = KinoMikro
    def scrapeHosts: Set[String] = Set("example.test")
    def fetch(): Seq[CinemaMovie] = throw new tools.HttpStatusException(404, "GET", "https://example.test/", None)
  }

  private class Wiring(filmweb: Boolean, kinoprogramm: Map[Cinema, String] = Map.empty) {
    // Runs an hour apart are separate runs; retries minutes apart are not (SeparateRuns).
    private val testClock = new MutableClock(TestWiring.FixedInstant)
    val wiring = new FixtureTestWiring("08-06-2026") {
      override lazy val clock: java.time.Clock = testClock
      override protected def filmwebEnabled: Boolean = filmweb
      override protected def kinoprogrammFallbackPaths: Map[Cinema, String] = kinoprogramm
      override lazy val filmwebFallbackStore: FallbackStore = new InMemoryFallbackStore
    }
    private val scraper = wiring.recordingScraper(failing, eligible = true)
    def failRun(): Unit = { Try(scraper.fetch()); testClock.advance(java.time.Duration.ofHours(1)) }
    def state: Option[FallbackState] = wiring.filmwebFallbackStore.get(KinoMikro.displayName)
  }

  // Filmweb is a Polish aggregator: outside a Filmweb country the wrapper had no
  // Filmweb id to fall back to, and all it did was page "Filmweb has nothing to
  // serve" for a German venue Filmweb never could have covered (Mephisto Augsburg,
  // 2026-09-26).
  "recordingScraper" should "give an eligible venue a Filmweb fallback in a Filmweb country" in {
    val w = new Wiring(filmweb = true)
    w.failRun()
    w.state.map(_.fallbackSource) shouldBe Some("Filmweb")
  }

  it should "give no Filmweb fallback outside a Filmweb country" in {
    val w = new Wiring(filmweb = false)
    w.failRun()
    w.state shouldBe None
  }

  // German venues are scraped ~10-hourly, so a 6h window would hand a venue over on
  // its second failure. Kinoprogramm waits for three SEPARATE failed runs instead —
  // an hour apart here, far inside a 6h window, so only a run count can trip it.
  it should "give a venue kinoprogramm.com lists a Kinoprogramm fallback, after three failed runs" in {
    val w = new Wiring(filmweb = false, kinoprogramm = Map(KinoMikro -> "/kino/somewhere/kino-mikro-1"))
    w.failRun(); w.failRun()
    w.state.map(_.fallbackSource) shouldBe Some("Kinoprogramm")
    w.state.map(_.fallbackRef) shouldBe Some(Some("/kino/somewhere/kino-mikro-1"))
    w.state.map(_.history) shouldBe Some(Nil)   // two failures: still riding it out
    w.failRun()
    // The third reaches for the fallback; the fixture corpus has no kinoprogramm.com
    // page, so it has nothing to serve and the venue pages UNCOVERED instead of ENTER.
    w.state.flatMap(_.history.headOption).map(_.event) shouldBe Some(FallbackEvent.Uncovered)
  }

  // A fallback is fetched in the scrape's own call, outside the per-scrape adaptive timeout a
  // primary gets: a feed whose requests stall (a Flicks fallback walks every day tab in turn)
  // held the scrape slot for as long as all its request timeouts summed.
  it should "cut a fallback that runs past its budget, and page the venue uncovered" in {
    val stalling = new tools.GetOnlyHttpFetch {
      override def get(url: String): String = { if (url.contains("kinoprogramm.com")) Thread.sleep(5000); "" }
    }
    val testClock = new MutableClock(TestWiring.FixedInstant)
    val wiring = new FixtureTestWiring("08-06-2026") {
      override lazy val clock: java.time.Clock = testClock
      override protected def filmwebEnabled: Boolean = false
      override protected def kinoprogrammFallbackPaths: Map[Cinema, String] = Map(KinoMikro -> "/kino/somewhere/kino-mikro-1")
      override lazy val filmwebFallbackStore: FallbackStore = new InMemoryFallbackStore
      override lazy val httpFetch: tools.HttpFetch = stalling
      override lazy val fallbackScrapeStats: HostScrapeStats = new HostScrapeStats(minSamples = 1, floor = 50.millis, ceiling = 200.millis)
      override protected lazy val adaptiveTimeoutExecutor: java.util.concurrent.ExecutorService =
        tools.DaemonExecutors.virtualThreadEC("fallback-spec")
    }
    val scraper = wiring.recordingScraper(failing, eligible = true)
    object w {
      def failRun(): Unit = { Try(scraper.fetch()); testClock.advance(java.time.Duration.ofHours(1)) }
      def state: Option[FallbackState] = wiring.filmwebFallbackStore.get(KinoMikro.displayName)
    }
    w.failRun(); w.failRun()
    val started = System.nanoTime()
    w.failRun()
    (System.nanoTime() - started).nanos should be < 3.seconds
    w.state.flatMap(_.history.headOption).map(_.event) shouldBe Some(FallbackEvent.Uncovered)
  }
}
