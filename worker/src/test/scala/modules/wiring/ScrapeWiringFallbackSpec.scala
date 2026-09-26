package modules.wiring

import models.{Cinema, CinemaMovie, KinoMikro}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.fallback.{FallbackEvent, FallbackState, FallbackStore, InMemoryFallbackStore}
import tools.FixtureTestWiring

import scala.util.Try

/** Which fallback `recordingScraper` gives a venue. A wrapped venue shows up as a
 *  fallback-state row as soon as its primary fails, so that row is the probe. */
class ScrapeWiringFallbackSpec extends AnyFlatSpec with Matchers {

  private val failing: CinemaScraper = new CinemaScraper {
    val cinema = KinoMikro
    def scrapeHosts: Set[String] = Set("example.test")
    def fetch(): Seq[CinemaMovie] = throw new RuntimeException("HTTP 404")
  }

  private class Wiring(filmweb: Boolean, kinoprogramm: Map[Cinema, String] = Map.empty) {
    val wiring = new FixtureTestWiring("08-06-2026") {
      override protected def filmwebEnabled: Boolean = filmweb
      override protected def kinoprogrammFallbackPaths: Map[Cinema, String] = kinoprogramm
      override lazy val filmwebFallbackStore: FallbackStore = new InMemoryFallbackStore
    }
    private val scraper = wiring.recordingScraper(failing, eligible = true)
    def failRun(): Unit = { Try(scraper.fetch()); () }
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
  // and the harness clock never moves here, so only a run count can trip it.
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
}
