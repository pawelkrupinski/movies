package modules.wiring

import models.{CinemaMovie, KinoMikro}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.fallback.{FallbackStore, InMemoryFallbackStore}
import tools.FixtureTestWiring

import scala.util.Try

/** Filmweb is a Polish aggregator, so only a Filmweb country gives its single venues a
 *  Filmweb fallback. Elsewhere the wrapper has no Filmweb id to fall back to, and all
 *  it did was page "Filmweb has nothing to serve" for a German venue that Filmweb
 *  never could have covered (Mephisto Augsburg, 2026-09-26). A wrapped venue shows up
 *  as a fallback-state row as soon as its primary fails, so that row is the probe. */
class ScrapeWiringFilmwebFallbackSpec extends AnyFlatSpec with Matchers {

  private val failing: CinemaScraper = new CinemaScraper {
    val cinema = KinoMikro
    def scrapeHosts: Set[String] = Set("example.test")
    def fetch(): Seq[CinemaMovie] = throw new RuntimeException("HTTP 404")
  }

  private def fallbackStateAfterFailure(filmweb: Boolean) = {
    val wiring = new FixtureTestWiring("08-06-2026") {
      override protected def filmwebEnabled: Boolean = filmweb
      override lazy val filmwebFallbackStore: FallbackStore = new InMemoryFallbackStore
    }
    Try(wiring.recordingScraper(failing, eligible = true).fetch())
    wiring.filmwebFallbackStore.get(KinoMikro.displayName)
  }

  "recordingScraper" should "give an eligible venue a Filmweb fallback in a Filmweb country" in {
    fallbackStateAfterFailure(filmweb = true).map(_.fallbackSource) shouldBe Some("Filmweb")
  }

  it should "give no Filmweb fallback outside a Filmweb country" in {
    fallbackStateAfterFailure(filmweb = false) shouldBe None
  }
}
