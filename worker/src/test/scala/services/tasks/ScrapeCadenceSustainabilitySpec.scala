package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog
import services.freshness.Freshness
import tools.GetOnlyHttpFetch

import java.time.LocalDate

/**
 * Guards that the SHIPPED scrape-enqueue caps keep pace with the freshness
 * setting over the REAL cinema catalogue — even while the CPU-credit safety net
 * is engaged. This is the regression for raising
 * `KINOWO_SCRAPE_THROTTLED_MAX_ENQUEUE_PER_TICK` from 3 (which drained the
 * ~290-cinema corpus only every ~97 ticks, ~1.5h out of date) to a value that
 * fits one window.
 *
 * Capacity over a freshness window = `cap × ticksPerWindow`; to never fall behind
 * it must cover the whole corpus. The corpus size is read from the live catalogue,
 * so adding cinemas faster than the cap can sustain fails HERE and forces a
 * re-tune (the "remember for the future" guard) instead of silently letting
 * scraping lag.
 */
class ScrapeCadenceSustainabilitySpec extends AnyFlatSpec with Matchers {

  // The catalogue builds its scraper objects without touching the network — nothing
  // fetches until tick()/handle() — so a no-op fetch is enough to count the corpus.
  private object NoFetch extends GetOnlyHttpFetch { def get(url: String): String = "" }

  private val corpusSize: Int =
    new CinemaScraperCatalog(NoFetch, LocalDate.of(2026, 6, 21)).all.size

  private val ticksPerWindow: Int =
    (Freshness.defaultScrapeTtl.toMillis / ScrapeCadence.ReaperTickInterval.toMillis).toInt

  private def capacityPerWindow(cap: Int): Long = cap.toLong * ticksPerWindow

  "the throttled scrape cap" should
    "drain the whole catalogue within one freshness window, so a throttle episode can't park the corpus stale" in {
    withClue(s"corpus=$corpusSize, ticks/window=$ticksPerWindow, " +
      s"throttledCap=${ScrapeCadence.ThrottledMaxEnqueuePerTick}, " +
      s"capacity=${capacityPerWindow(ScrapeCadence.ThrottledMaxEnqueuePerTick)}: ") {
      capacityPerWindow(ScrapeCadence.ThrottledMaxEnqueuePerTick) should be >= corpusSize.toLong
    }
  }

  it should "stay below the healthy cap so the worker pool still earns idle to rebuild CPU credit" in {
    ScrapeCadence.ThrottledMaxEnqueuePerTick should be < ScrapeCadence.MaxEnqueuePerTick
  }

  "the healthy scrape cap" should
    "clear the catalogue within one window with comfortable (≥1.5×) headroom over the corpus" in {
    withClue(s"corpus=$corpusSize, ticks/window=$ticksPerWindow, " +
      s"healthyCap=${ScrapeCadence.MaxEnqueuePerTick}, " +
      s"capacity=${capacityPerWindow(ScrapeCadence.MaxEnqueuePerTick)}: ") {
      capacityPerWindow(ScrapeCadence.MaxEnqueuePerTick) should be >= (corpusSize.toLong * 3 / 2)
    }
  }
}
