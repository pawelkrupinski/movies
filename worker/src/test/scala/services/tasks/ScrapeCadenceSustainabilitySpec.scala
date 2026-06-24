package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog
import services.freshness.Freshness
import tools.GetOnlyHttpFetch

import java.time.LocalDate

/**
 * Guards that the SHIPPED scrape-enqueue caps keep pace with the freshness
 * setting over the REAL cinema catalogue.
 *
 * NOTE on the throttled cap: [[ScrapeReaper]] treats
 * `KINOWO_SCRAPE_THROTTLED_MAX_ENQUEUE_PER_TICK` as a bound on the OUTSTANDING
 * waiting-scrape backlog, not a per-tick drain rate. So while a SHORT throttle
 * blip keeps pace (the backlog sits near empty, so each tick tops it up by ~the
 * full cap — the `cap × ticksPerWindow ≥ corpus` capacity below), a SUSTAINED
 * throttle deliberately slows: the backlog stays bounded so the credit-starved
 * pool idles and rebuilds credit instead of staying pinned busy (the 2026-06-24
 * spiral). The capacity guard still matters — it sizes the cap big enough that the
 * common blip doesn't lag — and the corpus size is read from the live catalogue,
 * so adding cinemas faster than the cap can sustain fails HERE and forces a
 * re-tune instead of silently letting scraping lag.
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
    "be big enough that a short throttle blip still keeps pace (capacity over a window ≥ corpus)" in {
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
