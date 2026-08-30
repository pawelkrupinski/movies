package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog
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
 *
 * Checked PER COUNTRY against that country's OWN deployed window, because the
 * caps are shared but the windows are not: each worker scrapes one country and
 * reads its own `KINOWO_SCRAPE_FRESHNESS_MINUTES` (60 PL, 420 UK, 600 DE, 840
 * US). Measuring every country against `Freshness.defaultScrapeTtl` — as this
 * spec used to — models a deployment that does not exist, and it was already
 * wrong before it was ever caught: Germany's 1,529 venues sat against a
 * default-window capacity of 1,560, a 31-venue margin, on a country that in
 * fact runs a 10h window and had ~10x the room it was credited with. The US
 * made it fail outright (5,031 venues against that same 1,560) despite having
 * the LARGEST window in the fleet.
 */
class ScrapeCadenceSustainabilitySpec extends AnyFlatSpec with Matchers {

  // The catalogue builds its scraper objects without touching the network — nothing
  // fetches until tick()/handle() — so a no-op fetch is enough to count the corpus.
  private object NoFetch extends GetOnlyHttpFetch { def get(url: String): String = "" }

  // Post the per-country worker split each machine scrapes exactly ONE country's
  // cinemas (its own Mongo db + change-stream), so the caps are never asked to
  // drain the global sum — only one country's roster, within that country's own
  // window. Both halves are read from the repo rather than assumed: the corpus
  // from the live catalogue, the window from the overlay that deploys the worker.
  private val catalog = new CinemaScraperCatalog(NoFetch, LocalDate.of(2026, 6, 21))

  private def corpusOf(country: models.Country): Int =
    country.cities.flatMap(city => catalog.byCity.getOrElse(city.slug, Nil)).size

  /** Reaper ticks inside one country's freshness window. Its cadence lives ONLY in
   *  its k3s overlay (`deploy.RepoFile.deployedFreshnessMinutes`) — no `Country`
   *  field carries it and no running-JVM test can reach it — so a country missing
   *  that config fails loudly here rather than silently borrowing a default window
   *  it does not run at. */
  private def ticksPerWindowOf(country: models.Country): Int = {
    val minutes = deploy.RepoFile
      .deployedFreshnessMinutes(country.code)
      .getOrElse(fail(s"no KINOWO_SCRAPE_FRESHNESS_MINUTES for '${country.code}' — " +
        "every country's worker overlay must set its own cadence"))
    (minutes.toLong * 60000L / ScrapeCadence.ReaperTickInterval.toMillis).toInt
  }

  private def capacityOf(country: models.Country, cap: Int): Long =
    cap.toLong * ticksPerWindowOf(country)

  /** Every country, each carrying the two numbers its own worker actually runs. */
  private def countries: Seq[(models.Country, Int, Int)] =
    models.Country.all.map(c => (c, corpusOf(c), ticksPerWindowOf(c)))

  "the throttled scrape cap" should
    "be big enough that a short throttle blip still keeps pace in EVERY country" in {
    countries.foreach { case (country, corpus, ticks) =>
      val capacity = capacityOf(country, ScrapeCadence.ThrottledMaxEnqueuePerTick)
      withClue(s"${country.code}: corpus=$corpus, ticks/window=$ticks, " +
        s"throttledCap=${ScrapeCadence.ThrottledMaxEnqueuePerTick}, capacity=$capacity: ") {
        capacity should be >= corpus.toLong
      }
    }
  }

  it should "stay below the healthy cap so the worker pool still earns idle to rebuild CPU credit" in {
    ScrapeCadence.ThrottledMaxEnqueuePerTick should be < ScrapeCadence.MaxEnqueuePerTick
  }

  "the healthy scrape cap" should
    "clear EVERY country's catalogue within its own window with >=1.5x headroom" in {
    countries.foreach { case (country, corpus, ticks) =>
      val capacity = capacityOf(country, ScrapeCadence.MaxEnqueuePerTick)
      withClue(s"${country.code}: corpus=$corpus, ticks/window=$ticks, " +
        s"healthyCap=${ScrapeCadence.MaxEnqueuePerTick}, capacity=$capacity: ") {
        capacity should be >= (corpus.toLong * 3 / 2)
      }
    }
  }
}
