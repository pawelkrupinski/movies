package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The share-card budget is per country and fixed, so a country whose roster grows outgrows it: on
 *  2026-10-11 Germany's new venues took its corpus from 1,890 films to 4,750 overnight, still rising
 *  ~230 an hour and slowing, and its CURRENT files (the card, base and poster nothing may evict)
 *  from 0.80 GB to 2.23 GB — ShareCardsOverBudget against the 1 GiB of the time, and on course for
 *  ShareCardsNearBudget against the 3 GiB after it. */
class ShareCardBudgetSpec extends AnyFlatSpec with Matchers {

  /** A film's current files on 2026-10-12 in Germany: 2.23 GB over 4,750 films. */
  private val MeasuredBytesPerFilm = 470L * 1000
  /** Where Germany's corpus was heading on 2026-10-12, its hourly growth falling ~40 every 3 hours. */
  private val GermanyPlateauFilms = 6500L
  /** ShareCardsNearBudget's threshold (share-cards.rules). */
  private val NearBudget = 0.85

  "The default share-card budget" should "hold Germany's grown corpus without warning it is near" in {
    val budgetBytes = ShareCardWiring.DefaultBudgetMb * 1024 * 1024
    (GermanyPlateauFilms * MeasuredBytesPerFilm).toDouble should be < NearBudget * budgetBytes
  }

  it should "leave the node room for every country at it, above the cards' free-space floor" in {
    val countries = 5   // pl, uk, de, es, us share k3s-worker-1's 160 GB root
    val rootBytes = 160L * 1000 * 1000 * 1000
    countries * ShareCardWiring.DefaultBudgetMb + ShareCardWiring.DefaultMinimumFreeMb should be < rootBytes / (1024 * 1024) / 4
  }
}
