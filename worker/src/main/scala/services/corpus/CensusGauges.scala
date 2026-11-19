package services.corpus

import io.prometheus.metrics.core.metrics.Gauge
import models.City

import java.time.Instant

/** The census gauges of one country's `cities`: the corpus subsets, the films served by (city, scope), the upcoming
 *  showtimes by city, the widest film, and each city's off-season verdict. */
private[corpus] final class CensusGauges(corpus: Gauge, served: Gauge, showtimes: Gauge, widest: Gauge, mayBeDark: Gauge,
                                         countryCode: String, cities: Seq[City]) {

  /** Every (city, scope) and city at 0, so a city that empties reads as an explicit 0, not a vanished series. The
   *  corpus subsets are not: a 0 there reads as an empty corpus, and every restart drew the coverage chart to 0 and
   *  back. Each appears with the first complete census instead. */
  def seeded(): Unit = {
    for (c <- cities) {
      CensusSubsets.ServedScopes.foreach(scope => served.labelValues(countryCode, c.slug, scope).set(0.0))
      showtimes.labelValues(countryCode, c.slug).set(0.0)
    }
    widest.labelValues(countryCode).set(0.0)
  }

  /** Each city's [[services.metrics.WorkerCitySeasonMetrics]] verdict, off the clock alone — whatever the corpus holds. */
  def seasons(now: Instant): Unit =
    cities.foreach(c => mayBeDark.labelValues(countryCode, c.slug).set(if (c.mayBeDarkAt(now)) 1.0 else 0.0))

  def published(now: CensusReading): Unit = {
    now.corpus.foreach { case (subset, n) => corpus.labelValues(countryCode, subset).set(n.toDouble) }
    for (c <- cities) {
      CensusSubsets.ServedScopes.foreach(scope =>
        served.labelValues(countryCode, c.slug, scope).set(now.served.getOrElse((c.slug, scope), 0).toDouble))
      showtimes.labelValues(countryCode, c.slug).set(now.showtimes.getOrElse(c.slug, 0).toDouble)
    }
    widest.labelValues(countryCode).set(now.widest.toDouble)
  }
}
