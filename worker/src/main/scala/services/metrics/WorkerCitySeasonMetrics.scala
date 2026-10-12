package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * Per city, whether every one of its venues may be dark by design ([[models.City.mayBeDarkAt]]):
 * 1 for a town whose only screens are an open-air cinema or a drive-in, off their season or in
 * their window's final month — the window is whole months, and a programme can end any day of its
 * last one; 0 for every other city. Beside [[WorkerSourceFilmsMetrics]] on the same `{country, city}` labels, because
 * its one reader is `CityDarkInBothTiers`: a city that served films all summer and then none
 * after its season closed is dark by design, and the alert stands down for exactly the cities
 * this reads 1 for. A city with any year-round venue reads 0, so it alerts as before.
 *
 * Published by [[CorpusCensus]] at every tick, off the calendar alone — it needs no corpus, so
 * it is written even on a tick that holds the census gauges back.
 */
object WorkerCitySeasonMetrics {
  val Name = "kinowo_worker_city_off_season"

  /** Build and register the ONE shared gauge every country's census writes into. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("1 when every venue of the city is a seasonal one (open-air, summer, drive-in) outside its season or in its season window's final month, else 0, per country and city. Such a city may be dark by design, and CityDarkInBothTiers does not fire for it.")
      .labelNames("country", "city")
      .register(registry)
}
