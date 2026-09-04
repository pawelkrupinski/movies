package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.movies.StoredMovieRecord

/**
 * How many cinema slots the WIDEST film in the corpus carries — the blast radius of a
 * single film's write, and the one number that says in advance how expensive this
 * country's worst case is.
 *
 * WHY A MAXIMUM AND NOT AN AVERAGE. Every write path in the read-split is per-FILM:
 * `MovieRepository.upsert` re-stitches a film, `ScreeningsRepository.replaceFilm` writes a
 * film's rows, and the `screenings` cursor rings once per row written. So the cost of one
 * venue changing one showtime is the SLOT COUNT OF THE FILM IT CHANGED, not the corpus
 * average — and the distribution is extremely long-tailed. Measured 2026-09-04: Germany's
 * mean is ~16 venues per film while its widest sits at 698, and the United States averages
 * ~53 against a widest of 3,327 (`coyotevsacme|2026`). An average of 53 says this is cheap;
 * the maximum says one changed showtime on one film can cost three thousand projections.
 *
 * `changedSlots` removed the REDUNDANT part of that cost — the rows a whole-film write
 * touched without changing. It cannot remove the rest: a wide release that genuinely does
 * change everywhere still writes every row it has, and still buys a projection for each.
 * This gauge is what watches the part the fix does not cover, and what will say — before it
 * bites — that a new market has landed a film wider than anything the pipeline has carried.
 *
 * Counted off the SHARED [[WorkerCorpusScan]] pass (default every 5 min) like its three
 * sibling censuses, so it costs no reads of its own — it reads only each row's slot count
 * and ignores the showtimes the pass stitches. Mirrors [[WorkerCorpusMetrics]]' shape,
 * including its refusal to publish a partial pass.
 */
class WorkerSlotFanoutMetrics(widest: Gauge, countryCode: String) extends CorpusMetricsCollector {

  // Seed at 0 so the series exists from boot — a country whose corpus empties must read as
  // an explicit 0, not as a vanished series.
  widest.labelValues(countryCode).set(0.0)

  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private var max = 0

    // The row's OWN slot map, not the projection's: it is what `replaceFilm` writes and
    // therefore what the change stream rings for. A row held back from the read model still
    // has its slots written, so this deliberately does not gate on `readyToProject` the way
    // the served-films censuses do.
    def accept(row: StoredMovieRecord): Unit = if (row.record.data.size > max) max = row.record.data.size

    /** Publishes ONLY a complete census, for [[WorkerCorpusMetrics]]' reason and one of its
     *  own: a MAXIMUM over a truncated scan is not a smaller maximum, it is the maximum of
     *  whichever rows the scan happened to reach, and published as a gauge the two look
     *  identical. Skipping leaves the last good value and `WorkerCorpusScan` counts the miss. */
    def publish(scanComplete: Boolean): Unit = if (scanComplete) widest.labelValues(countryCode).set(max.toDouble)
  }
}

object WorkerSlotFanoutMetrics {
  val Name = "kinowo_worker_film_widest_slots"

  /** Build and register the ONE shared gauge every country's sampler writes into. Called
   *  once when the shared worker registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Cinema slots carried by the WIDEST film in the country's movies corpus — the blast radius of one film's write. Every write path in the read-split is per-film, and the screenings change stream rings once per row written, so one venue changing one showtime costs the slot count of the film it changed: 3,327 for the widest US film against a ~53 average. A maximum, not an average, because the distribution is long-tailed and only the tail is expensive.")
      .labelNames("country")
      .register(registry)
}
