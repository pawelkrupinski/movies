package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

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
 * THOSE FOUR NUMBERS WERE MEASURED UNDER THE OLD ALL-SOURCES COUNT and are therefore each two or
 * three slots high: until 2026-09-06 this counted every source on the film, metadata included.
 * They are kept because the argument they support is about orders of magnitude, and re-measuring
 * only moves 3,327 to about 3,324. Read them as a ceiling.
 *
 * `changedSlots` removed the REDUNDANT part of that cost — the rows a whole-film write
 * touched without changing. It cannot remove the rest: a wide release that genuinely does
 * change everywhere still writes every row it has, and still buys a projection for each.
 * This gauge is what watches the part the fix does not cover, and what will say — before it
 * bites — that a new market has landed a film wider than anything the pipeline has carried.
 *
 * CINEMA SLOTS ONLY (`MovieRecord.cinemaSlotCount`): this counted every source until
 * 2026-09-06, which added the Tmdb / Imdb / Filmweb metadata slots, and only cinema slots
 * become `screenings` rows. A slot counts whether or not it holds showtimes, and a row held
 * back from the read model counts too: its slots are written all the same.
 *
 * Counted by [[CorpusCensus]] from the films the worker's cache holds, like its three
 * sibling censuses, including their refusal to publish a partial corpus.
 */
object WorkerSlotFanoutMetrics {
  val Name = "kinowo_worker_film_widest_slots"

  /** Build and register the ONE shared gauge every country's census writes into. Called
   *  once when the shared worker registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Cinema slots carried by the WIDEST film in the country's movies corpus — the blast radius of one film's write. Every write path in the read-split is per-film, and the screenings change stream rings once per row written, so one venue changing one showtime costs the slot count of the film it changed: about 3,327 for the widest US film against a ~53 average (measured 2026-09-04, when this counted metadata sources too, so both are a couple high). A maximum, not an average, because the distribution is long-tailed and only the tail is expensive.")
      .labelNames("country")
      .register(registry)
}
