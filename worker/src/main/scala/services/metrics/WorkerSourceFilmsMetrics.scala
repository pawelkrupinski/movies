package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{City, CityScreening}
import services.movies.StoredMovieRecord
import services.readmodel.ReadModelProjection

import java.time.{Clock, LocalDateTime}
import scala.util.{Failure, Success, Try}

/**
 * Per-city census of how many films the SOURCE `movies` collection would serve —
 * the worker-side mirror of the web's [[controllers.WebMovieMetrics]]
 * (`kinowo_web_movies_served`), which counts the same thing off the READ MODEL the
 * web actually serves. Both gauges carry identical `{city,scope}` labels and apply
 * the identical future/tomorrow rule ([[models.Showtime.isUpcoming]]), so a Grafana
 * panel plots them side by side: when `kinowo_worker_movies_served` and
 * `kinowo_web_movies_served` diverge for a city, the projection or a read-model
 * write has drifted from what the corpus holds — the exact signal the read-model
 * outage (a malformed `web_movies` doc silently empties a city) needs.
 *
 * Apples-to-apples by construction: the count runs each row through the REAL
 * projection's screenings path ([[ReadModelProjection.screeningsAll]] — the screenings
 * half of `projectAll`, same per-title card split + cinema→city bucketing, minus the
 * unused `ResolvedMovie` metadata) and gates on the same `readyToProject` predicate the
 * projector writes by, so a film still pending TMDB enrichment is absent from BOTH
 * sides rather than inflating the source count.
 *
 * Counted off the SHARED [[WorkerCorpusScan]] pass (default every 5 min), decoupled
 * from the Fly scrape rate and read-only, so it adds no write load and no reads of its
 * own; between samples the gauge re-reads its cached value. Mirrors
 * [[WorkerCorpusMetrics]]' sample-and-cache shape.
 */
class WorkerSourceFilmsMetrics(
  served:      Gauge,
  countryCode: String,
  clock:       Clock     = Clock.systemDefaultZone(),
  cities:      Seq[City] = City.all
) extends CorpusMetricsCollector {
  import WorkerSourceFilmsMetrics._

  // Seed every (city, scope) at 0 so a city that empties reads as an explicit 0,
  // not a vanished series — the swing/floor alerts need the zero present.
  for (c <- cities; scope <- Scope.all) served.labelValues(countryCode, c.slug, scope).set(0.0)

  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private val tally = new FilmTally(cities, clock)

    def accept(row: StoredMovieRecord): Unit = tally.accept(row)

    /** Publishes on a partial scan too — matches what this gauge did before the scan
     *  was shared (it ignored the scan's completeness boolean). */
    def publish(scanComplete: Boolean): Unit = {
      val counts = tally.counts
      for (c <- cities; scope <- Scope.all)
        served.labelValues(countryCode, c.slug, scope).set(counts.getOrElse((c.slug, scope), 0).toDouble)
    }
  }
}

object WorkerSourceFilmsMetrics {
  /** Paired with the web's `kinowo_web_movies_served`: same suffix, same city/scope
   *  labels (plus the worker's leading `country`), worker-vs-web prefix — so Grafana
   *  overlays the two as source-vs-read-model. */
  val Name = "kinowo_worker_movies_served"

  /** Build and register the ONE shared gauge every country's sampler writes into
   *  (leading `country` label, then `city`, `scope`). Called once when the shared
   *  worker registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Films the source `movies` collection would serve per country and city, by scope (all = any future showing, tomorrow = showing tomorrow) — the projection-side mirror of the web's kinowo_web_movies_served, for spotting read-model drift.")
      .labelNames("country", "city", "scope")
      .register(registry)

  /** Scope label values, matching `kinowo_web_movies_served`'s scopes exactly. */
  object Scope {
    val All      = "all"
    val Tomorrow = "tomorrow"
    val all: Seq[String] = Seq(All, Tomorrow)
  }

  /** Running tally, per (city slug, scope), of how many ready films the corpus would
   *  serve — folded one row at a time so the shared scan never buffers the corpus.
   *  Pure given a fixed `clock`, so it's unit-tested directly (see [[countAll]]). Each
   *  row is projected exactly as the read model does (per-title split + cinema→city
   *  bucketing) and gated on `readyToProject`; a row that fails to project is skipped,
   *  matching the projector's per-row resilience. */
  class FilmTally(cities: Seq[City], clock: Clock) {
    private val bySlug = cities.map(c => c.slug -> c).toMap
    private val acc    = scala.collection.mutable.Map.empty[(String, String), Int].withDefaultValue(0)

    def accept(stored: StoredMovieRecord): Unit =
      if (stored.record.readyToProject)
        // Only the SCREENINGS half is needed to count qualifying cards per city;
        // `screeningsAll` skips the `resolve`/synopsis/ratings materialisation
        // `projectAll` does (this census re-reads the whole corpus every 5 min,
        // and that metadata work was the worker's single biggest CPU consumer).
        Try(ReadModelProjection.screeningsAll(stored)) match {
          case Success(cards) =>
            cards.foreach { screenings =>
              qualifyingKeys(screenings, bySlug, clock).foreach(key => acc(key) += 1)
            }
          case Failure(_) => () // a row that won't project simply doesn't count
        }

    def counts: Map[(String, String), Int] = acc.toMap
  }

  /** The tally over a fixed set of rows — the pure-logic entry point the specs drive;
   *  production folds the same [[FilmTally]] row-by-row off the shared scan. */
  def countAll(rows: IterableOnce[StoredMovieRecord], cities: Seq[City], clock: Clock): Map[(String, String), Int] = {
    val tally = new FilmTally(cities, clock)
    rows.iterator.foreach(tally.accept)
    tally.counts
  }

  /** The (city slug, scope) keys ONE projected card qualifies for: `all` for each
   *  city it has any upcoming showtime in, `tomorrow` for each city it shows in on
   *  that city's local tomorrow. The two scopes are independent — a card can hit
   *  both — and a city the card never plays in contributes nothing. */
  private def qualifyingKeys(screenings: Seq[CityScreening],
                             bySlug:     Map[String, City],
                             clock:      Clock): Set[(String, String)] =
    // `.toSeq` before flatMap: a Map#flatMap returning (citySlug, scope) pairs would
    // rebuild a Map keyed by citySlug, collapsing a city's `all` and `tomorrow` keys
    // into one (last wins). A Seq keeps both.
    screenings.groupBy(_.city).toSeq.flatMap { case (citySlug, scs) =>
      bySlug.get(citySlug).toSeq.flatMap { city =>
        val now       = LocalDateTime.now(clock.withZone(city.zoneId))
        val tomorrow  = now.toLocalDate.plusDays(1)
        val showtimes = scs.flatMap(_.showtimes)
        Seq(
          Option.when(showtimes.exists(_.isUpcoming(now)))(citySlug -> Scope.All),
          Option.when(showtimes.exists(_.dateTime.toLocalDate == tomorrow))(citySlug -> Scope.Tomorrow)
        ).flatten
      }
    }.toSet
}
