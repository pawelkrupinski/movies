package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.adapters.{IdentityModelMetrics, ModelBatch}

/**
 * The incremental identity model (`IdentityModelService`), per country:
 *
 *  - `kinowo_worker_identity_model_families` — families the model holds after its last drain;
 *  - `kinowo_worker_identity_model_resolved_total` — families re-resolved, summed over drains: the
 *    work the pipeline's events cost (rate() of it is the model's load);
 *  - `kinowo_worker_identity_model_events_total` — venue scrapes and observations drained, by kind;
 *  - `kinowo_worker_identity_model_drain_seconds` — how long the last drain that did work took;
 *  - `kinowo_worker_identity_model_rebuilds_total` — drains that failed and rebuilt the model from
 *    its store (0 is healthy);
 *  - `kinowo_worker_identity_model_takeup_failures_total` — take-ups that failed, each leaving the
 *    model down until a retry succeeds (0 is healthy);
 *  - `kinowo_worker_identity_model_largest_family_listings` / `_largest_family_nodes` — the largest
 *    family's listings and the most evidence nodes any family holds;
 *  - `kinowo_worker_identity_model_large_families` — families larger than one region: each
 *    resolves slowly whenever anything in it moves (0 is healthy);
 *  - `kinowo_worker_identity_history_dropped_total` — identity history entries dropped because its
 *    writer's queue was full (0 is healthy);
 *  - the model's input gate (docs/design/identity-structural-fixes.md, fix 1), which no replay runs:
 *    `kinowo_worker_identity_model_held_listings` — listings whose newest version it holds on their own questions;
 *    `kinowo_worker_identity_model_held_oldest_seconds` — how long the oldest of them has waited (0 with none: something
 *    stuck shows here and pages, `IdentityGateStuck`); `kinowo_worker_identity_model_promoted_total{at}` — held listings
 *    taken in, at a drain or by a take-up (`at="boot"`: answered while the worker was down), or past the hold's bound
 *    (`at="bound"`: the backstop -- something the queue does not reach);
 *    `kinowo_worker_identity_model_hold_seconds` — how long each waited.
 *
 * Every country's series exist from boot: the counters at 0, the gauges at NaN until the first drain measures
 * them (a 0 would claim an empty model). A country whose model is off keeps them so.
 */
final class IdentityModelGauges(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val families: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_families")
    .help("Families the incremental identity model holds after its last drain.")
    .labelNames("country")
    .register(registry)

  private val historyDropped: Counter = Counter.builder()
    .name("kinowo_worker_identity_history_dropped_total")
    .help("Identity history entries dropped because its writer's queue was full (the history is diagnostic: never back-pressure).")
    .labelNames("country")
    .register(registry)

  /** Count `n` history entries dropped for `country` ([[services.identity.adapters.AsyncIdentityHistory]]). */
  def historyDropped(country: String)(n: Long): Unit = historyDropped.labelValues(country).inc(n.toDouble)

  private val resolved: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_resolved_total")
    .help("Families the incremental identity model re-resolved, summed over its drains.")
    .labelNames("country")
    .register(registry)

  private val events: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_events_total")
    .help("Events the incremental identity model drained, by kind (venue scrapes, observations).")
    .labelNames("country", "kind")
    .register(registry)

  private val seconds: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_drain_seconds")
    .help("Wall-clock seconds the model's last working drain took.")
    .labelNames("country")
    .register(registry)

  private val rebuilds: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_rebuilds_total")
    .help("Drains that failed and rebuilt the incremental identity model from its store (0 is healthy).")
    .labelNames("country")
    .register(registry)

  private val takeUpFailures: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_takeup_failures_total")
    .help("Take-ups of the incremental identity model that failed, leaving it down until a retry succeeds (0 is healthy).")
    .labelNames("country")
    .register(registry)

  private val largestListings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_largest_family_listings")
    .help("Listings in the incremental identity model's largest family.")
    .labelNames("country")
    .register(registry)

  private val largestNodes: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_largest_family_nodes")
    .help("The most evidence nodes any family of the incremental identity model holds.")
    .labelNames("country")
    .register(registry)

  private val large: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_large_families")
    .help("Families of the incremental identity model larger than one region (0 is healthy).")
    .labelNames("country")
    .register(registry)

  private val held: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_held_listings")
    .help("Listings whose newest version the identity model's input gate holds until their own questions are answered.")
    .labelNames("country")
    .register(registry)

  private val heldOldest: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_held_oldest_seconds")
    .help("Seconds the oldest listing the identity model's input gate holds has waited (0 with none held).")
    .labelNames("country")
    .register(registry)

  private val present: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_present_listings")
    .help("Listings the identity model holds that are billed now (with showtimes), after its last drain or take-up.")
    .labelNames("country")
    .register(registry)

  private val kept: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_kept_listings")
    .help("Departed listings the identity model holds as kept inputs (ListingRetention: a billed listing relates to them by title).")
    .labelNames("country")
    .register(registry)

  private val departed: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_departed_listings")
    .help("Departed listings the identity model's store holds as inputs, within the retention window (identity_retained).")
    .labelNames("country")
    .register(registry)

  private val twinsDeleted: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_retained_twins_deleted_total")
    .help("Departed listings deleted from identity_retained as twins of a listing its venue bills again under another key (at=boot: a take-up).")
    .labelNames("country", "at")
    .register(registry)

  private val promoted: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_promoted_total")
    .help("Listings the identity model's input gate held and took in once answered: at a drain, or by a take-up (at=boot).")
    .labelNames("country", "at")
    .register(registry)

  private val holdSeconds: Histogram = Histogram.builder()
    .name("kinowo_worker_identity_model_hold_seconds")
    .help("Seconds each listing the identity model's input gate took in had waited on its own questions.")
    .labelNames("country")
    .classicUpperBounds(1, 5, 15, 30, 60, 120, 300, 900, 1800, 3600, 7200, 21600, 86400)
    .register(registry)

  countryCodes.foreach { c =>
    Seq(families, seconds, largestListings, largestNodes, large, held, heldOldest, present, kept, departed).foreach(_.labelValues(c).set(Double.NaN))
    Seq(resolved, rebuilds, takeUpFailures, historyDropped).foreach(_.labelValues(c))
    Seq("venue", "observation").foreach(events.labelValues(c, _))
    Seq("drain", "boot", "bound").foreach(promoted.labelValues(c, _))
    holdSeconds.labelValues(c)
  }

  def forCountry(country: String): IdentityModelMetrics = new IdentityModelMetrics {
    def batch(batch: ModelBatch): Unit = {
      families.labelValues(country).set(batch.families.toDouble)
      resolved.labelValues(country).inc(batch.familiesResolved.toDouble)
      events.labelValues(country, "venue").inc(batch.venues.toDouble)
      events.labelValues(country, "observation").inc(batch.observations.toDouble)
      seconds.labelValues(country).set(batch.seconds)
      largestListings.labelValues(country).set(batch.sizes.largestListings.toDouble)
      largestNodes.labelValues(country).set(batch.sizes.largestNodes.toDouble)
      large.labelValues(country).set(batch.sizes.large.toDouble)
    }
    def rebuilt(): Unit = rebuilds.labelValues(country).inc()
    def takeUpFailed(): Unit = takeUpFailures.labelValues(country).inc()
    override def held(count: Int, oldestSeconds: Double): Unit = {
      IdentityModelGauges.this.held.labelValues(country).set(count.toDouble)
      heldOldest.labelValues(country).set(oldestSeconds)
    }
    override def twinsDeleted(count: Int, atBoot: Boolean): Unit =
      IdentityModelGauges.this.twinsDeleted.labelValues(country, if (atBoot) "boot" else "drain").inc(count.toDouble)
    override def retention(present: Int, kept: Int, departed: Int): Unit = {
      IdentityModelGauges.this.present.labelValues(country).set(present.toDouble)
      IdentityModelGauges.this.kept.labelValues(country).set(kept.toDouble)
      IdentityModelGauges.this.departed.labelValues(country).set(departed.toDouble)
    }
    override def bounded(heldSeconds: Seq[Double]): Unit = {
      IdentityModelGauges.this.promoted.labelValues(country, "bound").inc(heldSeconds.size.toDouble)
      heldSeconds.foreach(holdSeconds.labelValues(country).observe)
    }
    override def promoted(heldSeconds: Seq[Double], atBoot: Boolean): Unit = {
      IdentityModelGauges.this.promoted.labelValues(country, if (atBoot) "boot" else "drain").inc(heldSeconds.size.toDouble)
      heldSeconds.foreach(holdSeconds.labelValues(country).observe)
    }
  }
}
