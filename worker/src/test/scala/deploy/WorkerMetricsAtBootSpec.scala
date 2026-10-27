package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerMetrics

import scala.jdk.CollectionConverters._

/**
 * EVERY WORKER METRIC EXISTS FROM BOOT: each `kinowo_worker_*` family carries a series for every country the moment
 * [[WorkerMetrics]] — the registry `WorkerMain` builds before any wiring — is constructed, at 0 or, for a reading
 * nothing has taken yet, NaN.
 *
 * WHY. A series that appears with its first event makes every alert over it blind until then, and indistinguishable
 * from a family nobody exports: `increase()` cannot see a counter's first step from absent to 1, a `> 0` over a gauge
 * that is not there yet is silent, and an absent-series alert cannot tell "nothing happened" from "the worker stopped
 * publishing it". `kinowo_identity_listing_flips` vanished every quiet day for exactly this reason (a73fc8c2c). With
 * every family present from boot, absence means a fault, and an absent-series alert can page on it.
 *
 * That every family the worker's sources name IS registered by [[WorkerMetrics]] — none created lazily, later, by
 * some wiring — is `GrafanaMetricCoverageSpec`'s "reach every family" case; this spec asks the next question: does
 * each registered family have its series yet.
 */
class WorkerMetricsAtBootSpec extends AnyFlatSpec with Matchers {

  private val countries = Country.all.map(_.code)

  /** Families whose series only a wiring can name, or whose absence is itself the signal — each with the reason. */
  private val NotSeededAtBoot: Map[String, String] = {
    val cache = "one series per in-process cache, added by each wiring's start() (WorkerMetrics.registerCache): the caches are the wiring's"
    val ticks = "one series per periodic loop, written as the loop starts (TickRecorder.started): the loops are the wiring's"
    val mongo = "one series per database and collection, seeded at 0 as the wiring first opens each (MongoWriteErrors.touched/opened), before a write to it can fail"
    val gated = "recorded at boot (AlertingWiring, WorkerMain) over the features only they build; a seed of 0 would read as disabled"
    Seq("entries", "evictions", "held_bytes", "hit_ratio", "max_bytes", "max_entries").map(f => s"kinowo_worker_cache_$f" -> cache).toMap ++
      Seq("interval_seconds", "last_success_timestamp_seconds").map(f => s"kinowo_worker_scheduled_tick_$f" -> ticks) ++
      Seq("kinowo_worker_mongo_write_errors" -> mongo, "kinowo_worker_mongo_unauthorized" -> mongo,
        "kinowo_worker_alerter_enabled" -> gated, "kinowo_worker_integration_enabled" -> gated,
        "kinowo_worker_http_circuit_open" -> "one series per (leg, host) whose breaker opened: the hosts are unbounded, and a closed breaker's series goes",
        "kinowo_worker_share_cards_coverage_ratio" -> "withdrawn while no film on screen is tracked, and ShareCardCoverageAbsent pages on that absence",
        "kinowo_worker_identity_billing_reversals" -> "one series per billing that reversed, removed once forgotten (BillingReversals)",
        "kinowo_worker_identity_rule_fires" -> "one series per resolver rule that fired: the rule names are the resolver's trace, not an enumeration",
        "kinowo_worker_identity_agreement_phase_seconds" -> "observed per projection run mode, an enum private to IdentityProjection",
        "kinowo_worker_identity_agreement_phase_allocated_bytes" -> "observed per projection run mode, an enum private to IdentityProjection")
  }

  /** (family, countries with a series, values) for every `kinowo_*` family on a freshly built registry. */
  private lazy val atBoot: Seq[(String, Set[String], Seq[Double])] =
    new WorkerMetrics(countries, settings.WorkerPoolSize(1)).registry.scrape().asScala.toSeq
      .filter(_.getMetadata.getPrometheusName.startsWith("kinowo_"))
      .map { snapshot =>
        val points = snapshot.getDataPoints.asScala.toSeq
        (snapshot.getMetadata.getPrometheusName.stripSuffix("_total"), points.flatMap(p => Option(p.getLabels.get("country"))).toSet, points.map(valueOf))
      }

  private def valueOf(point: io.prometheus.metrics.model.snapshots.DataPointSnapshot): Double = point match {
    case g: io.prometheus.metrics.model.snapshots.GaugeSnapshot.GaugeDataPointSnapshot     => g.getValue
    case c: io.prometheus.metrics.model.snapshots.CounterSnapshot.CounterDataPointSnapshot => c.getValue
    case h: io.prometheus.metrics.model.snapshots.HistogramSnapshot.HistogramDataPointSnapshot => h.getCount.toDouble
    case _                                                                                  => 0.0
  }

  "every worker metric family" should "carry a series for every country from the moment the registry is built" in {
    atBoot.size should be > 100 // the scrape reaches the families
    val lazily = atBoot.collect {
      case (family, _, values) if values.isEmpty && !NotSeededAtBoot.contains(family) => s"$family: no series"
      case (family, seen, values) if values.nonEmpty && seen.nonEmpty && !NotSeededAtBoot.contains(family) && countries.exists(c => !seen(c)) =>
        s"$family: none for ${countries.filterNot(seen).mkString(", ")}"
    }
    withClue(
      s"created with its first event instead of at boot:\n  ${lazily.mkString("\n  ")}\nSeed each series where the family " +
        "is registered — counters by touching labelValues, a gauge at 0, or at NaN for a reading not taken yet — over " +
        "every finite label value (country, city, an enum's values). A family whose labels only a wiring can name goes " +
        "in NotSeededAtBoot with the reason. "
    ) {
      lazily shouldBe empty
    }
  }

  /** The worker's half of the metric catalogue (services.metrics.MetricCatalogue): what the infra checks read as the
   *  one declaration of each family — its labels and seeded values, and which families read NaN at boot. */
  it should "be written out as infra/test/app-metrics-catalogue/worker.json" in {
    val rendered = services.metrics.MetricCatalogue.render(
      new WorkerMetrics(countries, settings.WorkerPoolSize(1)).registry.scrape(), "worker")
    services.metrics.MetricCatalogue.check(java.nio.file.Path.of("infra/test/app-metrics-catalogue/worker.json"), rendered,
      "worker/testOnly deploy.WorkerMetricsAtBootSpec") shouldBe None
  }

  "NotSeededAtBoot" should "not exempt a family that has its series after all, or that no longer exists" in {
    val families = atBoot.map(_._1).toSet
    NotSeededAtBoot.keys.filterNot(families) shouldBe empty
    NotSeededAtBoot.keys.filter(f => atBoot.exists { case (family, _, values) => family == f && values.nonEmpty }) shouldBe empty
  }
}
