package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoTtlIndex

/**
 * A TTL index whose expiry disagrees with the code was invisible for the whole life
 * of this fleet: `createIndex` cannot ALTER one, `readWrite` does not carry
 * `collMod`, and the rejection was logged at `logger.debug`. It cost a real,
 * unnoticed defect — `detailCache-cinema-city` was still reaping at 6h on
 * 2026-09-06, a day after the code moved it to 2h, which meant half of that
 * chain's scheduled refreshes could not observe a change and were recorded as
 * though they had.
 *
 * `MongoTtlIndex` now rebuilds a disagreeing index itself. This gauge is for the
 * case where even the rebuild fails — including the expensive one, a drop that
 * succeeded and a create that did not, leaving the collection reaping nothing.
 * This spec guards that the gauge is EXPORTED rather than merely defined, and that
 * it reads zero on a healthy process rather than going absent.
 */
class TtlIndexMetricsSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  private val sentinel = "__spec_ttl_metric_collection"

  override protected def afterEach(): Unit =
    try MongoTtlIndex.Mismatches.resolved(sentinel) finally super.afterEach()

  private def scrape(): String = {
    val registry = new PrometheusRegistry()
    TtlIndexMetrics.register(registry)
    PrometheusExposition.render(registry)
  }

  "the TTL-index gauge" should "be exported" in {
    PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") should not be empty
  }

  /** THE POINT OF A COUNT RATHER THAN A LABELLED SERIES. An alerting expression fires
   *  on the PRESENCE of a sample, not on its truth, so a gauge that disappears when
   *  everything is fine cannot be told apart from one that disappeared because the
   *  scrape did — the mistake `MongodNoPrimary` was written as and paged on. Zero has
   *  to be a value this metric actually publishes. */
  it should "publish ZERO when every index agrees, rather than going absent" in {
    PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") shouldBe Some(0.0)
  }

  it should "count an index the reconciler could not bring into line" in {
    MongoTtlIndex.Mismatches.record(sentinel)
    PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") shouldBe Some(1.0)
  }

  /** THE COLLISION THIS GAUGE WOULD OTHERWISE HIDE. A worker JVM builds one wiring per
   *  country in `KINOWO_COUNTRIES`, and every country owns an `uptimeBuckets`, a
   *  `resolve_*` and a `detailCache-*` of its own. Keyed by the bare collection name,
   *  one country reconciling its copy would clear another country's record of a broken
   *  one and drop the gauge to zero with the index still wrong — a false negative in
   *  the metric that exists to prevent false negatives. */
  it should "not let one database's entry clear another's for the same collection name" in {
    MongoTtlIndex.Mismatches.record("kinowo_pl.uptimeBuckets")
    MongoTtlIndex.Mismatches.record("kinowo_de.uptimeBuckets")
    try {
      PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") shouldBe Some(2.0)
      MongoTtlIndex.Mismatches.resolved("kinowo_de.uptimeBuckets")
      withClue("Germany's healthy index cleared Poland's broken one: ")(
        PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") shouldBe Some(1.0))
    } finally {
      MongoTtlIndex.Mismatches.resolved("kinowo_pl.uptimeBuckets")
      MongoTtlIndex.Mismatches.resolved("kinowo_de.uptimeBuckets")
    }
  }

  it should "fall back to zero once that index is reconciled, so the alert clears itself" in {
    MongoTtlIndex.Mismatches.record(sentinel)
    MongoTtlIndex.Mismatches.resolved(sentinel)
    PrometheusExposition.value(scrape(), "kinowo_worker_ttl_index_mismatches") shouldBe Some(0.0)
  }

  it should "add no country label of its own — one reconciler serves every country in the JVM" in {
    // `MongoTtlIndex` is an `object`, so the collections Poland reconciles are recorded in
    // the same set Germany's wiring writes to; a country label MINTED HERE would be a lie.
    // Prometheus still attaches a `country` TARGET label naming the pod, which is honest and
    // is what `TtlIndexUnreconciled` groups by — this asserts the metric does not invent one,
    // not that the alert has none to group by.
    val line = scrape().linesIterator.find(l => l.startsWith("kinowo_worker_ttl_index_mismatches") && !l.startsWith("#"))
    line.getOrElse(fail("gauge not exported")) should not include "country="
  }
}
