package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StringPool

/**
 * The intern pool's bound was unobservable, and that is the whole reason it could
 * fail unnoticed: past `MaxEntries` Caffeine evicts, the next lookup of an evicted
 * value allocates a fresh String, and interning quietly becomes a no-op. No error,
 * no log, just a heap that grows.
 *
 * It was suspected of exactly that when `worker-us` OOMed twice (2026-09-01,
 * 2026-09-03) holding 66.7% duplicate String payload. IT WAS NOT THE CAUSE -- see
 * [[StringPool.MaxEntries]] for the measurement that cleared it. These gauges turn
 * that one-off `mongosh` count into something the process reports on every scrape,
 * so nobody has to re-derive it. This spec guards that they are exported rather
 * than merely defined.
 */
class StringPoolMetricsSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def scrape(): String = {
    val registry = new PrometheusRegistry()
    StringPoolMetrics.register(registry)
    PrometheusExposition.render(registry)
  }

  "the intern-pool metrics" should "export occupancy, its cap, evictions and hit ratio" in {
    val text = scrape()
    PrometheusExposition.value(text, "kinowo_worker_string_pool_entries")         should not be empty
    PrometheusExposition.value(text, "kinowo_worker_string_pool_max_entries")     should not be empty
    PrometheusExposition.value(text, "kinowo_worker_string_pool_evictions_total") should not be empty
    PrometheusExposition.value(text, "kinowo_worker_string_pool_hit_ratio")       should not be empty
  }

  it should "publish the cap from the pool itself, so a dashboard can't go stale against a retune" in {
    val published = PrometheusExposition.value(scrape(), "kinowo_worker_string_pool_max_entries")
    published shouldBe Some(StringPool.MaxEntries.toDouble)
  }

  it should "carry no country label — one pool serves every country's wiring in the JVM" in {
    // A country label here would be a lie: StringPool is an `object`, so the strings
    // Poland interns are the same instances Germany gets back.
    scrape() should not include "kinowo_worker_string_pool_entries{"
  }

  it should "report an occupancy that tracks the live pool" in {
    StringPool.canonical(s"exported occupancy probe ${java.util.UUID.randomUUID()}")
    val entries = PrometheusExposition.value(scrape(), "kinowo_worker_string_pool_entries")
    entries.value should be > 0.0
  }
}
