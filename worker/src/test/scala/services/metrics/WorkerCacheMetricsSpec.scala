package services.metrics

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `kinowo_worker_cache_*` publishes what a cache holds against what it may hold —
 * and, just as importantly, publishes NOTHING where a cache has no honest answer.
 *
 * The reason that second half matters: a maximum of zero is not "unbounded", it is
 * "full". Every ratio panel and every threshold written over these series would read
 * an unbounded cache as permanently at 100%, which is exactly the false alarm that
 * makes a dashboard get ignored — and this family exists because the REAL alarm
 * (worker-pl, 2026-09-05, a byte-blind bound holding 228 MiB of a 313 MiB old gen)
 * had no series at all.
 */
class WorkerCacheMetricsSpec extends AnyFlatSpec with Matchers {

  private def registryWith(caches: (String, CacheOccupancy)*): String = {
    val registry = new PrometheusRegistry()
    WorkerCacheMetrics.register(registry,
      () => caches.map { case (name, occupancy) => WorkerCacheMetrics.Registration("pl", name, () => occupancy) })
    PrometheusExposition.render(registry)
  }

  import CacheMetricSamples.sample

  /** [[CacheOccupancy]] still MODELS a byte-bounded cache even though no worker
   *  cache is weighed in bytes today (the venue detail cache was, and was deleted
   *  once nothing read it). Kept because it is the general shape of "held against
   *  its bound", and because the web tier's byte-bounded `OgCardCache` is the
   *  obvious next thing to measure. */
  "A byte-bounded cache" should "report bytes held against its budget" in {
    val weighted: Cache[String, String] =
      Caffeine.newBuilder().maximumWeight(1024L).weigher((_: String, v: String) => v.length).build()
    weighted.put("a", "x" * 100)
    weighted.cleanUp()

    // Asserted on the MODEL, not through the exporter: `WorkerCacheMetrics`
    // publishes no byte series while no worker cache is weighed in bytes.
    val occupancy = CacheOccupancy.of(weighted, weighted = true)
    occupancy.heldBytes  shouldBe Some(100L)
    occupancy.maxBytes   shouldBe Some(1024L)
    occupancy.entries    shouldBe 1L
    // Its ceiling is bytes; an entry maximum would be a different unit in the same field.
    occupancy.maxEntries shouldBe None
  }

  "A count-bounded cache" should "publish its entry maximum and no byte weight" in {
    val counted: Cache[String, String] = Caffeine.newBuilder().maximumSize(500L).build()
    counted.put("a", "irrelevant")
    counted.cleanUp()

    val exposition = registryWith("task_dedup" -> CacheOccupancy.of(counted, weighted = false))
    sample(exposition, "kinowo_worker_cache_max_entries", "task_dedup") shouldBe Some(500.0)
    sample(exposition, "kinowo_worker_cache_entries", "task_dedup")     shouldBe Some(1.0)
    sample(exposition, "kinowo_worker_cache_held_bytes", "task_dedup")  shouldBe None
    sample(exposition, "kinowo_worker_cache_max_bytes", "task_dedup")   shouldBe None
  }

  /** THE ZERO-MAXIMUM TRAP. `movie_corpus` is the hydrated corpus, deliberately
   *  unbounded — it has no ceiling to be near. Publishing 0 would make every
   *  held/max panel show it pinned at 100% forever. */
  "An unbounded cache" should "publish entries but no maximum at all" in {
    val unbounded: Cache[String, String] = Caffeine.newBuilder().build()
    unbounded.put("a", "x"); unbounded.put("b", "y")
    unbounded.cleanUp()

    val exposition = registryWith("movie_corpus" -> CacheOccupancy.of(unbounded, weighted = false))
    sample(exposition, "kinowo_worker_cache_entries", "movie_corpus")     shouldBe Some(2.0)
    sample(exposition, "kinowo_worker_cache_max_entries", "movie_corpus") shouldBe None
    sample(exposition, "kinowo_worker_cache_max_bytes", "movie_corpus")   shouldBe None
  }

  /** Without `recordStats()` Caffeine leaves every counter at zero, which on a panel
   *  is indistinguishable from a cache that has served traffic and evicted nothing. */
  "A cache with no stats recording" should "publish no hit ratio rather than a flat zero" in {
    val plain: Cache[String, String] = Caffeine.newBuilder().maximumSize(10L).build()
    plain.getIfPresent("absent")

    val exposition = registryWith("quiet" -> CacheOccupancy.of(plain, weighted = false))
    sample(exposition, "kinowo_worker_cache_hit_ratio", "quiet") shouldBe None
  }

  it should "publish the hit ratio once the cache is actually recording" in {
    val recording: Cache[String, String] = Caffeine.newBuilder().maximumSize(10L).recordStats().build()
    recording.put("a", "x")
    recording.getIfPresent("a")      // hit
    recording.getIfPresent("absent") // miss

    val exposition = registryWith("recorded" -> CacheOccupancy.of(recording, weighted = false))
    sample(exposition, "kinowo_worker_cache_hit_ratio", "recorded") shouldBe Some(0.5)
  }

  "Several caches" should "share the family, separated by the cache label" in {
    val a: Cache[String, String] = Caffeine.newBuilder().maximumSize(10L).build()
    val b: Cache[String, String] = Caffeine.newBuilder().maximumSize(20L).build()
    a.put("k", "v"); a.cleanUp(); b.cleanUp()

    val exposition = registryWith(
      "weighed_cache" -> CacheOccupancy.of(a, weighted = false),
      "task_dedup"   -> CacheOccupancy.of(b, weighted = false))
    sample(exposition, "kinowo_worker_cache_max_entries", "weighed_cache") shouldBe Some(10.0)
    sample(exposition, "kinowo_worker_cache_max_entries", "task_dedup")   shouldBe Some(20.0)
  }
}
