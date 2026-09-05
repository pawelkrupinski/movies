package services.metrics

import controllers.GzippedResponseCache
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * Locks `kinowo_web_cache_*` — what each of this tier's in-heap caches holds,
 * against what it may hold.
 *
 * The gauges exist because those numbers were unknowable twice. The
 * gzipped-response cache was unbounded and the JRE image carries no `jcmd`, so
 * when `web-us` OOM-crash-looped roughly hourly the split between "the read model
 * is simply big" and "the cache has pinned all 55 states" could only be guessed
 * at. Then the SHARE-CARD cache repeated it behind an entry-count bound over
 * rendered images, and had no gauge at all.
 *
 * Two properties matter and both are asserted here: the gauges must read the LIVE
 * cache at scrape time (a value captured at registration reads zero forever,
 * which is the most reassuring wrong answer available), and a cache that cannot
 * answer a question must publish NO SERIES for it rather than a zero.
 */
class WebCacheMetricsSpec extends AnyFlatSpec with Matchers {

  private val version = Instant.parse("2026-09-02T10:00:00Z")

  private def register(registry: PrometheusRegistry, caches: (String, () => CacheOccupancy)*): Unit =
    new WebCacheMetrics(registry, country = "us", caches = caches.toSeq)

  import CacheMetricSamples.sample

  "the response-cache gauges" should "report what the cache holds at scrape time, not at registration" in {
    val cache    = new GzippedResponseCache()
    val registry = new PrometheusRegistry()
    register(registry, "response" -> (() => cache.occupancy))

    // Everything cached AFTER the gauges were registered must still be counted.
    cache.gzippedBody("/california/", version)("<html>california</html>")
    cache.gzippedBody("/texas/", version)("<html>texas</html>")

    val text = PrometheusExposition.render(registry)
    sample(text, "kinowo_web_cache_entries", "response") shouldBe Some(2.0)
    sample(text, "kinowo_web_cache_held_bytes", "response").getOrElse(fail("no held_bytes")) should be > 0.0
  }

  /** The budget is the whole point of `held_bytes`, and it was the half missing:
   *  the old help text told a reader to compare against `DefaultMaxBytes` while
   *  publishing no such series, so any panel had to hard-code the number and go
   *  stale the moment it was retuned. */
  it should "publish the budget beside what is held" in {
    val budget   = 32L * 1024
    val registry = new PrometheusRegistry()
    register(registry, "response" -> (() => new GzippedResponseCache(maxBytes = budget).occupancy))

    sample(PrometheusExposition.render(registry), "kinowo_web_cache_max_bytes", "response") shouldBe Some(budget.toDouble)
  }

  it should "stay under the budget once eviction has kicked in" in {
    val budget = 32L * 1024
    val cache  = new GzippedResponseCache(maxBytes = budget)
    val random = new scala.util.Random(7)
    (1 to 30).foreach { state =>
      cache.gzippedBody(s"/state-$state/", version)(random.alphanumeric.take(8 * 1024).mkString)
    }
    val registry = new PrometheusRegistry()
    register(registry, "response" -> (() => cache.occupancy))

    sample(PrometheusExposition.render(registry), "kinowo_web_cache_held_bytes", "response")
      .getOrElse(fail("no held_bytes")) should be <= budget.toDouble
  }

  /** The response cache is an access-ordered `LinkedHashMap` with no hit counters.
   *  Publishing 0.0 would read as a cache serving nothing at all — the opposite of
   *  the truth — so it must publish nothing. */
  "a cache with no hit counters" should "publish no ratio rather than a zero" in {
    val registry = new PrometheusRegistry()
    register(registry, "response" -> (() => new GzippedResponseCache().occupancy))

    val text = PrometheusExposition.render(registry)
    sample(text, "kinowo_web_cache_hit_ratio", "response")        shouldBe None
    sample(text, "kinowo_web_cache_evictions_total", "response")  shouldBe None
  }

  "several caches" should "share the family, separated by the cache label" in {
    val registry = new PrometheusRegistry()
    register(registry,
      "og_card_film" -> (() => CacheOccupancy(entries = 3L, heldBytes = Some(900L), maxBytes = Some(4096L),
                                              hitRatio = Some(0.75))),
      "og_card_city" -> (() => CacheOccupancy(entries = 1L, heldBytes = Some(100L), maxBytes = Some(1024L))))

    val text = PrometheusExposition.render(registry)
    sample(text, "kinowo_web_cache_max_bytes", "og_card_film")  shouldBe Some(4096.0)
    sample(text, "kinowo_web_cache_max_bytes", "og_card_city")  shouldBe Some(1024.0)
    sample(text, "kinowo_web_cache_hit_ratio", "og_card_film")  shouldBe Some(0.75)
    // The city cards' cache reported no ratio, so it has no series — not a zero.
    sample(text, "kinowo_web_cache_hit_ratio", "og_card_city")  shouldBe None
  }
}
