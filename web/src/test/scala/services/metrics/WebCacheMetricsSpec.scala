package services.metrics

import controllers.GzippedResponseCache
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * Locks `kinowo_web_response_cache_*` — the heap the gzipped-response cache is
 * holding, and across how many paths.
 *
 * The gauges exist because that number was unknowable. The cache was unbounded
 * and the JRE image carries no `jcmd`, so when `web-us` OOM-crash-looped roughly
 * hourly the split between "the read model is simply big" and "the cache has
 * pinned all 55 states" could only be guessed at. Both assertions below are about
 * that: the gauges must read the LIVE cache at scrape time, not a value captured
 * when they were registered — a snapshot taken at boot reads zero forever, which
 * is the most reassuring wrong answer available.
 */
class WebCacheMetricsSpec extends AnyFlatSpec with Matchers {

  private val version = Instant.parse("2026-09-02T10:00:00Z")

  private def exposition(cache: GzippedResponseCache): String = {
    val registry = new PrometheusRegistry()
    new WebCacheMetrics(registry, country = "us", cache = cache)
    PrometheusExposition.render(registry)
  }

  private def sample(text: String, name: String): Option[Double] =
    text.linesIterator.collectFirst {
      case line if line.startsWith(s"""$name{country="us"} """) => line.split(' ').last.toDouble
    }

  "the response-cache gauges" should "report what the cache holds at scrape time, not at registration" in {
    val cache = new GzippedResponseCache()
    val registry = new PrometheusRegistry()
    new WebCacheMetrics(registry, country = "us", cache = cache)

    // Everything cached AFTER the gauges were registered must still be counted.
    cache.gzippedBody("/california/", version)("<html>california</html>")
    cache.gzippedBody("/texas/", version)("<html>texas</html>")

    val text = PrometheusExposition.render(registry)
    sample(text, "kinowo_web_response_cache_entries") shouldBe Some(2.0)
    sample(text, "kinowo_web_response_cache_held_bytes").getOrElse(fail("no held_bytes sample")) should be > 0.0
  }

  // The budget is the whole point of the gauge, so a cache that is evicting must
  // read at or under it rather than climbing past.
  it should "stay under the budget once eviction has kicked in" in {
    val budget = 32L * 1024
    val cache = new GzippedResponseCache(maxBytes = budget)
    val random = new scala.util.Random(7)
    (1 to 30).foreach { state =>
      cache.gzippedBody(s"/state-$state/", version)(random.alphanumeric.take(8 * 1024).mkString)
    }

    sample(exposition(cache), "kinowo_web_response_cache_held_bytes")
      .getOrElse(fail("no held_bytes sample")) should be <= budget.toDouble
  }
}
