package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.HistogramSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

/** One page render's allocation lands in `kinowo_web_page_render_allocated_bytes`, under
 *  the country and the page it was — what separates a render's own cost from the rest
 *  of a pod's allocation. */
class WebRenderMetricsSpec extends AnyFlatSpec with Matchers {

  "a recorded render" should "be one observation of its bytes under its country and page" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WebRenderMetrics(registry, "us")
    metrics.record("listing", 12L * 1024 * 1024)
    metrics.record("listing", 3L * 1024 * 1024)

    val histogram = registry.scrape().asScala.collectFirst {
      case h: HistogramSnapshot if h.getMetadata.getPrometheusName == "kinowo_web_page_render_allocated_bytes" => h
    }.getOrElse(fail("no kinowo_web_page_render_allocated_bytes histogram"))
    val point = histogram.getDataPoints.asScala.find(p =>
      p.getLabels.get("country") == "us" && p.getLabels.get("page") == "listing").getOrElse(fail("no us/listing series"))
    point.getCount shouldBe 2
    point.getSum shouldBe (15.0 * 1024 * 1024)
  }
}
