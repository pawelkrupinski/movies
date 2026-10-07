package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsValue, Json}

/**
 * The application-health dashboard charts how often each JVM runs a FULL collection and how much of the wall clock
 * collecting takes, per hour, on both tiers. Its heap panels showed only occupancy, and the heap layout of
 * 2026-10-07 (each worker's -Xmn from its live set, PL and UK given bigger heaps) was judged off ad-hoc queries:
 * worker-pl on 448m ran 24 full collections an hour, 2.1% of its wall time, with no boot among them.
 */
class GrafanaGcChartsSpec extends AnyFlatSpec with Matchers {
  private val dashboard = Json.parse(RepoFile.read("infra/nix/files/monitoring/grafana/dashboards/apps/application-health.json"))

  private def panels(v: JsValue): Seq[JsValue] =
    (v \ "panels").asOpt[JsArray].toSeq.flatMap(_.value).flatMap(p => p +: panels(p))
  private def exprs: Seq[String] = panels(dashboard).flatMap(p => (p \ "targets").asOpt[JsArray].toSeq.flatMap(_.value)).flatMap(t => (t \ "expr").asOpt[String])

  "the application-health dashboard" should "chart each tier's full collections per hour" in {
    val full = exprs.filter(e => e.contains("increase(jvm_gc_collection_seconds_count") && e.contains("[1h]"))
    full.exists(e => e.contains("MarkSweepCompact") && e.contains("G1 Old Generation")) shouldBe true // the worker's Serial old, web's G1 old
  }

  it should "chart the share of the wall clock each collector takes, per hour" in {
    exprs.exists(e => e.contains("increase(jvm_gc_collection_seconds_sum") && e.contains("[1h]") && e.contains("by (job, country, gc)")) shouldBe true
  }
}
