package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.UptimeMonitor
import services.UptimeMonitor.BucketSnapshot
import services.metrics.WebJvmMetrics
import services.readmodel.TestReadModel

/**
 * Locks the Prometheus `/metrics` exposition that feeds the self-hosted Grafana:
 * recent per-service success/failure counts, windowed so a gauge reflects
 * *current* health (an incident recovers to 0) rather than a since-boot total.
 * The residential-proxy alert divides these two families, so the windowing and
 * the summing are the load-bearing behaviour.
 */
class MetricsControllerSpec extends AnyFlatSpec with Matchers {

  private val now = 1_700_000_000_000L
  private def minutesAgo(m: Int) = now - m * 60 * 1000L

  "render" should "sum only the buckets inside the recent window, per family" in {
    val snapshots = Map(
      "Residential proxy" -> Seq(
        BucketSnapshot(minutesAgo(5), successes = 0, failures = 12, zeroes = 0, errors = Seq("boom")),
        BucketSnapshot(minutesAgo(1), successes = 1, failures = 3, zeroes = 0, errors = Nil),
        // 40 min old — outside the 30-min window, must NOT count.
        BucketSnapshot(minutesAgo(40), successes = 99, failures = 99, zeroes = 0, errors = Nil)
      )
    )

    val out = MetricsController.render(snapshots, now, "pl")

    out should include ("kinowo_uptime_recent_failures{country=\"pl\",service=\"Residential proxy\"} 15")
    out should include ("kinowo_uptime_recent_successes{country=\"pl\",service=\"Residential proxy\"} 1")
    out should not include "99" // the stale bucket is excluded entirely
  }

  it should "emit one HELP and TYPE header per metric family" in {
    val out = MetricsController.render(Map("X" -> Seq(BucketSnapshot(now, 1, 0, 0, Nil))), now, "pl")

    out should include ("# TYPE kinowo_uptime_recent_successes gauge")
    out should include ("# TYPE kinowo_uptime_recent_failures gauge")
    out should include ("# TYPE kinowo_uptime_recent_zeroes gauge")
    out should include ("# HELP kinowo_uptime_recent_failures")
  }

  it should "escape quotes in a service-name label value" in {
    val out = MetricsController.render(Map("weird\"name" -> Seq(BucketSnapshot(now, 1, 0, 0, Nil))), now, "pl")

    out should include ("service=\"weird\\\"name\"")
  }

  "the controller" should "serve recorded uptime health as Prometheus text" in {
    val monitor = new UptimeMonitor() // no Mongo — purely in-memory record/history
    (1 to 4).foreach(_ => monitor.recordFailure("Residential proxy", "too many authentication attempts. Limit: 3"))
    monitor.recordSuccess("Residential proxy")
    val controller = newController(monitor)

    val result = controller.metrics(FakeRequest())

    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/plain")
    val body = contentAsString(result)
    body should include ("kinowo_uptime_recent_failures{country=\"pl\",service=\"Residential proxy\"} 4")
    body should include ("kinowo_uptime_recent_successes{country=\"pl\",service=\"Residential proxy\"} 1")
  }

  // The Fly-health dashboard charts the web JVM's heap against its -Xmx the same
  // way it does the worker's; that panel is empty unless these series ship in the
  // very same exposition the uptime gauges do.
  it should "append the JVM + process resource collectors to the same exposition" in {
    val body = contentAsString(newController(new UptimeMonitor()).metrics(FakeRequest()))

    body should include ("jvm_memory_used_bytes")
    body should include ("jvm_memory_max_bytes")
    body should include ("process_cpu_seconds_total")
    // NOT asserted: process_resident_memory_bytes / process_virtual_memory_bytes.
    // The collector reads them from /proc, so they exist on the Fly (Linux) box
    // but never on a macOS dev machine — asserting them would fail locally only.
    // Heap is the series the panel's "Heap used" target sums — assert the label
    // dimension it selects on, not just the family name.
    body should include ("jvm_memory_used_bytes{area=\"heap\"")
    // Still one well-formed document: the business gauges survive the append.
    body should include ("kinowo_uptime_recent_failures")
  }

  private def newController(monitor: UptimeMonitor) = {
    val movieMetrics = new WebMovieMetrics(new MovieControllerService(TestReadModel.fromRecords(Seq.empty)))
    new MetricsController(Helpers.stubControllerComponents(), monitor, movieMetrics, new WebJvmMetrics)
  }
}
