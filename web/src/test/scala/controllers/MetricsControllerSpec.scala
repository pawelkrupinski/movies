package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.UptimeMonitor
import services.UptimeMonitor.RecentTotals
import services.fallback.{FallbackState, InMemoryFallbackStore}
import services.metrics.WebJvmMetrics
import services.readmodel.TestReadModel

import java.time.Instant

/**
 * Locks the Prometheus `/metrics` exposition that feeds the self-hosted Grafana:
 * recent per-service success/failure counts, windowed so a gauge reflects
 * *current* health (an incident recovers to 0) rather than a since-boot total.
 * The residential-proxy alert divides these two families, so the windowing and
 * the summing are the load-bearing behaviour.
 */
class MetricsControllerSpec extends AnyFlatSpec with Matchers {

  // The window is applied by `UptimeMonitor.recentTotals` now, not here — the
  // roster is one service per venue and summing 96 slots apiece on every scrape is
  // what OOM-killed `web-us`. `UptimeMonitorSpec` owns the windowing tests; these
  // own the exposition format.
  "render" should "emit each family's total per service" in {
    val totals = Seq("Residential proxy" -> RecentTotals(successes = 1, failures = 15, zeroes = 2))

    val out = MetricsController.render(totals, "pl")

    out should include ("kinowo_uptime_recent_failures{country=\"pl\",service=\"Residential proxy\"} 15")
    out should include ("kinowo_uptime_recent_successes{country=\"pl\",service=\"Residential proxy\"} 1")
    out should include ("kinowo_uptime_recent_zeroes{country=\"pl\",service=\"Residential proxy\"} 2")
  }

  it should "emit one HELP and TYPE header per metric family" in {
    val out = MetricsController.render(Seq("X" -> RecentTotals(1, 0, 0)), "pl")

    out should include ("# TYPE kinowo_uptime_recent_successes gauge")
    out should include ("# TYPE kinowo_uptime_recent_failures gauge")
    out should include ("# TYPE kinowo_uptime_recent_zeroes gauge")
    out should include ("# HELP kinowo_uptime_recent_failures")
  }

  it should "escape quotes in a service-name label value" in {
    val out = MetricsController.render(Seq("weird\"name" -> RecentTotals(1, 0, 0)), "pl")

    out should include ("service=\"weird\\\"name\"")
  }

  it should "emit services in name order, so the exposition is deterministic" in {
    val out = MetricsController.render(Seq("Zeta" -> RecentTotals(1, 0, 0), "Alpha" -> RecentTotals(1, 0, 0)), "pl")

    out.indexOf("service=\"Alpha\"") should be < out.indexOf("service=\"Zeta\"")
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

  // ── kinowo_fallback_active_venues / kinowo_fallback_total_venues ──────────
  //
  // The Cineworld-relaunch incident (2026-09-17): 87 venues, one shared
  // scraper client, ALL simultaneously riding their Flicks fallback for over
  // a day, invisible because every cinema bar on /uptime stayed green. These
  // gauges are what a `sum by (client) / sum by (client)` alert reads.
  "renderFallbackSaturation" should "count active and total venues per shared client" in {
    val tags = Map(
      "Cineworld Leeds" -> Set("shared:CineworldClient"),
      "Cineworld York" -> Set("shared:CineworldClient"),
      "Odeon Luton" -> Set("shared:OdeonClient")
    )
    val states = Seq(fallbackState("Cineworld Leeds", active = true), fallbackState("Cineworld York", active = true))

    val out = MetricsController.renderFallbackSaturation(tags, states, "uk")

    out should include ("kinowo_fallback_active_venues{country=\"uk\",client=\"CineworldClient\"} 2")
    out should include ("kinowo_fallback_total_venues{country=\"uk\",client=\"CineworldClient\"} 2")
    out should include ("kinowo_fallback_active_venues{country=\"uk\",client=\"OdeonClient\"} 0")
    out should include ("kinowo_fallback_total_venues{country=\"uk\",client=\"OdeonClient\"} 1")
  }

  it should "exclude custom (bespoke, single-venue) clients — only shared markers get a ratio" in {
    val tags = Map("Rialto" -> Set("custom:RialtoClient"))
    val states = Seq(fallbackState("Rialto", active = true))

    val out = MetricsController.renderFallbackSaturation(tags, states, "pl")

    out should not include "RialtoClient"
  }

  it should "not count a fallback-active cinema that recovered (active=false)" in {
    val tags = Map("Cineworld Leeds" -> Set("shared:CineworldClient"))
    val states = Seq(fallbackState("Cineworld Leeds", active = false))

    val out = MetricsController.renderFallbackSaturation(tags, states, "uk")

    out should include ("kinowo_fallback_active_venues{country=\"uk\",client=\"CineworldClient\"} 0")
  }

  it should "emit clients in name order and one HELP/TYPE pair per family" in {
    val tags = Map("A" -> Set("shared:Zeta"), "B" -> Set("shared:Alpha"))

    val out = MetricsController.renderFallbackSaturation(tags, Seq.empty, "pl")

    out.indexOf("client=\"Alpha\"") should be < out.indexOf("client=\"Zeta\"")
    out should include ("# TYPE kinowo_fallback_active_venues gauge")
    out should include ("# TYPE kinowo_fallback_total_venues gauge")
  }

  "the controller" should "append the fallback-saturation gauges to the /metrics exposition" in {
    val monitor = new UptimeMonitor() // no Mongo — tagService still writes its in-memory snapshot
    monitor.tagService("Cineworld Leeds", Set("shared:CineworldClient"))
    val fallbackStore = new InMemoryFallbackStore
    fallbackStore.put(fallbackState("Cineworld Leeds", active = true))
    val controller = newController(monitor, fallbackStore)

    val body = contentAsString(controller.metrics(FakeRequest()))

    body should include ("kinowo_fallback_active_venues{country=\"pl\",client=\"CineworldClient\"} 1")
    body should include ("kinowo_fallback_total_venues{country=\"pl\",client=\"CineworldClient\"} 1")
  }

  private def fallbackState(cinema: String, active: Boolean) = FallbackState(
    cinema = cinema,
    active = active,
    fallbackSource = "Flicks",
    fallbackRef = None,
    since = Some(Instant.EPOCH),
    lastReason = None,
    consecutiveFailures = 1,
    lastPrimaryProbeAt = None,
    nextPrimaryProbeAt = None,
    updatedAt = Instant.EPOCH,
    history = List.empty
  )

  private def newController(monitor: UptimeMonitor, fallbackStore: InMemoryFallbackStore = new InMemoryFallbackStore) = {
    val movieMetrics = new WebMovieMetrics(new MovieControllerService(TestReadModel.fromRecords(Seq.empty), TestMovieController.clock), models.Country.Poland, TestMovieController.clock)
    new MetricsController(Helpers.stubControllerComponents(), monitor, fallbackStore, movieMetrics, new WebJvmMetrics, models.Country.Poland.code)
  }
}
