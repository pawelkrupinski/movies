package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import tools.MutableClock

import java.time.{Duration, Instant}

class LegacyUserStateMetricsSpec extends AnyFlatSpec with Matchers {

  private val GaugeName = "kinowo_web_legacy_userstate_put_last_called_seconds"

  private val Start = Instant.parse("2026-05-19T12:00:00Z")

  "LegacyUserStateMetrics" should "publish no series before the first recorded call" in {
    val registry = new PrometheusRegistry()
    new LegacyUserStateMetrics(registry, "pl", java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC))
    PrometheusExposition.render(registry) should not include GaugeName
  }

  /** The exposition renders the gauge value in scientific notation
   *  (`1.779192E9`, not `1779192000`), so assertions compare the PARSED
   *  double rather than a literal substring. */
  private def gaugeValue(body: String): Double = {
    val Pattern = (GaugeName + """\{country="pl"\}\s+([0-9.E+]+)""").r
    Pattern.findFirstMatchIn(body).map(_.group(1).toDouble).getOrElse(fail(s"no $GaugeName{country=\"pl\"} sample in:\n$body"))
  }

  it should "publish the injected clock's instant at the call, labelled by country" in {
    val registry = new PrometheusRegistry()
    val metrics  = new LegacyUserStateMetrics(registry, "pl", new MutableClock(Start))

    metrics.recordPutCall()

    val body = PrometheusExposition.render(registry)
    body should include (GaugeName)
    body should include ("country=\"pl\"")
    gaugeValue(body) shouldBe Start.getEpochSecond.toDouble
  }

  it should "publish the LATEST call, not the first, once called again" in {
    val registry = new PrometheusRegistry()
    val clock    = new MutableClock(Start)
    val metrics  = new LegacyUserStateMetrics(registry, "pl", clock)
    metrics.recordPutCall()
    clock.advance(Duration.ofHours(21))
    metrics.recordPutCall()

    gaugeValue(PrometheusExposition.render(registry)) shouldBe Start.plus(Duration.ofHours(21)).getEpochSecond.toDouble
  }
}
