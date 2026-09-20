package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class LegacyUserStateMetricsSpec extends AnyFlatSpec with Matchers {

  private val GaugeName = "kinowo_web_legacy_userstate_put_last_called_seconds"

  "LegacyUserStateMetrics" should "publish no series before the first recorded call" in {
    val registry = new PrometheusRegistry()
    new LegacyUserStateMetrics(registry, "pl")
    PrometheusExposition.render(registry) should not include GaugeName
  }

  /** The exposition renders the gauge value in scientific notation
   *  (`1.779192E9`, not `1779192000`), so assertions compare the PARSED
   *  double rather than a literal substring. */
  private def gaugeValue(body: String): Double = {
    val Pattern = (GaugeName + """\{country="pl"\}\s+([0-9.E+]+)""").r
    Pattern.findFirstMatchIn(body).map(_.group(1).toDouble).getOrElse(fail(s"no $GaugeName{country=\"pl\"} sample in:\n$body"))
  }

  it should "publish the recorded call's epoch-seconds instant, labelled by country" in {
    val registry = new PrometheusRegistry()
    val metrics  = new LegacyUserStateMetrics(registry, "pl")
    val at = Instant.parse("2026-05-19T12:00:00Z")

    metrics.recordPutCall(at)

    val body = PrometheusExposition.render(registry)
    body should include (GaugeName)
    body should include ("country=\"pl\"")
    gaugeValue(body) shouldBe at.getEpochSecond.toDouble
  }

  it should "publish the LATEST call, not the first, once called again" in {
    val registry = new PrometheusRegistry()
    val metrics  = new LegacyUserStateMetrics(registry, "pl")
    metrics.recordPutCall(Instant.parse("2026-05-19T12:00:00Z"))
    val later = Instant.parse("2026-05-20T09:00:00Z")
    metrics.recordPutCall(later)

    gaugeValue(PrometheusExposition.render(registry)) shouldBe later.getEpochSecond.toDouble
  }
}
