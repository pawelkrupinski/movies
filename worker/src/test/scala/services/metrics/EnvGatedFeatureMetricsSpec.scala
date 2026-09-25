package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The gauges that make a feature wired OFF by a missing env var visible: 1 for on, 0 for
 *  off — present either way, so an alert compares a value rather than a sample's presence. */
class EnvGatedFeatureMetricsSpec extends AnyFlatSpec with Matchers {

  private val off = EnvGatedFeature("filmweb_drop", Seq("KINOWO_FILMWEB_DROP_TG_CHAT_ID"))
  private val on  = EnvGatedFeature("staging_stuck", Nil)

  "EnvGatedFeatureMetrics" should "export each alerter as 1 or 0 under its country" in {
    val registry = new PrometheusRegistry()
    new EnvGatedFeatureMetrics(registry).recordAlerters("pl", Seq(off, on))
    val text = PrometheusExposition.render(registry)
    PrometheusExposition.sample(text, "kinowo_worker_alerter_enabled", """alerter="filmweb_drop",country="pl"""") shouldBe Some(0.0)
    PrometheusExposition.sample(text, "kinowo_worker_alerter_enabled", """alerter="staging_stuck",country="pl"""") shouldBe Some(1.0)
  }

  it should "export each integration as 1 or 0" in {
    val registry = new PrometheusRegistry()
    new EnvGatedFeatureMetrics(registry).recordIntegrations(Seq(EnvGatedFeature("sentry", Seq("SENTRY_DSN")), EnvGatedFeature("tmdb", Nil)))
    val text = PrometheusExposition.render(registry)
    PrometheusExposition.sample(text, "kinowo_worker_integration_enabled", """integration="sentry"""") shouldBe Some(0.0)
    PrometheusExposition.sample(text, "kinowo_worker_integration_enabled", """integration="tmdb"""") shouldBe Some(1.0)
  }

  "EnvGatedFeature.disabledWarning" should "name each disabled feature and the keys that switched it off" in {
    EnvGatedFeature.disabledWarning("alerter", Seq(off, on)) shouldBe Some(
      "alerter filmweb_drop is OFF: missing KINOWO_FILMWEB_DROP_TG_CHAT_ID")
    EnvGatedFeature.disabledWarning("alerter", Seq(on)) shouldBe None
  }

  "WorkerMetrics" should "register both gauges on the shared registry" in {
    val metrics = WorkerMetrics.singleCountry(models.Country.Poland, poolSize = 1)
    metrics.envGatedFeatures.recordAlerters("pl", Seq(on))
    metrics.envGatedFeatures.recordIntegrations(Seq(on))
    val text = PrometheusExposition.render(metrics.registry)
    text should include ("kinowo_worker_alerter_enabled{")
    text should include ("kinowo_worker_integration_enabled{")
  }
}
