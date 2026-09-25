package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.EnvGatedFeature

/** The external integrations the worker quietly runs without when a secret is missing — each
 *  a degraded production (no TMDB resolution, no residential proxy, no Sentry) that raises
 *  nothing. Pins that each is reported, with the key that switched it off. */
class WorkerIntegrationsSpec extends AnyFlatSpec with Matchers {

  "WorkerIntegrations.features" should "report every integration off, with its keys, in an empty environment" in {
    WorkerIntegrations.features(_ => None) shouldBe Seq(
      EnvGatedFeature("tmdb", Seq("TMDB_API_KEY")),
      EnvGatedFeature("omdb", Seq("OMDB_API_KEY")),
      EnvGatedFeature("residential_proxy", Seq("KINOWO_PROXY_USER", "KINOWO_PROXY_PASS")),
      EnvGatedFeature("zyte", Seq("ZYTE_API_KEY")),
      EnvGatedFeature("sentry", Seq("SENTRY_DSN")),
      EnvGatedFeature("facebook_rescrape", Seq("FACEBOOK_APP_ID", "FACEBOOK_APP_SECRET")))
  }

  it should "report one integration off when only its own key is missing" in {
    val features = WorkerIntegrations.features(key => Option.when(key != "KINOWO_PROXY_PASS")("x"))
    features.filterNot(_.enabled) shouldBe Seq(EnvGatedFeature("residential_proxy", Seq("KINOWO_PROXY_PASS")))
  }
}
