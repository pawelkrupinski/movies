package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.EnvGatedFeature
import settings.{GatedIntegration, MissingSetting, ProcessConfiguration}

/** The external integrations the worker quietly runs without when a secret is missing — each
 *  a degraded production (no TMDB resolution, no residential proxy, no Sentry) that raises
 *  nothing. Pins that each is reported, with the key that switched it off. */
class WorkerIntegrationsSpec extends AnyFlatSpec with Matchers {

  private def resolved(vars: (String, String)*) = new ProcessConfiguration(tools.Env.of(vars*))

  "WorkerIntegrations.features" should "report every integration off, with its keys, in an empty environment" in {
    WorkerIntegrations.features(resolved()) shouldBe Seq(
      EnvGatedFeature("tmdb", Seq(MissingSetting("TMDB_API_KEY"))),
      EnvGatedFeature("omdb", Seq(MissingSetting("OMDB_API_KEY"))),
      EnvGatedFeature("residential_proxy", Seq(MissingSetting("KINOWO_PROXY_USER"), MissingSetting("KINOWO_PROXY_PASS"))),
      EnvGatedFeature("zyte", Seq(MissingSetting("ZYTE_API_KEY"))),
      EnvGatedFeature("sentry", Seq(MissingSetting("SENTRY_DSN"))),
      EnvGatedFeature("facebook_rescrape", Seq(MissingSetting("FACEBOOK_APP_ID"), MissingSetting("FACEBOOK_APP_SECRET"))))
  }

  it should "report one integration off when only its own key is missing" in {
    val everyKeyButOne = GatedIntegration.values.toSeq.flatMap(_.keys).filterNot(_ == "KINOWO_PROXY_PASS").map(_ -> "x")
    val features = WorkerIntegrations.features(resolved(everyKeyButOne*))
    features.filterNot(_.enabled) shouldBe Seq(EnvGatedFeature("residential_proxy", Seq(MissingSetting("KINOWO_PROXY_PASS"))))
  }
}
