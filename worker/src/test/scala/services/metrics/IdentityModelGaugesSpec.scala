package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.AgreementStage

/** The held gauges draw both holds of a take on its answers -- the input gate's listings and the agreement stage's
 *  previous takes standing while a refreshed answer's questions are checked -- so one pair of alerts
 *  (IdentityInputGateStuck / IdentityInputGateWaitsLong) pages on either. */
class IdentityModelGaugesSpec extends AnyFlatSpec with Matchers {
  import PrometheusExposition.{render, sample}

  "the held gauges" should "sum the gate's and the stage's holds, the oldest of either, and count a stage hold past the bound" in {
    val registry = new PrometheusRegistry()
    val gauges   = new IdentityModelGauges(Seq("us"), registry)
    gauges.forCountry("us").held(2, 30.0)
    val standing = gauges.standing("us")
    standing.applied(AgreementStage.Applied(0, 0, 0, 0, 0, Map.empty, 0, 0, 0.0, held = AgreementStage.Held(12, 400.0)))
    def read(name: String) = sample(render(registry), name, """country="us"""")
    read("kinowo_worker_identity_model_held_listings") shouldBe Some(14.0)
    read("kinowo_worker_identity_model_held_oldest_seconds") shouldBe Some(400.0)
    // the stage's hold ends (one at its bound), the gate's stands
    standing.applied(AgreementStage.Applied(0, 0, 0, 0, 0, Map.empty, 0, 0, 0.0, holdsBounded = Seq(600.0)))
    read("kinowo_worker_identity_model_held_listings") shouldBe Some(2.0)
    read("kinowo_worker_identity_model_held_oldest_seconds") shouldBe Some(30.0)
    sample(render(registry), "kinowo_worker_identity_model_promoted_total", """country="us",at="bound"""")
      .orElse(sample(render(registry), "kinowo_worker_identity_model_promoted_total", """at="bound",country="us"""")) shouldBe Some(1.0)
  }
}
