package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** `/health` answers whether the process is up (liveness — never wait on data, or a slow Mongo
 *  restarts a healthy pod); `/ready` whether it may take traffic (readiness — a rolling deploy
 *  must not retire a warm pod in favour of one whose boot read of the read model failed and so
 *  serves every city empty). */
class HealthControllerSpec extends AnyFlatSpec with Matchers {

  "ready" should "answer 503 until the read model has hydrated, then 200" in {
    @volatile var hydrated = false
    val controller = new HealthController(stubControllerComponents(), readiness = () => hydrated)

    status(controller.ready(FakeRequest("GET", "/ready"))) shouldBe SERVICE_UNAVAILABLE
    hydrated = true
    status(controller.ready(FakeRequest("GET", "/ready"))) shouldBe OK
  }

  "check" should "stay 200 while the read model is still cold — liveness never waits on data" in {
    val controller = new HealthController(stubControllerComponents(), readiness = () => false)
    status(controller.check(FakeRequest("GET", "/health"))) shouldBe OK
  }

  private final class Binding(@volatile var boundDegraded: Boolean, @volatile var restartRequired: Boolean) extends services.DatabaseBinding

  // A boot into an unreachable Mongo wires every repository to no database; the read model then
  // read "nothing" as a complete empty corpus and the pod reported ready serving no films.
  "ready" should "answer 503 while the process is bound to a Mongo it found unreachable at boot" in {
    val binding    = new Binding(boundDegraded = true, restartRequired = false)
    val controller = new HealthController(stubControllerComponents(), readiness = () => true, databases = Seq(binding))
    status(controller.ready(FakeRequest("GET", "/ready"))) shouldBe SERVICE_UNAVAILABLE
  }

  // ... and once Mongo is back nothing wired at boot can use it: only a restart rebinds them.
  "check" should "stay 200 while Mongo is still down, and answer 503 once it is back, so the pod restarts onto it" in {
    val binding    = new Binding(boundDegraded = true, restartRequired = false)
    val controller = new HealthController(stubControllerComponents(), readiness = () => true, databases = Seq(services.DatabaseBinding.Bound, binding))
    status(controller.check(FakeRequest("GET", "/health"))) shouldBe OK
    binding.restartRequired = true
    status(controller.check(FakeRequest("GET", "/health"))) shouldBe SERVICE_UNAVAILABLE
  }
}
