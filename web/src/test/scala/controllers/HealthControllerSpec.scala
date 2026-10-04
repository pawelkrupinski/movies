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
}
