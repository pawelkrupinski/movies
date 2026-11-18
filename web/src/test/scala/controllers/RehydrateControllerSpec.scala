package controllers

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** `POST /{city}/debug/rehydrate`: the read-model reload, the one /debug endpoint that runs in prod. */
class RehydrateControllerSpec extends AnyFlatSpec with Matchers {

  private val records = Seq(
    ("Belle", Some(2021), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))),
  )

  // Unlike the other /debug pages, rehydrate runs in every mode (it reconciles a
  // live prod instance's caches) and mutates state — so it's gated by the admin
  // allowlist instead of left open, even in prod.
  private val rehydrateRequest = FakeRequest(POST, "/wroclaw/debug/rehydrate")

  "POST /…/debug/rehydrate" should "401 an anonymous request" in {
    val ctrl = TestDebugController.rehydrate(records)._1
    status(ctrl.rehydrate("wroclaw").apply(rehydrateRequest)) shouldBe UNAUTHORIZED
  }

  it should "403 a logged-in user not on the allowlist" in {
    val ctrl = TestDebugController.rehydrate(records,
      adminAction = TestAdminAction(allow = Set("someone-else@example.com")))._1
    status(ctrl.rehydrate("wroclaw").apply(
      rehydrateRequest.withSession("userId" -> TestAdminAction.AdminUserId))) shouldBe FORBIDDEN
  }

  it should "reload the caches for an allowlisted admin" in {
    val ctrl = TestDebugController.rehydrate(records)._1
    val result = ctrl.rehydrate("wroclaw").apply(
      rehydrateRequest.withSession("userId" -> TestAdminAction.AdminUserId))
    status(result) shouldBe OK
    contentAsString(result) should include("rehydrated")
  }
}
