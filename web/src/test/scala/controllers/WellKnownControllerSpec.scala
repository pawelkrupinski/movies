package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

/**
 * Locks the mobile app-association files: a malformed or wrongly-typed
 * Universal Links / App Links payload silently stops the apps from deep-linking
 * (the OS just falls back to the browser with no error), so the contract — valid
 * JSON, the right app identifiers, and `application/json` even for the
 * extensionless AASA path — is worth guarding.
 */
class WellKnownControllerSpec extends AnyFlatSpec with Matchers {

  private val controller = new WellKnownController(Helpers.stubControllerComponents())

  "apple-app-site-association" should "be valid JSON served as application/json" in {
    val result = controller.appleAppSiteAssociation(FakeRequest())

    status(result) shouldBe OK
    contentType(result) shouldBe Some("application/json")
    val json = Json.parse(contentAsString(result))
    (json \ "applinks" \ "details" \ 0 \ "appIDs" \ 0).as[String] shouldBe "CQ4YC43YDM.dev.kinowo.Kinowo"
  }

  it should "match the film and city paths but exclude OAuth callbacks" in {
    val json = Json.parse(contentAsString(controller.appleAppSiteAssociation(FakeRequest())))
    val components = (json \ "applinks" \ "details" \ 0 \ "components").as[Seq[JsObject]]
    val patterns = components.map(c => (c \ "/").as[String])

    patterns should contain ("/*/film")
    patterns should contain ("/*/")
    // OAuth must keep opening in the browser, so its exclude must come first.
    val authIdx = patterns.indexOf("/auth/*")
    val cityIdx = patterns.indexOf("/*/")
    (authIdx >= 0 && authIdx < cityIdx) shouldBe true
    (components(authIdx) \ "exclude").as[Boolean] shouldBe true
  }

  "assetlinks.json" should "be valid JSON declaring the pl.kinowo package" in {
    val result = controller.assetLinks(FakeRequest())

    status(result) shouldBe OK
    contentType(result) shouldBe Some("application/json")
    val json = Json.parse(contentAsString(result))
    val packages = (json \\ "package_name").map(_.as[String])
    packages should contain ("pl.kinowo")
    packages should contain ("pl.kinowo.debug")
    (json \ 0 \ "relation" \ 0).as[String] shouldBe "delegate_permission/common.handle_all_urls"
  }
}
