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

    patterns should contain ("/*/movie")
    // `/film` was the detail page's address before the rename. A Universal Link
    // is matched against this file and handed straight to the app, never
    // reaching the server's 301, so an old link only opens the app if the file
    // still claims the old shape too.
    patterns should contain ("/*/film")
    patterns should contain ("/*/")
    // OAuth must keep opening in the browser, so its exclude must come first.
    val authIdx = patterns.indexOf("/auth/*")
    val cityIdx = patterns.indexOf("/*/")
    (authIdx >= 0 && authIdx < cityIdx) shouldBe true
    (components(authIdx) \ "exclude").as[Boolean] shouldBe true
  }

  it should "send to Safari exactly the pages installed builds know another country's city by, and claim everything else" in {
    def components(body: String) = (Json.parse(body) \ "applinks" \ "details" \ 0 \ "components").as[Seq[JsObject]]
    val served = components(contentAsString(controller.appleAppSiteAssociation(FakeRequest())))
    val file   = components(scala.io.Source.fromResource("wellknown/apple-app-site-association", getClass.getClassLoader).mkString)
    def isExclude(c: JsObject) = (c \ "exclude").asOpt[Boolean].contains(true)

    // The displaced pages: /uk/york/ is Pennsylvania's York to a 2.0.11 build,
    // which drops the country segment and picks the city by slug.
    val expected = models.InstalledBuildSlugs.displaced.flatMap { case (country, city) =>
      Seq(s"${country.pathPrefix}/${city.slug}/", s"${country.pathPrefix}/${city.slug}/*")
    }
    expected should contain allOf ("/uk/york/", "/uk/york/*", "/us/kent/*", "/es/toledo/")
    expected should not contain "/us/york/"
    expected.size shouldBe 2 * models.InstalledBuildSlugs.displaced.size

    // Every entry of the file, in its order, with the excludes between its own
    // excludes and its first claim — an exclude only counts ahead of what it carves out of.
    val (fileExcludes, fileClaims) = file.span(isExclude)
    fileClaims.exists(isExclude) shouldBe false
    val added = served.drop(fileExcludes.size).take(expected.size)
    served shouldBe fileExcludes ++ added ++ fileClaims
    added.map(c => (c \ "/").as[String]) shouldBe expected
    added.forall(isExclude) shouldBe true
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
