package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

/**
 * Runs real site paths through the SERVED apple-app-site-association's
 * components the way iOS does, so a pattern that reads right but matches
 * nothing (a bare `movie` pattern against a film path with its slug) fails here
 * instead of quietly opening Safari on a phone.
 *
 * Apple's semantics, as [[AppleAppSiteAssociationMatchingSpec.decide]] mirrors
 * them: the components are tried in order and the FIRST whose pattern matches
 * the WHOLE path decides (an `exclude` one sends the link to the browser);
 * `*` matches any run of characters, `/` included, and `?` exactly one;
 * no match at all means the browser.
 */
class AppleAppSiteAssociationMatchingSpec extends AnyFlatSpec with Matchers {
  import AppleAppSiteAssociationMatchingSpec._

  private val components: Seq[JsObject] = {
    val body = contentAsString(new WellKnownController(Helpers.stubControllerComponents()).appleAppSiteAssociation(FakeRequest()))
    (Json.parse(body) \ "applinks" \ "details" \ 0 \ "components").as[Seq[JsObject]]
  }
  private def opensApp(path: String): Boolean = decide(components, path).contains(true)

  "the matcher" should "follow Apple's glob rules" in {
    globMatches("/*/movie", "/poznan/movie") shouldBe true
    globMatches("/*/movie", "/poznan/movie/zobaczysz-wszystko") shouldBe false
    globMatches("/*/", "/uk/london/") shouldBe true // `*` crosses `/`
    globMatches("/?/", "/a/") shouldBe true
    globMatches("/?/", "/ab/") shouldBe false
    decide(Seq(Json.obj("/" -> "/a*", "exclude" -> true), Json.obj("/" -> "/*")), "/ab") shouldBe Some(false)
    decide(Seq(Json.obj("/" -> "/x")), "/y") shouldBe None
  }

  "the served file" should "open film pages in the app, with or without a country prefix" in {
    for (path <- Seq(
           "/poznan/movie/zobaczysz-wszystko",
           "/poznan/film/zobaczysz-wszystko",
           "/warszawa/movie",
           "/uk/london/movie/dune",
           "/us/california/film/dune",
           "/uk/london/movie"))
      withClue(path)(opensApp(path) shouldBe true)
  }

  it should "open city listings in the app" in {
    for (path <- Seq("/poznan/", "/uk/london/", "/de/berlin/"))
      withClue(path)(opensApp(path) shouldBe true)
  }

  it should "keep the operational paths and the misopened installed-build cities in the browser" in {
    val misopened = models.InstalledBuildSlugs.misopened.flatMap { case (country, segment) =>
      Seq(s"${country.pathPrefix}/$segment/", s"${country.pathPrefix}/$segment/movie/dune")
    }
    misopened should contain ("/uk/york/movie/dune")
    for (path <- Seq(
           "/auth/google/callback",
           "/api/repertoire",
           "/poznan/api/repertoire",
           "/uk/auth/google/callback",
           "/uk/london/api/repertoire",
           "/admin/films",
           "/uk/admin/films",
           "/debug",
           "/uptime",
           "/tasks/run",
           "/metrics",
           "/uk/health",
           "/.well-known/apple-app-site-association",
           "/uk/.well-known/assetlinks.json") ++ misopened)
      withClue(path)(decide(components, path) shouldBe Some(false))
  }
}

object AppleAppSiteAssociationMatchingSpec {

  /** Whether Apple's `/` pattern matches the whole `path`: `*` any run (`/` included), `?` one character. */
  def globMatches(pattern: String, path: String): Boolean = {
    val p = pattern.toCharArray
    val s = path.toCharArray
    // dp(i)(j): p.take(i) matches s.take(j)
    val dp = Array.ofDim[Boolean](p.length + 1, s.length + 1)
    dp(0)(0) = true
    for (i <- 1 to p.length) {
      if (p(i - 1) == '*') dp(i)(0) = dp(i - 1)(0)
      for (j <- 1 to s.length)
        dp(i)(j) = p(i - 1) match {
          case '*' => dp(i - 1)(j) || dp(i)(j - 1)
          case '?' => dp(i - 1)(j - 1)
          case c   => c == s(j - 1) && dp(i - 1)(j - 1)
        }
    }
    dp(p.length)(s.length)
  }

  /** Some(true) the app, Some(false) an exclude matched first, None nothing matched (the browser). */
  def decide(components: Seq[JsObject], path: String): Option[Boolean] =
    components
      .find(c => globMatches((c \ "/").as[String], path))
      .map(c => !(c \ "exclude").asOpt[Boolean].contains(true))
}
