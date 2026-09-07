package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** `/{city}` — the listing address without its trailing slash.
 *
 *  The routes file binds the listing at `/{city}/`, so the slash-less spelling
 *  404'd: Googlebot asked showtimes.cc for `/de/karlsruhe` while
 *  `/de/karlsruhe/` answered 200. It now 301s onto the canonical form.
 *
 *  What these cases guard is the two ways that fold can go wrong without going
 *  missing — a redirect that drops the mount prefix lands off the country's own
 *  site, and a catch-all that redirects before resolving turns every mistyped
 *  top-level path into a 301 onto a URL that 404s one hop later. */
class CityTrailingSlashRedirectSpec extends AnyFlatSpec with Matchers {

  private def controller(country: models.Country = models.Country.default): MovieController =
    TestMovieController.build(Nil, servingCountry = country)._1

  private def locationOf(result: scala.concurrent.Future[play.api.mvc.Result]): String =
    header(LOCATION, result).getOrElse(fail("no Location header"))

  "the slash-less city address" should "301 onto the canonical trailing-slash form" in {
    val result = controller().indexNoTrailingSlash("poznan").apply(FakeRequest(GET, "/poznan"))
    status(result)     shouldBe MOVED_PERMANENTLY
    locationOf(result) shouldBe "/poznan/"
  }

  it should "carry the query string, so a shared filter link survives the hop" in {
    val result = controller().indexNoTrailingSlash("poznan")
      .apply(FakeRequest(GET, "/poznan?cinema=Helios&genre=Komedia"))
    locationOf(result) shouldBe "/poznan/?cinema=Helios&genre=Komedia"
  }

  // Play strips `play.http.context` before matching, so a Location built from
  // the route's own `:city` would send `showtimes.cc/uk/kent` to
  // `showtimes.cc/kent` — off this country's site entirely.
  "a country that shares the brand domain" should "keep its mount prefix in the redirect" in {
    val uk = controller(models.Country.UnitedKingdom)
    locationOf(uk.indexNoTrailingSlash("kent").apply(FakeRequest(GET, "/kent"))) shouldBe "/uk/kent/"
  }

  // The route is a catch-all for ANY single top-level segment — every mistyped
  // path and every scanner probe lands here. Redirecting before resolving would
  // answer each one with a 301 onto a URL that 404s one hop later.
  "an unknown city" should "404 rather than redirect onto a URL that 404s one hop later" in {
    status(controller().indexNoTrailingSlash("atlantyda").apply(FakeRequest(GET, "/atlantyda"))) shouldBe NOT_FOUND
  }

  "a city this deployment does not serve" should "404 like any other unknown slug" in {
    // Berlin is a real city, just not a Polish one — the same country scope
    // `ServedCity` applies to the listing itself.
    status(controller().indexNoTrailingSlash("berlin").apply(FakeRequest(GET, "/berlin"))) shouldBe NOT_FOUND
  }

  /** The routes file is the only place that says `/{city}` is REACHABLE, and
   *  the only place that says it does not shadow anything. `/:city` matches
   *  every single-segment path — `/health`, `/metrics`, `/robots.txt`,
   *  `/sitemap.xml` included — so Play's top-down matching is what keeps those
   *  answering themselves rather than being read as city slugs. It holds only
   *  while the catch-all stays BELOW them. */
  "the routes file" should "bind /:city below every literal top-level route" in {
    val stream = getClass.getResourceAsStream("/routes")
    stream should not be null
    val source = scala.io.Source.fromInputStream(stream)
    val lines  = try source.getLines().toList finally source.close()

    val catchAll = lines.indexWhere(_.startsWith("GET     /:city "))
    catchAll should be >= 0

    val literals = Seq("/health", "/metrics", "/uptime", "/tasks", "/debug",
                       "/support", "/robots.txt", "/sitemap.xml", "/privacy-policy")
    literals.foreach { path =>
      val at = lines.indexWhere(l => l.startsWith("GET ") && l.split("\\s+").lift(1).contains(path))
      withClue(s"$path must be routed before the /:city catch-all: ") {
        at should be >= 0
        at should be < catchAll
      }
    }
  }
}
