package modules

import models.Country

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.http.HttpConfiguration
import play.api.test.FakeRequest
import play.api.{ApplicationLoader, Environment}

/**
 * Where this deployment's URLs START.
 *
 * Three of the four countries share `showtimes.cc` and are told apart by a
 * leading path segment (`showtimes.cc/uk/kent/`); Poland keeps `kinowo.net/`
 * at the root. One process still serves one country against one database — only
 * the address moved — so the whole application is MOUNTED at that segment via
 * Play's `play.http.context`, which the loader derives from the country rather
 * than repeating as a literal in each Kubernetes overlay.
 *
 * The two halves that must agree are asserted separately here because they are
 * reached through different mechanisms: the configuration the loader writes
 * (which Play turns into route matching and cookie paths) and the reverse
 * routes the templates emit. Both take the mount point verbatim.
 */
class AppMountPointSpec extends AnyFlatSpec with Matchers {

  private def stubAction(body: String): play.api.mvc.Action[play.api.mvc.AnyContent] =
    play.api.test.Helpers.stubControllerComponents().actionBuilder(play.api.mvc.Results.Ok(body))

  private def httpConfigFor(country: Country): HttpConfiguration = {
    val context = AppLoader.mountedAt(
      ApplicationLoader.Context.create(Environment.simple()), country)
    HttpConfiguration.fromConfiguration(context.initialConfiguration, context.environment)
  }

  "the loader" should "mount a shared-domain country under its country segment" in {
    httpConfigFor(Country.UnitedKingdom).context shouldBe "/uk/"
    httpConfigFor(Country.Germany).context       shouldBe "/de/"
    httpConfigFor(Country.UnitedStates).context  shouldBe "/us/"
  }

  // Poland owns kinowo.net outright: it must stay at the root, byte-for-byte on
  // the URLs it has always served. A prefix here would break every published
  // link, every share card and both app stores' listings at once.
  it should "leave the country that owns its domain at the root" in {
    httpConfigFor(Country.Poland).context shouldBe "/"
  }

  // On a shared domain a cookie left at "/" is a cookie the NEIGHBOURING
  // countries send and overwrite — one signed-in session, or one remembered
  // city, leaking across /uk and /de. Play defaults both paths to
  // `${play.http.context}` in reference.conf, but that substitution resolves
  // when the file is parsed, so the loader has to set them alongside the
  // context rather than trusting the default to follow.
  it should "scope the session and flash cookies to the mount point" in {
    val uk = httpConfigFor(Country.UnitedKingdom)
    uk.session.path shouldBe "/uk/"
    uk.flash.path   shouldBe "/uk/"

    val pl = httpConfigFor(Country.Poland)
    pl.session.path shouldBe "/"
    pl.flash.path   shouldBe "/"
  }

  // Every `@routes.…` in a template and every `controllers.routes.…` in a
  // controller goes through one of these, reading the prefix `Routes.withPrefix`
  // published. Instantiated directly with the prefix so the assertion doesn't
  // have to mutate the process-wide `router.RoutesPrefix` other suites render
  // against.
  "reverse routes" should "carry the mount point on every kind of URL" in {
    new controllers.ReverseLandingController("/uk/").index().url shouldBe "/uk/"
    new controllers.ReverseMovieController("/uk/").index("kent").url shouldBe "/uk/kent/"
    new controllers.ReverseMovieController("/uk/").filmBySlug("kent", "dune").url shouldBe
      "/uk/kent/film/dune"
    new controllers.ReverseMovieController("/uk/").sitemap.url shouldBe "/uk/sitemap.xml"
    new controllers.ReverseAssets("/uk/").versioned(controllers.Assets.Asset("js/shared.js")).url shouldBe
      "/uk/assets/js/shared.js"
  }

  // `/health` is hit by the kubelet on the POD's own address and `/metrics` by a
  // Prometheus that scrapes the NodePort directly. Neither request goes through
  // Caddy, so neither ever carries the country prefix — mounting them under it
  // would crashloop every non-Polish pod and blank its metrics.
  "the operational endpoints" should "answer at the host root, wherever the app is mounted" in {
    val routes = AppLoader.rootOperationalRoutes(stubAction("health"), stubAction("metrics"))
    routes.handlerFor(FakeRequest("GET", "/health")) should be (defined)
    routes.handlerFor(FakeRequest("GET", "/metrics")) should be (defined)
  }

  it should "claim nothing else, so the mounted router still owns every page" in {
    val routes = AppLoader.rootOperationalRoutes(stubAction("health"), stubAction("metrics"))
    routes.handlerFor(FakeRequest("GET", "/")) shouldBe empty
    routes.handlerFor(FakeRequest("GET", "/uk/kent/")) shouldBe empty
    routes.handlerFor(FakeRequest("GET", "/uk/health")) shouldBe empty
    routes.handlerFor(FakeRequest("POST", "/health")) shouldBe empty
  }

  it should "be unchanged at the root, so Poland's URLs cannot move" in {
    new controllers.ReverseLandingController("/").index().url shouldBe "/"
    new controllers.ReverseMovieController("/").index("poznan").url shouldBe "/poznan/"
    new controllers.ReverseMovieController("/").filmBySlug("poznan", "diuna").url shouldBe
      "/poznan/film/diuna"
    new controllers.ReverseAssets("/").versioned(controllers.Assets.Asset("js/shared.js")).url shouldBe
      "/assets/js/shared.js"
  }
}
