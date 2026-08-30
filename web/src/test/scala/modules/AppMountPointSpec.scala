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

  // THE SIGN-IN IS SHARED ACROSS THE COUNTRIES ON ONE DOMAIN, and this is the
  // line that makes it so. /uk, /de and /us are one origin, so a session cookie
  // left at the host root is sent to all three: sign in on one and you are the
  // same person on the next. Scoped to the mount point instead — which is what
  // Play's `${play.http.context}` default would do, and what this deployment
  // used to do — crossing a path segment silently signs the visitor out.
  //
  // It is safe to share because the `userId` inside resolves against the SHARED
  // users database (`Country.usersDbName`), so it names the same account
  // whichever mount decodes it.
  it should "leave the session cookie at the host root, so one sign-in covers every country on the domain" in {
    httpConfigFor(Country.UnitedKingdom).session.path shouldBe "/"
    httpConfigFor(Country.Germany).session.path       shouldBe "/"
    httpConfigFor(Country.UnitedStates).session.path  shouldBe "/"

    // Poland is alone on kinowo.net and already mounted at the root; nothing
    // about it moves.
    httpConfigFor(Country.Poland).session.path shouldBe "/"
  }

  // Flash does NOT come along. It is a one-shot message attached to a single
  // redirect inside one deployment, so there is nothing it could mean a country
  // over — sharing it would just let /uk's message pop on /de. Play defaults it
  // to `${play.http.context}`, but that substitution resolves when
  // reference.conf is parsed, so the loader still has to set it alongside the
  // context rather than trust the default to follow.
  it should "keep the flash cookie scoped to the mount point" in {
    httpConfigFor(Country.UnitedKingdom).flash.path shouldBe "/uk/"
    httpConfigFor(Country.Germany).flash.path       shouldBe "/de/"
    httpConfigFor(Country.UnitedStates).flash.path  shouldBe "/us/"
    httpConfigFor(Country.Poland).flash.path        shouldBe "/"
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
      "/uk/kent/movie/dune"
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
      "/poznan/movie/diuna"
    new controllers.ReverseAssets("/").versioned(controllers.Assets.Asset("js/shared.js")).url shouldBe
      "/assets/js/shared.js"
  }
}
