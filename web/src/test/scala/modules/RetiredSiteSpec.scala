package modules

import controllers.{FilterDescription, RetiredSiteController, WellKnownController}
import models.Country
import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{Action, AnyContent, Handler, Result, Results}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import play.api.{ApplicationLoader, Environment}

import scala.concurrent.Future

/**
 * `kinowo.fly.dev` after the move to `kinowo.net`: a host that still answers,
 * but whose site lives somewhere else now.
 *
 * The whole point of keeping the old host alive is that the links pointing at it
 * keep working, so this spec is written against the audiences that follow such a
 * link and want different things — a person, a client, and a crawler:
 *
 *   - a PERSON who typed the old address, opened an old bookmark, or followed a
 *     deep link (a film page, `/plan`, a legal page) is told it changed, and
 *     handed the new one. `/` and `/{city}/` do this with the live page's own
 *     metadata; everything else does it with the brand-level fallback, because
 *     there is no per-page title/image to give it with no database behind this
 *     process.
 *   - a CLIENT calling `/api/…` is told to upgrade, not transparently handed the
 *     live API's current (possibly drifted) response.
 *   - a CLIENT writing outside `/api/…` (`/auth/token`, `/uptime/img-event`) is
 *     redirected, permanently, method and query intact — unchanged, because a
 *     program wants the resource, not a page to read.
 *   - a CRAWLER scraping a shared `/` or `/{city}/` link finds the LIVE page's
 *     own metadata, so a preview posted years ago still renders the way it
 *     always did; a crawler on any other link gets the same brand-level notice
 *     a person landing there does.
 */
class RetiredSiteSpec extends AnyFlatSpec with Matchers {

  private val country = Country.Poland
  private val poznan  = country.bySlug("poznan")

  private val router = AppLoader.retiredRoutes(
    new RetiredSiteController(
      Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi), country),
    new WellKnownController(Helpers.stubControllerComponents()),
    file => Helpers.stubControllerComponents().actionBuilder(Results.Ok(s"asset:$file")))

  private def respond(method: String, path: String): Future[Result] = {
    val request = FakeRequest(method, path)
    router.routes.lift(request) match {
      case Some(action: Action[?]) => action.asInstanceOf[Action[AnyContent]].apply(request)
      case Some(other: Handler)    => fail(s"$method $path routed to a non-action handler: $other")
      case None                    => fail(s"$method $path is not routed at all")
    }
  }

  private def body(path: String): String = contentAsString(respond("GET", path))

  // ── the person ────────────────────────────────────────────────────────────
  "the landing" should "say the address changed and link to the live site" in {
    val html = body("/")
    status(respond("GET", "/")) shouldBe OK
    html should include ("Zmieniliśmy adres")
    html should include ("""<a class="go" href="https://kinowo.net/">""")
    html should include ("Przejdź do kinowo.net")
  }

  "a city page" should "link to that same city on the live site, not just its front page" in {
    val html = body("/poznan/")
    status(respond("GET", "/poznan/")) shouldBe OK
    html should include ("Zmieniliśmy adres")
    html should include ("""<a class="go" href="https://kinowo.net/poznan/">""")
    html should include ("Przejdź do kinowo.net/poznan")
  }

  // A `/{segment}/` that is not one of this country's cities is not a city page
  // at all, so it gets a client's answer rather than a reader's.
  it should "redirect a segment that is not one of this country's cities" in {
    val result = respond("GET", "/not-a-city/")
    status(result)                     shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("https://kinowo.net/not-a-city/")
  }

  // ── the crawler ───────────────────────────────────────────────────────────
  // Byte-for-byte the values the live `/{city}/` page passes to `_ogTagsApp`
  // (`repertoire.scala.html`) and `landing.scala.html` pass to it — computed
  // from the city and the country, never from the repertoire, which is what
  // lets a database-less process still render them.
  "a shared link's preview" should "carry the live city page's own title, description and card" in {
    val html = body("/poznan/")
    html should include (s"<title>${FilterDescription.defaultTitle(poznan)}</title>")
    html should include (s"""<meta property="og:title"       content="${FilterDescription.defaultTitle(poznan)}">""")
    html should include (s"""<meta property="og:description" content="${FilterDescription.defaultDescription(poznan)}">""")
    html should include ("""<meta property="og:image"       content="https://kinowo.net/assets/img/og-poznan.jpg">""")
    html should include ("""<meta name="twitter:card"        content="summary_large_image">""")
  }

  it should "carry the live landing's own title, description and card" in {
    val html = body("/")
    html should include ("<title>Kinowo — repertuar kin w Twoim mieście</title>")
    html should include (s"""<meta property="og:image"       content="https://kinowo.net/assets/img/${country.homeOgImage}">""")
  }

  // The old host is still its own Search Console property, and that property
  // rests on this tag. Losing it un-verifies the property, and an unverified
  // property cannot be handed the Change of Address that tells Google the move
  // is permanent — the one thing a retired host is in a position to do.
  it should "keep the verification tag the retired host's own Search Console property rests on" in {
    body("/") should include ("""<meta name="google-site-verification" content="GHV7eYMZc7PnJlXt03b8TU5ZsLib0pSDYOgIr08ifTE" />""")
  }

  // og:url and rel=canonical both name the LIVE page. Self-canonicalising here
  // would leave the retired host competing with the site it points at for the
  // same query, which is the one thing a permanent move is supposed to settle.
  it should "point og:url and the canonical at the live page, not at this host" in {
    body("/poznan/") should include ("""<meta property="og:url"         content="https://kinowo.net/poznan/">""")
    body("/poznan/") should include ("""<link rel="canonical" href="https://kinowo.net/poznan/">""")
    body("/")        should include ("""<link rel="canonical" href="https://kinowo.net/">""")
  }

  // ── the client ────────────────────────────────────────────────────────────
  "an API call" should "be told to upgrade rather than handed the live API's response" in {
    val result = respond("GET", "/api/catalog")
    status(result) shouldBe UPGRADE_REQUIRED
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe models.ClientSupport.json
  }

  it should "carry the same ETag GET /api/client-support serves on the live site" in {
    header("ETag", respond("GET", "/api/catalog")) shouldBe Some(models.ClientSupport.etag)
  }

  it should "answer the same way for a city-scoped API path" in {
    status(respond("GET", "/poznan/api/repertoire?date=2026-08-30")) shouldBe UPGRADE_REQUIRED
  }

  // A write is told to upgrade exactly like a read — `/api/me/state` and
  // `/api/me` both carry `/api/` — but a write OUTSIDE `/api/…` still redirects,
  // preserving method and body: a 301 there would let a client turn a PUT into a
  // GET and drop the body, which is how a write silently stops working.
  it should "tell a write under /api/ to upgrade too, but still redirect one outside it" in {
    status(respond("PUT",    "/api/me/state")) shouldBe UPGRADE_REQUIRED
    status(respond("DELETE", "/api/me"))       shouldBe UPGRADE_REQUIRED
    status(respond("POST",   "/auth/token"))   shouldBe PERMANENT_REDIRECT
  }

  // ── the deep link ─────────────────────────────────────────────────────────
  // No database means no real title/poster for a specific film, so it falls
  // back to the same brand-level notice `/` renders — but the LINK still goes
  // to the actual film, not to the landing page.
  "a deep link into a film page" should "render the brand-level notice, linking to the same film" in {
    val html = body("/poznan/movie/diuna-czesc-druga")
    status(respond("GET", "/poznan/movie/diuna-czesc-druga")) shouldBe OK
    html should include ("Zmieniliśmy adres")
    html should include ("""<a class="go" href="https://kinowo.net/poznan/movie/diuna-czesc-druga">""")
    html should include ("<title>Kinowo — repertuar kin w Twoim mieście</title>")
  }

  // The table is total on purpose: a retired host has no 404s to give, so a page
  // the live site grew after the move still gets the notice without this router
  // learning about it.
  "a path this router has never heard of" should "still render the notice, linking to the live site" in {
    val result = respond("GET", "/whatever/the/live/site/grew")
    status(result) shouldBe OK
    contentAsString(result) should include ("""<a class="go" href="https://kinowo.net/whatever/the/live/site/grew">""")
  }

  // robots.txt/sitemap.xml/og-image are read by a machine, not a person — a
  // notice page would be the wrong bytes for all three, so they keep the
  // redirect even though they are plain GETs.
  "a machine file" should "still redirect rather than render the notice" in {
    redirectLocation(respond("GET", "/robots.txt"))              shouldBe Some("https://kinowo.net/robots.txt")
    redirectLocation(respond("GET", "/sitemap.xml"))              shouldBe Some("https://kinowo.net/sitemap.xml")
    redirectLocation(respond("GET", "/poznan/og-image"))          shouldBe Some("https://kinowo.net/poznan/og-image")
    redirectLocation(respond("GET", "/poznan/movie/og-image"))    shouldBe Some("https://kinowo.net/poznan/movie/og-image")
  }

  // ── what has to keep answering locally ────────────────────────────────────
  // An app installed before the move still resolves its Universal Links against
  // this host; redirecting the association file breaks them (Apple's CDN does
  // not follow one), and the app opens Safari instead.
  "the app-association files" should "still be served here rather than redirected" in {
    status(respond("GET", "/.well-known/apple-app-site-association")) shouldBe OK
    status(respond("GET", "/.well-known/assetlinks.json"))            shouldBe OK
  }

  "the notice page's own assets" should "be served locally, so it can render itself" in {
    body("/assets/img/favicon.svg") shouldBe "asset:img/favicon.svg"
  }

  // ── the boot ──────────────────────────────────────────────────────────────
  // THE REASON RETIREMENT IS A SEPARATE COMPOSITION ROOT rather than a flag
  // inside the real one: with no `Wiring` mixed in there is no Mongo client, no
  // change stream and no users database to open, so the host cannot go on
  // loading the live site's database for as long as it stays up. This boots the
  // real application — filters, mount point, error handler and all — with no
  // MONGODB_URI in the environment, which the serving components treat as a hard
  // boot failure.
  "a retired deployment" should "boot and serve with no database behind it" in {
    val application = new RetiredComponents(
      AppLoader.mountedAt(ApplicationLoader.Context.create(Environment.simple()), country),
      country).application
    Helpers.running(application) {
      status(Helpers.route(application, FakeRequest("GET", "/health")).get)   shouldBe OK
      status(Helpers.route(application, FakeRequest("GET", "/metrics")).get)  shouldBe NOT_FOUND
      contentAsString(Helpers.route(application, FakeRequest("GET", "/poznan/")).get) should include ("Zmieniliśmy adres")
      status(Helpers.route(application, FakeRequest("GET", "/api/catalog")).get) shouldBe UPGRADE_REQUIRED
    }
  }
}
