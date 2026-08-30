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
 * keep working, so this spec is written against the three audiences that follow
 * such a link and want different things — a person, a client, and a crawler:
 *
 *   - a PERSON who typed the old address or opened an old bookmark is told it
 *     changed, and handed the new one. That is the only thing a redirect could
 *     not do, and the only reason two of these routes render a page at all.
 *   - a CLIENT (the mobile apps' `/api/…` calls, a deep link into a film page)
 *     is redirected, permanently, method and query intact.
 *   - a CRAWLER scraping a shared link finds the LIVE page's own metadata, so a
 *     preview posted years ago still renders the way it always did.
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
  "an API call" should "move permanently to the same endpoint on the live site" in {
    val result = respond("GET", "/api/catalog")
    status(result)           shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("https://kinowo.net/api/catalog")
  }

  it should "keep the query string, which is where the whole request lives for a filtered call" in {
    redirectLocation(respond("GET", "/poznan/api/repertoire?date=2026-08-30&cinema=Kino+Muza")) shouldBe
      Some("https://kinowo.net/poznan/api/repertoire?date=2026-08-30&cinema=Kino+Muza")
  }

  // 308, not 301: a 301 lets a client turn a PUT into a GET and drop the body,
  // which is how a write silently stops working against the new host.
  it should "preserve the method and body of a write" in {
    status(respond("PUT",    "/api/me/state")) shouldBe PERMANENT_REDIRECT
    status(respond("POST",   "/auth/token"))   shouldBe PERMANENT_REDIRECT
    status(respond("DELETE", "/api/me"))       shouldBe PERMANENT_REDIRECT
  }

  "a deep link into a film page" should "land on the same film on the live site" in {
    val result = respond("GET", "/poznan/movie/diuna-czesc-druga")
    status(result)           shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("https://kinowo.net/poznan/movie/diuna-czesc-druga")
  }

  // The table is total on purpose: a retired host has no 404s to give, so a page
  // the live site grew after the move still gets there without this router
  // learning about it.
  "a path this router has never heard of" should "still be sent to the live site" in {
    redirectLocation(respond("GET", "/whatever/the/live/site/grew")) shouldBe
      Some("https://kinowo.net/whatever/the/live/site/grew")
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
      redirectLocation(Helpers.route(application, FakeRequest("GET", "/api/catalog")).get) shouldBe
        Some("https://kinowo.net/api/catalog")
    }
  }
}
