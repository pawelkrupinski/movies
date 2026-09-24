package modules

import controllers._
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.libs.streams.Accumulator
import play.api.mvc._
import play.api.routing.Router
import play.api.test.{FakeRequest, Helpers}
import play.api.{ApplicationLoader, BuiltInComponentsFromContext, Environment, Mode}
import play.filters.HttpFiltersComponents
import play.filters.cors.CORSComponents
import play.filters.gzip.GzipFilterComponents
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.Try

/**
 * Every route the application's router serves, and how each one that can change
 * state is protected — declared once, here, and then PROBED through the filter
 * chain production runs (`AppLoader.filterChain`) and the real controller.
 *
 * WHY A MATRIX. Every write route is `nocsrf` (the browser JS calls them with
 * `fetch`, the apps with a plain HTTP client, Meta server-to-server), so Play's
 * token check guards none of them. What stands between another site's page and
 * a visitor's account is: `CrossSiteWriteFilter`, the CORS policy refusing
 * credentials, the session cookie's SameSite — and whichever identity check the
 * controller makes. A new write route inherits none of that thinking by default;
 * this spec makes adding one a decision rather than an accident. It fails when:
 *
 *  - the router grows a non-GET route this matrix does not declare, or the
 *    matrix declares one the router no longer has;
 *  - a route's `nocsrf` modifier disagrees with what is declared;
 *  - a cross-site write (the browser's own `Sec-Fetch-Site: cross-site`, riding
 *    a signed-in admin's cookie) reaches ANY write route's controller — which is
 *    what removing `CrossSiteWriteFilter` from the chain does;
 *  - any origin is granted `Access-Control-Allow-Credentials` on any route,
 *    preflight or not — which is what Play's default `supportsCredentials = true`
 *    does;
 *  - a route declared as needing an identity answers an anonymous caller, or a
 *    signed-in account that is not an admin, with anything but what its class
 *    promises (admin routes: 403 to a non-admin).
 *
 * The harness is not a stand-in for the guards: the positive control below
 * shows the same requests from the site's own pages DO reach every controller,
 * so a refusal is the guard's doing and not a broken fixture.
 */
class RouteProtectionMatrixSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  import RouteProtectionMatrixSpec._

  // ── The declared matrix ────────────────────────────────────────────────────

  /** Every non-GET route, as the routes file spells it. */
  private val Matrix: Map[(String, String), Protection] = Map(
    ("POST",   "/debug/reenrich")                        -> Protection(Auth.DevOnly,  Csrf.CrossSiteFilter),
    ("POST",   "/:city/debug/rehydrate")                 -> Protection(Auth.Admin,    Csrf.CrossSiteFilter),
    // Signing out needs no identity: the worst a forged one does is sign you out,
    // and the cross-site filter refuses even that.
    ("POST",   "/auth/logout")                           -> Protection(Auth.Public,   Csrf.CrossSiteFilter),
    ("POST",   "/auth/sessions/revoke")                  -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    // The credential IS the body: a provider ID token / a one-shot code + verifier.
    ("POST",   "/auth/token")                            -> Protection(Auth.Public,   Csrf.CrossSiteFilter),
    ("POST",   "/auth/exchange")                         -> Protection(Auth.Public,   Csrf.CrossSiteFilter),
    ("PUT",    "/api/me/state")                          -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    ("PUT",    "/api/me/:country/hidden-films/:title")   -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    ("DELETE", "/api/me/:country/hidden-films/:title")   -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    ("DELETE", "/api/me/:country/hidden-films")          -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    ("DELETE", "/api/me")                                -> Protection(Auth.SignedIn, Csrf.CrossSiteFilter),
    // Anonymous by design (poster-load beacons), bounded by UptimeController.
    ("POST",   "/uptime/img-event")                      -> Protection(Auth.Public,   Csrf.CrossSiteFilter),
    ("POST",   "/tasks/run/:job")                        -> Protection(Auth.Admin,    Csrf.CrossSiteFilter),
    // Authenticated by Meta's HMAC-signed request, not a session.
    ("POST",   "/facebook/data-deletion")                -> Protection(Auth.Public,   Csrf.CrossSiteFilter),
    ("POST",   "/admin/config/set")                      -> Protection(Auth.Admin,    Csrf.CrossSiteFilter),
    ("POST",   "/admin/config/reset")                    -> Protection(Auth.Admin,    Csrf.CrossSiteFilter),
  )

  // ── The application under test: the real router, the real chain ────────────

  private val context = ApplicationLoader.Context.create(Environment.simple())

  /** Play's own filters, CORS and gzip from the configuration the loader boots
   *  with (`application.conf` over Play's defaults) — no database behind it. */
  private object Components extends BuiltInComponentsFromContext(context)
      with HttpFiltersComponents with CORSComponents with GzipFilterComponents {
    lazy val router: Router = Router.empty
  }
  private given Materializer = Components.materializer

  private val chain: Seq[EssentialFilter] = AppLoader.filterChain(
    metrics        = PassThrough,
    playDefaults   = Components.httpFilters,
    crossSiteWrite = new CrossSiteWriteFilter(),
    renamedCity    = new RenamedCityRedirectFilter("")(using Components.materializer),
    cors           = Components.corsFilter,
    csp            = new CspFilter()(using Components.materializer, Components.executionContext),
    gzip           = Components.gzipFilter)

  override def afterAll(): Unit = Await.result(Components.actorSystem.terminate(), 10.seconds): Unit

  private val users = TestAdminAction.adminRepository
  private val admin = users.findById(TestAdminAction.AdminUserId).get
  // Signed in, a real account, and on nobody's admin allowlist.
  private val member = {
    val user = models.User("member1", "google", "sub-member", Some("member@example.com"), Some("Member"), None,
      java.time.Instant.EPOCH, java.time.Instant.EPOCH)
    users.upsert(user)
    user
  }

  /** The generated router with a real controller behind every write route (its
   *  identity check is part of what is probed) and nothing behind the GET-only
   *  ones: a non-GET probe never dereferences them. */
  private val routes: router.Routes = {
    val cc          = Helpers.stubControllerComponents()
    val adminAction = TestAdminAction(users)
    given Messages  = testsupport.TestMessages.deployment
    val debug       = new DebugController(cc, null, null, adminAction, Mode.Prod)
    val auth        = new AuthController(cc, Map.empty, users, new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore), models.Country.Poland)
    val userState   = new UserStateController(cc, null, null, null,
      new services.metrics.LegacyUserStateMetrics(new io.prometheus.metrics.model.registry.PrometheusRegistry(), "pl"), users)
    val uptime      = new UptimeController(cc, adminAction, null, null, models.Country.Poland)
    val tasks       = new TasksController(cc, adminAction, null, null)
    val facebook    = new FacebookDataDeletionController(cc, None, users, null)
    val envConfig   = new EnvConfigController(cc, adminAction, null)
    new router.Routes(Components.httpErrorHandler, null, null, null, null, null, debug, null, auth, userState,
      null, null, uptime, tasks, null, null, facebook, envConfig, null)
  }

  /** `(verb, path as the routes file spells it)` for every route the router serves. */
  private val served: Seq[(String, String)] =
    routes.documentation.map { case (verb, pattern, _) => verb -> RoutesFileSpelling(pattern) }

  /** The query string a route's required non-path parameters need, or the router
   *  answers 400 itself and the probe never reaches the route. */
  private val requiredQuery: Map[(String, String), String] =
    routes.documentation.map { case (verb, pattern, call) =>
      val path   = RoutesFileSpelling(pattern)
      val params = "\\((.*)\\)".r.findFirstMatchIn(call).map(_.group(1)).toSeq
        .flatMap(_.split(",\\s*")).filter(_.contains(":"))
        .map { p => val Array(name, tpe) = p.split(":", 2); name.trim -> tpe.trim }
      val query  = params.collect {
        case (name, tpe) if !path.contains(s":$name") && !path.contains(s"*$name") && !tpe.startsWith("Option") =>
          s"$name=${if (tpe == "Int") "1" else "x"}"
      }
      (verb, path) -> (if (query.isEmpty) "" else query.mkString("?", "&", ""))
    }.toMap

  private val writes: Seq[(String, String)] = served.filterNot { case (verb, _) => verb == "GET" }

  // ── Driving one request through router + chain, as Play's handler does ─────

  /** What happened to one request: its response, and whether the controller's
   *  action was entered at all. */
  private final case class Outcome(status: Int, reachedController: Boolean, headers: Map[String, String])

  private def concrete(path: String): String = ":[a-zA-Z]+|\\*[a-zA-Z]+".r.replaceAllIn(path, "pl")

  /** Signed in as `user` as the account stands NOW: `/auth/sessions/revoke`,
   *  probed along the way, bumps the account's session version and so signs out
   *  every session minted before it. */
  private def signedInAs(user: models.User)(request: FakeRequest[AnyContentAsEmpty.type]) =
    request.withSession(SignedInUser.establish(Session(), users.findById(user.id).get).data.toSeq*)

  private def signedInAsAdmin(request: FakeRequest[AnyContentAsEmpty.type]) = signedInAs(admin)(request)

  /** Route `request` (as `DefaultHttpRequestHandler` does: find the handler, let
   *  it tag the request with its route definition), then run the tagged request
   *  through the production filter chain with a JSON body. */
  private def dispatch(request: FakeRequest[AnyContentAsEmpty.type]): Outcome = {
    val reached = new AtomicBoolean(false)
    val handler = routes.handlerFor(request).getOrElse(fail(s"no route for ${request.method} ${request.path}"))
    val (tagged, action) = Handler.applyStages(request, handler) match {
      case (rh, a: EssentialAction) => rh -> a
      case (_, other)               => fail(s"unexpected handler $other")
    }
    val observed = EssentialAction { rh => reached.set(true); action(rh) }
    val result = Try(Await.result(
      Filters(observed, chain*)(tagged).run(ByteString("{}")), 10.seconds))
    // A controller whose collaborators this spec leaves unwired throws once it is
    // past its own identity check; that it got that far is all a probe needs.
    val response = result.getOrElse(Results.InternalServerError)
    Outcome(response.header.status, reached.get, response.header.headers)
  }

  private def write(verb: String, path: String, headers: (String, String)*) =
    FakeRequest(verb, concrete(path) + requiredQuery((verb, path))).withHeaders((("Content-Type" -> "application/json") +: headers)*)

  // ── Declaration ────────────────────────────────────────────────────────────

  "the route protection matrix" should "declare every non-GET route the router serves, and nothing else" in {
    withClue("served but undeclared: ") { (writes.toSet -- Matrix.keySet) shouldBe empty }
    withClue("declared but not served: ") { (Matrix.keySet -- writes.toSet) shouldBe empty }
  }

  it should "declare each route's CSRF protection as its modifiers actually set it" in {
    writes.foreach { case (verb, path) =>
      val request   = write(verb, path)
      val (tagged, _) = Handler.applyStages(request, routes.handlerFor(request).get)
      val modifiers = tagged.attrs.get(Router.Attrs.HandlerDef).map(_.modifiers).getOrElse(Nil)
      val declared  = if (Matrix((verb, path)).csrf == Csrf.CrossSiteFilter) Seq("nocsrf") else Nil
      withClue(s"$verb $path: ") { modifiers.filter(_ == "nocsrf") shouldBe declared }
    }
  }

  // ── Probes ─────────────────────────────────────────────────────────────────

  "every write route" should "refuse a cross-site request before its controller, even riding a signed-in admin's cookie" in {
    writes.foreach { case (verb, path) =>
      val outcome = dispatch(signedInAsAdmin(write(verb, path,
        "Sec-Fetch-Site" -> "cross-site", "Origin" -> "https://evil.example", "Cookie" -> "PLAY_SESSION=x")))
      withClue(s"$verb $path: ") {
        outcome.reachedController shouldBe false
        outcome.status shouldBe 403
      }
    }
  }

  // The positive control for the probe above.
  it should "reach its controller from the site's own pages and from the native apps" in {
    writes.foreach { case (verb, path) =>
      withClue(s"$verb $path, same-origin: ") {
        dispatch(signedInAsAdmin(write(verb, path, "Sec-Fetch-Site" -> "same-origin", "Cookie" -> "PLAY_SESSION=x")))
          .reachedController shouldBe true
      }
      withClue(s"$verb $path, native app (no Sec-Fetch-*): ") {
        val outcome = dispatch(signedInAsAdmin(write(verb, path)))
        outcome.reachedController shouldBe true
        // Signed in for real: no identity class refuses an admin (dev-only
        // routes aside, which production 404s for everybody).
        if (Matrix((verb, path)).auth != Auth.DevOnly && Matrix((verb, path)).auth != Auth.Public)
          Seq(401, 403) should not contain outcome.status
      }
    }
  }

  it should "refuse an anonymous caller the way its declared identity class promises" in {
    writes.foreach { case (verb, path) =>
      val outcome = dispatch(write(verb, path, "Sec-Fetch-Site" -> "same-origin"))
      withClue(s"$verb $path (${Matrix((verb, path)).auth}): ") {
        Matrix((verb, path)).auth match {
          case Auth.SignedIn | Auth.Admin => outcome.status shouldBe 401
          case Auth.DevOnly               => outcome.status shouldBe 404
          case Auth.Public                => outcome.reachedController shouldBe true
        }
      }
    }
  }

  it should "answer a signed-in non-admin the way its declared identity class promises" in {
    writes.foreach { case (verb, path) =>
      val outcome = dispatch(signedInAs(member)(write(verb, path, "Sec-Fetch-Site" -> "same-origin")))
      withClue(s"$verb $path (${Matrix((verb, path)).auth}): ") {
        Matrix((verb, path)).auth match {
          case Auth.Admin                   => outcome.status shouldBe 403
          case Auth.DevOnly                 => outcome.status shouldBe 404
          case Auth.SignedIn =>
            outcome.reachedController shouldBe true
            outcome.status should not be 401
            outcome.status should not be 403
          // Its credential is the body (a code, a token, a signature), which
          // this probe does not carry — reaching the controller is the claim.
          case Auth.Public => outcome.reachedController shouldBe true
        }
      }
    }
  }

  "every route" should "never grant a foreign origin credentials, on a preflight or on the request itself" in {
    served.foreach { case (verb, path) =>
      val preflight = Await.result(Filters(Terminal, chain*)(FakeRequest("OPTIONS", concrete(path)).withHeaders(
        "Origin" -> "https://evil.example", "Access-Control-Request-Method" -> verb)).run(), 10.seconds)
      val direct = Await.result(Filters(Terminal, chain*)(FakeRequest(verb, concrete(path)).withHeaders(
        "Origin" -> "https://evil.example", "Cookie" -> "PLAY_SESSION=x")).run(), 10.seconds)
      withClue(s"$verb $path: ") {
        preflight.header.headers.get("Access-Control-Allow-Credentials") shouldBe None
        direct.header.headers.get("Access-Control-Allow-Credentials") shouldBe None
      }
    }
  }
}

object RouteProtectionMatrixSpec {

  /** Who may call a write route, as its controller decides. */
  enum Auth {
    /** A signed-in visitor (`SignedInUser`): anonymous gets 401. */
    case SignedIn
    /** An allowlisted admin (`AdminAction`): anonymous gets 401. */
    case Admin
    /** A dev-only endpoint (`DevMode.gate`): 404 in production. */
    case DevOnly
    /** No session identity; the route authenticates its body, or needs none. */
    case Public
  }

  /** What stands in for a CSRF token on a write route. */
  enum Csrf {
    /** `nocsrf`: Play's token check is off, `CrossSiteWriteFilter` refuses
     *  cross-site writes instead. */
    case CrossSiteFilter
    /** Play's CSRF token check applies. No route uses it today. */
    case PlayToken
  }

  final case class Protection(auth: Auth, csrf: Csrf)

  /** A generated route pattern (`/$city<[^/]+>/x`) as the routes file spells it
   *  (`/:city/x`), so the matrix reads like the file it guards. */
  def RoutesFileSpelling(pattern: String): String =
    "\\$([a-zA-Z]+)<\\[\\^/\\]\\+>".r.replaceAllIn(
      "\\$([a-zA-Z]+)<\\.\\+>".r.replaceAllIn(pattern, m => s"*${m.group(1)}"),
      m => s":${m.group(1)}")

  private object PassThrough extends EssentialFilter {
    def apply(next: EssentialAction): EssentialAction = next
  }

  private val Terminal: EssentialAction = EssentialAction(_ => Accumulator.done(Results.Ok("ok")))
}
