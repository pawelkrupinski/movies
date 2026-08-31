package controllers

import models.User
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.auth.{AppleTokenValidator, AuthExchangeCodes, FacebookTokenValidator, GoogleTokenValidator, OauthProfile, OauthProvider}
import services.users.UserRepository

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Clock
import java.util.UUID
import scala.util.{Failure, Success, Try}

/**
 * OAuth2 authorization-code flow controller. Two endpoints per
 * provider:
 *
 *   - GET `/auth/:provider/start` — generate a CSRF `state`, stash it
 *     in the session cookie, redirect to the provider's consent URL.
 *   - GET `/auth/:provider/callback` — verify `state`, exchange `code`
 *     for a profile via `OauthProvider.exchangeCode`, upsert the
 *     `User`, set `userId` in the session, redirect home.
 *
 * Plus a single logout endpoint that drops the session, and the pair that hands
 * a live session to a deployment the cookie cannot reach:
 *
 *   - GET `/auth/sso/start?to=<base url>` — on the deployment the visitor is
 *     signed in to: mint a one-shot code and redirect to `to`'s `finish`.
 *   - GET `/auth/sso/finish?code=…` — on the deployment being handed the
 *     session: spend the code, set `userId`, land on that country's home.
 *
 * That pair exists for ONE boundary. `/uk`, `/de` and `/us` share an origin and
 * so share the session cookie outright (`AppLoader.mountedAt` leaves its path at
 * the host root); Poland is on `kinowo.net`, a different registrable domain,
 * where no cookie can follow. `to` is matched against the deployed countries'
 * own base URLs and nothing else, so it cannot be pointed anywhere off-brand.
 *
 * `providers` is keyed by `OauthProvider.name` so the `:provider`
 * route segment indexes directly into it. An unknown provider name
 * (or one whose env vars weren't set so it wasn't wired) returns
 * 404 — the UI never renders the corresponding login button in that
 * case, so this is purely a defence-in-depth check against hand-
 * crafted URLs.
 *
 * Session keys used:
 *   - `oauthState`    — random UUID, set on start, verified on callback
 *   - `oauthProvider` — which provider issued the state (defends against
 *     mixing state from /auth/google/start with a callback to
 *     /auth/facebook/callback)
 *   - `userId`        — set on successful callback, dropped on logout
 */
class AuthController(
  cc:                     ControllerComponents,
  providers:              Map[String, OauthProvider],
  userRepository:               UserRepository,
  // One-shot codes for the two handoffs a session cookie cannot make: the
  // native apps' `kinowo://` deep link, and the cross-domain country switch.
  exchangeCodes:          AuthExchangeCodes,
  // The country THIS deployment serves. A web process serves exactly one (see
  // `Wiring.deploymentMessages`), so it is fixed at boot rather than derived per
  // request: it is what a starting flow stamps into its `state`, and what tells
  // a finishing one whether it is home or standing in for a sibling.
  country:                models.Country,
  googleTokenValidator:   Option[GoogleTokenValidator] = None,
  facebookTokenValidator: Option[FacebookTokenValidator] = None,
  appleTokenValidator:    Option[AppleTokenValidator] = None,
  clock:                  Clock = Clock.systemUTC()
) extends AbstractController(cc) with Logging {

  // OAuth state cookie expires after this — long enough that the user
  // can take a couple of minutes on the provider's consent screen,
  // short enough that a stale browser tab carrying old state isn't a
  // forever-valid CSRF surface. The value's not security-critical
  // (state is single-use and tied to a random UUID), but bounding it
  // is hygiene.
  private val OauthStateTtl = java.time.Duration.ofMinutes(10)

  def start(provider: String): Action[AnyContent] = Action { request =>
    providers.get(provider) match {
      case None =>
        NotFound(s"Provider not configured: $provider")
      case Some(p) =>
        val state       = AuthController.newState(country)
        val redirectUri = callbackUrl(provider, request)
        // Native iOS *and* Android clients pass `?platform=…`; both want the
        // callback to bounce back into the app via the `kinowo://` deep link
        // (carrying a one-shot exchange code) instead of redirecting to `/`.
        val isMobile = request.getQueryString("platform").exists(Set("ios", "android"))
        Redirect(p.authUrl(state, redirectUri))
          .withSession(request.session
            + ("oauthState"     -> state)
            + ("oauthProvider"  -> provider)
            + ("oauthStateTimestamp"   -> clock.instant().toEpochMilli.toString)
            ++ (if (isMobile) Seq("mobileClient" -> "1") else Seq.empty))
    }
  }

  /** Where the provider comes back to — ONE registered URL per provider for the
   *  whole project, so this action runs on the apex deployment for flows that
   *  started anywhere.
   *
   *  It answers one question before it does anything else: is the browser in
   *  front of me still carrying the cookie that holds this flow's CSRF state? It
   *  is exactly when the country that STARTED the flow is served by the origin
   *  this request arrived on — the three countries under `showtimes.cc` share
   *  one, `kinowo.net` is its own. When it is, finish here. When it is not,
   *  hand the provider's own `code` and `state` straight on to the deployment
   *  that can, unread and unexchanged.
   *
   *  Relaying rather than verifying a self-signed `state` is the whole point of
   *  the shape. The check that survives is "the state in this URL equals the one
   *  in YOUR cookie", which binds the callback to the browser that started the
   *  flow; a state we merely signed ourselves would prove only that we issued
   *  it, and would let an attacker finish a flow of their own in someone else's
   *  browser. */
  def callback(provider: String): Action[AnyContent] = Action { request =>
    relayTarget(provider, request) match {
      case Some(target) =>
        logger.info(s"Relaying the $provider callback on to $target — this flow started on another origin.")
        Redirect(target)
      case None =>
        complete(provider, request)
    }
  }

  /** The absolute callback URL this flow should have landed on, when that is NOT
   *  here. `None` — finish it here — whenever the flow is at home on this
   *  request's origin, whenever the `state` does not name a country (a flow
   *  started before this shape existed, mid-deploy), and always off a deployed
   *  origin, so a developer on localhost finishes where they started instead of
   *  being thrown at production. */
  private def relayTarget(provider: String, request: RequestHeader): Option[String] = {
    val origin = ForwardedUrl.base(request)
    for {
      state <- request.getQueryString("state")
      home  <- AuthController.stateCountry(state)
      // ORIGIN decides — that is what the browser scopes a cookie to, and the
      // whole question is whether this request still carries the flow's state.
      if models.Country.deployedOrigins.contains(origin) && !home.webOrigin.contains(origin)
      // BASE URL builds the address, because a country's callback lives under
      // its mount point. Identical today for the only country this relays to,
      // and not an assumption worth leaving buried.
      base  <- home.webUrl
    } yield base + AuthController.callbackPath(provider) + AuthController.relayQuery(request)
  }

  private def complete(provider: String, request: RequestHeader): Result = {
    val parsed = for {
      p             <- providers.get(provider).toRight(s"Unknown provider: $provider")
      code          <- request.getQueryString("code").toRight("Missing code")
      state         <- request.getQueryString("state").toRight("Missing state")
      expectedState <- request.session.get("oauthState").toRight("Missing session state — start over from /auth")
      sessionProv   <- request.session.get("oauthProvider").toRight("Missing session provider")
      issuedMs      <- request.session.get("oauthStateTimestamp").flatMap(_.toLongOption).toRight("Missing or unparseable oauthStateTimestamp")
      _             <- Either.cond(state == expectedState, (), "OAuth state mismatch (possible CSRF)")
      _             <- Either.cond(sessionProv == provider, (), s"Provider mismatch: session=$sessionProv, callback=$provider")
      _             <- Either.cond(
                         clock.instant().toEpochMilli - issuedMs <= OauthStateTtl.toMillis,
                         (),
                         s"OAuth state expired (issued ${(clock.instant().toEpochMilli - issuedMs)/1000}s ago, max ${OauthStateTtl.toMinutes}min)"
                       )
    } yield (p, code)

    parsed match {
      case Left(reason) =>
        logger.warn(s"OAuth callback for $provider rejected: $reason")
        BadRequest(s"OAuth callback failed: $reason")
      case Right((p, code)) =>
        val redirectUri = callbackUrl(provider, request)
        Try {
          val profile = p.exchangeCode(code, redirectUri)
          upsertUser(provider, profile)
        } match {
          case Failure(exception) =>
            logger.error(s"OAuth sign-in failed for $provider: ${exception.getMessage}", exception)
            InternalServerError("Couldn't complete sign-in. Please try again.")
          case Success(user) =>
            val nextSession = request.session
              - "oauthState" - "oauthProvider" - "oauthStateTimestamp" - "mobileClient"
              + ("userId" -> user.id)
            if (request.session.get("mobileClient").contains("1")) {
              Redirect(s"kinowo://auth-done?code=${exchangeCodes.mint(user.id)}").withSession(nextSession)
            } else {
              Redirect(landingFor(request)).withSession(nextSession)
            }
        }
    }
  }

  /** Where a finished sign-in drops the visitor: the site they started on.
   *
   *  Usually this deployment's own landing, and expressed as a reverse route so
   *  it keeps working off a deployed origin (a developer on localhost, a spec).
   *  But the apex deployment finishes flows on behalf of its siblings — a `/uk`
   *  sign-in is completed by the process mounted at `/` — and sending those
   *  visitors to the country picker, or worse to Poland, would be a sign-in that
   *  silently moved them to another country's repertoire. */
  private def landingFor(request: RequestHeader): String =
    request.getQueryString("state").flatMap(AuthController.stateCountry) match {
      case Some(home) if home != country => home.webUrl.map(_ + "/").getOrElse(routes.LandingController.index().url)
      case _                             => routes.LandingController.index().url
    }

  def token(): Action[JsValue] = Action(parse.json) { request =>
    val body = request.body
    ((body \ "provider").asOpt[String], (body \ "token").asOpt[String]) match {
      case (None, _) => BadRequest(Json.obj("error" -> "missing provider"))
      case (_, None) => BadRequest(Json.obj("error" -> "missing token"))
      case (Some(provider), Some(tokenStr)) =>
        val fullName    = (body \ "fullName").asOpt[String]
        val redirectUri = (body \ "redirectUri").asOpt[String]
        Try(provider match {
          case "apple" =>
            appleTokenValidator.getOrElse(throw new RuntimeException("Apple not configured"))
              .validate(tokenStr, fullName)
          case "google" =>
            redirectUri match {
              case Some(uri) =>
                providers.getOrElse("google", throw new RuntimeException("Google not configured"))
                  .exchangeCode(tokenStr, uri)
              case None =>
                googleTokenValidator.getOrElse(throw new RuntimeException("Google not configured"))
                  .validate(tokenStr)
            }
          case "facebook" =>
            redirectUri match {
              case Some(uri) =>
                providers.getOrElse("facebook", throw new RuntimeException("Facebook not configured"))
                  .exchangeCode(tokenStr, uri)
              case None =>
                facebookTokenValidator.getOrElse(throw new RuntimeException("Facebook not configured"))
                  .validate(tokenStr)
            }
          case other => throw new RuntimeException(s"Unknown provider: $other")
        }) match {
          case Failure(exception) =>
            logger.warn(s"Token validation failed for $provider: ${exception.getMessage}")
            Unauthorized(Json.obj("error" -> exception.getMessage))
          case Success(profile) =>
            Try(upsertUser(provider, profile)) match {
              case Failure(exception) =>
                logger.error(s"Token sign-in failed for $provider: ${exception.getMessage}", exception)
                InternalServerError(Json.obj("error" -> "Couldn't complete sign-in."))
              case Success(user) =>
                Ok(Json.obj(
                  "displayName" -> user.displayName,
                  "email"       -> user.email,
                  "avatarUrl"   -> user.avatarUrl,
                  "provider"    -> user.provider
                )).withSession("userId" -> user.id)
            }
        }
    }
  }

  def me(): Action[AnyContent] = Action { request =>
    request.session.get("userId").flatMap(userRepository.findById) match {
      case None => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(user) => Ok(Json.obj(
        "displayName" -> user.displayName,
        "email"       -> user.email,
        "avatarUrl"   -> user.avatarUrl,
        "provider"    -> user.provider
      ))
    }
  }

  def logout(): Action[AnyContent] = Action { request =>
    Redirect(routes.LandingController.index()).withSession(request.session - "userId" - "oauthState" - "oauthProvider" - "oauthStateTimestamp")
  }

  private def upsertUser(provider: String, profile: OauthProfile): User = {
    val now   = clock.instant()
    val email = profile.email.getOrElse(
      throw new RuntimeException(s"OAuth $provider profile has no email — cannot identify user")
    ).toLowerCase
    val user = userRepository.findById(email) match {
      case Some(existing) =>
        existing.copy(
          provider    = provider,
          providerSub = profile.sub,
          displayName = profile.displayName.orElse(existing.displayName),
          avatarUrl   = profile.avatarUrl.orElse(existing.avatarUrl),
          lastSeenAt  = now
        )
      case None =>
        User(
          id          = email,
          provider    = provider,
          providerSub = profile.sub,
          email       = Some(email),
          displayName = profile.displayName,
          avatarUrl   = profile.avatarUrl,
          createdAt   = now,
          lastSeenAt  = now
        )
    }
    userRepository.upsert(user)
    user
  }

  def exchange(): Action[JsValue] = Action(parse.json) { request =>
    (request.body \ "code").asOpt[String].flatMap(exchangeCodes.redeem) match {
      case None =>
        Unauthorized(Json.obj("error" -> "invalid or expired code"))
      case Some(userId) =>
        userRepository.findById(userId) match {
          case None =>
            Unauthorized(Json.obj("error" -> "user not found"))
          case Some(user) =>
            Ok(Json.obj(
              "displayName" -> user.displayName,
              "email"       -> user.email,
              "avatarUrl"   -> user.avatarUrl,
              "provider"    -> user.provider
            )).withSession("userId" -> user.id)
        }
    }
  }

  /** Hand this visitor's signed-in session to another country's deployment.
   *
   *  Only needed across an ORIGIN boundary — the Showtimes countries share one
   *  and so share the cookie — but it is harmless where it is not needed, so the
   *  switcher does not have to know which pairs those are.
   *
   *  Signed OUT is not an error: there is nothing to hand over, so this is just
   *  the link the switcher would have followed anyway, and the visitor lands
   *  where they asked to go. An unknown `to` IS an error, and a loud one: it can
   *  only be a hand-crafted URL, since the only thing that builds these is a
   *  `<select>` of the deployed countries.
   *
   *  The code rides in the query string, which is the one thing worth being
   *  uncomfortable about — mitigated by making it single-use, two minutes long,
   *  and redeemed by a URL that only ever answers with a redirect, so no page
   *  ever renders (and no subresource sends a `Referer`) while it is still in
   *  the address bar. */
  def ssoStart(): Action[AnyContent] = Action { request =>
    AuthController.switchTarget(request.getQueryString("to")) match {
      case None =>
        logger.warn(s"SSO start refused: '${request.getQueryString("to").getOrElse("")}' is not a deployed country.")
        BadRequest("Unknown country")
      case Some(target) =>
        request.session.get("userId").flatMap(userRepository.findById) match {
          // NOTHING TO HAND OVER, and the marker matters. This endpoint is now
          // also reached unprompted, by a signed-out arrival probing whether the
          // visitor has a session on the other domain; sending it back to a bare
          // landing would leave the page it lands on unable to tell a first visit
          // from one that has just been answered, and it would probe again. The
          // probe's own cookie guard is the durable half; this is what holds when
          // a browser is refusing cookies, which is also a browser that can never
          // hold a session and must not be put in a loop chasing one.
          case None       => Redirect(s"$target/?${AuthController.ProbedMarker}")
          case Some(user) =>
            val code = URLEncoder.encode(exchangeCodes.mint(user.id), UTF_8)
            Redirect(s"$target/auth/sso/finish?code=$code")
        }
    }
  }

  /** Receive a session handed over by [[ssoStart]] on another domain.
   *
   *  A code that does not redeem lands the visitor on this country's home page
   *  signed out rather than on an error: by the time they are here they have
   *  already left the page they came from, and the only thing they can do about
   *  a stale code is sign in again — which is exactly what the home page offers.
   *  The warning is for us, not them. */
  def ssoFinish(): Action[AnyContent] = Action { request =>
    val home = Redirect(routes.LandingController.index())
    request.getQueryString("code").flatMap(exchangeCodes.redeem).flatMap(userRepository.findById) match {
      case None =>
        logger.warn("SSO handoff arrived without a redeemable code — landing signed out.")
        home
      case Some(user) =>
        home.withSession(request.session + ("userId" -> user.id))
    }
  }

  /** The `redirect_uri` handed to the provider, and handed back at exchange —
   *  the two have to be byte-identical or the exchange is a
   *  `redirect_uri_mismatch`.
   *
   *  ONE per provider for the whole project: every deployed country names the
   *  apex, so the consoles hold two URLs rather than two per country. Off a
   *  deployed origin (localhost, a preview) it is the caller's own address
   *  instead — nothing has registered the apex on their behalf, and pointing a
   *  developer's sign-in at production would be worse than useless.
   *
   *  See `ForwardedUrl` for why the origin comes from the forwarded headers. */
  private def callbackUrl(provider: String, request: RequestHeader): String =
    AuthController.callbackUrlFor(provider, ForwardedUrl.base(request))
}

object AuthController {

  /** The `state` a flow starts with: a random nonce, and the code of the country
   *  that started it.
   *
   *  The NONCE is the security half and is unchanged — it is compared against the
   *  copy in the browser's own session cookie, so it still binds the callback to
   *  the browser that began the flow. The country code is a routing hint and
   *  nothing more: it is read only to pick one of the four deployments we ship,
   *  never trusted as a URL, so a hand-edited `state` can send a callback to
   *  another of our own countries and nowhere else. */
  private[controllers] def newState(country: models.Country): String =
    s"${UUID.randomUUID()}.${country.code}"

  /** The country a `state` says started this flow, or `None` when it does not say
   *  — a `state` minted before this shape existed, still in flight across a
   *  deploy, or one somebody made up. Callers treat `None` as "finish it here",
   *  which is what those flows did before and what a fabricated one deserves. */
  private[controllers] def stateCountry(state: String): Option[models.Country] =
    Some(state.lastIndexOf('.')).filter(_ >= 0)
      .map(at => state.substring(at + 1))
      .flatMap(models.Country.byCode)

  /** The path the provider redirects to. Deliberately the ROOT path rather than
   *  this deployment's mounted one: `showtimes.cc/auth/google/callback` is the
   *  registered URL for every country, and the process that answers it is the one
   *  mounted at `/`. `AuthControllerCallbackUrlSpec` pins it against the reverse
   *  route of a root-mounted deployment so the literal cannot drift from the
   *  routes file. */
  private[controllers] def callbackPath(provider: String): String = s"/auth/$provider/callback"

  /** The `redirect_uri` for a request that arrived on `requestOrigin` — the apex
   *  on any origin we deploy, the caller's own address anywhere else. */
  private[controllers] def callbackUrlFor(provider: String, requestOrigin: String): String =
    (if (models.Country.deployedOrigins.contains(requestOrigin)) models.Country.oauthCallbackOrigin
     else requestOrigin) + callbackPath(provider)

  /** The provider's own query, forwarded verbatim to the deployment that can
   *  finish the flow.
   *
   *  Everything is carried, not just `code` and `state`: a provider that comes
   *  back with `error=access_denied` has no code, and the deployment holding the
   *  session is the one that should say so. Re-encoded rather than passed as a
   *  raw string so a value containing `&` cannot split into extra parameters. */
  private[controllers] def relayQuery(request: RequestHeader): String =
    request.queryString.toSeq.sortBy(_._1).flatMap { case (key, values) =>
      values.map(value => s"${URLEncoder.encode(key, UTF_8)}=${URLEncoder.encode(value, UTF_8)}")
    } match {
      case Nil    => ""
      case params => params.mkString("?", "&", "")
    }

  /** Query marker on the bounce back from a session probe that found nothing —
   *  see [[AuthController.ssoStart]]. Read by the page's own probe, which skips
   *  when it is present, so a browser with no usable cookie jar gets one hop
   *  rather than a loop. */
  val ProbedMarker = "sso=1"

  /** Where [[AuthController.ssoStart]] is willing to send a session: an EXACT
   *  match against a deployed country's own base URL (`Country.webUrl`), and
   *  nothing else.
   *
   *  An allowlist rather than a validated URL because this redirect carries a
   *  credential. Any rule of the shape "same host" or "https and one of our
   *  domains" is one open-redirect away from handing a live sign-in code to
   *  somebody else's server; a list of the four addresses we actually deploy is
   *  not. A trailing slash is tolerated because the switcher's `<option>` values
   *  are base URLs and callers append to them.
   *
   *  Pure, so the refusal can be asserted without a request. */
  private[controllers] def switchTarget(to: Option[String]): Option[String] =
    to.map(_.trim.stripSuffix("/")).filter(_.nonEmpty)
      .flatMap(candidate => models.Country.switchable.flatMap(_.webUrl).find(_ == candidate))
}
