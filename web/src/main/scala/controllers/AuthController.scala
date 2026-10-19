package controllers

import models.User
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.auth.{AppleTokenValidator, AuthExchangeCodes, FacebookTokenValidator, GoogleTokenValidator, NativeTokenVerifier, OauthAccounts, OauthProvider}
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
 * Plus a single logout endpoint that signs the account out on every device, the
 * native apps' token sign-in and code exchange, and `/api/me`. The legs that hand a
 * live session to a deployment the cookie cannot reach (under `/auth/sso`) are
 * [[SsoController]]'s; this controller sends a finished sign-in, and a sign-out,
 * through them.
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
 *   - `mobileChallenge` — a native start's PKCE-style `challenge`, stamped
 *     onto the deep-link code the callback mints
 */
class AuthController(
  cc:                     ControllerComponents,
  providers:              Map[String, OauthProvider],
  userRepository:               UserRepository,
  // One-shot codes for the two handoffs a session cookie cannot make: the
  // native apps' `kinowo://` deep link, and the cross-domain country switch.
  exchangeCodes:          AuthExchangeCodes,
  // The country THIS deployment serves. A web process serves exactly one (see
  // `Wiring.deploymentLang`), so it is fixed at boot rather than derived per
  // request: it is what a starting flow stamps into its `state`, and what tells
  // a finishing one whether it is home or standing in for a sibling.
  country:                models.Country,
  googleTokenValidator:   Option[GoogleTokenValidator] = None,
  facebookTokenValidator: Option[FacebookTokenValidator] = None,
  appleTokenValidator:    Option[AppleTokenValidator] = None,
  clock:                  Clock
) extends AbstractController(cc) with Logging {
  import AuthController.{signedOut, uncacheable}

  // OAuth state cookie expires after this — long enough that the user
  // can take a couple of minutes on the provider's consent screen,
  // short enough that a stale browser tab carrying old state isn't a
  // forever-valid CSRF surface. The value's not security-critical
  // (state is single-use and tied to a random UUID), but bounding it
  // is hygiene.
  private val OauthStateTtl = java.time.Duration.ofMinutes(10)

  // Composed from the collaborators above: who a native token names, and the account a profile signs in as.
  private val nativeTokens = new NativeTokenVerifier(providers, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  private val accounts     = new OauthAccounts(userRepository, clock)

  def start(provider: String): Action[AnyContent] = Action { request =>
    providers.get(provider) match {
      case None    => NotFound(s"Provider not configured: $provider")
      case Some(p) => startFlow(p, provider, request)
    }
  }

  private def startFlow(p: OauthProvider, provider: String, request: RequestHeader): Result = {
    val state = AuthController.newState(country)
    // Native iOS *and* Android clients pass `?platform=…`; both want the
    // callback to bounce back into the app via the `kinowo://` deep link
    // (carrying a one-shot exchange code) instead of redirecting to `/`.
    val isMobile = request.getQueryString("platform").exists(Set("ios", "android"))
    // The app's PKCE-style challenge (see `PendingExchangeCode.challenge`),
    // carried to the code the callback mints. Absent from released app
    // versions; malformed is refused rather than silently dropped, which
    // would mint a code the app's own verifier could then never spend.
    val challenge = request.getQueryString("challenge").filter(_ => isMobile)
    if (challenge.exists(c => !AuthExchangeCodes.ChallengePattern.matches(c)))
      BadRequest("challenge must be base64url(SHA-256(verifier)), unpadded")
    else
      // A start is a NEW flow: an earlier native one abandoned at the
      // provider must not lend it `mobileClient` or its challenge — Android's
      // Custom Tab shares the browser's cookie jar with ordinary web visits.
      Redirect(p.authUrl(state, callbackUrl(provider, request)))
        .withSession(request.session - "mobileClient" - AuthController.MobileChallengeKey
          + ("oauthState"     -> state)
          + ("oauthProvider"  -> provider)
          + ("oauthStateTimestamp"   -> clock.instant().toEpochMilli.toString)
          ++ (if (isMobile) Seq("mobileClient" -> "1") else Seq.empty)
          ++ challenge.map(AuthController.MobileChallengeKey -> _))
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
      if models.DeploymentOrigins.deployedOrigins.contains(origin) && !home.webOrigin.contains(origin)
      // BASE URL builds the address, because a country's callback lives under
      // its mount point. Identical today for the only country this relays to,
      // and not an assumption worth leaving buried.
      base  <- home.webUrl
    } yield base + AuthController.callbackPath(provider) + AuthController.relayQuery(request)
  }

  private def complete(provider: String, request: RequestHeader): Result =
    verifiedCallback(provider, request) match {
      case Left(reason) =>
        logger.warn(s"OAuth callback for $provider rejected: $reason")
        BadRequest(s"OAuth callback failed: $reason")
      case Right((p, code)) =>
        Try(accounts.signIn(provider, p.exchangeCode(code, callbackUrl(provider, request)))) match {
          case Failure(exception) =>
            logger.error(s"OAuth sign-in failed for $provider: ${exception.getMessage}", exception)
            InternalServerError("Couldn't complete sign-in. Please try again.")
          case Success(user) => signedInRedirect(user, request)
        }
    }

  /** The provider and its `code`, once the callback is proven to finish the flow
   *  THIS browser started — same state, same provider, not expired — or why not. */
  private def verifiedCallback(provider: String, request: RequestHeader): Either[String, (OauthProvider, String)] =
    for {
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

  /** The finished web or native flow's redirect, `user` signed in and the flow's own session keys spent. */
  private def signedInRedirect(user: User, request: RequestHeader): Result = {
    val nextSession = SignedInUser.establish(
      request.session - "oauthState" - "oauthProvider" - "oauthStateTimestamp" - "mobileClient" - AuthController.MobileChallengeKey,
      user)
    if (request.session.get("mobileClient").contains("1")) {
      // The native apps keep their own cookie jar per host and finish
      // through the deep link, so there is no sibling to pair.
      uncacheable(Redirect(s"kinowo://auth-done?code=${exchangeCodes.mint(user.id,
        challenge = request.session.get(AuthController.MobileChallengeKey))}").withSession(nextSession))
    } else {
      uncacheable(Redirect(pairSiblingThen(landingFor(request), request)).withSession(nextSession))
    }
  }

  /** Route the finished sign-in through the OTHER domain on its way home, so the
   *  visitor ends up signed in on both.
   *
   *  WHY HERE AND NOT ON ARRIVAL. kinowo.net and showtimes.cc share no cookie, so
   *  something has to establish a session on each. Asking on arrival — the shape
   *  this replaces — spends a redirect on every signed-out visitor of a site that
   *  is overwhelmingly anonymous, and can only be afforded once per browser,
   *  which makes it useless the second time somebody signs in. Doing it HERE
   *  costs nothing to anyone who never signs in, happens while the visitor is
   *  already watching a redirect chain, and works on every sign-in rather than
   *  the first.
   *
   *  It is a TOP-LEVEL navigation, which is the other reason this shape and not a
   *  hidden iframe: the cookie the far side sets is first-party, so it survives
   *  the third-party cookie rules that make the iframe version fail silently in
   *  Safari and increasingly in Chrome.
   *
   *  Codeless: the sibling gives the browser a binding and asks back here for a
   *  code minted for it (see [[SsoController.ssoFinish]]).
   *
   *  No pairing without a sibling, and none FROM the pairing leg itself —
   *  [[SsoController.ssoFinish]] never calls this, so the chain is one hop by construction. */
  private def pairSiblingThen(landing: String, request: RequestHeader): String =
    viaSibling(SsoController.SsoFinishPath, landing, request)

  /** `landing` by way of the other domain's `path`, which sends the visitor on to
   *  it — or `landing` itself when this origin has no sibling. */
  private def viaSibling(path: String, landing: String, request: RequestHeader): String =
    models.DeploymentOrigins.siblingOfOrigin(ForwardedUrl.base(request)) match {
      case None          => landing
      case Some(sibling) => s"$sibling$path${AuthController.query(Seq("next" -> absolute(landing, request)))}"
    }

  /** `landing` as an absolute URL, so the far side can send the visitor back to
   *  it across the domain boundary — see [[AuthController.absoluteLanding]]. */
  private def absolute(landing: String, request: RequestHeader): String =
    AuthController.absoluteLanding(ForwardedUrl.base(request), landing)

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

  /** Native sign-in: a provider token in, the user plus a session cookie out —
   *  `PerUserResponse`, like every response here that names someone. */
  def token(): Action[JsValue] = Action(parse.json) { request => PerUserResponse(signInWithToken(request.body)) }

  private def signInWithToken(body: JsValue): Result =
    ((body \ "provider").asOpt[String], (body \ "token").asOpt[String]) match {
      case (None, _) => BadRequest(Json.obj("error" -> "missing provider"))
      case (_, None) => BadRequest(Json.obj("error" -> "missing token"))
      case (Some(provider), Some(token)) =>
        Try(nativeTokens.profile(provider, token, (body \ "fullName").asOpt[String], (body \ "redirectUri").asOpt[String])) match {
          case Failure(exception) =>
            logger.warn(s"Token validation failed for $provider: ${exception.getMessage}", exception)
            Unauthorized(Json.obj("error" -> AuthController.TokenRejected))
          case Success(profile) =>
            Try(accounts.signIn(provider, profile)) match {
              case Failure(exception) =>
                logger.error(s"Token sign-in failed for $provider: ${exception.getMessage}", exception)
                InternalServerError(Json.obj("error" -> "Couldn't complete sign-in."))
              case Success(user) => signedInJson(user)
            }
        }
    }

  /** A native sign-in's answer: who, plus a fresh session cookie naming them. */
  private def signedInJson(user: User): Result =
    Ok(AuthController.userJson(user)).withSession(SignedInUser.establish(Session.emptyCookie, user))

  /** Who is looking — the ONLY place this application names a visitor now.
   *
   *  The HTML pages stopped rendering the avatar so that a shared cache could
   *  hold them (`CachePolicy.RevalidatedAnywhere`), which moved the whole
   *  per-user surface here: `shared.js` calls this on every page load and builds
   *  the account menu from the answer. So this response, and only this response,
   *  is the one nothing may keep a copy of — a stored 200 is an avatar rebuilt
   *  after a sign-out, or on a device the answer was never for. */
  def me(): Action[AnyContent] = Action { request =>
    SignedInUser.answering(SignedInUser(request, userRepository) match {
      case None => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(user) => Ok(AuthController.userJson(user))
    })
  }

  /** Sign out — EVERYWHERE: every browser, every app, every deployment.
   *
   *  The account's `sessionVersion` is bumped first, which fails the
   *  `SignedInUser` check for every cookie issued before it — the web's on
   *  every device and the native apps' (theirs is the same session cookie,
   *  handed out by `/auth/token`) — on every deployment, since they all read
   *  the one shared `users` row. Only a session that still resolves bumps it:
   *  an anonymous caller, or a cookie an earlier logout already killed, must
   *  not sign the account out of the sessions it has opened since. A store
   *  that cannot be read or written is logged and does not stop THIS cookie
   *  being dropped — logging out never fails here.
   *
   *  The version bump already reaches the other domain's session, but its
   *  cookie still names the account until it is next checked, so the visitor
   *  is sent through that domain's [[SsoController.ssoLogout]] too: the pairing at sign-in
   *  establishes a session on both, and "log out" has to mean the far cookie
   *  is gone, not merely dead. Same one hop, in the other direction. */
  def logout(): Action[AnyContent] = Action { request =>
    revokeEverySession(request)
    val target = viaSibling(SsoController.SsoLogoutPath, routes.LandingController.index().url, request)
    // `withNewSession` DISCARDS the cookie rather than re-signing a smaller one.
    // Removing the keys would leave a valid session cookie behind holding
    // nothing, which is the same thing to this application and not the same
    // thing at all to a browser, a proxy, or anyone reading the response.
    signedOut(Redirect(target).withNewSession)
  }

  private def revokeEverySession(request: RequestHeader): Unit =
    try SignedInUser(request, userRepository).foreach { user =>
      if (userRepository.revokeSessions(user.id).isEmpty)
        logger.warn(s"logout: sessions of ${user.id} could not be revoked; only this cookie is dropped")
    } catch {
      case SignedInUser.LookupFailed(exception) =>
        logger.warn(s"logout: user unreadable, only this cookie is dropped: ${exception.getMessage}")
    }

  def exchange(): Action[JsValue] = Action(parse.json) { request => PerUserResponse(redeemExchangeCode(request.body)) }

  private def redeemExchangeCode(body: JsValue): Result = {
    (body \ "code").asOpt[String].flatMap(exchangeCodes.redeem(_, verifier = (body \ "verifier").asOpt[String])) match {
      case None =>
        Unauthorized(Json.obj("error" -> "invalid or expired code"))
      case Some(userId) =>
        userRepository.findById(userId) match {
          case None =>
            Unauthorized(Json.obj("error" -> "user not found"))
          case Some(user) => signedInJson(user)
        }
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

  /** An auth transition nothing may keep a copy of.
   *
   *  These responses hand out or tear down a session, so a cached one is either
   *  a stale sign-in or a logout that appears not to have happened. Cheap
   *  insurance: they are redirects nobody benefits from re-using. */
  private[controllers] def uncacheable(result: Result): Result = PerUserResponse(result)

  /** A sign-out, plus an instruction to forget what this origin looked like
   *  while signed in.
   *
   *  Clearing the cookie is not enough on its own, and the gap is exactly what a
   *  logout that "doesn't carry" looks like: the far domain's session is gone,
   *  but the browser still holds a page it rendered while that session was
   *  alive, so the next visit shows an avatar for an account it no longer has.
   *  `no-store` stops NEW signed-in pages being kept; it cannot reach the ones
   *  already in the cache, and this is the only thing that can.
   *
   *  `"cache"` alone, deliberately — `"cookies"` would take the remembered city
   *  with it, and the session cookie is already discarded precisely. Ignored by
   *  browsers that do not implement it, which lose nothing they had before. */
  private[controllers] def signedOut(result: Result): Result =
    uncacheable(result).withHeaders("Clear-Site-Data" -> "\"cache\"")

  /** Who a signed-in response says is signed in — the one shape `/api/me`,
   *  `/auth/token` and `/auth/exchange` all answer with. */
  private[controllers] def userJson(user: User): JsValue = Json.obj(
    "displayName" -> user.displayName,
    "email"       -> user.email,
    "avatarUrl"   -> user.avatarUrl,
    "provider"    -> user.provider
  )

  /** What a refused `POST /auth/token` tells the client — the same words
   *  whatever went wrong; the reason goes to the server log. */
  val TokenRejected = "token not accepted"

  /** The `state` a flow starts with: a random nonce, and the code of the country
   *  that started it.
   *
   *  The NONCE is the security half and is unchanged — it is compared against the
   *  copy in the browser's own session cookie, so it still binds the callback to
   *  the browser that began the flow. The country code is a routing hint and
   *  nothing more: it is read only to pick one of the deployments we ship,
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
    (if (models.DeploymentOrigins.deployedOrigins.contains(requestOrigin)) models.DeploymentOrigins.oauthCallbackOrigin
     else requestOrigin) + callbackPath(provider)

  /** The provider's own query, forwarded verbatim to the deployment that can
   *  finish the flow.
   *
   *  Everything is carried, not just `code` and `state`: a provider that comes
   *  back with `error=access_denied` has no code, and the deployment holding the
   *  session is the one that should say so. Re-encoded rather than passed as a
   *  raw string so a value containing `&` cannot split into extra parameters. */
  private[controllers] def relayQuery(request: RequestHeader): String =
    query(request.queryString.toSeq.sortBy(_._1).flatMap { case (key, values) => values.map(key -> _) })

  /** Session key holding a native flow's `challenge` from start to callback. */
  val MobileChallengeKey = "mobileChallenge"

  /** `params` as a query string, keys and values encoded; empty for none. */
  private[controllers] def query(params: Seq[(String, String)]): String =
    if (params.isEmpty) ""
    else params.map { case (k, v) => s"${URLEncoder.encode(k, UTF_8)}=${URLEncoder.encode(v, UTF_8)}" }.mkString("?", "&", "")

  /** A landing as an absolute URL on `origin`.
   *
   *  Against the ORIGIN, because a relative landing came from a reverse route
   *  and already carries this deployment's mount point. Resolving `/uk/` against
   *  the COUNTRY's base URL — which itself ends in `/uk` — yields
   *  `showtimes.cc/uk/uk/`, which is not a deployed country, so the far side's
   *  allowlist refuses it and the visitor is handed to the wrong site entirely.
   *  That shipped, as a logout on any Showtimes country landing on kinowo.net.
   *
   *  Pure, because the reverse routes a spec sees are mounted at `/` however the
   *  deployment is configured, so the doubling is unreachable through a request. */
  private[controllers] def absoluteLanding(origin: String, landing: String): String =
    if (landing.startsWith("http")) landing else origin + landing
}
