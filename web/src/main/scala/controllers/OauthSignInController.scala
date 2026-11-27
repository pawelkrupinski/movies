package controllers

import java.time.Clock

import models.User
import play.api.Logging
import play.api.mvc.*
import services.auth.{AuthExchangeCodes, OauthAccounts, OauthProvider}

/**
 * The browser's OAuth2 authorization-code flow. Two endpoints per
 * provider:
 *
 *   - GET `/auth/:provider/start` — generate a CSRF `state`, stash it
 *     in the session cookie, redirect to the provider's consent URL.
 *   - GET `/auth/:provider/callback` — verify `state`, exchange `code`
 *     for a profile via `OauthProvider.exchangeCode`, upsert the
 *     `User`, set `userId` in the session, redirect home.
 *
 * The legs that hand a live session to a deployment the cookie cannot reach
 * (under `/auth/sso`) are [[SsoController]]'s; this controller sends a finished
 * sign-in through them. The native apps' flows are [[NativeSignInController]]'s,
 * who is signed in and sign-out are [[SessionController]]'s, and what the three
 * share is [[SignInFlow]].
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
 *   - `oauthStateTimestamp` — when the state was issued; a callback after
 *     `OauthStateTtl` is refused
 *   - `mobileClient`  — "1" when a native app started the flow
 *   - `mobileChallenge` — a native start's PKCE-style `challenge`, stamped
 *     onto the deep-link code the callback mints
 *   - `userId`        — set on successful callback
 *
 * All but `userId` are the flow's own ([[SignInFlow.FlowKeys]]): a start
 * replaces them, a finished sign-in spends them.
 */
class OauthSignInController(
  cc:            ControllerComponents,
  providers:     Map[String, OauthProvider],
  // The account a provider's profile signs in as.
  accounts:      OauthAccounts,
  // One-shot codes for the two handoffs a session cookie cannot make: the
  // native apps' `kinowo://` deep link, and the cross-domain country switch.
  exchangeCodes: AuthExchangeCodes,
  // The country THIS deployment serves. A web process serves exactly one (see
  // `Wiring.deploymentLang`), so it is fixed at boot rather than derived per
  // request: it is what a starting flow stamps into its `state`, and what tells
  // a finishing one whether it is home or standing in for a sibling.
  country:       models.Country,
  clock:         Clock
) extends AbstractController(cc) with Logging {

  // OAuth state cookie expires after this — long enough that the user
  // can take a couple of minutes on the provider's consent screen,
  // short enough that a stale browser tab carrying old state isn't a
  // forever-valid CSRF surface. The value's not security-critical
  // (state is single-use and tied to a random UUID), but bounding it
  // is hygiene.
  private val OauthStateTtl = java.time.Duration.ofMinutes(10)

  def start(provider: String): Action[AnyContent] = Action { request =>
    providers.get(provider).fold(NotFound(s"Provider not configured: $provider"))(startFlow(_, provider, request))
  }

  private def startFlow(p: OauthProvider, provider: String, request: RequestHeader): Result = {
    val state = SignInFlow.newState(country)
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
      Redirect(p.authUrl(state, SignInFlow.callbackUrl(provider, request)))
        .withSession(request.session -- SignInFlow.FlowKeys
          + ("oauthState"          -> state)
          + ("oauthProvider"       -> provider)
          + ("oauthStateTimestamp" -> clock.instant().toEpochMilli.toString)
          ++ Option.when(isMobile)("mobileClient" -> "1")
          ++ challenge.map(SignInFlow.MobileChallengeKey -> _))
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
      home  <- SignInFlow.stateCountry(state)
      // ORIGIN decides — that is what the browser scopes a cookie to, and the
      // whole question is whether this request still carries the flow's state.
      if models.DeploymentOrigins.deployedOrigins.contains(origin) && !home.webOrigin.contains(origin)
      // BASE URL builds the address, because a country's callback lives under
      // its mount point. Identical today for the only country this relays to,
      // and not an assumption worth leaving buried.
      base  <- home.webUrl
    } yield base + SignInFlow.callbackPath(provider) + SignInFlow.relayQuery(request)
  }

  private def complete(provider: String, request: RequestHeader): Result =
    verifiedCallback(provider, request) match {
      case Left(reason) =>
        logger.warn(s"OAuth callback for $provider rejected: $reason")
        BadRequest(s"OAuth callback failed: $reason")
      case Right((p, code)) =>
        SignInFlow.signIn(accounts, provider, p.exchangeCode(code, SignInFlow.callbackUrl(provider, request)),
          failed = InternalServerError("Couldn't complete sign-in. Please try again."))(signedInRedirect(_, request))
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
      ageMs          = clock.instant().toEpochMilli - issuedMs
      _             <- Either.cond(ageMs <= OauthStateTtl.toMillis, (),
                         s"OAuth state expired (issued ${ageMs / 1000}s ago, max ${OauthStateTtl.toMinutes}min)")
    } yield (p, code)

  /** The finished flow's redirect, `user` signed in and the flow's own session keys spent.
   *
   *  A native app finishes through the `kinowo://` deep link, carrying a one-shot
   *  code: the apps keep their own cookie jar per host, so there is no sibling to pair.
   *
   *  A browser is routed through the OTHER domain on its way home, so the visitor
   *  ends up signed in on both.
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
   *  [[SsoController.ssoFinish]] never pairs, so the chain is one hop by construction. */
  private def signedInRedirect(user: User, request: RequestHeader): Result = {
    val target =
      if (request.session.get("mobileClient").contains("1"))
        s"kinowo://auth-done?code=${exchangeCodes.mint(user.id, challenge = request.session.get(SignInFlow.MobileChallengeKey))}"
      else SignInFlow.viaSibling(SsoController.SsoFinishPath, landingFor(request), request)
    PerUserResponse(Redirect(target).withSession(SignedInUser.establish(request.session -- SignInFlow.FlowKeys, user)))
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
    request.getQueryString("state").flatMap(SignInFlow.stateCountry) match {
      case Some(home) if home != country => home.webUrl.map(_ + "/").getOrElse(routes.LandingController.index().url)
      case _                             => routes.LandingController.index().url
    }
}
