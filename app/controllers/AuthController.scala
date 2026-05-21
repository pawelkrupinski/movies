package controllers

import models.User
import play.api.Logging
import play.api.mvc._
import services.auth.{OauthProfile, OauthProvider}
import services.users.UserRepo

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
 * Plus a single logout endpoint that drops the session.
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
  cc:        ControllerComponents,
  providers: Map[String, OauthProvider],
  userRepo:  UserRepo,
  clock:     Clock = Clock.systemUTC()
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
        val state       = UUID.randomUUID().toString
        val redirectUri = callbackUrl(provider, request)
        Redirect(p.authUrl(state, redirectUri))
          .withSession(request.session
            + ("oauthState"     -> state)
            + ("oauthProvider"  -> provider)
            + ("oauthStateTs"   -> clock.instant().toEpochMilli.toString))
    }
  }

  def callback(provider: String): Action[AnyContent] = Action { request =>
    val parsed = for {
      p             <- providers.get(provider).toRight(s"Unknown provider: $provider")
      code          <- request.getQueryString("code").toRight("Missing code")
      state         <- request.getQueryString("state").toRight("Missing state")
      expectedState <- request.session.get("oauthState").toRight("Missing session state — start over from /auth")
      sessionProv   <- request.session.get("oauthProvider").toRight("Missing session provider")
      issuedMs      <- request.session.get("oauthStateTs").flatMap(_.toLongOption).toRight("Missing or unparseable oauthStateTs")
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
        Try(p.exchangeCode(code, redirectUri)) match {
          case Failure(ex) =>
            logger.error(s"OAuth code exchange failed for $provider: ${ex.getMessage}", ex)
            InternalServerError("Couldn't complete sign-in. Please try again.")
          case Success(profile) =>
            val user = upsertUser(provider, profile)
            // Drop the one-shot CSRF state, keep everything else, add userId.
            val nextSession = request.session
              - "oauthState" - "oauthProvider" - "oauthStateTs"
              + ("userId" -> user.id)
            Redirect("/").withSession(nextSession)
        }
    }
  }

  def logout(): Action[AnyContent] = Action { request =>
    Redirect("/").withSession(request.session - "userId" - "oauthState" - "oauthProvider" - "oauthStateTs")
  }

  private def upsertUser(provider: String, profile: OauthProfile): User = {
    val now = clock.instant()
    // Resolution precedence:
    //   1. Exact (provider, sub) hit — returning user, same provider.
    //   2. Email hit on a different provider — account LINKING. The
    //      OAuth callback's profile has a verified email (Google +
    //      Facebook both verify before exposing); if it matches an
    //      existing user, attach the new provider to that row rather
    //      than fork a duplicate account. The existing user's
    //      `(provider, providerSub)` get overwritten so the next
    //      login via either provider resolves to the same id — see
    //      the comment below for the trade-off.
    //   3. No match — fresh signup.
    val user = userRepo.findByProviderSub(provider, profile.sub) match {
      case Some(existing) =>
        existing.copy(
          email       = profile.email.orElse(existing.email),
          displayName = profile.displayName.orElse(existing.displayName),
          avatarUrl   = profile.avatarUrl.orElse(existing.avatarUrl),
          lastSeenAt  = now
        )
      case None =>
        profile.email.flatMap(userRepo.findByEmail) match {
          case Some(linked) =>
            // Account linking: the matched user signed up via a
            // different provider earlier. Rewrite (provider, sub) to
            // this login's pair — that means the user can ONLY log in
            // via the most-recently-used provider going forward. The
            // alternative is a `providers: Set[(name, sub)]` collection
            // on User, which is cleaner but a bigger schema change.
            // Defer that until someone actually reports needing both
            // providers active simultaneously.
            logger.info(s"OAuth $provider link — attaching to user ${linked.id} via email match")
            linked.copy(
              provider    = provider,
              providerSub = profile.sub,
              displayName = profile.displayName.orElse(linked.displayName),
              avatarUrl   = profile.avatarUrl.orElse(linked.avatarUrl),
              lastSeenAt  = now
            )
          case None =>
            User(
              id          = UUID.randomUUID().toString,
              provider    = provider,
              providerSub = profile.sub,
              email       = profile.email,
              displayName = profile.displayName,
              avatarUrl   = profile.avatarUrl,
              createdAt   = now,
              lastSeenAt  = now
            )
        }
    }
    userRepo.upsert(user)
    user
  }

  // Build the absolute callback URL from the request. Behind Fly's
  // edge proxy, TLS terminates at the edge and the container receives
  // plain HTTP with `X-Forwarded-Proto: https` + `X-Forwarded-Host`.
  // Read those headers directly rather than relying on Play's
  // `play.http.forwarded.trustedProxies` machinery — the
  // configuration didn't make `request.secure` reflect the proxied
  // scheme on this Play 3.0 setup, so we just trust the headers
  // (safe: Fly's edge is the only ingress to our container, the
  // internet can't reach us to forge these). Falls back to
  // `request.secure` / `request.host` when the headers are absent
  // (local dev hitting localhost:9000 directly).
  private def callbackUrl(provider: String, request: RequestHeader): String = {
    val scheme = request.headers.get("X-Forwarded-Proto")
      .getOrElse(if (request.secure) "https" else "http")
    val host   = request.headers.get("X-Forwarded-Host").getOrElse(request.host)
    s"$scheme://$host/auth/$provider/callback"
  }
}
