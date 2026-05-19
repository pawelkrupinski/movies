package controllers

import models.User
import play.api.Logging
import play.api.mvc._
import services.auth.{OauthProfile, OauthProvider}
import services.users.UserRepo

import java.time.Instant
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
  userRepo:  UserRepo
) extends AbstractController(cc) with Logging {

  def start(provider: String): Action[AnyContent] = Action { request =>
    providers.get(provider) match {
      case None =>
        NotFound(s"Provider not configured: $provider")
      case Some(p) =>
        val state       = UUID.randomUUID().toString
        val redirectUri = callbackUrl(provider, request)
        Redirect(p.authUrl(state, redirectUri))
          .withSession(request.session
            + ("oauthState"    -> state)
            + ("oauthProvider" -> provider))
    }
  }

  def callback(provider: String): Action[AnyContent] = Action { request =>
    val parsed = for {
      p             <- providers.get(provider).toRight(s"Unknown provider: $provider")
      code          <- request.getQueryString("code").toRight("Missing code")
      state         <- request.getQueryString("state").toRight("Missing state")
      expectedState <- request.session.get("oauthState").toRight("Missing session state — start over from /auth")
      sessionProv   <- request.session.get("oauthProvider").toRight("Missing session provider")
      _             <- Either.cond(state == expectedState, (), "OAuth state mismatch (possible CSRF)")
      _             <- Either.cond(sessionProv == provider, (), s"Provider mismatch: session=$sessionProv, callback=$provider")
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
              - "oauthState" - "oauthProvider"
              + ("userId" -> user.id)
            Redirect("/").withSession(nextSession)
        }
    }
  }

  def logout(): Action[AnyContent] = Action { request =>
    Redirect("/").withSession(request.session - "userId" - "oauthState" - "oauthProvider")
  }

  private def upsertUser(provider: String, profile: OauthProfile): User = {
    val now = Instant.now()
    val user = userRepo.findByProviderSub(provider, profile.sub) match {
      case Some(existing) =>
        // Returning user — refresh whatever the provider sent this time
        // (display name / avatar can change) but keep the existing
        // values when the provider returned None (user revoked email
        // permission, etc — don't blow away what we already have).
        existing.copy(
          email       = profile.email.orElse(existing.email),
          displayName = profile.displayName.orElse(existing.displayName),
          avatarUrl   = profile.avatarUrl.orElse(existing.avatarUrl),
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
    userRepo.upsert(user)
    user
  }

  // Build the absolute callback URL from the request. Behind Fly's
  // proxy, `request.host` is the public hostname and `request.secure`
  // reflects the X-Forwarded-Proto header (Play respects it when the
  // proxy is trusted — Fly's edge always sets it). Locally
  // `http://localhost:9000`.
  private def callbackUrl(provider: String, request: RequestHeader): String = {
    val scheme = if (request.secure) "https" else "http"
    s"$scheme://${request.host}/auth/$provider/callback"
  }
}
