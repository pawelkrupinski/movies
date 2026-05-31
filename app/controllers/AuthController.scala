package controllers

import models.User
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.auth.{AppleTokenValidator, FacebookTokenValidator, GoogleTokenValidator, OauthProfile, OauthProvider}
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
  cc:                     ControllerComponents,
  providers:              Map[String, OauthProvider],
  userRepo:               UserRepo,
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
        val state       = UUID.randomUUID().toString
        val redirectUri = callbackUrl(provider, request)
        // Native iOS *and* Android clients pass `?platform=…`; both want the
        // callback to bounce back into the app via the `kinowo://` deep link
        // (carrying a one-shot exchange code) instead of redirecting to `/`.
        val isMobile = request.getQueryString("platform").exists(Set("ios", "android"))
        Redirect(p.authUrl(state, redirectUri))
          .withSession(request.session
            + ("oauthState"     -> state)
            + ("oauthProvider"  -> provider)
            + ("oauthStateTs"   -> clock.instant().toEpochMilli.toString)
            ++ (if (isMobile) Seq("mobileClient" -> "1") else Seq.empty))
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
        Try {
          val profile = p.exchangeCode(code, redirectUri)
          upsertUser(provider, profile)
        } match {
          case Failure(ex) =>
            logger.error(s"OAuth sign-in failed for $provider: ${ex.getMessage}", ex)
            InternalServerError("Couldn't complete sign-in. Please try again.")
          case Success(user) =>
            val nextSession = request.session
              - "oauthState" - "oauthProvider" - "oauthStateTs" - "mobileClient"
              + ("userId" -> user.id)
            if (request.session.get("mobileClient").contains("1")) {
              val code = UUID.randomUUID().toString
              AuthController.pendingExchangeCodes.put(code, user.id)
              Redirect(s"kinowo://auth-done?code=$code").withSession(nextSession)
            } else {
              Redirect("/").withSession(nextSession)
            }
        }
    }
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
          case Failure(ex) =>
            logger.warn(s"Token validation failed for $provider: ${ex.getMessage}")
            Unauthorized(Json.obj("error" -> ex.getMessage))
          case Success(profile) =>
            Try(upsertUser(provider, profile)) match {
              case Failure(ex) =>
                logger.error(s"Token sign-in failed for $provider: ${ex.getMessage}", ex)
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
    request.session.get("userId").flatMap(userRepo.findById) match {
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
    Redirect("/").withSession(request.session - "userId" - "oauthState" - "oauthProvider" - "oauthStateTs")
  }

  private def upsertUser(provider: String, profile: OauthProfile): User = {
    val now   = clock.instant()
    val email = profile.email.getOrElse(
      throw new RuntimeException(s"OAuth $provider profile has no email — cannot identify user")
    ).toLowerCase
    val user = userRepo.findById(email) match {
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
    userRepo.upsert(user)
    user
  }

  def exchange(): Action[JsValue] = Action(parse.json) { request =>
    (request.body \ "code").asOpt[String].flatMap { code =>
      Option(AuthController.pendingExchangeCodes.getIfPresent(code)).map { userId =>
        AuthController.pendingExchangeCodes.invalidate(code)
        userId
      }
    } match {
      case None =>
        Unauthorized(Json.obj("error" -> "invalid or expired code"))
      case Some(userId) =>
        userRepo.findById(userId) match {
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

  // Absolute callback URL the provider redirects back to. See
  // `ForwardedUrl` for why we read the forwarded headers directly.
  private def callbackUrl(provider: String, request: RequestHeader): String =
    ForwardedUrl.base(request) + s"/auth/$provider/callback"
}

object AuthController {
  import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
  import java.util.concurrent.TimeUnit
  val pendingExchangeCodes: Cache[String, String] =
    Caffeine.newBuilder().expireAfterWrite(2, TimeUnit.MINUTES).maximumSize(100).build()
}
