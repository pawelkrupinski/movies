package modules.webwiring

import controllers.{NativeSignInController, OauthSignInController, SessionController, SsoController}
import services.auth.{AppleTokenValidator, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, NativeTokenVerifier, OauthAccounts, OauthProvider}

/** ── Sign-in ───────────────────────────────────────────────────────────────
 *  How a visitor signs in: the OAuth providers and the native apps' token validators, the account a profile signs in
 *  as, and the sign-in and cross-domain SSO controllers -- over [[UsersWiring]]'s users and exchange codes. */
trait AuthWiring { self: Wiring =>

  // ── OAuth providers ──────────────────────────────────────────────────────
  // Each provider is wired only when its env vars are present. Missing keys →
  // provider absent → start route 404s and the navbar hides the login button.
  lazy val oauthProviders: Map[String, OauthProvider] = {
    val google = for {
      id     <- processConfiguration.googleClientId
      secret <- processConfiguration.googleClientSecret
    } yield new GoogleOauthProvider(httpFetch, id, secret)
    val facebook = for {
      id     <- processConfiguration.facebookAppId
      secret <- processConfiguration.facebookAppSecret
    } yield new FacebookOauthProvider(facebookHttpFetch, id, secret)
    Seq(google, facebook).flatten.map(p => p.name -> (p: OauthProvider)).toMap
  }

  lazy val googleTokenValidator: Option[GoogleTokenValidator] =
    processConfiguration.googleClientId.map(id => new GoogleTokenValidator(httpFetch, id))

  lazy val facebookTokenValidator: Option[FacebookTokenValidator] =
    for {
      id     <- processConfiguration.facebookAppId
      secret <- processConfiguration.facebookAppSecret
    } yield new FacebookTokenValidator(facebookHttpFetch, id, secret)

  lazy val appleTokenValidator: Option[AppleTokenValidator] =
    Some(new AppleTokenValidator(httpFetch, processConfiguration.appleBundleId, clock))

  /** The account a provider's profile signs in as. */
  lazy val oauthAccounts: OauthAccounts = new OauthAccounts(userRepository, clock)
  lazy val oauthSignInController = new OauthSignInController(controllerComponents, oauthProviders, oauthAccounts, authExchangeCodes, country, clock)
  lazy val nativeSignInController = new NativeSignInController(controllerComponents, userRepository, oauthAccounts, authExchangeCodes,
    new NativeTokenVerifier(oauthProviders, googleTokenValidator, facebookTokenValidator, appleTokenValidator))
  lazy val sessionController = new SessionController(controllerComponents, userRepository)
  lazy val ssoController    = new SsoController(controllerComponents, userRepository, authExchangeCodes, country)

  /** The controllers the router serves the sign-in routes with ([[modules.AppComponents.router]]). */
  lazy val authRouted = (ssoController, oauthSignInController, nativeSignInController, sessionController)
}
