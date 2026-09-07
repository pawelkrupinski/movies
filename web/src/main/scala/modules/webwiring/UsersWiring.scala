package modules.webwiring

import controllers.{AuthController, FacebookDataDeletionController, UserStateController}
import modules.Wiring
import services.auth.{AppleTokenValidator, AuthExchangeCodeStore, AuthExchangeCodes, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, InMemoryAuthExchangeCodeStore, MongoAuthExchangeCodeStore, OauthProvider}
import services.users.{AccountDeletion, CachingUserRepository, CachingUserStateRepository, MongoUserRepository, MongoUserStateRepository, UserRepository, UserStateRepository}
import tools.{Env, HttpFetch, MonitoringHttpFetch, RealHttpFetch}

/** ── Accounts ──────────────────────────────────────────────────────────────
 *  The signed-in visitor: the users + user-state repositories on the shared
 *  users database, the OAuth providers and native-app token validators that
 *  sign a visitor in, the one-shot exchange codes that carry a sign-in across a
 *  domain hop, and the controllers that serve all of it. */
trait UsersWiring { self: Wiring =>

  // OAuth providers + token validators make outbound HTTP; the monitoring
  // wrapper records their latency on the same /uptime surface the worker feeds.
  lazy val httoFetch: HttpFetch = new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor)

  // Caching decorators trim the Atlas RTT off the logged-in critical path.
  lazy val userRepository:      UserRepository      = new CachingUserRepository(new MongoUserRepository(usersConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepository: UserStateRepository = new CachingUserStateRepository(new MongoUserStateRepository(usersConnection.database, fallbackToOwnInit = false))

  // ── OAuth providers ──────────────────────────────────────────────────────
  // Each provider is wired only when its env vars are present. Missing keys →
  // provider absent → start route 404s and the navbar hides the login button.
  lazy val oauthProviders: Map[String, OauthProvider] = {
    val google = for {
      id     <- Env.get("GOOGLE_CLIENT_ID")
      secret <- Env.get("GOOGLE_CLIENT_SECRET")
    } yield new GoogleOauthProvider(httoFetch, id, secret)
    val facebook = for {
      id     <- Env.get("FACEBOOK_APP_ID")
      secret <- Env.get("FACEBOOK_APP_SECRET")
    } yield new FacebookOauthProvider(httoFetch, id, secret)
    Seq(google, facebook).flatten.map(p => p.name -> (p: OauthProvider)).toMap
  }

  lazy val googleTokenValidator: Option[GoogleTokenValidator] =
    Env.get("GOOGLE_CLIENT_ID").map(id => new GoogleTokenValidator(httoFetch, id))

  lazy val facebookTokenValidator: Option[FacebookTokenValidator] =
    for {
      id     <- Env.get("FACEBOOK_APP_ID")
      secret <- Env.get("FACEBOOK_APP_SECRET")
    } yield new FacebookTokenValidator(httoFetch, id, secret)

  lazy val appleTokenValidator: Option[AppleTokenValidator] =
    Env.get("APPLE_BUNDLE_ID").orElse(Some("dev.kinowo.Kinowo"))
      .map(bundleId => new AppleTokenValidator(httoFetch, bundleId))

  // One-shot sign-in codes for the two handoffs a session cookie cannot make:
  // the native apps' `kinowo://` deep link, and the country switch across the
  // kinowo.net / showtimes.cc domain boundary. They live in the SHARED users
  // database because the cross-domain hop mints on one pod and redeems on
  // ANOTHER — an in-process cache is exactly as unreachable there as the cookie
  // it stands in for. With no Mongo at all (local dev) the in-process store
  // keeps the native-app handoff working, since that one does start and finish
  // on the same pod.
  lazy val authExchangeCodes: AuthExchangeCodes = new AuthExchangeCodes(
    usersConnection.database.fold[AuthExchangeCodeStore](new InMemoryAuthExchangeCodeStore)(
      database => new MongoAuthExchangeCodeStore(Some(database))))

  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepository, authExchangeCodes, models.Country.fromEnv, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepository, userStateRepository)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, accountDeletion)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepository, accountDeletion)
}
