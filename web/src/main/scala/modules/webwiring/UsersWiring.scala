package modules.webwiring

import controllers.{AuthController, FacebookDataDeletionController, UserStateController}
import modules.Wiring
import services.auth.{AppleTokenValidator, AuthExchangeCodeStore, AuthExchangeCodes, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, InMemoryAuthExchangeCodeStore, MongoAuthExchangeCodeStore, OauthProvider}
import services.users.{AccountDeletion, CaffeineUserChangeTimeCache, MongoUserRepository, MongoUserStateRepository, UserRepository, UserStateRepository}
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

  lazy val userRepository:      UserRepository      = UsersWiring.podUserRepository(new MongoUserRepository(usersConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepository: UserStateRepository = UsersWiring.podUserStateRepository(new MongoUserStateRepository(usersConnection.database, fallbackToOwnInit = false, writeOutcomes = userStateWriteMetrics, indexHealth = userStateIndexMetrics))

  // The last-1000-active-users change-time cache behind `hiddenFilms()`'s
  // fast path — see `CaffeineUserChangeTimeCache`'s doc comment for why it
  // invalidates wholesale on a stream failure rather than tolerating
  // staleness like `MovieCache`.
  // Typed concrete, not `UserChangeTimeCache` — `Wiring.start()`/`stop()` need
  // its lifecycle methods, which the lookup-only trait deliberately omits
  // (same split as `MovieCache`'s trait vs. its `Stoppable` real impl).
  lazy val userChangeTimeCache: CaffeineUserChangeTimeCache = new CaffeineUserChangeTimeCache(userStateRepository)

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
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, accountDeletion, userChangeTimeCache, legacyUserStateMetrics, userRepository, clock)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepository, accountDeletion)
}

object UsersWiring {

  /** What a pod puts between its controllers and the SHARED users database:
   *  nothing — every pod reads the store itself.
   *
   *  One person is served by several processes at once: each country is its own
   *  pod (and the showtimes.cc ones share the session cookie), the apex pod
   *  answers `/auth/…` for all of them, and a rolling deploy runs two of one
   *  country side by side. A per-process copy of a row is invisible to every
   *  other pod's writes, and both rows here are ones those writes change:
   *  `sessionVersion` ("sign out everywhere" kept working on every other pod
   *  for the copy's hour, and signed the asking device out of them), and the
   *  hidden-films sets (a pod wrote back the row as IT last saw it, erasing
   *  another country's hide, and answered reads with the old set for ten
   *  minutes). Both reads are one `_id`/`userId` lookup against the fleet's own
   *  replica set, made only for signed-in visitors. `UserAcrossPodsSpec` pins
   *  these sequences. */
  def podUserRepository(shared: UserRepository): UserRepository = shared

  /** See [[podUserRepository]]. */
  def podUserStateRepository(shared: UserStateRepository): UserStateRepository = shared
}
