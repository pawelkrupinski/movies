package modules.webwiring

import controllers.{AccountController, FacebookDataDeletionController, UserStateController}
import services.auth.{AuthExchangeCodeStore, AuthExchangeCodes, FacebookGraphFetch, InMemoryAuthExchangeCodeStore, MongoAuthExchangeCodeStore}
import services.users.{AccountDeletion, CaffeineUserChangeTimeCache, MongoUserRepository, MongoUserStateRepository, UserRepository, UserStateChangeStream, UserStateRepository}
import services.facebook.{FacebookUsageBrake, MongoFacebookSlots}
import tools.{HttpFetch, RealHttpFetch, TlsTrust}
import services.MonitoringHttpFetch

/** ── Accounts ──────────────────────────────────────────────────────────────
 *  The signed-in visitor: the users + user-state repositories on the shared
 *  users database, the one-shot exchange codes kept there that carry a sign-in
 *  across a domain hop, the outbound fetches, and the account controllers. How a
 *  visitor signs in is [[AuthWiring]]'s. */
trait UsersWiring { self: Wiring =>

  // OAuth providers + token validators make outbound HTTP; the monitoring
  // wrapper records their latency on the same /uptime surface the worker feeds.
  lazy val httpFetch: HttpFetch = new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor)

  // Facebook's sign-in calls spend the app's hourly allowance the worker's re-scrapes pace on, so
  // each reports Facebook's usage reading to the fleet's brake on them (the fleet database's
  // re-scrape quota). Without the fleet database, the plain fetch: nothing to hold.
  lazy val facebookHttpFetch: HttpFetch = fleetConnection.database.fold(httpFetch)(db =>
    new MonitoringHttpFetch(new FacebookGraphFetch(new FacebookUsageBrake(
      new MongoFacebookSlots(db.getCollection(MongoFacebookSlots.Collection)), clock), TlsTrust.newContext()), uptimeMonitor))

  // The SHARED users database, read by every pod directly — nothing between it
  // and the controllers. One person is served by several processes at once:
  // each country is its own pod (and the showtimes.cc ones share the session
  // cookie), the apex pod answers `/auth/…` for all of them, and a rolling
  // deploy runs two of one country side by side. A per-process copy of a row
  // is invisible to every other pod's writes, and both rows here are ones those
  // writes change: `sessionVersion` (a logout's revoked cookie kept working on every
  // other pod for the copy's hour, and signed the asking device out of them),
  // and the hidden-films sets (a pod wrote back the row as IT last saw it,
  // erasing another country's hide, and answered reads with the old set for ten
  // minutes). Both reads are one `_id`/`userId` lookup against the fleet's own
  // replica set, made only for signed-in visitors. `UserAcrossPodsSpec` pins
  // these sequences.
  lazy val userRepository:      UserRepository      = new MongoUserRepository(usersConnection.database)
  lazy val userStateRepository: UserStateRepository = new MongoUserStateRepository(usersConnection.database,
    new UserStateChangeStream(usersConnection.database, clock, stopwatch = stopwatch, decodeFailures = webDecodeFailureMetrics),
    writeOutcomes = userStateWriteMetrics, indexHealth = userStateIndexMetrics)

  // The last-1000-active-users change-time cache behind `hiddenFilms()`'s
  // fast path — see `CaffeineUserChangeTimeCache`'s doc comment for why it
  // invalidates wholesale on a stream failure rather than tolerating
  // staleness like `MovieCache`.
  // Typed concrete, not `UserChangeTimeCache` — `Wiring.start()`/`stop()` need
  // its lifecycle methods, which the lookup-only trait deliberately omits
  // (same split as `MovieCache`'s trait vs. its `Stoppable` real impl).
  lazy val userChangeTimeCache: CaffeineUserChangeTimeCache = managedResources.stopping(new CaffeineUserChangeTimeCache(userStateRepository))

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
      database => new MongoAuthExchangeCodeStore(Some(database))), clock)

  lazy val accountDeletion   = new AccountDeletion(userRepository, userStateRepository)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, userChangeTimeCache, userRepository, clock)
  lazy val accountController = new AccountController(controllerComponents, accountDeletion, userRepository)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, country, processConfiguration.facebookAppSecret, userRepository, accountDeletion)

  /** The controllers the router serves the account and per-user routes with ([[modules.AppComponents.router]]). */
  lazy val usersRouted = (userStateController, accountController, facebookDataDeletionController)
}
