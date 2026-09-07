package modules

import modules.webwiring.{AdminWiring, ControllersWiring, DebugWiring, MetricsWiring, MongoWiring, ReadModelWiring, UsersWiring}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.MongoConnection

/**
 * Read/serving composition root. Builds the content-serving half of the app: the
 * shared data layer (Mongo + the read model, kept warm purely from Mongo via the
 * change streams — this process never scrapes), the user/auth stack, and the
 * controllers. The scrape + enrichment half lives in the separate `worker` app
 * (`modules.WorkerWiring`); the two share only the Mongo database.
 *
 * Each area's wiring lives in its own `modules.webwiring.*` trait — its own package,
 * because the worker's `modules.wiring` already has a `ReadModelWiring` and a
 * `MetricsWiring`, and the e2e project compiles against both modules (self-typed to
 * this trait, so every member stays a lazy val a test wiring can override by
 * name); this trait keeps only the seams Play provides, and the start/stop
 * lifecycle. The router itself is built in `AppComponents`.
 */
trait Wiring
    extends MongoWiring with UsersWiring with ReadModelWiring with DebugWiring
    with AdminWiring with MetricsWiring with ControllersWiring {

  def controllerComponents: ControllerComponents
  def environmentMode: Mode
  implicit def materializer: org.apache.pekko.stream.Materializer

  // Play's i18n API, provided by `BuiltInComponentsFromContext` (I18nComponents)
  // in `AppComponents`. Loads `conf/messages` (Polish default) + `messages.en`.
  def messagesApi: play.api.i18n.MessagesApi

  // Start the data layer. Force the Mongo connection at boot (so connection
  // errors surface in the boot timeline, not mid-request), then start the cache
  // — hydrate from Mongo + open the change stream that keeps it warm.
  protected def start(): Unit = {
    mongoConnection.database
    // Install the override source first so boot-time knob reads already see flips.
    envConfigService.start()
    // Hydrate the read model from the derived collections + open their change
    // streams. (No `movies` watch — see ReadModelWiring.)
    webReadModel.start()
    // Sample per-city served-film counts once a minute for /metrics. Started
    // after the read model so the first sample reads a warm corpus.
    webMovieMetrics.start()
    // Force the Dev-only per-country debug stacks so their extra database views'
    // boot probes surface now, not on the first /debug?country= switch. A no-op
    // in prod (no extras) and cheap in Dev (one shared client, N db views).
    debugCountries
  }

  protected def stop(): Unit = {
    envConfigService.stop()
    uptimeMonitor.close()
    webMovieMetrics.stop()
    webReadModel.stop()
    // Each repository's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    readModelRepository.close()
    movieRepository.close()
    userRepository.close()
    userStateRepository.close()
    // A users database of its own is a second view on the shared client. When it
    // is this deployment's own database `usersConnection` IS `mongoConnection`,
    // and closing it here would be closing the primary early.
    if (usersConnection ne mongoConnection) usersConnection.close()
    // The /debug read-mirror owns its own MongoClient when distinct from the
    // shared prod connection (i.e. MONGODB_MOVIES_MIRROR_URI was set).
    if (movieMirrorConnection ne mongoConnection) movieMirrorConnection.close()
    // Dev-only per-country debug stacks share ONE client (built in DebugWiring);
    // their connections' own close() is a no-op, so close the shared client once.
    debugExtraClient.foreach(_.close())
    mongoConnection.close()
    // Every connection above BORROWED this client, so none of their own close()
    // calls touched it — it is owned here, and closed once, last.
    mongoSharedClient.foreach(_.close())
  }
}

object Wiring {
  /** Where a /debug data source reads from — the boot country's `movies` corpus,
   *  and equally each extra country's per-database stack.
   *  With `MONGODB_MOVIES_MIRROR_URI` set, always the local mirror `openMirror`
   *  builds — there is deliberately NO fall-back to the prod tunnel, even when
   *  the mirror is unreachable (then that connection is simply disabled and
   *  /debug renders empty). Unset → the shared `prod` connection. `prod` is
   *  by-name so a configured mirror never forces the primary connection here. */
  def debugMirrorConnection(mirrorUri: Option[String],
                            openMirror: String => MongoConnection,
                            prod: => MongoConnection): MongoConnection =
    mirrorUri.fold(prod)(openMirror)

  /** Which connection the `users` + `userStates` collections bind to.
   *
   *  A second database view costs a boot probe and a `close()` to get right, and
   *  the common case — `MONGODB_USERS_DB` unset, so the users database IS this
   *  deployment's own — needs neither: reuse the connection already open on that
   *  exact database. Only a genuinely DIFFERENT name opens a second view, which
   *  is why `own` is by-name.
   *
   *  Split out here rather than inlined as an `if` because it is the whole of the
   *  shared-account decision: get it backwards and every country silently keeps
   *  its own private copy of every account again, and no page renders any
   *  differently until someone switches country. */
  def usersConnection(ownDbName: String,
                      usersDbName: String,
                      own: => MongoConnection,
                      openUsers: String => MongoConnection): MongoConnection =
    if (usersDbName == ownDbName) own else openUsers(usersDbName)
}
