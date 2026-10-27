package modules

import modules.webwiring.{AdminWiring, ControllersWiring, DebugWiring, MetricsWiring, MongoWiring, ReadModelWiring, UsersWiring}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoAddress, MongoConnection}

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

  // The process's config (env vars + admin overrides), built once by `AppLoader`.
  // Every knob in the web wiring is read through it; EnvConfigService installs the
  // override source into it.
  def env: tools.Env

  // Every setting the web wiring reads, typed — resolved over `env`, so an admin override
  // installed into it reaches a value read per use (the page tags, say) without a restart.
  lazy val processConfiguration: settings.ProcessConfiguration = new settings.ProcessConfiguration(env)

  // The ONE country this deployment serves. Resolved from the process by `AppLoader` —
  // the composition root — and handed in; nothing in the wiring reads it from the
  // environment. Every component that differs by country takes it from this member, so a
  // test wiring is simply built as another country's site (see CountryIsolationMatrixSpec).
  def country: models.Country

  // Where this deployment's Mongo is (cluster + optional explicit database). Resolved by
  // `AppLoader` like `country`; a test wiring hands in `MongoAddress.Disabled`.
  def mongoAddress: MongoAddress

  // The one wall clock the serving app reads. Everything that asks "what time is it" takes
  // it by constructor; a test wiring overrides it to pin the time.
  lazy val clock: java.time.Clock = java.time.Clock.systemUTC()
  /** The one monotonic source elapsed time is measured on — the clock's twin for durations, never a date. */
  lazy val stopwatch: tools.Stopwatch = tools.Stopwatch.System

  /** The latest year a title may name, read off [[clock]] at each ask: what the record accessors the debug pages
   *  render (a venue's representative slot, the display title) cap a title's year by. */
  given latestTitleYear: services.movies.LatestTitleYear = services.movies.LatestTitleYear(clock)

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
    userChangeTimeCache.start()
  }

  /** Every pool and closeable this wiring created, shut by [[stop]] newest first — before Mongo closes. */
  lazy val managedResources: tools.ManagedResources = new tools.ManagedResources(stopwatch)

  protected def stop(): Unit = {
    // Every service and pool the wiring built (the config ticker, the uptime monitor, the metrics sampler,
    // page re-renders, the read model), newest first, before the repositories and
    // Mongo go: a page re-render reads both.
    managedResources.closeAll()
    // Each repository's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    readModelRepository.close()
    userRepository.close()
    userStateRepository.close()
    // A users database of its own is a second view on the shared client. When it
    // is this deployment's own database `usersConnection` IS `mongoConnection`,
    // and closing it here would be closing the primary early.
    if (usersConnection ne mongoConnection) usersConnection.close()
    // The same for the fleet database: its own view unless it is this deployment's own (Poland's
    // while `MONGODB_FLEET_DB` is unset).
    if (fleetConnection ne mongoConnection) fleetConnection.close()
    mongoConnection.close()
    // Every connection above BORROWED this client, so none of their own close()
    // calls touched it — it is owned here, and closed once, last.
    mongoSharedClient.foreach(_.close())
  }
}

object Wiring {
  /** Which connection a database shared across deployments binds to — the users database
   *  (`users` + `userStates`), the fleet database (the film pages Facebook fetched).
   *
   *  A second database view costs a boot probe and a `close()` to get right, and
   *  the common case — the shared database IS this deployment's own (`MONGODB_USERS_DB`
   *  unset; Poland's deployment for the fleet database) — needs neither: reuse the
   *  connection already open on that exact database. Only a genuinely DIFFERENT name
   *  opens a second view, which is why `own` is by-name.
   *
   *  Split out here rather than inlined as an `if` because, for the users database, it is
   *  the whole of the shared-account decision: get it backwards and every country silently
   *  keeps its own private copy of every account again, and no page renders any
   *  differently until someone switches country. */
  def connectionTo(ownDbName: settings.MongoDatabaseName,
                   sharedDbName: settings.MongoDatabaseName,
                   own: => MongoConnection,
                   openShared: settings.MongoDatabaseName => MongoConnection): MongoConnection =
    if (sharedDbName == ownDbName) own else openShared(sharedDbName)
}
