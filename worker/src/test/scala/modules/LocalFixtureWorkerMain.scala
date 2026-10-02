package modules

import services.MongoAddress
import settings.{FixtureRoot, MongoDatabaseName, MongoUri, ProcessConfiguration}

import java.util.concurrent.CountDownLatch

/**
 * Local dev entry point behind `sbt localStack`. Runs the REAL worker pipeline
 * (scrape → enrich → project to the read model) against a LOCAL Mongo, but
 * replays every HTTP fetch from a fixture directory (default `today`) instead of
 * hitting the internet. `localStack` also starts `web/run` against the same
 * local Mongo, so the web app serves the projected fixture corpus while still
 * fetching posters/etc. from the real internet.
 *
 * Test scope, not production: its wiring ([[ReplayWorkerWiring]]) replays through `FakeHttpFetch`
 * (testkit), and it is launched via `worker/Test/bgRunMain`.
 */
object LocalFixtureWorkerMain {
  // Defaults MUST match the `localStack` command in build.sbt so the worker
  // (this forked JVM) and `web/run` (the sbt JVM) land on the SAME local db: the
  // native brew Mongo on :28017 (scripts/local-mirror/start-local-mongo.sh), the
  // same instance the /debug mirror + scripts/reset-corpus.sh --local use. NOT
  // :27017 — that's the `flyctl proxy` to prod, and this stack must never touch
  // the prod db.
  private[modules] val DefaultMongoUri = "mongodb://127.0.0.1:28017/?directConnection=true"
  private[modules] val DefaultMongoDb  = "kinowo_local"

  def main(args: Array[String]): Unit = {
    val process          = ProcessConfiguration.resolve()
    val mongo            = localMongo(ProcessConfiguration.resolveExported().mongoAddress, process)
    val fixtureRoot      = fixtureRootFor(process, new java.io.File(".").getCanonicalFile)
    val fixtureDirectory = process.localStackFixtureDirectory.fold("today")(_.value)
    println(s"[local-fixture-worker] replaying HTTP from ${fixtureRoot.of(fixtureDirectory)} " +
      s"into Mongo ${mongo.uri.fold("?")(_.value)} db=${mongo.database.fold("?")(_.value)}")

    val wiring = new ReplayWorkerWiring(fixtureDirectory, mongo, fixtureRoot, process.env)
    wiring.start()
    println("[local-fixture-worker] started — scraping the fixture corpus into the local read model. Ctrl-C to stop.")

    val latch = new CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      try wiring.stop() catch { case _: Throwable => () }
      finally latch.countDown()
    }))
    latch.await()
  }

  /** The LOCAL Mongo this worker writes into, distinct from the prod database `.env.local`
   *  reaches: a MONGODB_URI / MONGODB_DB exported in the process environment itself still
   *  wins (a user who exported one keeps control), else KINOWO_LOCAL_MONGO_URI / _DB, else
   *  the `localStack` defaults. `.env.local`'s own MONGODB_URI — prod — never counts, which
   *  is why `exported` is resolved without that file (`ProcessConfiguration.resolveExported`). Handed to the wiring as
   *  its address; nothing rewrites the process's MONGODB_URI to get it there. */
  private[modules] def localMongo(exported: MongoAddress, configuration: ProcessConfiguration): MongoAddress =
    MongoAddress(
      uri      = Some(exported.uri
        .getOrElse(configuration.localStackMongoUri.fold(MongoUri(DefaultMongoUri))(uri => MongoUri(uri.value)))),
      database = Some(exported.database
        .getOrElse(configuration.localStackDatabase.fold(MongoDatabaseName(DefaultMongoDb))(database => MongoDatabaseName(database.value)))))

  /** Where the fixture corpus lives. `bgRunMain` forks with CWD = the worker module
   *  directory, but the corpus is `test/resources/fixtures/…` under the repository root, so
   *  walk up from `workingDirectory` to the directory holding it — unless KINOWO_FIXTURE_ROOT
   *  already names one. Handed to the wiring's fetches rather than set as a property. */
  private[modules] def fixtureRootFor(configuration: ProcessConfiguration, workingDirectory: java.io.File): FixtureRoot = {
    val configured = configuration.fixtureRoot
    if (configured != FixtureRoot.RepositoryRelative) configured
    else Iterator.iterate(workingDirectory)(_.getParentFile).takeWhile(_ != null)
      .map(_.toPath.resolve(FixtureRoot.RepositoryRelative.value))
      .find(java.nio.file.Files.isDirectory(_))
      .fold(FixtureRoot.RepositoryRelative)(FixtureRoot(_))
  }
}
