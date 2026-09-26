package modules

import clients.tools.{FakeHttpFetch, FixtureRoot}
import services.tasks.{DetailReaper, ScrapeReaper}
import services.MongoAddress
import tools.{Env, HttpFetch}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.CountDownLatch
import scala.concurrent.duration._

/**
 * Local dev entry point behind `sbt localStack`. Runs the REAL worker pipeline
 * (scrape → enrich → project to the read model) against a LOCAL Mongo, but
 * replays every HTTP fetch from a fixture directory (default `today`) instead of
 * hitting the internet. `localStack` also starts `web/run` against the same
 * local Mongo, so the web app serves the projected fixture corpus while still
 * fetching posters/etc. from the real internet.
 *
 * Test scope, not production: it pulls in `FakeHttpFetch` (testkit, a Test-only
 * dep) and is launched via `worker/Test/bgRunMain`.
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
    val env              = Env.fromProcess()
    val mongo            = localMongo(key => Option(System.getenv(key)), env)
    val fixtureRoot      = fixtureRootFor(env, new java.io.File(".").getCanonicalFile)
    val fixtureDirectory = env.get("KINOWO_FIXTURE_DIR").getOrElse("today")
    println(s"[local-fixture-worker] replaying HTTP from ${fixtureRoot.of(fixtureDirectory)} " +
      s"into Mongo ${mongo.uri.getOrElse("?")} db=${mongo.database.getOrElse("?")}")

    val wiring = new FixtureWorkerWiring(fixtureDirectory, mongo, fixtureRoot, env)
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
   *  is why the process environment is asked separately from `env`. Handed to the wiring as
   *  its address; nothing rewrites the process's MONGODB_URI to get it there. */
  private[modules] def localMongo(processEnvironment: String => Option[String], env: Env): MongoAddress =
    MongoAddress(
      uri      = Some(processEnvironment("MONGODB_URI").filter(_.nonEmpty)
        .getOrElse(env.get("KINOWO_LOCAL_MONGO_URI").getOrElse(DefaultMongoUri))),
      database = Some(processEnvironment("MONGODB_DB").filter(_.nonEmpty)
        .getOrElse(env.get("KINOWO_LOCAL_MONGO_DB").getOrElse(DefaultMongoDb))))

  /** Where the fixture corpus lives. `bgRunMain` forks with CWD = the worker module
   *  directory, but the corpus is `test/resources/fixtures/…` under the repository root, so
   *  walk up from `workingDirectory` to the directory holding it — unless KINOWO_FIXTURE_ROOT
   *  already names one. Handed to the wiring's fetches rather than set as a property. */
  private[modules] def fixtureRootFor(env: Env, workingDirectory: java.io.File): FixtureRoot =
    env.get("KINOWO_FIXTURE_ROOT").filter(_.nonEmpty).map(FixtureRoot(_)).getOrElse {
      Iterator.iterate(workingDirectory)(_.getParentFile).takeWhile(_ != null)
        .map(new java.io.File(_, FixtureRoot.RepositoryRelative.directory))
        .find(_.isDirectory)
        .fold(FixtureRoot.RepositoryRelative)(directory => FixtureRoot(directory.getPath))
    }
}

/**
 * `WorkerWiring` with fixture-replay HTTP but the real (local) Mongo + read-model
 * projection. Mirrors `FixtureTestWiring`'s fetch overrides, minus its in-memory
 * repos — here the projector writes to the local Mongo at `localMongo` so `web` can
 * serve it, and the fixtures are read from under `fixtureRoot`.
 */
class FixtureWorkerWiring(fixtureDirectory: String, localMongo: MongoAddress, fixtureRoot: FixtureRoot, environment: Env)
    extends WorkerWiring(env = environment) {
  override lazy val mongoAddress: MongoAddress = localMongo
  override lazy val httoFetch: HttpFetch      = new FakeHttpFetch(fixtureDirectory, root = fixtureRoot)
  override lazy val multikinoFetch: HttpFetch = httoFetch
  override lazy val biletynaFetch: HttpFetch  = httoFetch

  // A missing fixture is a permanent local miss — one attempt, no retry storm.
  override protected def scrapeAttemptCeiling: Int = 1

  // The corpus is STATIC, so re-scraping it on the production 1-min cadence only
  // re-triggers the same fuzzy-resolution misses — a film whose director-walk
  // resolves to a TMDB id whose `external_ids` the recorder never captured fails
  // unretryably and the production loop "retries forever". Populate the read
  // model once at boot, then idle: push the scrape + detail reapers out to a day
  // so they don't re-enqueue the static fixtures. (Web still serves; a fresh
  // corpus is a localStack restart away.)
  override lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore,
      interval = 24.hours, initialDelay = initialScrapeDelaySeconds.seconds, runStore = scheduledRunStore)
  override lazy val detailReaper =
    new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore, eventBus,
      tickInterval = 24.hours, runStore = scheduledRunStore)

  // Helios bakes the scrape day into its REST URLs, so pin it to the captured
  // day or every Helios fixture misses. Prefer <directory>/CAPTURE_DATE (written by
  // the recorder), fall back to the directory name if it's dd-MM-yyyy, else the real
  // date (FakeHttpFetch then returns its empty fallback for the day's URLs).
  override protected def heliosToday: LocalDate =
    FixtureWorkerWiring.captureDate(fixtureDirectory).getOrElse(super.heliosToday)
}

object FixtureWorkerWiring {
  private val Fmt = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  /** The scrape day for a fixture directory: `date=dd-MM-yyyy` from its CAPTURE_DATE
   *  file, else the directory name when it is itself a `dd-MM-yyyy` date. */
  def captureDate(fixtureDirectory: String): Option[LocalDate] = {
    val fromFile = scala.util.Try {
      val f = new java.io.File(s"test/resources/fixtures/$fixtureDirectory/CAPTURE_DATE")
      val src = scala.io.Source.fromFile(f, "UTF-8")
      try src.getLines().find(_.startsWith("date=")).map(_.stripPrefix("date=").trim)
      finally src.close()
    }.toOption.flatten
    (fromFile.toList :+ fixtureDirectory)
      .flatMap(s => scala.util.Try(LocalDate.parse(s, Fmt)).toOption)
      .headOption
  }
}
