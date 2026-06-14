package modules

import clients.tools.FakeHttpFetch
import services.tasks.{DetailReaper, ScrapeReaper}
import tools.{Env, HttpFetch}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.CountDownLatch
import scala.concurrent.duration._

/**
 * Local dev entry point behind `sbt localStack`. Runs the REAL worker pipeline
 * (scrape → enrich → project to the read model) against a LOCAL Mongo, but
 * replays every HTTP fetch from a fixture dir (default `today`) instead of
 * hitting the internet. `localStack` also starts `web/run` against the same
 * local Mongo, so the web app serves the projected fixture corpus while still
 * fetching posters/etc. from the real internet.
 *
 * Test scope, not production: it pulls in `FakeHttpFetch` (testkit, a Test-only
 * dep) and is launched via `worker/Test/bgRunMain`.
 */
object LocalFixtureWorkerMain {
  // Defaults MUST match the `localStack` command in build.sbt so the worker
  // (this forked JVM) and `web/run` (the sbt JVM) land on the SAME local db.
  // Port 27018, NOT 27017 — 27017 is commonly the `flyctl proxy` to prod Mongo,
  // and this stack must never touch the prod db.
  private val DefaultMongoUri = "mongodb://127.0.0.1:27018"
  private val DefaultMongoDb  = "kinowo_local"

  def main(args: Array[String]): Unit = {
    forceLocalMongo()
    locateFixtureRoot()
    val fixtureDir = Env.get("KINOWO_FIXTURE_DIR").getOrElse("today")
    println(s"[local-fixture-worker] replaying HTTP from test/resources/fixtures/$fixtureDir " +
      s"into Mongo ${Env.get("MONGODB_URI").getOrElse("?")} db=${Env.get("MONGODB_DB").getOrElse("kinowo")}")

    val wiring = new FixtureWorkerWiring(fixtureDir)
    wiring.start()
    println("[local-fixture-worker] started — scraping the fixture corpus into the local read model. Ctrl-C to stop.")

    val latch = new CountDownLatch(1)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      try wiring.stop() catch { case _: Throwable => () }
      finally latch.countDown()
    }))
    latch.await()
  }

  /** Point this worker at a LOCAL Mongo, distinct from the prod db `.env.local`
   *  reaches. A JVM property beats `.env.local` in `Env`'s precedence, so this
   *  overrides the prod URI; an explicit process-env MONGODB_URI still wins, so
   *  a user who exported one keeps control. */
  private def forceLocalMongo(): Unit = {
    if (System.getenv("MONGODB_URI") == null)
      System.setProperty("MONGODB_URI", Env.get("KINOWO_LOCAL_MONGO_URI").getOrElse(DefaultMongoUri))
    if (System.getenv("MONGODB_DB") == null)
      System.setProperty("MONGODB_DB", Env.get("KINOWO_LOCAL_MONGO_DB").getOrElse(DefaultMongoDb))
  }

  /** `bgRunMain` forks with CWD = the worker module dir, but FakeHttpFetch reads
   *  `test/resources/fixtures/…` relative to the repository root. Walk up from the CWD
   *  to the dir that contains `test/resources/fixtures` and pin it as
   *  KINOWO_FIXTURE_ROOT (a JVM prop FakeHttpFetch honours), so the corpus
   *  resolves regardless of where the fork started. No-op if already set. */
  private def locateFixtureRoot(): Unit = {
    if (Env.get("KINOWO_FIXTURE_ROOT").isEmpty) {
      var dir = new java.io.File(".").getCanonicalFile
      while (dir != null && !new java.io.File(dir, "test/resources/fixtures").isDirectory)
        dir = dir.getParentFile
      if (dir != null)
        System.setProperty("KINOWO_FIXTURE_ROOT", new java.io.File(dir, "test/resources/fixtures").getPath)
    }
  }
}

/**
 * `WorkerWiring` with fixture-replay HTTP but the real (local) Mongo + read-model
 * projection. Mirrors `FixtureTestWiring`'s fetch overrides, minus its in-memory
 * repos — here the projector writes to the local Mongo so `web` can serve it.
 */
class FixtureWorkerWiring(fixtureDir: String) extends WorkerWiring {
  override lazy val httoFetch: HttpFetch      = new FakeHttpFetch(fixtureDir)
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
      interval = 24.hours, runStore = scheduledRunStore)

  // Helios bakes the scrape day into its REST URLs, so pin it to the captured
  // day or every Helios fixture misses. Prefer <dir>/CAPTURE_DATE (written by
  // the recorder), fall back to the dir name if it's dd-MM-yyyy, else the real
  // date (FakeHttpFetch then returns its empty fallback for the day's URLs).
  override protected def heliosToday: LocalDate =
    FixtureWorkerWiring.captureDate(fixtureDir).getOrElse(super.heliosToday)
}

object FixtureWorkerWiring {
  private val Fmt = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  /** The scrape day for a fixture dir: `date=dd-MM-yyyy` from its CAPTURE_DATE
   *  file, else the dir name when it is itself a `dd-MM-yyyy` date. */
  def captureDate(fixtureDir: String): Option[LocalDate] = {
    val fromFile = scala.util.Try {
      val f = new java.io.File(s"test/resources/fixtures/$fixtureDir/CAPTURE_DATE")
      val src = scala.io.Source.fromFile(f, "UTF-8")
      try src.getLines().find(_.startsWith("date=")).map(_.stripPrefix("date=").trim)
      finally src.close()
    }.toOption.flatten
    (fromFile.toList :+ fixtureDir)
      .flatMap(s => scala.util.Try(LocalDate.parse(s, Fmt)).toOption)
      .headOption
  }
}
