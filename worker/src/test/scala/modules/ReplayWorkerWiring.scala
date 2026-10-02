package modules

import clients.tools.FakeHttpFetch
import services.MongoAddress
import services.tasks.{DetailReaper, ScrapeReaper}
import settings.FixtureRoot
import tools.{Env, HttpFetch}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._

/**
 * `WorkerWiring` with fixture-replay HTTP but a real Mongo + read-model projection: what
 * `sbt localStack` runs.
 * Mirrors `FixtureTestWiring`'s fetch overrides, minus its in-memory
 * repos — here the projector writes to the local Mongo at `localMongo` so `web` can
 * serve it, and the fixtures are read from under `fixtureRoot`.
 */
class ReplayWorkerWiring(fixtureDirectory: String, localMongo: MongoAddress, fixtureRoot: FixtureRoot, environment: Env)
    extends WorkerWiring(env = environment) {
  override lazy val mongoAddress: MongoAddress = localMongo
  // EVERY fetch seam replays from the corpus — `ReplayWorkerWiringSpec` holds the list to
  // production's. A seam left alone keeps production's chain (the enrichment phase fetch, the Zyte
  // fallback, the residential proxy), and that chain reaches the live site.
  override lazy val httoFetch: HttpFetch            = new FakeHttpFetch(fixtureDirectory, root = fixtureRoot)
  override protected def realHttpLeaf: HttpFetch    = httoFetch
  override lazy val enrichmentFetch: HttpFetch      = httoFetch
  override lazy val identityLookupFetch: HttpFetch  = httoFetch
  override lazy val multikinoFetch: HttpFetch       = httoFetch
  override lazy val multikinoPosterFetch: HttpFetch = httoFetch
  override lazy val zyteFetch: HttpFetch            = httoFetch
  override lazy val biletynaFetch: HttpFetch        = httoFetch
  override lazy val flicksFetch: HttpFetch          = httoFetch
  override lazy val vueFetch: HttpFetch             = httoFetch
  override lazy val odeonFetch: HttpFetch           = httoFetch

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
      interval = services.tasks.ScrapeReaper.TickInterval(24.hours), initialDelay = settings.ScrapeInitialDelay(initialScrapeDelay.value), runStore = scheduledRunStore)
  override lazy val detailReaper =
    new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore, eventBus,
      tickInterval = settings.DetailTickInterval(24.hours), runStore = scheduledRunStore)

  // Helios bakes the scrape day into its REST URLs, so pin it to the captured
  // day or every Helios fixture misses. Prefer <directory>/CAPTURE_DATE (written by
  // the recorder), fall back to the directory name if it's dd-MM-yyyy, else the real
  // date (FakeHttpFetch then returns its empty fallback for the day's URLs).
  override protected def heliosToday: LocalDate =
    ReplayWorkerWiring.captureDate(fixtureDirectory, fixtureRoot).getOrElse(super.heliosToday)
}

object ReplayWorkerWiring {
  private val Fmt = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  /** The scrape day for a fixture directory: `date=dd-MM-yyyy` from its CAPTURE_DATE
   *  file, else the directory name when it is itself a `dd-MM-yyyy` date. */
  def captureDate(fixtureDirectory: String, root: FixtureRoot = FixtureRoot.RepositoryRelative): Option[LocalDate] = {
    val fromFile = scala.util.Try {
      val f = new java.io.File(root.of(fixtureDirectory), "CAPTURE_DATE")
      val src = scala.io.Source.fromFile(f, "UTF-8")
      try src.getLines().find(_.startsWith("date=")).map(_.stripPrefix("date=").trim)
      finally src.close()
    }.toOption.flatten
    (fromFile.toList :+ fixtureDirectory)
      .flatMap(s => scala.util.Try(LocalDate.parse(s, Fmt)).toOption)
      .headOption
  }
}
