package tools

import org.mongodb.scala.MongoDatabase
import services.MongoConnection
import services.movies._
import services.readmodel.{InMemoryReadModelRepository, MongoReadModelRepository, ReadModelReader, ReadModelWriter}
import services.scrapes.{InMemoryScrapeArchiveRepository, MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.enrichment.{InMemoryOmdbAttemptStore, MongoOmdbAttemptStore, OmdbAttemptStore}
import services.freshness.{FreshnessStore, InMemoryFreshnessStore, MongoFreshnessStore}
import services.staging._
import services.tasks.{ChunkScrapeStore, InMemoryChunkScrapeStore, InMemoryTaskQueue, MongoChunkScrapeStore,
  MongoTaskQueue, TaskQueue}

/**
 * Where a convergence run keeps the state it is making claims about — in memory, or in
 * a real MongoDB.
 *
 * The suite asks whether the pipeline reaches a fixpoint, and until now it asked that
 * of a pipeline with no database under it: every repository was an in-memory fake. That
 * is fast and deterministic, and it silently excludes the entire persistence layer from
 * a claim that reads as though it covers the whole pipeline. Nothing in those runs could
 * have caught a BSON codec that drops a field, a `findAll` that recurses the async
 * driver into a StackOverflowError once a collection grows, a required DTO field that
 * fails a whole batch's decode, or a staging fold that needs a real transaction — all of
 * which this repository has actually shipped.
 *
 * So the storage is now a choice, and both sides run the SAME assertions. CI takes the
 * Mongo path against a container; a local run takes whichever it is given. Neither is a
 * reduced suite: if a claim holds in memory and not in Mongo, that difference is the
 * finding.
 *
 * A container, emphatically NOT a tunnel. The enrichment cache used to reach a
 * production cluster over a `flyctl proxy` and that proxy caused every serious failure
 * this suite has had. A `mongo` service beside the job has none of those properties: it
 * is on localhost, needs no credential, holds nothing anyone would miss, and is thrown
 * away with the runner.
 */
trait ConvergenceStorage {

  /** For the run to say out loud which side of the choice it took — a suite that can
   *  silently fall back to memory is one that quietly stops testing what it claims. */
  def describe: String

  def connection:  MongoConnection
  def movies:      MovieRepository
  def screenings:  ScreeningsRepository
  def slots:       SlotsRepository
  def readModel:   ReadModelReader & ReadModelWriter
  def staging:     StagingRepository
  def archive:     ScrapeArchiveRepository

  // The rest of the collections production keeps beside the pipeline's own state. They
  // are not what the assertions READ, which is exactly why they were easy to leave
  // faked — and why leaving them faked quietly narrows the claim: a settle that
  // enqueues a task, stamps freshness or records an OMDb attempt is doing real writes
  // in production and no writes at all here, so nothing about those paths is exercised.
  def tasks:       TaskQueue
  def freshness:   FreshnessStore
  def chunkScrape: ChunkScrapeStore
  def omdbAttempt: OmdbAttemptStore

  /** Takes the repository rather than closing over one, because the in-memory folder
   *  works against `movies` directly while the Mongo one goes through a transaction on
   *  the connection. */
  def stagingFolder(movieRepository: MovieRepository): StagingFolder

  def close(): Unit = ()
}

object ConvergenceStorage {

  /** How long to wait for a database that is either on localhost or absent. */
  val LocalServerSelectionTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.DurationInt(10).seconds

  /**
   * Mongo when `MONGODB_URI` names one, memory otherwise.
   *
   * Deliberately NOT the other way round, and deliberately not silent. A suite that
   * defaults to Mongo and falls back to memory when it cannot connect is a suite that
   * reports success for a run that tested half of what it says it did — the exact shape
   * of the enrichment gate that resolved 0 of 892 films while three specs passed. Naming
   * a URI is an explicit request for the Mongo path, and getting it is then guaranteed:
   * an unreachable database fails the run rather than degrading it.
   */
  def fromEnv(purpose: String): ConvergenceStorage =
    Env.get("MONGODB_URI").filter(_.nonEmpty).fold(inMemory)(uri => mongo(uri, purpose))

  def inMemory: ConvergenceStorage = new ConvergenceStorage {
    override val describe = "in-memory repositories"

    // Production's storage SHAPE even without production's storage — showtimes in
    // `screenings`, slots in `movie_slots`, neither inlined on the `movies` row. A fake
    // that inlined everything would carry showtimes across a rename for free, and a
    // merge is a rename, so it could not express the bug.
    override lazy val screenings = new InMemoryScreeningsRepository
    override lazy val slots      = new InMemorySlotsRepository
    override lazy val movies     =
      new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots))
    override lazy val readModel: ReadModelReader & ReadModelWriter = new InMemoryReadModelRepository()
    override lazy val staging    = new InMemoryStagingRepository()
    override lazy val archive    = new InMemoryScrapeArchiveRepository
    override lazy val connection = new MongoConnection(uri = None, dbName = "kinowo", required = false)
    override lazy val tasks: TaskQueue             = new InMemoryTaskQueue
    override lazy val freshness: FreshnessStore    = new InMemoryFreshnessStore
    override lazy val chunkScrape: ChunkScrapeStore = new InMemoryChunkScrapeStore()
    override lazy val omdbAttempt: OmdbAttemptStore = new InMemoryOmdbAttemptStore

    override def stagingFolder(movieRepository: MovieRepository): StagingFolder =
      new InMemoryStagingFolder(staging, movieRepository)
  }

  /** A uniquely-named throwaway database on `uri`, dropped by [[ConvergenceStorage.close]].
   *  Unique per run so the three country legs — and anything else on the `it` layer —
   *  can share one cluster without colliding, including with a re-run of themselves. */
  def mongo(uri: String, purpose: String): ConvergenceStorage = {
    // The name is taken FROM the opened database, never generated a second time.
    // `IsolatedMongoDatabase.nameFor` embeds `System.nanoTime()`, so calling it again for
    // the connection produced a DIFFERENT database from the one the repositories were
    // handed: staging wrote 6,975 rows to one, `MongoStagingFolder` looked for them in
    // the other, found none, and correctly reported nothing to fold. The corpus never
    // reached `movies`, the suite reported `resolved NOTHING — 0 films`, and nothing
    // anywhere was in error — each half was doing exactly what it was told.
    val database = IsolatedMongoDatabase.open(uri, purpose)
    new MongoConvergenceStorage(database, uri, database.name)
  }

  private final class MongoConvergenceStorage(database: MongoDatabase, uri: String, name: String)
    extends ConvergenceStorage {

    override val describe = s"MongoDB $name"

    private val shared = Some(database)

    // `required = true` so an unreachable database FAILS the run: a convergence leg that
    // degraded to no-Mongo would report success for a run that tested half of what it
    // says it did. And a short server-selection cap because this is a container on
    // localhost — if it isn't there, it isn't coming, and the driver's 30s default just
    // turns a misconfiguration into a hang. (The production default is deliberately long
    // to ride out a recovering replica-set node; nothing here has one.)
    override lazy val connection = new MongoConnection(
      uri = Some(uri), dbName = name, required = true,
      serverSelectionTimeout = Some(ConvergenceStorage.LocalServerSelectionTimeout))

    // `fallbackToOwnInit = false`: the database is handed in, so a `None` here would
    // mean the caller's connection failed, and re-running the repository's own init
    // would just hit the same timeout twice.
    override lazy val movies     = new MongoMovieRepository(shared, fallbackToOwnInit = false)
    override lazy val screenings = new MongoScreeningsRepository(shared)
    override lazy val slots      = new MongoSlotsRepository(shared)
    override lazy val readModel: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(shared)
    override lazy val staging    = new MongoStagingRepository(shared)
    override lazy val archive    = new MongoScrapeArchiveRepository(shared)
    override lazy val tasks: TaskQueue              = new MongoTaskQueue(shared)
    override lazy val freshness: FreshnessStore     = new MongoFreshnessStore(shared)
    override lazy val chunkScrape: ChunkScrapeStore = new MongoChunkScrapeStore(shared)
    override lazy val omdbAttempt: OmdbAttemptStore = new MongoOmdbAttemptStore(shared)

    override def stagingFolder(movieRepository: MovieRepository): StagingFolder =
      new MongoStagingFolder(connection)

    override def close(): Unit = IsolatedMongoDatabase.closeAll()
  }
}
