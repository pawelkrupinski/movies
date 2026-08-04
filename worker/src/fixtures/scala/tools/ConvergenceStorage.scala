package tools

import org.mongodb.scala.MongoDatabase
import services.MongoConnection
import services.movies._
import services.readmodel.{MongoReadModelRepository, ReadModelReader, ReadModelWriter}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.enrichment.{MongoOmdbAttemptStore, OmdbAttemptStore}
import services.freshness.{FreshnessStore, MongoFreshnessStore}
import services.staging._
import services.tasks.{ChunkScrapeStore, MongoChunkScrapeStore, MongoTaskQueue, TaskQueue}
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Where a convergence run keeps the state it is making claims about: a real MongoDB.
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
 * So there is no in-memory side any more — CI and a local run both go through a real
 * database. Keeping the choice would have meant keeping a path that passes for reasons
 * production never gets to rely on, and the default is what everything quietly picks.
 *
 * A container, emphatically NOT a tunnel. The enrichment cache used to reach a
 * production cluster over a `flyctl proxy` and that proxy caused every serious failure
 * this suite has had. A `mongo` service beside the job has none of those properties: it
 * is on localhost, needs no credential, holds nothing anyone would miss, and is thrown
 * away with the runner.
 */
trait ConvergenceStorage {

  /** For the run to name the database it used — a suite that cannot say where its state
   *  went is one you cannot go and look at when it disagrees with you. */
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

  /** Takes the repository rather than closing over one: the fold is defined in terms of
   *  the `movies` the caller is using, and the Mongo folder reaches it through a
   *  transaction on the connection rather than the repository handle. */
  def stagingFolder(movieRepository: MovieRepository): StagingFolder

  def close(): Unit = ()
}

object ConvergenceStorage {

  /** How long to wait for a database that is either on localhost or absent. */
  val LocalServerSelectionTimeout: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.DurationInt(10).seconds

  /**
   * Mongo from `MONGODB_URI`, or a failure — never a fallback.
   *
   * Deliberately not silent. A suite that falls back to memory when it cannot connect is
   * a suite that reports success for a run that tested half of what it says it did — the
   * exact shape of the enrichment gate that resolved 0 of 892 films while three specs
   * passed. An unreachable database fails the run rather than degrading it.
   */
  def fromEnv(purpose: String): ConvergenceStorage =
    Env.get("MONGODB_URI").filter(_.nonEmpty)
      .map(uri => mongo(uri, purpose))
      .getOrElse(throw new IllegalStateException(
        "MONGODB_URI is not set. This suite runs on a real database only — there is no " +
        "in-memory storage any more, because a claim proved against a map is not a claim " +
        "about the pipeline that ships. Start one with `scripts/convergence-local.sh <code>`, " +
        "or name an existing throwaway."))

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
    //
    // NOT wired with `screenings`/`slots`, unlike `WorkerWiring` — so showtimes stay
    // embedded in the film document here and the side collections, though present below,
    // are never written by the pipeline. That is a real remaining difference from prod,
    // and it is what makes the order-independence spec's screenings axis vacuous (see the
    // note there). Passing the two arguments is NOT the fix: tried 2026-07-31, it took
    // the read model to 0 cinemas / 0 screenings / 0 films and made an identical
    // re-scrape churn 3,079 writes, with `movies` still holding all 773 films.
    override lazy val movies     = new MongoMovieRepository(shared, fallbackToOwnInit = false, normalizer = titleNormalizer)
    override lazy val screenings = new MongoScreeningsRepository(shared)
    override lazy val slots      = new MongoSlotsRepository(shared)
    override lazy val readModel: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(shared)
    override lazy val staging    = new MongoStagingRepository(shared, normalizer = titleNormalizer)
    override lazy val archive    = new MongoScrapeArchiveRepository(shared)
    override lazy val tasks: TaskQueue              = new MongoTaskQueue(shared)
    override lazy val freshness: FreshnessStore     = new MongoFreshnessStore(shared)
    override lazy val chunkScrape: ChunkScrapeStore = new MongoChunkScrapeStore(shared)
    override lazy val omdbAttempt: OmdbAttemptStore = new MongoOmdbAttemptStore(shared)

    override def stagingFolder(movieRepository: MovieRepository): StagingFolder =
      new MongoStagingFolder(connection, normalizer = titleNormalizer)

    // Only OURS. `closeAll` drops every isolated database in the process, which is fine
    // when a leg is the only holder and destructive the moment anything else is.
    override def close(): Unit = IsolatedMongoDatabase.drop(database)
  }
}
