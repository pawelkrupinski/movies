package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.MongoConnection
import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, StoredMovieRecord}
import services.staging.{MongoStagingFolder, StagingRepository}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The Mongo fixture the staging-fold it-specs share.
 *
 * `MongoStagingFolder` needs transactions, so every spec that reaches it opens a real client,
 * seeds RAW documents (the fold plans against raw `movies`, so a spec that writes through the
 * repository is not exercising the same read), and purges them afterwards. Three specs were
 * each carrying their own copy of that — ~90 lines of identical client/collection/teardown
 * plumbing — and the copies had already drifted: one cleaned `movies` and staging but not the
 * side rows, which left a `movie_slots` row behind on every run. Those rows are not inert,
 * because the it suites share one database and the fold pulls cross-title siblings by `tmdbId`
 * from the WHOLE collection, so a leftover row is a stray cinema waiting to join someone
 * else's group — it made a passing spec fail for a reason unrelated to the change under test.
 *
 * Hence `purge` takes the anchors up front and runs in a `finally`: a spec cannot forget a
 * collection, and adding one here fixes every caller at once.
 *
 * Each spec still needs its OWN sentinel anchor. The suites run in parallel against one
 * database, so two specs sharing a title (or a `tmdbId` — the fold's sibling lookup makes that
 * a shared namespace too) will delete each other's rows mid-fold.
 */
object FoldFixture {

  /** Refuse to run against anything but a throwaway Mongo, and skip when none is configured. */
  def requireThrowawayMongo(): Unit = {
    assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
    tools.IntegrationMongo.requireThrowaway()
  }

  private def uri    = Env.get("MONGODB_URI").get
  private def dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

  private val Timeout = 10.seconds
  private def now     = java.util.Date.from(java.time.Instant.now())

  /** Everything a fold spec touches, so it never has to open its own client. */
  final class Handles(val db: MongoDatabase, val connection: MongoConnection) {
    val movies:     MongoCollection[Document] = db.getCollection(MovieRepository.Collection)
    val staging:    MongoCollection[Document] = db.getCollection(StagingRepository.Collection)
    val slots:      MongoSlotsRepository      = new MongoSlotsRepository(Some(db))
    val screenings: MongoScreeningsRepository = new MongoScreeningsRepository(Some(db))

    /** The repository as production wires it — split-aware, so a film written through it lands
     *  its cinemas in `movie_slots` and its showtimes in `screenings` rather than embedded. */
    def splitAwareRepository: MovieRepository =
      new services.movies.MongoMovieRepository(Some(db), fallbackToOwnInit = false,
        normalizer = titleNormalizer, screenings = Some(screenings), slots = Some(slots))

    /** The folder as production wires it, or over a repository a spec supplies to inject a
     *  failure (see `FoldOnUnreadableRowSpec`). */
    def folder(repository: MovieRepository = splitAwareRepository, maxRetries: Int = 3): MongoStagingFolder =
      new MongoStagingFolder(connection, normalizer = titleNormalizer, movieRepository = repository,
        maxRetries = maxRetries)

    /** A MIGRATED film: a raw `movies` document carrying NO `sourceData`, which is the shape
     *  prod's corpus is converging to and the one that makes the fold blind to the film's
     *  cinemas unless it reads them back from `movie_slots`. Returns its `_id`. */
    def seedMigratedFilm(title: String, year: Option[Int], tmdbId: Int): String = {
      val id = StoredMovieRecord.idFor(title, year, titleNormalizer)
      Await.result(movies.replaceOne(Filters.eq("_id", id),
        Document("_id" -> id, "tmdbId" -> tmdbId, "sourceData" -> Document(), "updatedAt" -> now),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), Timeout)
      id
    }

    /** One cinema's staging row, CONCLUDED (`tmdbId` present) — an unconcluded group is not
     *  foldable, so a spec that omits it asserts against a fold that never ran. Returns its
     *  `_id`. */
    def seedStagingRow(cinema: String, title: String, year: Option[Int], tmdbId: Int): String = {
      val id = s"$cinema|${titleNormalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"
      Await.result(staging.replaceOne(Filters.eq("_id", id),
        Document("_id" -> id, "tmdbId" -> tmdbId,
          "sourceData" -> Document(cinema -> Document("title" -> title)),
          "updatedAt" -> now),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), Timeout)
      id
    }

    /** The `movies` `_id`s currently in a sanitize group — what a fold left standing. */
    def filmIds(sanitize: String): Seq[String] =
      Await.result(movies.find(Filters.regex("_id", s"^$sanitize\\|")).toFuture(), Timeout)
        .flatMap(_.get("_id").map(_.asString().getValue))

    /** Is this staging row still there? An assertion about what a fold decided means nothing
     *  unless the fold actually consumed its input. */
    def stagingRowExists(id: String): Boolean =
      Await.result(staging.find(Filters.eq("_id", id)).toFuture(), Timeout).nonEmpty

    private[integration] def purge(anchors: Seq[String]): Unit = anchors.foreach { anchor =>
      // Side rows FIRST, off the ids that still exist — once the `movies` documents are gone
      // there is nothing left to derive the film ids from.
      filmIds(anchor).foreach { id => slots.deleteFilm(id); screenings.deleteFilm(id) }
      Await.ready(movies.deleteMany(Filters.regex("_id", s"^$anchor\\|")).toFuture(), Timeout)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*$anchor.*")).toFuture(), Timeout)
    }
  }

  /** Run `test` against a live throwaway Mongo, purging every `anchor`'s rows afterwards
   *  whatever the test did — including when it threw. */
  def withFold[A](anchors: String*)(test: Handles => A): A = {
    val client = MongoClient(uri)
    val db     = client.getDatabase(dbName)
    val handles = new Handles(db, new MongoConnection(Some(uri), dbName, required = false))
    try test(handles)
    finally {
      try handles.purge(anchors) finally client.close()
    }
  }
}
