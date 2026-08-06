package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.Multikino
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, SingleObservableFuture, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.{StoredMovieRecord, UnreadableByIdMovieRepository}
import services.staging.{MongoStagingFolder, StagingRepository}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * What the staging fold does when the storage underneath it does not answer — the branches
 * that decide whether a film keeps its board, and none of which a healthy run reaches.
 *
 * Two are "a failed read is not data" guards, and they are the reason the fold plans and
 * completes against a stitched view at all. The other two are the side-collection MIGRATION
 * failing, which must not take a COMMITTED fold down with it.
 *
 * Under the storage split a film's cinemas live in `movie_slots`, so the fold reads them back
 * through `MovieRepository.findByIdChecked`. That read can FAIL — a Mongo timeout, a slot-read
 * error — and `findByIdChecked` answers `(None, false)` precisely so a caller cannot mistake
 * "I could not tell you" for "this film has no cinemas". Both mistakes are destructive here:
 *
 *   - voting on an empty pool re-keys the film onto whatever single staging row is in hand,
 *     which is the settle-beat oscillation (`FoldSpellingAgreesWithSettleSpec`);
 *   - completing on an empty record makes `upsert`'s `replaceFilm` delete every slot and
 *     screening the record does not name, which is the shape that took prod PL from 39,413
 *     upcoming showtimes to 18,161 (@8033e39c6).
 *
 * So the fold refuses rather than guesses. These are the branches that say so.
 */
class FoldOnUnreadableRowSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  private val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

  // Its own sentinel prefix and its own cleanup — the it suites share one database and run
  // in parallel (see the naming note in StagingFoldIntegrationSpec).
  private val title    = "__foldunreadable-it-sentinel__"
  private val sanitize = titleNormalizer.sanitize(title)
  private val tmdbId   = 42433

  private def withFold(test: (MongoStagingFolder, UnreadableByIdMovieRepository, MongoClient) => Unit): Unit = {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    val staging    = db.getCollection(StagingRepository.Collection)
    val existing   = StoredMovieRecord.idFor(title, Some(2026), titleNormalizer)
    // The film exists in `movies` and is MIGRATED — no embedded `sourceData`, so the fold has
    // nothing to plan on unless it can read the side rows back.
    val repository = new UnreadableByIdMovieRepository()
    try {
      Await.result(movies.replaceOne(Filters.eq("_id", existing),
        org.mongodb.scala.Document("_id" -> existing, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      val stagingId = s"${Multikino.displayName}|$sanitize|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
            org.mongodb.scala.Document("title" -> title)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      test(new MongoStagingFolder(connection, normalizer = titleNormalizer,
        movieRepository = repository), repository, client)
    } finally {
      Await.ready(movies.deleteMany(Filters.regex("_id", s"^$sanitize\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*$sanitize.*")).toFuture(), 10.seconds)
      client.close()
    }
  }

  it should "refuse to fold a film whose cinemas it could not read, rather than re-key it on the staging spelling" in {
    withFold { (folder, _, _) =>
      val thrown = intercept[IllegalStateException](folder.foldGroup(title))
      withClue(s"the fold failed, but not for the reason under test: ${thrown.getMessage}\n") {
        thrown.getMessage should include("Refusing to re-key the film")
      }
    }
  }

  /** A repository whose side-collection MIGRATION fails, the two ways it can: reporting
   *  `false` (a read or write it depended on did not happen) or raising outright. */
  private class UnmovableFilmRepository(raise: Boolean) extends UnreadableByIdMovieRepository {
    failing = false
    val attempts = new java.util.concurrent.atomic.AtomicInteger(0)
    override def moveFilm(fromId: String, toId: String): Boolean = {
      attempts.incrementAndGet()
      if (raise) throw new RuntimeException("simulated side-collection move failure") else false
    }
  }

  /** Two year-variants of one film, which `planGroup` collapses onto the TMDB year — so the
   *  fold RETIRES one key, which is what makes it try to migrate that key's side rows. */
  private def seedTwoYearVariants(movies: org.mongodb.scala.MongoCollection[org.mongodb.scala.Document],
                                  staging: org.mongodb.scala.MongoCollection[org.mongodb.scala.Document]): String = {
    Seq(2025, 2026).foreach { year =>
      val id = StoredMovieRecord.idFor(title, Some(year), titleNormalizer)
      Await.result(movies.replaceOne(Filters.eq("_id", id),
        org.mongodb.scala.Document("_id" -> id, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    }
    val stagingId = s"${Multikino.displayName}|$sanitize|2026"
    Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
      org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> tmdbId,
        "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
          org.mongodb.scala.Document("title" -> title)),
        "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
      new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    stagingId
  }

  /** A migration that fails must NOT take the fold down with it. The fold has already
   *  COMMITTED by the time the side rows are carried across, so raising here would reschedule
   *  a fold that already happened — the `Commit`-vs-`Abandon` confusion `nextAfterAttempt`
   *  exists to prevent, and the reason `pending_movies` once grew without bound. A stranded
   *  side row is the recoverable direction: it stays under the old id, where it remains the
   *  only copy, and `ReapOrphanedFilmRows` clears it out of band. */
  Seq("reports failure" -> false, "raises" -> true).foreach { case (label, raise) =>
    it should s"commit the fold even when carrying a retired key's cinemas $label" in {
      val client     = MongoClient(uri)
      val db         = client.getDatabase(dbName)
      val connection = new MongoConnection(Some(uri), dbName, required = false)
      val movies     = db.getCollection(services.movies.MovieRepository.Collection)
      val staging    = db.getCollection(StagingRepository.Collection)
      try {
        val stagingId = seedTwoYearVariants(movies, staging)
        val repository = new UnmovableFilmRepository(raise)
        val folder     = new MongoStagingFolder(connection, normalizer = titleNormalizer,
          movieRepository = repository)

        noException should be thrownBy folder.foldGroup(title)

        // Without this the test passes just as well when the migration is never attempted,
        // which is precisely the state this suite is here to rule out.
        val attempted = repository.attempts.get()
        withClue("no migration was attempted, so the failure branch under test never ran: ")(
          attempted should be > 0)

        withClue("the fold did not consume its staging row, so it never reached the " +
                 "migration and this asserts nothing: ")(
          Await.result(staging.find(Filters.eq("_id", stagingId)).toFuture(), 10.seconds) shouldBe empty)
        withClue("the fold collapsed nothing, so no key was retired and no migration was " +
                 "attempted: ")(
          Await.result(movies.find(Filters.regex("_id", s"^$sanitize\\|")).toFuture(), 10.seconds)
            .size shouldBe 1)
      } finally {
        Await.ready(movies.deleteMany(Filters.regex("_id", s"^$sanitize\\|")).toFuture(), 10.seconds)
        Await.ready(staging.deleteMany(Filters.regex("_id", s".*$sanitize.*")).toFuture(), 10.seconds)
        client.close()
      }
    }
  }

  /** Reads succeed for the transaction body and fail afterwards, which is what separates the
   *  two guards: `stitchedCinemaTitles` reads once per group row INSIDE the transaction,
   *  `completeSideCollections` reads again per upsert AFTER the commit. Counts `upsert` so the
   *  assertion can be on the write that must not happen, rather than on a downstream shape
   *  that a no-op would satisfy anyway. */
  private class FailAfterPlanningRepository extends UnreadableByIdMovieRepository {
    failing = false
    val completionWrites = new java.util.concurrent.atomic.AtomicInteger(0)
    private val reads = new java.util.concurrent.atomic.AtomicInteger(0)
    override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) =
      if (reads.incrementAndGet() > 1) (None, false) else super.findByIdChecked(id)
    override def upsert(t: String, y: Option[Int], e: models.MovieRecord): Unit = {
      completionWrites.incrementAndGet()
      super.upsert(t, y, e)
    }
  }

  it should "skip the completion write when the film cannot be read back, rather than write one that deletes its cinemas" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    val staging    = db.getCollection(StagingRepository.Collection)
    val repository = new FailAfterPlanningRepository
    val existing   = StoredMovieRecord.idFor(title, Some(2026), titleNormalizer)
    try {
      Await.result(movies.replaceOne(Filters.eq("_id", existing),
        org.mongodb.scala.Document("_id" -> existing, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      val stagingId = s"${Multikino.displayName}|$sanitize|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> tmdbId,
          "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
            org.mongodb.scala.Document("title" -> title)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      // The fold must COMMIT (the transaction succeeded) and then decline the completion.
      new MongoStagingFolder(connection, normalizer = titleNormalizer,
        movieRepository = repository).foldGroup(title)

      withClue("the staging row was not consumed, so the fold never got past planning and " +
               "this asserts nothing about the completion guard: ")(
        Await.result(staging.find(Filters.eq("_id", stagingId)).toFuture(), 10.seconds) shouldBe empty)

      val writes = repository.completionWrites.get()
      withClue("the completion wrote the film after a FAILED read-back — that write names no " +
               "cinemas, and `replaceFilm` deletes every slot and screening it does not name: ")(
        writes shouldBe 0)
    } finally {
      Await.ready(movies.deleteMany(Filters.regex("_id", s"^$sanitize\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*$sanitize.*")).toFuture(), 10.seconds)
      client.close()
    }
  }
}
